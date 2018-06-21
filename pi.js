function ProcScope() {
    this.count = 0;
    this.varnames = [];
    this.varnums = [];
}

ProcScope.prototype.contains = function(name) {
    return (this.varnames.indexOf(name) !== -1);
};

ProcScope.prototype.get = function(name) {
    var index = this.varnames.indexOf(name);
    return this.varnums[index];
};

ProcScope.prototype.incr = function() {
    var c = this.count;
    this.count += 1;
    return c;
};

ProcScope.prototype.new_var = function(name) {
    if (this.contains(name)) { throw "Error"; }
    var c = this.incr();
    this.varnames.push(name);
    this.varnums.push(c);
    return c;
};


function PScope(parent) {
    this.parent = parent;
    this.varnames = [];
    this.varnums = [];
}

PScope.prototype._contains = function(name) {
    return (this.varnames.indexOf(name) !== -1);
};

PScope.prototype._get = function(name) {
    var index = this.varnames.indexOf(name);
    return this.varnums[index];
};

PScope.prototype.contains = function(name) {
    if (this._contains(name)) {
        return true;
    } else {
        return this.parent.contains(name);
    }
};

PScope.prototype.get = function(name) {
    if (this._contains(name)) {
        return this._get(name);
    } else {
        return this.parent.get(name);
    }
};

PScope.prototype.incr = function() {
    return this.parent.incr();
};

PScope.prototype.new_var = function(name) {
    if (this._contains(name)) { throw "Error"; }
    var c = this.incr();
    this.varnames.push(name);
    this.varnums.push(c);
    return c;
};


function Label() {
    this.pos = null;
}


function mark_label(asms) {
    var l = 0;

    for (let asm of asms) {
        if (asm[0] === 'label') {
            asm[1].pos = l;
        } else {
            l += 1;
        }
    }

    var new_asms = [];

    for (let a of asms) {
        if (a[0] === 'label') {
            continue;
        } else if (a[0] === 'goto') {
            new_asms.push(['goto', a[1].pos]);
        } else if (a[0] === 'branch') {
            new_asms.push(['branch', a[1].pos]);
        } else {
            new_asms.push(a);
        }
    }

    return new_asms;
}


function compile_p(scope, asms, p) {
    var type = p[0];

    if (type === 'seq') {
        compile_p(scope, asms, p[1]);
        compile_p(scope, asms, p[2]);
    } else if (type === 'branch') {
        var l1 = new Label();
        var l2 = new Label();
        asms.push(['branch', l1]);
        compile_p(new PScope(scope), asms, p[1]);
        asms.push(['goto', l2], ['label', l1]);
        compile_p(new PScope(scope), asms, p[2]);
        asms.push(['label', l2]);
    } else if (type === 'invoke') {
        var [name, args] = p[1];
        asms.push(["invoke", name, args.map(arg => scope.get(arg))]);
    } else if (type === 'channel') {
        var new_scope = new PScope(scope);
        for (let arg of p[1]) {
            asms.push(['channel', new_scope.new_var(arg)]);
        }
        compile_p(new_scope, asms, p[2]);
    } else if (type === 'button') {
        var new_scope = new PScope(scope);
        for (let arg of p[1]) {
            asms.push(['button', new_scope.new_var(arg)]);
        }
        compile_p(new_scope, asms, p[2]);
    } else if (type === 'when') {
        var [name, args] = p[1];
        asms.push(["when", name, args.map(arg => scope.get(arg))]);
    } else if (type === 'send') {
        asms.push(['send', scope.get(p[1]), scope.get(p[2])]);
    } else if (type === 'recv') {
        asms.push(['recv', scope.new_var(p[1]), scope.get(p[2])]);
    } else if (type === 'attach') {
        asms.push(['attach', scope.get(p[1]), p[2]]);
    } else if (type === 'detach') {
        asms.push(['detach', scope.get(p[1])]);
    } else if (type === 'wait') {
        asms.push(['wait', scope.get(p[1])]);
    } else if (type === 'title') {
        asms.push(['title', scope.get(p[1]), p[2]]);
    } else if (type === 'style') {
        asms.push(['style', scope.get(p[1]), p[2]]);
    } else {
        throw "NEVER REACH HERE";
    }
}


function compile_pdef(pdef) {
    var [_, [name, args], body] = pdef;
    var scope = new ProcScope();
    var asms = [];

    for (let arg of args) {
        scope.new_var(arg);
    }

    compile_p(scope, asms, body);
    asms.push(['return']);

    return [name, args.length, scope.count, mark_label(asms)];
}


function compile_code(code) {
    return code.map(pdef => compile_pdef(pdef));
}


function parse_code(s) {
    if (s === "") {
        return [true, []];
    }

    var result = PiLexer.tokenize(s);

    if (!result[0]) {
        return result;
    }

    if (result[1].length === 1) {
        return [true, []];
    }

    var ast = PiParser.parse_code(result[1]);
    if (ast === false) {
        return [false, "syntax error in rules"];
    }

    return [true, compile_code(ast)];
}




function Frame(parent, code, pc, vars) {
    this.parent = parent;
    this.code = code;
    this.pc = pc;
    this.vars = vars;
}


Frame.prototype.copy = function() {
    if (this.parent === null) {
        return new Frame(null, this.code, this.pc, this.vars);
    }
    return new Frame(this.parent.copy(), this.code, this.pc, this.vars);
};


function Channel(num) {
    this.num = num;
    this.senders = [];
    this.receivers = [];
}


function Button(num) {
    this.num = num;
    this.pos = null;
    this.waiting = null;
    this.elem = null;
    this.title = "go";
    this.style = "go";
}


Button.prototype.attach = function(pos) {
    this.pos = pos;
};


Button.prototype.detach = function(pos) {
    this.pos = null;
};


Button.prototype.wait = function(thread) {
    if (this.waiting !== null) {
        throw "ERROR wait";
    }
    this.waiting = thread;
};


function Thread(num, stack) {
    this.num = num;
    this.stack = stack;
    this.blocked = false;
}


function *range(begin, end) {
    for (var i=begin; i<end; i++) {
        yield i;
    }
}


function State(procs) {
    this.pnames = [];
    this.pcodes = [];

    for (let [name,args,vars,insts] of procs) {
        this.pnames.push(name);
        this.pcodes.push([args, vars, insts]);
    }

    this.channel_count = 0;
    this.thread_count = 0;
    this.button_count = 0;
    this.threads = [];
    this.channels = [];
    this.buttons = [];

    this.new_thread(this.call(null, "main", []));
}


State.prototype.new_thread = function(frame) {
    var thread = new Thread(this.thread_count, frame);
    this.thread_count += 1;
    this.step_thread(thread);
    this.threads.push(thread);
    return thread;
};


State.prototype.new_channel = function() {
    var channel = new Channel(this.channel_count);
    this.channel_count += 1;
    this.channels.push(channel);
    return channel;
};


State.prototype.new_button = function() {
    var button = new Button(this.button_count);
    this.button_count += 1;
    this.buttons.push(button);
    return button;
};


State.prototype.call = function(stack, name, args) {
    var i = this.pnames.indexOf(name);
    if (i === -1) {
        throw "ERROR call name";
    }

    var [argc, varc, insts] = this.pcodes[i];

    if (argc !== args.length) {
        throw "ERROR call args";
    }

    var vars = args.map(c => c);

    var frame = new Frame(stack, insts, 0, vars);
    return frame;
};


State.prototype.step = function() {
    for (let chan of this.channels) {
        if ((chan.senders.length === 0) || (chan.receivers.length === 0)) {
            continue;
        }

        var sender = chan.senders.shift();
        var receiver = chan.receivers.shift();

        var sender_inst = sender.stack.code[sender.stack.pc];
        var receiver_inst = receiver.stack.code[receiver.stack.pc];
        var message = sender.stack.vars[sender_inst[1]];
        receiver.stack.vars[receiver_inst[1]] = message;

        sender.stack.pc += 1;
        receiver.stack.pc += 1;
        sender.blocked = false;
        receiver.blocked = false;

        this.step_thread(sender);
        this.step_thread(receiver);

        return false;
    }

    if (this.blocked() === true) {
        return true;
    }

    for (let thread of this.threads) {
        if (thread.blocked === true) {
            continue;
        }

        var stack = thread.stack;

        if (stack === null) {
            continue;
        }

        var inst = stack.code[stack.pc];

        if (inst[0] !== 'branch') {
            continue;
        }

        var frame = stack.copy();
        stack.pc += 1;
        this.step_thread(thread);

        frame.pc = inst[1];
        var new_thread = this.new_thread(frame);
        return false;
    }


    for (let i of range(0, this.threads.length)) {
        var thread = this.threads[i];

        if (thread.blocked === true) {
            continue;
        }

        var stack = thread.stack;
        if (stack === null) {
            thread.blocked = true;
            continue;
        }

        var inst = stack.code[stack.pc];

        if (inst[0] === 'channel') {
            var channel = this.new_channel();
            stack.vars[inst[1]] = channel;
            stack.pc += 1;
        } else if (inst[0] === 'button') {
            var button = this.new_button(thread);
            stack.vars[inst[1]] = button;
            stack.pc += 1;
        } else if (inst[0] === 'send') {
            stack.vars[inst[2]].senders.push(thread);
            thread.blocked = true;
        } else if (inst[0] === 'recv') {
            stack.vars[inst[2]].receivers.push(thread);
            thread.blocked = true;
        } else if (inst[0] === 'attach') {
            stack.vars[inst[1]].attach(inst[2]);
            stack.pc += 1;
        } else if (inst[0] === 'detach') {
            stack.vars[inst[1]].detach();
            stack.pc += 1;
        } else if (inst[0] === 'wait') {
            stack.vars[inst[1]].wait(thread);
            stack.pc += 1;
            thread.blocked = true;
        } else if (inst[0] === 'title') {
            stack.vars[inst[1]].title = inst[2];
            stack.pc += 1;
        } else if (inst[0] === 'style') {
            stack.vars[inst[1]].style = inst[2];
            stack.pc += 1;
        } else if (inst[0] === 'invoke') {
            var frame = this.call(stack, inst[1], inst[2].map(a => stack.vars[a]));
            thread.stack = frame;
        } else {
            throw "NEVER REACH HERE";
        }

        this.step_thread(thread);
    }

    return false;
};


State.prototype.step_thread = function(thread) {
    do {
        if (thread.blocked === true) {
            return;
        }

        var stack = thread.stack;
        var inst = stack.code[stack.pc];

        if (inst[0] === 'return') {
            thread.stack = stack.parent;
            if (thread.stack === null) {
                return;
            } else {
                thread.stack.pc += 1;
            }
        } else if (inst[0] === 'goto') {
            stack.pc = inst[1];
        } else {
            return;
        }
    } while(true);
};


State.prototype.blocked = function() {
    for (let thread of this.threads) {
        if (thread.blocked !== true) {
            return false;
        }
    }

    return true;
};


State.prototype.run = function() {
    while(this.step() === false) {}
};
