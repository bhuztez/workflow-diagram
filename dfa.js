var PiLexer = (
    function() {
        var CODE = {
            START: 0,
            MATCH: {
                '0': [],
                '1': [0],
                '2': [8],
                '3': [9],
                '4': [1],
                '5': [2],
                '6': [3],
                '7': [4],
                '8': [5],
                '9': [6],
                '10': [7]},

            DFA: {
                '0': {'0': 1, '1': 4, '2': 5, '3': 6, '4': 7, '5': 8, '6': 9, '7': 10, '8': 2, '9': 3},
                '1': {'0': 1},
                '2': {'8': 2, '9': 2, '10': 2},
                '3': {'8': 3, '9': 3, '10': 3},
                '4': {},
                '5': {},
                '6': {},
                '7': {},
                '8': {},
                '9': {},
                '10': {}}
        };

        var CODE_MAP = [-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,4,5,-1,-1,1,-1,-1,-1,10,10,10,10,10,10,10,10,10,10,-1,2,-1,-1,-1,-1,-1,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,-1,-1,-1,-1,10,-1,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,6,3,7,-1,-1];

        var MODE = [[CODE, CODE_MAP]];

        function next_mode(match, mode) {
            return mode;
        }

        function finish(state, dfa) {
            var match = dfa.MATCH[state] || [];

            if (match.length === 0) {
                return -1;
            } else {
                return match[0];
            }
        }

        function tokens(s, found, state, mode) {
            var result = [];

            while (s.length > 0) {
                var [dfa, charmap] = MODE[mode];
                var code = s.charCodeAt(0);

                code = (code>127)?-1:charmap[code];
                var next_state = (dfa.DFA[state] || {})[code];

                if (next_state === undefined) {
                    next_state = -1;
                }

                if (next_state >= 0) {
                    found += s[0];
                    state = next_state;
                    s = s.slice(1);
                } else {
                    var match = finish(state, dfa);

                    if (match < 0) {
                        return [false, mode, found];
                    }

                    result.push([mode, match, found]);

                    mode = next_mode(match, mode);
                    found = "";
                    state = MODE[mode][0].START;
                }

            }

            var match = finish(state, dfa);

            if (match < 0) {
                return [false, mode, found];
            }

            result.push([mode, match, found]);
            return [true, result];
        }

        function convert_tokens(tokens) {
            var result = [];

            for (let token of tokens) {
                var [mode, match, s] = token;

                if (match === 0) {
                    continue;
                }

                if (match === 9) {
                    if (s === "proc") {
                        result.push([11, s]);
                    } else if (s === "when") {
                        result.push([12, s]);
                    } else if (s === "channel") {
                        result.push([13, s]);
                    } else if (s === "in") {
                        result.push([14, s]);
                    } else if (s === "send") {
                        result.push([15, s]);
                    } else if (s === "to") {
                        result.push([16, s]);
                    } else if (s === "recv") {
                        result.push([17, s]);
                    } else if (s === "from") {
                        result.push([18, s]);
                    } else if (s === "button") {
                        result.push([19, s]);
                    } else if (s === "attach") {
                        result.push([20, s]);
                    } else if (s === "at") {
                        result.push([21, s]);
                    } else if (s === "detach") {
                        result.push([22, s]);
                    } else if (s === "wait") {
                        result.push([23, s]);
                    } else if (s === "set") {
                        result.push([24, s]);
                    } else if (s === "title") {
                        result.push([25, s]);
                    } else if (s === "style") {
                        result.push([26, s]);
                    } else {
                        result.push([match, s]);
                    }
                } else {
                    result.push([match, s]);
                }
            }

            result.push([-1, null]);
            return result;
        }


        function tokenize(s) {
            var result = tokens(s, '', MODE[0][0].START, 0);

            if (result[0]) {
                return [true, convert_tokens(result[1])];
            } else {
                return result;
            }
        }

        return {tokenize: tokenize};
    })();
