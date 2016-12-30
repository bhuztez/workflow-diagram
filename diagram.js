function onLoad() {
    var resetButton = document.getElementById("reset");
    var inputTextarea = document.getElementById("input");
    var resultDiv = document.getElementById("result");
    var state;

    function updateResult(text) {
        var newDiv = document.createElement("div");
        newDiv.id = "result";
        newDiv.appendChild(document.createTextNode(text));
        resultDiv.parentNode.replaceChild(newDiv, resultDiv);
        resultDiv = newDiv;
    }

    function reset() {
        var code = parse_code(inputTextarea.value);

        if (code[0] === false) {
            updateResult(code[1]);
            return;
        }

        updateResult("OK");

        for (let container of document.querySelectorAll("div.container")) {
            container.parentNode.replaceChild(container.cloneNode(false), container);
        }

        state = new State(code[1]);
        state.run();
        updateButtons();
    }

    function onClick() {
        var num = Number(this.id.slice(7));
        var button = state.buttons[num];
        button.waiting.blocked = false;
        button.waiting = null;
        state.run();
        updateButtons();
    }

    function updateButtons() {
        for (let button of state.buttons) {
            if (button.pos !== null) {
                if (button.elem === null) {
                    button.elem = document.createElementNS("http://www.w3.org/1999/xhtml", "button");
                    button.elem.id = "button-"+button.num;
                    button.elem.className = button.style;
                    button.elem.appendChild(document.createTextNode(button.title));
                    button.elem.addEventListener('click', onClick);
                    document.getElementById("container-"+button.pos).appendChild(button.elem);
                }
            } else {
                if (button.elem !== null) {
                    if (button.elem.parentNode) {
                        button.elem.parentNode.removeChild(button.elem);
                    }
                    button.elem = null;
                }
            }

            if (button.elem !== null) {
                if (button.waiting !== null) {
                    button.elem.disabled = "";
                } else {
                    button.elem.disabled = "disabled";
                }
            }
        }
    }

    resetButton.addEventListener("click", reset);
    reset();
}

window.addEventListener('load', onLoad);
