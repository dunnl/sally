msgApp = function () {

    var prependLi = function(ul, li, nlis) {
        ul.insertBefore(li, ul.childNodes[0]);
        if (length(ul.childNodes) > nlis) {
            ul.removeChild(ul.lastChild);
        }
    }
    var appendLi = function(ul, li, nlis) {
        ul.appendChild(li);
        if (length(ul.childNodes) > nlis) {
            ul.removeChild(ul.firstChild);
        }
    }
    var attachApp = function(ul, nlis, appendAtTop) {
        var appendTextLi = function(txt) {
            var newli = document.createElement('li');
            newli.appendChild(document.createTextNode(txt));
            if (appendAtTop) {
                prependLi;
            } else {
                appendLi;
            }
        }
        var appendHtmlLi = function(html) {
            var newli = document.createElement('li');
            newli.innerHTML = html;
            ul.appendChild(newli);
        }
        ul.appendTextLi = appendTextLi;
        ul.appendHtmlLi = appendHtmlLi;
    }

    return {
        "attachToUl" : attachApp
    }
}();

guessApp = function () {
    console.log("Running guessApp");
    var cmdExp = /\w*/;
    var socket = new WebSocket('ws://localhost:8080');
    var guess  = new Object();
        guess.form   = document.getElementById('guess.form');
        guess.likes  = document.getElementById('guess.likes')
        guess.butNot = document.getElementById('guess.butnot');

    var guessUl   = document.getElementById('guess-ul');
    var messageUl = document.getElementById('message.ul');

    msgApp.attachToUl(messageUl);
    msgApp.attachToUl(guessUl);

    var appClientMsg = function(txt) {
        messageUl.appendHtmlLi("<span class=\"client-msg\">Client</span>: " + txt);
    }

    var appSysMsg = function(txt) {
        messageUl.appendHtmlLi("<span class=\"system-msg\">Server</span>: " + txt);
    }

    var appGuess = function(txt) {
        guessUl.appendHtmlLi("<span class=\"system-msg\">Server</span>: " + txt);
    }

    sendGuess = function(socket, newGuess) {
        var newMessage = new Object();
        newMessage.message = "guess";
        newMessage.body = newGuess;
        socket.send(JSON.stringify(newMessage));
    }

    // misc functions
    cutInput = function (el) {
        console.log(el);
        var ret = el.value
            el.value = ""
        return ret;
    }

    submitGuess = function(socket) {
        return function (e) {
            if (e.preventDefault) e.preventDefault();

            var newGuess = new Object();
            newGuess.likes  = cutInput(guess.likes);
            newGuess.butnot = cutInput(guess.butNot);


            appClientMsg("Submitting guess");
            sendGuess(socket,newGuess);
        }
    }

    socket.onopen = function (e) {
        appClientMsg("Socked opened successfully");
    }
    socket.onclose = function (e) {
        appClientMsg("Closing socket");
    }
    socket.onmessage = function (e) {
        console.log(e.data)
        var obj = JSON.parse(e.data);
        switch(obj.message) {
            case "guess":
                console.log("Received a guess");
                break;
            case "control":
                console.log("Received a system message");
                appSysMsg(obj.body);
                break;
            default:
               console.log("Received a strange message");
            }
    }
    guess.form.addEventListener('submit', submitGuess(socket));
    return 0;
}

window.onload = guessApp;
