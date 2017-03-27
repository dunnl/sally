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
            if appendAtTop {
                prependLi
        }
        var appendHtmlLi = function(html) {
            var newli = document.createElement('li');
            newli.innerHTML = html;
            ul.appendChild(newli);
        }
        ul.appendTextLi = appendTextLi;
        ul.appendHtmlLi = appendHtmlLi;
    }

    return attachApp;
}

myApp = function () {
    var cmdExp = /\w*/;
    var socket = new WebSocket('ws://localhost:8080');
    var guess  = new Object();
        guess.form   = document.getElementById('guess.form');
        guess.likes  = document.getElementById('guess.likes')
        guess.butNot = document.getElementById('guess.butnot');

    var guessUl   = document.getElementById('guess-ul');
    var messageUl = document.getElementById('message.ul');

    setupMessages(messageUl);
    setupMessages(guessUl);

    var appClientMsg = function(txt) {
        messageUl.appendHtmlLi("<span class=\"client-msg\">Client</span>: " + txt);
    }

    var appSysMsg = function(txt) {
        messageUl.appendHtmlLi("<span class=\"system-msg\">Server</span>: " + txt);
    }

    var appGuess = function(txt) {
        guessUl.appendHtmlLi("<span class=\"system-msg\">Server</span>: " + txt);
    }

    sendGuess = function(socket, likes, butnot, un) {
        socket.send("Guess::"+un+","+likes+","+butnot);
    }

    // misc functions
    cutInput = function (el) {
        var ret = el.value
            el.value = ""
        return ret;
    }

    intercept = function(socket) {
        return function (e) {
            if (e.preventDefault) e.preventDefault();
            var un = document.getElementById('username.input').value
            var likes = cutInput(likesInput);
            var butnot = cutInput(butnotInput);
            sendGuess(socket, un, likes, butnot);
        }
    }

    appendGuess = function(guess) {
        appendTextLi(glu, "Guessed: " + guess.likes + " but not " + guess.butnot);
        return 0;
    }

    socket.onopen = function (e) {
        appClientMsg("Socked opened successfully");
        if (username.value){
            un = username.value
            appClientMsg("Attempting to register username " + un);
            sendName(socket, un);
        }
    }
    socket.onclose = function (e) {
        appendTextLi(messageUl, "socket.onclose(): Called");
    }
    socket.onmessage = function (e) {
        console.log(e.data)
        command = cmdExp.exec(e.data)[0]
        payload = e.data.split("::")[1];
        switch(command) {
            case "Guess":
                console.log("Received a guess");
                console.log(payload);
                appendGuess(JSON.parse(payload));
                msg = JSON.stringify(JSON.parse(payload));
                appClientMsg(msg);
                break;
            case "Echo":
                console.log("Received an system message");
                appSysMsg(payload);
                break;
            case "Broadcast":
                console.log("Received a chat message");
                un = payload.split(',')[0];
                umsg = payload.split(',')[1];
                appChatMsg(un,umsg);
                break;
            default:
                msg = e.data
                console.log("Received something else: " + e.data);
                appendTextLi(wlu, "socket.onmessage(): " + msg);
            }
    }
    document.getElementById('guess.form').addEventListener('submit', intercept(socket));
    return 0;
};

window.onload = myApp;
