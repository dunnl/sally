myChat = function () {
    var wsForm;
    var cmdExp = /\w*/;
    var socket = new WebSocket('ws://localhost:8080');
    var guess = new Object();
    var ws    = new Object();
    var appendTextLi = function(ul, txt) {
        var newli = document.createElement('li');
        newli.appendChild(document.createTextNode(txt));
        ul.appendChild(newli);
    }
    var appendHtmlLi = function(ul, html) {
        var newli = document.createElement('li');
        newli.innerHTML = html;
        ul.appendChild(newli);
    }
    var appClientMsg = function(txt) {
        appendHtmlLi(ws.ul, "<span class=\"client-msg\">Client</span>: " + txt);
    }

    var appSysMsg = function(txt) {
        appendHtmlLi(ws.ul, "<span class=\"system-msg\">Server</span>: " + txt);
    }

    var appChatMsg = function(un,txt) {
        appendHtmlLi(ws.ul, "<span class=\"chat-msg\">"+un+"</span>: " + txt);
    }
    sendGuess = function(socket, likes, butnot, un) {
        socket.send("Guess::"+un+","+likes+","+butnot);
    }

    sendEcho = function(socket, toEcho) {
        socket.send("Echo::"+toEcho);
    }
    sendName = function(socket, newName) {
        socket.send("Name::"+newName);
    }
    sendBroadcast = function(socket, un, msg) {
        socket.send("Broadcast::"+un+","+msg);
    }

    // misc functions
    cutInput = function (el) {
        var ret = el.value
            el.value = ""
        return ret;
    }

    chat = function(socket) {
        return function (e) {
            if (e.preventDefault) e.preventDefault();
            var msg = cutInput(document.getElementById('chat.input'))
            var un  = username.value
            sendBroadcast(socket, un, msg);
        }
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
    var setup = function() {
        guess.form = document.getElementById('guess.form');
        guess.likes  = document.getElementById('guess.likes')
        guess.butNot = document.getElementById('guess.butnot');
        guess.ul     = document.getElementById('guess-ul');
        ws.ul        = document.getElementById("websocket-ul");
        //ws.username = document.getElementById("username.input");
        appClientMsg("myChat");
        socket.onopen = function (e) {
            appClientMsg("Socked opened successfully");
            if (username.value){
                un = username.value
                appClientMsg("Attempting to register username " + un);
                sendName(socket, un);
            }
        }
        socket.onclose = function (e) {
            appendTextLi(ws.ul, "socket.onclose(): Called");
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
        document.getElementById('chat.form').addEventListener('submit', chat(socket));
    }

    return setup;
}();







window.onload = myChat;
