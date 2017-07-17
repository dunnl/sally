var MessageAppModule = (function() {
    var exports = {};

    var prependLi = function(ul, li, nlis) {
        ul.insertBefore(li, ul.childNodes[0]);
        console.log("PREPENDING LI");
        if (ul.childNodes.length > nlis) {
            ul.removeChild(ul.lastChild);
        }
    }
    var appendLi = function(ul, li, nlis) {
        ul.appendChild(li);
        if (ul.childNodes.length > nlis) {
            ul.removeChild(ul.firstChild);
        }
    }
    var MessageApp = function(ul, nlis, appendAtTop) {
        this.ul = ul;
        this.nlis = nlis;
        this.appendAtTop = appendAtTop;
    }
    MessageApp.prototype.appendLiNodes = function(nodes) {
        var newli = document.createElement('li');
        nodes.forEach(function (n) {newli.appendChild(n)});
        this.appendAtTop ? prependLi(this.ul, newli, this.nlis) : appendLi(this.ul, newli, this.nlis);
    }
    MessageApp.prototype.appendTextLi = function(txt) {
        var node = document.createTextNode(txt)
        appendLiNode([node]);
    }
    exports.MessageApp = MessageApp;
    return exports;
})();

guessApp = function () {
    var cmdExp = /\w*/;
    var socket = new WebSocket('ws://localhost:8080');
    var guess  = new Object();
        guess.form   = document.getElementById('guess-form');
        guess.likes  = document.getElementById('guess.likes')
        guess.notlikes = document.getElementById('guess.notlikes');
    var guessUl   = document.getElementById('guess-ul');
    var messageUl = document.getElementById('message-ul');

    var msgApp = MessageAppModule;
    commApp  = new msgApp.MessageApp(messageUl, 8, false);
    guessApp = new msgApp.MessageApp(guessUl, 8, true);

    var appClientMsg = function(txt) {
        span = document.createElement("span");
        span.className = "client-msg";
        span.appendChild(document.createTextNode("Client: "));
        txtnd = document.createTextNode(txt);
        commApp.appendLiNodes([span, txtnd]);
    }

    var appSysMsg = function(txt) {
        span = document.createElement("span");
        span.className = "server-msg";
        span.appendChild(document.createTextNode("Server: "));
        txtnd = document.createTextNode(txt);
        commApp.appendLiNodes([span, txtnd]);
    }

    var mkGuess = function(gsRes) {
        var msg = document.createElement("p");
            msg.innerHtml = "Silly sally likes " 
              + "<span class=\"big\">" + gsRes.resGs.likes+"</span>, "
              + "but not "
              + "<span class=\"big\">" + gsRes.resGs.notlikes+"</span>. ";
        if (gsRes.resValid) {
            msg.innerHtml += "<span class=\"true\">Correct</span>";
        } else {
            msg.innerHtml += "<span class=\"false\">Wrong</span>";
        }
        date = new Date();
        date.setTime(Date.parse(gsRes.resTime));
        dateStr = moment(date).utc().format("MM/DD/YYYY HH:mm");
        msg.innerHtml += "<span class=\"time\">" + dateStr + " UST</span>";
        console.log("Pretty message: " + msg.innerHtml)
        return msg;
    }

    var appGuess = function(gsRes) {
        var node = mkGuess(gsRes);
        guessApp.appendLiNodes([node]);
    }

    sendGuess = function(socket, newGuess) {
        var newMessage = new Object();
        newMessage.body = newGuess;
        newMessage.type = "guess";
        socket.send(JSON.stringify(newMessage));
    }

    cutValofNode = function (el) {
        var ret = el.value;
        el.value = "";
        return ret;
    }

    submitGuess = function(socket) {
        return function (e) {
            if (e.preventDefault) e.preventDefault();

            var newGuess = new Object();
            newGuess.notlikes = cutValofNode(guess.notlikes);
            newGuess.likes    = cutValofNode(guess.likes);
            appClientMsg("Submitting guess");
            sendGuess(socket,newGuess);
        }
    }

    socket.onerror = function (e) {
        appClientMsg("Socked experienced an error");
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
        switch(obj.type) {
            case "guess":
                appGuess(obj.body)
                break;
            case "control":
                appSysMsg(obj.body);
                break;
            default:
               console.log("Received a strange message:");
               console.log(JSON.stringify(obb, null, 2));
            }
    }
    guess.form.addEventListener('submit', submitGuess(socket));
    return 0;
}

window.onload = guessApp;
