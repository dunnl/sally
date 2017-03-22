var likesInput;
var butnotInput;
var wlu;
var glu;
var username;
var cmdExp = /\w*/

var setup = function() {

    likesInput = document.getElementById('guess.likes')
    butnotInput = document.getElementById('guess.butnot');
    wlu = document.getElementById("websocket-ul");
    glu = document.getElementById("guess-ul");
    username = document.getElementById("username.input");

    var socket = new WebSocket('ws://localhost:8080');

    socket.onopen = function (e) {
        appendTextLi(wlu, "socket.onopen(): Called, sending random number");
        var rn = Math.random()
        sendEcho(socket,rn);
        }
    socket.onclose = function (e) {
        appendTextLi(wlu, "socket.onclose(): Called");
        }
    socket.onmessage = function (e) {
        switch(cmdExp.exec(e.data)[0]) {
            case "Guess":
                console.log(e.data)
                console.log("Received a guess");
                payload = e.data.split("::")[1];
                console.log(payload);
                appendGuess(JSON.parse(payload));
                msg = JSON.stringify(JSON.parse(payload));
                break;
            case "Echo":
                console.log("Received an echo");
                msg = e.data;
                break;
            default:
                console.log("Received something else: " + e.data);
            }
        appendTextLi(wlu, "socket.onmessage(): " + msg);
        }
    document.getElementById('guess.form').addEventListener('submit', intercept(socket));
    document.getElementById('chat.form').addEventListener('submit', chat(socket));
    }

sendGuess = function(socket, likes, butnot, un) {
    socket.send("Guess::"+un+","+likes+","+butnot);
}

sendEcho = function(socket, toEcho) {
    socket.send("Echo::"+toEcho);
}
sendName = function(socket, newName) {
    socket.send("Name::"+toEcho);
}
sendBroadcast = function(socket, un, msg) {
    socket.send("Broadcast::"+un+","+msg);
}

appendTextLi = function(ul, txt) {
    var newli = document.createElement('li');
    newli.appendChild(document.createTextNode(txt));
    ul.appendChild(newli);
}

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
}

window.onload = setup
