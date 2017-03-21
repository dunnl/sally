var likesInput;
var butnotInput;
var wlu;
var username;

var setup = function() {

    likesInput = document.getElementById('guess.likes')
    butnotInput = document.getElementById('guess.butnot');
    wlu = document.getElementById("websocket-ul");
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
        appendTextLi(wlu, "socket.onmessage(): " + e.data);
        }
    document.getElementById('guess.form').addEventListener('submit', intercept(socket));
    document.getElementById('chat.form').addEventListener('submit', chat(socket));
    }

sendGuess = function(socket, likes, butnot, un) {
    socket.send("Guess:"+un+","+likes+","+butnot);
}

sendEcho = function(socket, toEcho) {
    socket.send("Echo:"+toEcho);
}
sendName = function(socket, newName) {
    socket.send("Name:"+toEcho);
}
sendBroadcast = function(socket, un, msg) {
    socket.send("Broadcast:"+un+","+msg);
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

window.onload = setup
