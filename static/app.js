(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
"use strict";

var _lists = require("./lists");

var MsgApp = _interopRequireWildcard(_lists);

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } else { var newObj = {}; if (obj != null) { for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) newObj[key] = obj[key]; } } newObj.default = obj; return newObj; } }

// An module for submitting and processing SS guesses over a websocket. Requires
// MsgApp for displaying game state and program messages
var SallyGame = function SallyGame(socketUrl, gameElts, messageList, gameList) {

    //We will later set uuid to be the UUID assigned to us.
    this.uuid;

    this.viewAll = false;

    //altGame will show all guesses
    var altGameList = gameList.cloneNode(true);
    altGameList.id = "alt-game-list";
    altGameList.style.display = "none";
    gameList.parentNode.insertBefore(altGameList, gameList);

    //Initialize message app on the two lists
    var msgApp = new MsgApp.App(messageList, 10, MsgApp.order["AppendAtBottom"]);
    var gameApp = new MsgApp.App(gameList, 8, MsgApp.order["AppendAtTop"]);
    var altGameApp = new MsgApp.App(altGameList, 8, MsgApp.order["AppendAtTop"]);

    var switchViews = function switchViews() {
        if (this.viewAll) {
            gameList.style.display = "none";
            altGameList.style.display = "block";
        } else {
            gameList.style.display = "block";
            altGameList.style.display = "none";
        }
    };

    //Clear server-generated list contents in order to process them ourselves
    gameApp.clearAll();

    var socket = new WebSocket(socketUrl);

    socket.onerror = function (e) {
        appClientMsg("Socked experienced an error");
    };
    socket.onopen = function (e) {
        appClientMsg("Socked opened successfully");
    };
    socket.onclose = function (e) {
        appClientMsg("Closing socket");
    };
    socket.onmessage = function (e) {
        var obj = JSON.parse(e.data);
        switch (obj.type) {
            case "guess":
                displayGuess.bind(this, obj.body);
                break;
            case "control":
                appSysMsg(obj.body);
                break;
            case "uuid":
                this.uuid = obj.body;
                break;
            default:
                appSysMsg("Received a strange message:" + JSON.stringify(obj, null, 2));
        }
    };

    var appClientMsg = function appClientMsg(txt) {
        var span = document.createElement("span");
        span.className = "client-msg";
        span.appendChild(document.createTextNode("Client: "));
        var txtnd = document.createTextNode(txt);
        msgApp.pushNewLiWith([span, txtnd]);
    };

    var appSysMsg = function appSysMsg(txt) {
        var span = document.createElement("span");
        span.className = "server-msg";
        span.appendChild(document.createTextNode("Server: "));
        var txtnd = document.createTextNode(txt);
        msgApp.pushNewLiWith([span, txtnd]);
    };

    var mkGuessNodes = function mkGuessNodes(gsRes) {
        var msg = document.createElement("p");
        var meta = document.createElement("p");

        var date = new Date();
        date.setTime(Date.parse(gsRes.resTime));
        var dateStr = moment(date).utc().format("MM/DD/YYYY HH:mm");

        msg.innerHTML = "Silly sally likes " + "<span class=\"big\">" + gsRes.resGs.gsLikes + "</span>, " + "but not " + "<span class=\"big\">" + gsRes.resGs.gsNotLikes + "</span>. ";

        if (gsRes.resValid) {
            msg.innerHTML += "<span class=\"true\">Correct</span>";
        } else {
            msg.innerHTML += "<span class=\"false\">Wrong</span>";
        }
        meta.innerHTML += "Submitted <span class=\"time\">" + dateStr + " UST</span>";
        return [msg, meta];
    };

    var displayGuess = function displayGuess(gsRes) {
        var nodes = mkGuessNodes(gsRes);
        altGameApp.pushNewLiWith(nodes);
        if (gsRes.resGs.user === this.uuid) {
            gameApp.pushNewLiWith(nodes);
        }
    };

    var sendGuess = function sendGuess(newGuess) {
        var newMsg = {};
        newMsg.body = newGuess;
        newMsg.type = "guess";
        socket.send(JSON.stringify(newMsg));
    };

    var cutValOfNode = function cutValOfNode(el) {
        var ret = el.value;
        el.value = "";
        return ret;
    };

    var submitGuessForm = function submitGuessForm(socket) {
        return function (e) {
            if (e.preventDefault) e.preventDefault();

            var newGuess = {
                clLikes: cutValOfNode(gameElts.likes),
                clNotLikes: cutValOfNode(gameElts.notlikes)
            };
            appClientMsg("Submitting guess");
            sendGuess(newGuess);
        };
    };

    gameElts.form.addEventListener('submit', submitGuessForm(socket));

    var exports = {};
    return exports;
};

window.onload = function () {

    var gameElts = {
        form: document.getElementById('guess-form'),
        likes: document.getElementById('guess.likes'),
        notlikes: document.getElementById('guess.notlikes')
    };

    var messageUl = document.getElementById('message-list');
    var gameUl = document.getElementById('game-list');

    new SallyGame("ws://localhost:8080", gameElts, messageUl, gameUl);

    return true;
};

},{"./lists":2}],2:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
    value: true
});

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var order = exports.order = {
    "AppendAtTop": 1,
    "AppendAtBottom": 2

    // A module for writing items to lists, with optional maximums and direction
    // Internal utility
};var addLi = function addLi(list, li, maxItems, appendWhere) {

    var addLiTop = function addLiTop(list, li, maxItems) {
        list.insertBefore(li, list.childNodes[0]);
        if (list.childNodes.length > maxItems) {
            list.removeChild(list.lastChild);
        }
    };
    var addLiBottom = function addLiBottom(list, li, maxItems) {
        list.appendChild(li);
        if (list.childNodes.length > maxItems) {
            list.removeChild(list.firstChild);
        }
    };

    appendWhere == order["AppendAtTop"] ? addLiTop(list, li, maxItems) : addLiBottom(list, li, maxItems);
};

var App = exports.App = function () {
    function App(listNode, maxItems, msgOrder) {
        _classCallCheck(this, App);

        this.listNode = listNode;
        this.maxItems = maxItems;
        this.msgOrder = msgOrder;
    }

    _createClass(App, [{
        key: "pushNewLiWith",
        value: function pushNewLiWith(nodes) {
            var newli = document.createElement('li');
            nodes.forEach(function (n) {
                newli.appendChild(n);
            });
            addLi(this.listNode, newli, this.maxItems, this.appendWhere);
        }
    }, {
        key: "pushTextLi",
        value: function pushTextLi(txt) {
            var node = document.createTextNode(txt);
            this.pushNewLiWith([node]);
        }
    }, {
        key: "clearAll",
        value: function clearAll(txt) {
            while (this.listNode.lastChild) {
                this.listNode.removeChild(this.listNode.lastChild);
            }
        }
    }]);

    return App;
}();

},{}]},{},[1]);
