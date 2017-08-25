(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
'use strict';

var _sallygame = require('./sallygame.js');

var _sallygame2 = _interopRequireDefault(_sallygame);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

window.onload = function () {

    var gameElts = {
        form: document.getElementById('guess__form'),
        likes: document.getElementById('guess.likes'),
        notlikes: document.getElementById('guess.notlikes'),
        guessHeader: document.getElementById('guess__header')
    };

    var messageUl = document.getElementById('message__list');
    var gameUl = document.getElementById('game__list');
    var subField = document.getElementById('game__subform');

    var game = new _sallygame2.default("ws://sally.dunnl.io", gameUl, messageUl, gameElts, subField);

    return true;
};

},{"./sallygame.js":3}],2:[function(require,module,exports){
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
};var addLi = function addLi(list, li, maxItems, msgOrder) {

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

    msgOrder == order["AppendAtTop"] ? addLiTop(list, li, maxItems) : addLiBottom(list, li, maxItems);
};

var App = exports.App = function () {
    function App(listNode, maxItems, msgOrder, postAction) {
        _classCallCheck(this, App);

        this.listNode = listNode;
        this.maxItems = maxItems;
        this.msgOrder = msgOrder;
        this.postAction = postAction;
    }

    _createClass(App, [{
        key: "pushNewLiWith",
        value: function pushNewLiWith(nodes) {
            var newli = document.createElement('li');
            nodes.forEach(function (n) {
                newli.appendChild(n);
            });
            addLi(this.listNode, newli, this.maxItems, this.msgOrder);
            this.update();
        }
    }, {
        key: "update",
        value: function update() {
            if (this.postAction) {
                this.postAction();
            }
        }
    }, {
        key: "pushTextLi",
        value: function pushTextLi(txt) {
            var node = document.createTextNode(txt);
            this.pushNewLiWith([node]);
            this.update();
        }
    }, {
        key: "clearAll",
        value: function clearAll(txt) {
            while (this.listNode.lastChild) {
                this.listNode.removeChild(this.listNode.lastChild);
            }
            this.update();
        }
    }, {
        key: "length",
        get: function get() {
            return this.listNode.childNodes.length;
        }
    }]);

    return App;
}();

},{}],3:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
    value: true
});

var _lists = require("./lists");

var MsgApp = _interopRequireWildcard(_lists);

var _sallysocket = require("./sallysocket");

var _sallysocket2 = _interopRequireDefault(_sallysocket);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } else { var newObj = {}; if (obj != null) { for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) newObj[key] = obj[key]; } } newObj.default = obj; return newObj; } }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

// Utility function
var cutValOfNode = function cutValOfNode(el) {
    var ret = el.value;
    el.value = "";
    return ret;
};

var SallyGame = function SallyGame(socketUrl, gameUl, messageUl, gameElts, subscribeForm) {
    var _this = this;

    _classCallCheck(this, SallyGame);

    this.handleClientMsg = function (txt) {
        _this.msgApp.pushNewLiWith(makeMessage("Client", "client-msg", txt));
    };

    this.handleServerMsg = function (txt) {
        _this.msgApp.pushNewLiWith(makeMessage("Server", "server-msg", txt));
    };

    this.setSubscription = function () {
        var fieldset = _this.subscribeForm.elements["subscription"];
        _this.subscription = fieldset.value;
        _this.gameApp.clearAll();
        console.log("Attempting to renew with " + _this.socket);
        _this.socket.renew(_this.subscription);
    };

    this.updateGuessCount = function () {
        if (_this.subscription === "SubAll") {
            var globalFlag = " (global)";
        } else {
            var globalFlag = "";
        }
        switch (_this.gameApp.length) {
            case 0:
                _this.gameElts.guessHeader.innerText = "No guesses yet" + globalFlag;
                break;
            case 1:
                _this.gameElts.guessHeader.innerText = "Last guess" + globalFlag;
                break;
            default:
                _this.gameElts.guessHeader.innerText = "Last " + _this.gameApp.length + " guesses" + globalFlag;
                break;
        }
    };

    this.handleGuess = function (gsRes, isSelf) {
        var nodes = mkGuessNodes(gsRes, isSelf);
        _this.gameApp.pushNewLiWith(nodes);
    };

    this.gameApp = new MsgApp.App(gameUl, 8, MsgApp.order["AppendAtTop"], this.updateGuessCount.bind(this));

    this.msgApp = new MsgApp.App(messageUl, 8, MsgApp.order["AppendAtBottom"]);

    this.socket = new _sallysocket2.default(socketUrl, this.handleServerMsg, this.handleClientMsg, this.handleGuess);

    this.socket.install(this.setSubscription.bind(this));

    this.subscription = "SubSelf";

    this.subscribeForm = subscribeForm;

    this.gameElts = gameElts;

    subscribeForm.addEventListener('change', function (e) {
        _this.setSubscription();
    });

    this.gameElts.form.addEventListener('submit', function (e) {
        if (e.preventDefault) e.preventDefault();

        var newGuess = {
            clLikes: cutValOfNode(gameElts.likes),
            clNotLikes: cutValOfNode(gameElts.notlikes)
        };

        _this.handleClientMsg("Submitting guess");
        _this.socket.sendGuess(newGuess);
    });
};

exports.default = SallyGame;


var makeMessage = function makeMessage(from, className, text) {
    var span = document.createElement("span");
    span.className = className;
    span.appendChild(document.createTextNode(from + ": "));
    var txtnd = document.createTextNode(text);
    return [span, txtnd];
};

var mkGuessNodes = function mkGuessNodes(gsRes, isSelf) {
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
    if (!isSelf) {
        meta.innerHTML += "Submitted by " + gsRes.resGs.gsUser + " at <span class=\"time\">" + dateStr + " UST</span>";
    } else {
        meta.innerHTML += "Submitted by <span class=\"you\">you</span> at <span class=\"time\">" + dateStr + " UST</span>";
    }
    return [msg, meta];
};

},{"./lists":2,"./sallysocket":4}],4:[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
    value: true
});

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

// An module for submitting and processing SS guesses over a websocket. Requires
// MsgApp for displaying game state and program messages
var _class = function () {
    function _class(socketUrl, handleSysMsg, handleCliMsg, handleGuess) {
        _classCallCheck(this, _class);

        this.socketUrl = socketUrl;
        this.handleSysMsg = handleSysMsg;
        this.handleCliMsg = handleCliMsg;
        this.handleGuess = handleGuess;
        this.uuid = undefined;
        this.self = this;
    }

    _createClass(_class, [{
        key: "install",
        value: function install(postInstallAction) {
            var _this = this;

            if (!this.socketUrl) {
                throw "SallySocket: No socketUrl specified";
            }

            var socket = new WebSocket(this.socketUrl);

            socket.onerror = function (e) {
                _this.handleCliMsg("Socked experienced an error");
            };
            socket.onopen = function (e) {
                _this.handleCliMsg("Socked opened successfully");
                if (postInstallAction) {
                    postInstallAction();
                }
            };
            socket.onclose = function (e) {
                _this.handleCliMsg("Closing socket");
            };
            socket.onmessage = function (e) {
                var obj = JSON.parse(e.data);
                switch (obj.type) {
                    case "guess":
                        console.log(obj.body);
                        _this.handleGuess(obj.body, obj.body.resGs.gsUser === _this.uuid);
                        break;
                    case "control":
                        _this.handleSysMsg(obj.body);
                        break;
                    case "uuid":
                        console.log("Got UUID");
                        _this.uuid = obj.body;
                        break;
                    default:
                        _this.handleCliMsg("Received a strange message:" + JSON.stringify(obj, null, 3));
                }
            };
            this.socket = socket;
        }
    }, {
        key: "renew",
        value: function renew(subscription) {
            var msg = {
                "type": "renew",
                "body": subscription
            };
            try {
                this.socket.send(JSON.stringify(msg));
            } catch (err) {
                console.log("rewnew: socket.send failed");
                console.log(err);
            }
        }
    }, {
        key: "sendGuess",
        value: function sendGuess(newGuess) {
            var newMsg = {
                "body": newGuess,
                "type": "guess"
            };
            try {
                this.socket.send(JSON.stringify(newMsg));
            } catch (err) {
                console.log("sendguess: socket.send failed");
                console.log(err);
            }
        }
    }]);

    return _class;
}();

exports.default = _class;

},{}]},{},[1]);
