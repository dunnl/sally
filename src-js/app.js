import * as MsgApp from "./lists"

// An module for submitting and processing SS guesses over a websocket. Requires
// MsgApp for displaying game state and program messages
const SallyGame = function (socketUrl, gameElts, messageList, gameList) {

    //We will later set uuid to be the UUID assigned to us.
    this.uuid;

    this.viewAll = false;

    //altGame will show all guesses
    var altGameList = gameList.cloneNode(true);
    altGameList.id = "alt-game-list";
    altGameList.style.display = "none";
    gameList.parentNode.insertBefore(altGameList, gameList);

    //Initialize message app on the two lists
    const msgApp   = new MsgApp.App(messageList, 10, MsgApp.order["AppendAtBottom"]);
    const gameApp = new MsgApp.App(gameList, 8, MsgApp.order["AppendAtTop"]);
    const altGameApp = new MsgApp.App(altGameList, 8, MsgApp.order["AppendAtTop"]);

    const switchViews = function () {
        if (this.viewAll) {
            gameList.style.display="none";
            altGameList.style.display="block";
        } else {
            gameList.style.display="block";
            altGameList.style.display="none";
        }
    }

    //Clear server-generated list contents in order to process them ourselves
    gameApp.clearAll();

    const socket = new WebSocket(socketUrl);

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
        var obj = JSON.parse(e.data);
        switch(obj.type) {
            case "guess":
                displayGuess.bind(this, obj.body)
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
    }

    const appClientMsg = function (txt) {
        var   span = document.createElement("span");
              span.className = "client-msg";
              span.appendChild(document.createTextNode("Client: "));
        const txtnd = document.createTextNode(txt);
        msgApp.pushNewLiWith([span, txtnd]);
    }

    const appSysMsg = function (txt) {
        var   span = document.createElement("span");
              span.className = "server-msg";
              span.appendChild(document.createTextNode("Server: "));
        const txtnd = document.createTextNode(txt);
        msgApp.pushNewLiWith([span, txtnd]);
    }

    const mkGuessNodes = function (gsRes) {
        var msg = document.createElement("p");
        var meta = document.createElement("p");

        var date = new Date();
        date.setTime(Date.parse(gsRes.resTime));
        var dateStr = moment(date).utc().format("MM/DD/YYYY HH:mm");

        msg.innerHTML = "Silly sally likes " 
          + "<span class=\"big\">" + gsRes.resGs.gsLikes+"</span>, "
          + "but not "
          + "<span class=\"big\">" + gsRes.resGs.gsNotLikes+"</span>. ";

        if (gsRes.resValid) {
            msg.innerHTML += "<span class=\"true\">Correct</span>";
        } else {
            msg.innerHTML += "<span class=\"false\">Wrong</span>";
        }
        meta.innerHTML += "Submitted <span class=\"time\">" + dateStr + " UST</span>";
        return [msg, meta];
    }

    const displayGuess = function (gsRes) {
        const nodes = mkGuessNodes(gsRes);
        altGameApp.pushNewLiWith(nodes);
        if (gsRes.resGs.user === this.uuid) {
            gameApp.pushNewLiWith(nodes);
        }
    }

    const sendGuess = function (newGuess) {
        var newMsg = {};
        newMsg.body = newGuess;
        newMsg.type = "guess";
        socket.send(JSON.stringify(newMsg));
    }

    const cutValOfNode = function (el) {
        var ret = el.value;
        el.value = "";
        return ret;
    }

    var submitGuessForm = function (socket) {
        return function (e) {
            if (e.preventDefault) e.preventDefault();

            const newGuess = {
                  clLikes : cutValOfNode(gameElts.likes)
                , clNotLikes : cutValOfNode(gameElts.notlikes)
            }
            appClientMsg("Submitting guess");
            sendGuess(newGuess);
        }
    }

    gameElts.form.addEventListener('submit', submitGuessForm(socket));

    const exports = {};
    return exports;
}

window.onload = function () {

    var gameElts = {
          form     : document.getElementById('guess-form')
        , likes    : document.getElementById('guess.likes')
        , notlikes : document.getElementById('guess.notlikes')
    }

    const messageUl = document.getElementById('message-list');
    const gameUl = document.getElementById('game-list');

    new SallyGame("ws://localhost:8080", gameElts, messageUl, gameUl);

    return true;
}
