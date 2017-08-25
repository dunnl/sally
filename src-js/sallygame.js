import * as MsgApp from "./lists"
import SallySocket from "./sallysocket"

// Utility function
const cutValOfNode = function (el) {
    var ret = el.value;
    el.value = "";
    return ret;
}

export default class SallyGame {
    constructor (socketUrl, gameUl, messageUl, gameElts, subscribeForm) {
        this.gameApp = new MsgApp.App(gameUl, 8, MsgApp.order["AppendAtTop"], this.updateGuessCount.bind(this));

        this.msgApp = new MsgApp.App(messageUl, 8, MsgApp.order["AppendAtBottom"]);

        this.socket = new SallySocket(socketUrl, this.handleServerMsg, this.handleClientMsg, this.handleGuess);

        this.socket.install(this.setSubscription.bind(this));

        this.subscription = "SubSelf";

        this.subscribeForm = subscribeForm;

        this.gameElts = gameElts;

        subscribeForm.addEventListener('change', e => {this.setSubscription();} );

        this.gameElts.form.addEventListener('submit', e => {
            if (e.preventDefault) e.preventDefault();

            const newGuess = {
                  clLikes : cutValOfNode(gameElts.likes)
                , clNotLikes : cutValOfNode(gameElts.notlikes)
            }

            this.handleClientMsg("Submitting guess");
            this.socket.sendGuess(newGuess);
        });
    }

    handleClientMsg = txt => {
        this.msgApp.pushNewLiWith(
            makeMessage("Client", "client-msg", txt)
        )
    }
    
    handleServerMsg = txt => {
        this.msgApp.pushNewLiWith(
            makeMessage("Server", "server-msg", txt)
        )
    }

    setSubscription = () => {
        var fieldset = this.subscribeForm.elements["subscription"]
        this.subscription = fieldset.value;
        this.gameApp.clearAll();
        console.log("Attempting to renew with " + this.socket);
        this.socket.renew(this.subscription);
    }

    updateGuessCount = () => {
        if (this.subscription === "SubAll") {
            var globalFlag = " (global)";
        } else {
                var globalFlag = "";
        }
        switch (this.gameApp.length) {
            case 0:
                this.gameElts.guessHeader.innerText = "No guesses yet" + globalFlag;
                break;
            case 1:
                this.gameElts.guessHeader.innerText = "Last guess" + globalFlag;
                break;
            default:
                this.gameElts.guessHeader.innerText = "Last " + this.gameApp.length + " guesses" + globalFlag;
                break;
        }
    }

    handleGuess = (gsRes, isSelf) => {
        const nodes = mkGuessNodes(gsRes, isSelf);
        this.gameApp.pushNewLiWith(nodes);
    }

}

var makeMessage = function (from, className, text) {
    var   span = document.createElement("span");
          span.className = className;
          span.appendChild(document.createTextNode(from + ": "));
    const txtnd = document.createTextNode(text);
    return ([span, txtnd]);
}

const mkGuessNodes = function (gsRes, isSelf) {
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
    if (!isSelf) {
        meta.innerHTML += "Submitted by " + gsRes.resGs.gsUser + " at <span class=\"time\">" + dateStr + " UST</span>";
    }
    else {
        meta.innerHTML += "Submitted by <span class=\"you\">you</span> at <span class=\"time\">" + dateStr + " UST</span>";
    }
    return [msg, meta];
}
