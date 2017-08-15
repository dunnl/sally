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
        this.gameApp = new MsgApp.App(gameUl, 8, MsgApp.order["AppendAtTop"]);

        this.msgApp = new MsgApp.App(messageUl, 8, MsgApp.order["AppendAtBottom"]);

        this.socket = new SallySocket(socketUrl, this.handleServerMsg, this.handleClientMsg, this.handleGuess);

        this.socket.install();

        this.subscription = "SubSelf";

        this.subscribeForm = subscribeForm;

        subscribeForm.addEventListener('change', e => {
            var fieldset = subscribeForm.elements["subscription"]
            this.subscription = fieldset.value;
            this.gameApp.clearAll();
            this.socket.renew(this.subscription);
        });

        gameElts.form.addEventListener('submit', e => {
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

    handleGuess = (gsRes, isSelf) => {
        const nodes = mkGuessNodes(gsRes);
        this.gameApp.pushNewLiWith(nodes);
        if (isSelf) {
            //this.gameApp.pushNewLiWith(nodes);
        }
    }

}

var makeMessage = function (from, className, text) {
    var   span = document.createElement("span");
          span.className = className;
          span.appendChild(document.createTextNode(from + ": "));
    const txtnd = document.createTextNode(text);
    return ([span, txtnd]);
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
