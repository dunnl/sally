// An module for submitting and processing SS guesses over a websocket. Requires
// MsgApp for displaying game state and program messages
export default class {
    
    constructor (socketUrl, handleSysMsg, handleCliMsg, handleGuess) {
        this.socketUrl = socketUrl;
        this.handleSysMsg = handleSysMsg;
        this.handleCliMsg = handleCliMsg;
        this.handleGuess = handleGuess;
        this.uuid = undefined;
        this.self = this;
    }

    install () {
        if (!this.socketUrl) {
            throw("SallySocket: No socketUrl specified")
        }

        var socket = new WebSocket(this.socketUrl);

        socket.onerror = e => {
            this.handleCliMsg("Socked experienced an error");
        }
        socket.onopen = e => {
            this.handleCliMsg("Socked opened successfully");
        }
        socket.onclose = e => {
            this.handleCliMsg("Closing socket");
        }
        socket.onmessage = e => {
            var obj = JSON.parse(e.data);
            switch(obj.type) {
                case "guess":
                    console.log(obj.body)
                    this.handleGuess(obj.body, obj.body.resGs.gsUser === this.uuid)
                    break;
                case "control":
                    this.handleSysMsg(obj.body)
                    break;
                case "uuid":
                    console.log("Got UUID")
                    this.uuid = obj.body;
                    break;
                default:
                   this.handleCliMsg("Received a strange message:" + JSON.stringify(obj, null, 3));
                }
        }
        this.socket = socket;
    }

    renew (subscription) {
        var msg = {
            "type": "renew",
            "body": subscription
        }
        try {
            this.socket.send(JSON.stringify(msg));
        }
        catch (err) {
            console.log("rewnew: socket.send failed");
            console.log(err);
        }
    }

    sendGuess (newGuess) {
        var newMsg = {
            "body" : newGuess,
            "type": "guess"
        }
        try {
            this.socket.send(JSON.stringify(newMsg));
        }
        catch (err) {
            console.log("sendguess: socket.send failed");
            console.log(err);
        }
    }
}


