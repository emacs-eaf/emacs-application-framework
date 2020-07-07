this.Pty = require("node-pty");
this.Websocket = require("ws").Server;

this.onclosed = () => {
    ws.send("ESCAPED|-- CLOSED");
};
this.onopened = () => {};
this.onresize = () => {};
this.ondisconnected = () => {};

var port = process.argv.slice(2)[0]
var dir = process.argv.slice(3)[0]
var commands = process.argv.slice(4)[0].split(" ")
var command = commands[0]
var args = commands.slice(1)

this.tty = this.Pty.spawn(command, args, {
    name: 'xterm-color',
    cols: 80,
    rows: 24,
    cwd: dir,
    env: process.env
});

this.tty.on('exit', (code, signal) => {
    this.onclosed(code, signal);
});

this.wss = new this.Websocket({
    port: port,
    clientTracking: true,
    verifyClient: (info) => {
        if (this.wss.clients.length >= 1) {
            return false;
        } else {
            return true;
        }
    }
});
this.wss.on('connection', (ws) => {
    this.onopened();
    ws.on('message', (msg) => {
        if (msg.startsWith("ESCAPED|-- ")) {
            if (msg.startsWith("ESCAPED|-- RESIZE:")) {
                msg = msg.substr(18);
                let cols = msg.slice(0, -4);
                let rows = msg.substr(4);
                this.tty.resize(Number(cols), Number(rows));
            }
        } else {
            this.tty.write(msg);
        }
    });
    this.tty.on('data', (data) => {
        try {
            ws.send(data);
        } catch (e) {
            // Websocket closed
        }
    });
});
this.wss.on('close', () => {
    this.ondisconnected();
});
