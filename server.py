#!/usr/bin/env python
# -*- coding: utf-8 -*-

from PyQt5.QtCore import QTimer
from PyQt5.QtWidgets import QApplication

from eaf_websocket import JsonrpcWebsocketServer

class Dispatcher:
    def __init__(self, server):
        self.server = server

    def from_emacs(self, message):
        print("from_message: ", message)
        result = self.server.request("eaf-get-theme-mode")
        return "result from python " + result

    def from_emacs_no_args(self):
        print("from_message: no args")
        return "result from python, no args"


    def timeout(self):
        print("async request")
        self.server.async_request("delay-return", 5, success_callback=lambda m : print("asunc_request return: ", m) )
        print("next request")
        print("sync request: ", self.server.request("delay-return", 3))
        print("sync request finished")

if __name__ == "__main__":
    import sys
    import signal

    app = QApplication(sys.argv)

    server = JsonrpcWebsocketServer("EAF Server", 12981)
    dispatcher = Dispatcher(server)
    server.register_dispatcher_object(dispatcher)
    server.notify("message", "hello, this is python's notify eamcs message")
    timer = QTimer()
    timer.setInterval(10000)
    timer.setSingleShot(1)
    timer.timeout.connect(dispatcher.timeout)
    #timer.start()
    signal.signal(signal.SIGINT, signal.SIG_DFL)
    sys.exit(app.exec_())
