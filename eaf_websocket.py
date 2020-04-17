#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2018 Andy Stewart
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

from PyQt5.QtCore import QObject, QTimer, QEventLoop, pyqtSignal, QUrl
from PyQt5.QtNetwork import QHostAddress
from PyQt5.QtWebSockets import QWebSocketServer, QWebSocket

import json
import os

class WebsocketServer(QObject):
    def __init__(self, name, port, dispatcher, parent = None):
        super().__init__(parent=parent)
        self.name = name
        self.port = port

        self.dispatcher = dispatcher

        self.__server = QWebSocketServer(self.name, QWebSocketServer.NonSecureMode)
        if self.__server.listen(QHostAddress.LocalHost, self.port):
            print(
                "Listened: "
                + self.__server.serverAddress().toString()
                + ": "
                + str(self.__server.serverPort())
            )
        else:
            raise Exception(
                "JsonrpcWebsocketServer Listen "
                + str(self.__server.serverPort())
                + " failed!"
            )
        self.__server.newConnection.connect(self.__on_new_connection)

    def __on_new_connection(self):
        self.__client_connection = self.__server.nextPendingConnection()
        if not self.__client_connection:
            return
        else:
            # no more connection
            self.__server.close()
        print("client connected")
        self.__client_connection.textMessageReceived.connect(
            self.__on_text_message_received
        )
        self.__client_connection.disconnected.connect(self.__on_disconnected)

    def __send_text_message(self, message):
        try:
            self.__client_connection.sendTextMessage(message)
        except RuntimeError:
            print("Client socket closed, send message failed. restart server and client!")

    def __on_disconnected(self):
        self.__client_connection.deleteLater()

    def __on_text_message_received(self, message_str):
        # for debug
        print("server received: ", message_str)
        reply = {}
        reply["jsonrpc"] = "2.0"
        message = None
        try:
            message = json.loads(message_str)
        except json.JSONDecodeError:
            reply["error"] = {"code": -32700, "message": "Parse error"}
            self.__send_text_message(json.dumps(reply))
            return
        if "method" in message:
            if not isinstance(message["method"], str):
                reply["error"] = {"code": -32600, "message": "method must be string"}
                self.__send_text_message(json.dumps(reply))
                return
            if "params" not in message or not message["params"]:
                params = []
            else:
                params = message["params"]
            if "id" in message and message["id"] != None:
                self.__request_dispatcher(message["id"], message["method"], params)
            else:
                self.__notify_dispatcher(message["method"], params)
            return

    def __request_dispatcher(self, request_id, method, params):
        if self.dispatcher:
            method = getattr(self.dispatcher, method)
            result =  method(*params)
            message = {"jsonrpc": "2.0", "result": result, "id": request_id}
            self.__send_text_message(json.dumps(message))

    def __notify_dispatcher(self, method, params):
        if self.dispatcher:
            method = getattr(self.dispatcher, method)
            method(*params)
        else:
            print("please register a dispatcher")


class WebsocketClient(QObject):
    def __init__(self, url, parent=None):
        super().__init__(parent)
        self.url = url

        self.__request_id = 0
        self.__loop = QEventLoop()
        self.__result = None
        self.__error = None
        self.__deferred_message = []
        self.__socket = QWebSocket()
        self.__socket.connected.connect(self.__on_connected)
        self.__socket.open(QUrl(self.url))

    def __on_connected(self):
        print("connected to ", self.url)
        self.__socket.textMessageReceived.connect(self.__on_text_message_received)
        while self.__deferred_message:
            self.__send_text_message(self.__deferred_message.pop())

    def __send_text_message(self, message):
        if self.__socket.isValid():
            self.__socket.sendTextMessage(message)
        else:
            self.__deferred_message.append(message)

    def __on_text_message_received(self, message_str):
        # for debug
        print("server received: ", message_str)
        reply = {}
        reply["jsonrpc"] = "2.0"
        message = None
        try:
            message = json.loads(message_str)
        except json.JSONDecodeError:
            reply["error"] = {"code": -32700, "message": "Parse error"}
            self.__send_text_message(json.dumps(reply))
            return
        if "result" in message and "id" in message:
            self.__result = message["result"]
            self.__loop.quit()
            return
        elif "error" in message and "id" in message:
            self.__error = message["error"]["message"]
            self.__loop.quit()
            return


    def notify(self, method, *params):
        message = {
            "jsonrpc": "2.0",
            "method": method,
            "params": params,
        }
        self.__send_text_message(json.dumps(message))

    def request(self, method, *params, timeout=3000):
        t = QTimer()
        t.setSingleShot(True)
        t.setInterval(timeout)
        t.timeout.connect(self.__loop.quit)
        self.__request_id += 1
        message = {
            "jsonrpc": "2.0",
            "id": self.__request_id,
            "method": method,
            "params": params,
        }
        self.__send_text_message(json.dumps(message))
        t.start()
        self.__loop.exec()
        if self.__result:
            result = self.__result
            self.__result = None
            return result
        elif self.__error:
            print("error:", self.__error )
            self.__error = None
            return ""
        else:
            print("timeout")
            return ""
