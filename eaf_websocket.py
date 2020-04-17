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

from PyQt5.QtCore import QObject, QTimer, QEventLoop, pyqtSignal, QUrl, QThread, QCoreApplication
from PyQt5.QtNetwork import QHostAddress
from PyQt5.QtWebSockets import QWebSocketServer, QWebSocket

import json
import os

class WebsocketServer(QObject):
    request_received = pyqtSignal(int, str, object)
    notify_received = pyqtSignal(str, object)
    def __init__(self, name, port, parent = None):
        super().__init__(parent=parent)
        self.name = name
        self.port = port

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
                self.request_received.emit(message["id"], message["method"], params)
            else:
                self.notify_received.emit(message["method"], params)
            return

    def send_result(self, request_id, result):
        message = {"jsonrpc": "2.0", "result": result, "id": request_id}
        self.__send_text_message(json.dumps(message))



#TODO: error handle
class WebsocketServerThread(QThread):
    send_result = pyqtSignal(int, str)
    def __init__(self, name, port, dispatcher, parent=None):
        super().__init__(parent=parent)
        self.name = name
        self.port = port
        self.dispatcher = dispatcher

    def run(self):
        self.__server = WebsocketServer(self.name, self.port)
        self.__server.request_received.connect(self.__request_dispatcher)
        self.__server.notify_received.connect(self.__notify_dispatcher)
        self.send_result.connect(self.__server.send_result)
        self.exec()

    def __request_dispatcher(self, request_id, method_name, params):
        try:
            if self.dispatcher:
                method = getattr(self.dispatcher, method_name)
                result =  method(*params)
                self.send_result.emit(request_id, result)
            else:
                self.send_error.emit(request_id, -32601, "No Dispatcher")
        except AttributeError:
            self.send_error.emit(request_id, -32601, "No Method")
        except Exception as e:
            self.send_error.emit(request_id, -32603, "method raise exception")

    def __notify_dispatcher(self, method_name, params):
        try:
            if self.dispatcher:
                method = getattr(self.dispatcher, method_name)
                method(*params)
            else:
                print("no dispatcher")
        except Exception:
            print("methid raise exception")


class WebsocketClient(QObject):
    def __init__(self, url, parent=None):
        super().__init__(parent)
        self.url = url

        self.__request_id = 0
        self.__request_cbs = {}
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
            cb = self.__request_cbs.pop(message["id"])["success_cb"]
            if cb:
                cb(message["result"])
            return
        elif "error" in message and "id" in message:
            cb = self.__request_cbs.pop([message["id"]])["error_cb"]
            if cb:
                cb(message["error"]["message"])
            return


    def notify(self, method, *params):
        message = {
            "jsonrpc": "2.0",
            "method": method,
            "params": params,
        }
        self.__send_text_message(json.dumps(message))


    def async_request(self, method, *params, success_cb=None, error_cb=None):
        self.__request_id += 1
        message = {
            "jsonrpc": "2.0",
            "id": self.__request_id,
            "method": method,
            "params": params,
        }
        self.__request_cbs[self.__request_id] = { "success_cb": success_cb , "error_cb": error_cb}
        self.__send_text_message(json.dumps(message))
