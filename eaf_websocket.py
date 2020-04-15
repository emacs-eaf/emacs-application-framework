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

import json
import os

from PyQt5.QtCore import QObject, QTimer, QEventLoop, pyqtSignal
from PyQt5.QtNetwork import QHostAddress
from PyQt5.QtWebSockets import QWebSocketServer, QWebSocket


class JsonrpcWebsocketServer(QObject):
    close = pyqtSignal()

    def __init__(self, server_name, port, parent=None):
        super().__init__(parent=parent)
        self.server_name = server_name
        self.port = port

        self.next_request_id = 0
        self.request_continuations = {}
        self.dispatcher = None
        self.deferred_notify_messages = []

        self.server = QWebSocketServer(
            self.server_name, QWebSocketServer.NonSecureMode, self
        )
        self.client_connection = None
        if self.server.listen(QHostAddress.LocalHost, port):
            print(
                "Listened: "
                + self.server.serverAddress().toString()
                + " : "
                + str(self.server.serverPort())
            )
        else:
            print("Listened error")

        self.server.newConnection.connect(self.__on_new_connection)

    def __send_text_message(self, message):
        self.client_connection.sendTextMessage(message)

    def __on_new_connection(self):
        conn = self.server.nextPendingConnection()
        self.server.close()
        if self.client_connection:
            print("Only one client")
            conn.close()
        print("client connected")
        self.client_connection = conn
        self.client_connection.textMessageReceived.connect(
            self.__handle_text_message_received
        )
        self.client_connection.binaryMessageReceived.connect(
            self.__handle_binary_message_received
        )
        self.client_connection.disconnected.connect(self.__handle_disconnected)
        # send deferred_notify_message
        while self.deferred_notify_messages:
            print(self.deferred_notify_messages)
            self.__send_text_message(self.deferred_notify_messages.pop())

    def __handle_binary_message_received(self, message_binary):
        pass

    def __handle_text_message_received(self, message_str):
        # for Debug
        reply = {}
        reply["jsonrpc"] = "2.0"
        message = None
        try:
            message = json.loads(message_str)
        except json.JSONDecodeError:
            reply["error"] = {"code": -32700, "message": "Parse error"}
            self.__send_text_message(json.dumps(reply))
            return
        if "method" in message and "id" in message and message["id"] != None:
            reply["id"] = message["id"]
            try:
                if "params" not in message or not message["params"]:
                    params = []
                else:
                    params = message["params"]
                reply["result"] = self.__request_dispatcher(message["method"], params)
            except Exception as error:
                reply["error"] = {"code": -32603, "message": "Internal error"}
            self.__send_text_message(json.dumps(reply))
        elif "method" in message:
            if "params" not in message or not message["params"]:
                params = []
            else:
                params = message["params"]

            self.__notification_dispatcher(
                message["method"], params,
            )
        elif "result" in message and "id" in message:
            continuation = self.request_continuations.pop(message["id"])
            if continuation["success_callback"]:
                continuation["success_callback"](message["result"])
            else:
                print(message["result"])
        elif "error" in message and "id" in message:
            continuation = self.request_continuations.pop(message["id"])
            if continuation["error_callback"]:
                continuation["error_callback"](message["error"])
            else:
                print(message["error"])

    def __handle_disconnected(self):
        if self.client_connection:
            self.client_connection.deleteLater()

    def __request_dispatcher(self, method_name, params):
        if self.dispatcher:
            method = getattr(self.dispatcher, method_name)
            return method(*params)
        else:
            raise Exception("Internal error")

    def __notification_dispatcher(self, method_name, params):
        if self.dispatcher:
            method = getattr(self.dispatcher, method_name)
            method(*params)
        else:
            print("please register a dispatcher")

    def register_dispatcher_object(self, instance):
        self.dispatcher = instance

    def async_request(
        self, method_name, *params, success_callback=None, error_callback=None
    ):
        if not self.client_connection:
            raise Exception("Client not connected")
        self.next_request_id += 1
        self.request_continuations[self.next_request_id] = {
            "success_callback": success_callback,
            "error_callback": error_callback,
        }
        message = {
            "jsonrpc": "2.0",
            "id": self.next_request_id,
            "method": method_name,
            "params": params,
        }
        self.__send_text_message(json.dumps(message))
        return self.next_request_id

    def request(self, method_name, *params, timeout=30000):
        pass
        ##TODO: not work
        # print("request: ", method_name)
        # if not self.client_connection:
        #     raise Exception("Client not connected")
        # q = QEventLoop()
        # success_retval = None
        # error_message = None
        # def request_success_callback(retval):
        #     print("success_callback: retval", retval)
        #     nonlocal success_retval
        #     nonlocal q
        #     success_retval = retval
        #     q.quit()

        # def request_error_callback(error):
        #     nonlocal error_message
        #     nonlocal q
        #     error_message = error
        #     q.quit()

        # id = self.async_request(
        #     method_name,
        #     *params,
        #     success_callback=lambda retval: request_success_callback(retval),
        #     error_callback=lambda error: request_error_callback(error),
        # )
        # q.exec()
        # if success_retval:
        #     return success_retval
        # elif error_message:
        #     print("request error: ", error_message)
        # else:
        #     print("request timeout:!")
        # return ""

    def notify(self, method_name, *params):
        message = {"jsonrpc": "2.0", "method": method_name, "params": params}
        if self.client_connection:
            self.__send_text_message(json.dumps(message))
        else:
            self.deferred_notify_messages.append(json.dumps(message))
