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

from PyQt5 import QtCore
from PyQt5.QtCore import QUrl, Qt
from PyQt5.QtGui import QColor
from PyQt5.QtWebKitWidgets import QWebView, QWebPage
from PyQt5.QtWidgets import QApplication
from PyQt5.QtWebKit import QWebSettings
from core.buffer import Buffer
from core.utils import PostGui
import socket
import subprocess
import threading
import os

class AppBuffer(Buffer):
    def __init__(self, buffer_id, url):
        Buffer.__init__(self, buffer_id, url, False, QColor(255, 255, 255, 255))

        # Get free port to render markdown.
        self.port = self.get_free_port()
        self.url = url

        # Start markdown render process.
        subprocess.Popen("grip {0} {1}".format(url, self.port), shell=True)

        # Init widget.
        self.add_widget(BrowserWidget())

        # Add timer make load markdown preview link after grip process start finish.
        timer = threading.Timer(2, self.load_markdown_server)
        timer.start()

    def get_free_port(self):
        """
        Determines a free port using sockets.
        """
        free_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        free_socket.bind(('0.0.0.0', 0))
        free_socket.listen(5)
        port = free_socket.getsockname()[1]
        free_socket.close()

        return port

    @PostGui()
    def load_markdown_server(self):
        self.buffer_widget.setUrl(QUrl("http://localhost:{0}".format(self.port)))

        paths = os.path.split(self.url)
        if len(paths) > 0:
            self.change_title(paths[-1])

class BrowserWidget(QWebView):

    def __init__(self):
        super(QWebView, self).__init__()

        self.web_page = WebPage()
        self.setPage(self.web_page)

        self.settings().setAttribute(QWebSettings.PluginsEnabled, True)
        self.settings().setAttribute(QWebSettings.JavascriptEnabled, True)
        self.settings().setAttribute(QWebSettings.JavascriptCanOpenWindows, True)

class WebPage(QWebPage):

    open_url_in_new_tab = QtCore.pyqtSignal(str)

    def __init__(self):
        super(WebPage, self).__init__()

    def acceptNavigationRequest(self, frame, request, type):
        modifiers = QApplication.keyboardModifiers()

        # Handle myself if got user event.
        if type == QWebPage.NavigationTypeLinkClicked:
            if modifiers == Qt.ControlModifier:
                self.open_url_in_new_tab.emit(request.url().toString())
            else:
                self.view().load(request.url())

            # Return False to stop default behavior.
            return False

        # # Otherwise, use default behavior.
        return QWebPage.acceptNavigationRequest(self, frame, request, type)

    def javaScriptConsoleMessage(self, msg, lineNumber, sourceID):
        pass
