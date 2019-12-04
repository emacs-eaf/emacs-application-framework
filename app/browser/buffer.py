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

from PyQt5.QtCore import QUrl
from PyQt5.QtGui import QColor
from PyQt5.QtWebEngineWidgets import QWebEngineSettings
from core.browser_buffer import BrowserBuffer

class AppBuffer(BrowserBuffer):
    def __init__(self, buffer_id, url, arguments):
        BrowserBuffer.__init__(self, buffer_id, url, arguments, False, QColor(255, 255, 255, 255))

        self.buffer_widget.setUrl(QUrl(url))
        self.buffer_widget.titleChanged.connect(self.change_title)
        self.buffer_widget.open_url_in_new_tab.connect(self.open_url)
        self.buffer_widget.translate_selected_text.connect(self.translate_text)

    def update_settings(self):
        settings = QWebEngineSettings.globalSettings()
        try:
            settings.setAttribute(QWebEngineSettings.PluginsEnabled, self.emacs_var_dict["eaf-browser-enable-plugin"] == "true")
            settings.setAttribute(QWebEngineSettings.JavascriptEnabled, self.emacs_var_dict["eaf-browser-enable-javascript"] == "true")
        except Exception:
            pass
