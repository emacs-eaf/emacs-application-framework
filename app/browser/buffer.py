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
from core.browser import BrowserBuffer
from core.utils import touch
import os
import re

class AppBuffer(BrowserBuffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, False, QColor(255, 255, 255, 255))

        self.config_dir = config_dir

        # When arguments is "temp_html_file", browser will load content of html file, then delete temp file.
        # Usually use for render html mail.
        if arguments == "temp_html_file":
            with open(url, "r") as html_file:
                self.buffer_widget.setHtml(html_file.read())
                if os.path.exists(url):
                    os.remove(url)
        else:
            self.buffer_widget.setUrl(QUrl(url))

        self.history_log_file_path = os.path.join(self.config_dir, "browser", "history", "log.txt")

        self.history_url_pattern = re.compile("(.*?)\s([^\s]+)$")

        self.buffer_widget.titleChanged.connect(self.record_history)
        self.buffer_widget.titleChanged.connect(self.change_title)
        self.buffer_widget.open_url_in_new_tab.connect(self.open_url)
        self.buffer_widget.translate_selected_text.connect(self.translate_text)

        # Reset to default zoom when page init or url changed.
        self.reset_default_zoom()
        self.buffer_widget.urlChanged.connect(lambda url: self.reset_default_zoom())

        settings = QWebEngineSettings.globalSettings()
        try:
            settings.setAttribute(QWebEngineSettings.PluginsEnabled, self.emacs_var_dict["eaf-browser-enable-plugin"] == "true")
            settings.setAttribute(QWebEngineSettings.JavascriptEnabled, self.emacs_var_dict["eaf-browser-enable-javascript"] == "true")
        except Exception:
            pass

    def clear_history(self):
        if os.path.exists(self.history_log_file_path):
            os.remove(self.history_log_file_path)
            self.message_to_emacs.emit("Cleared browsing history.")
        else:
            self.message_to_emacs.emit("There is no browsing history.")

    def record_history(self, new_title):
        if self.arguments == "temp_html_file":
            pass
        elif new_title == "about:blank":
            pass
        elif self.emacs_var_dict["eaf-browser-remember-history"] == "true":
            touch(self.history_log_file_path)
            with open(self.history_log_file_path, "r") as f:
                lines = f.readlines()

            new_url = self.buffer_widget.filter_url(self.buffer_widget.url().toString())
            exists = False
            with open(self.history_log_file_path, "w") as f:
                for line in lines:
                    title = re.match(self.history_url_pattern, line).group(1)
                    url = re.match(self.history_url_pattern, line).group(2)

                    if url == new_url:
                        exists = True
                        if new_title != title:
                            f.write(new_title + " " + new_url + "\n")
                    else:
                        f.write(line)
                if not exists:
                    f.write(new_title + " " + new_url + "\n")

    def new_blank_page(self):
        self.buffer_widget.eval_in_emacs.emit('''(eaf-open \"{0}\" \"browser\" \"\" t)'''''.format(self.emacs_var_dict["eaf-browser-blank-page-url"]))
