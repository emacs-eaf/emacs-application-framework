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
from core.browser import BrowserBuffer
from core.utils import touch
import os

class AppBuffer(BrowserBuffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, False)

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

        self.close_page.connect(self.record_close_page)

        self.buffer_widget.titleChanged.connect(self.record_history)
        self.buffer_widget.titleChanged.connect(self.change_title)

        self.buffer_widget.translate_selected_text.connect(self.translate_text)

        self.buffer_widget.open_url_in_new_tab.connect(self.open_url_in_new_tab)
        self.buffer_widget.open_url_in_background_tab.connect(self.open_url_in_background_tab)

        # Reset to default zoom when page init or url changed.
        self.reset_default_zoom()
        self.buffer_widget.urlChanged.connect(self.update_url)

    def update_url(self, _url):
        self.reset_default_zoom()
        self.url = self.buffer_widget.url().toString()
