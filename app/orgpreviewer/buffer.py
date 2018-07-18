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

from PyQt5.QtCore import QUrl, Qt
from PyQt5.QtGui import QColor
from core.browser import BrowserView
from core.buffer import Buffer
import os

class AppBuffer(Buffer):
    def __init__(self, buffer_id, url):
        Buffer.__init__(self, buffer_id, url, False, QColor(255, 255, 255, 255))

        self.url = url
        self.add_widget(BrowserView())

        self.load_org_html_file()

    def load_org_html_file(self):
        self.buffer_widget.setUrl(QUrl.fromLocalFile(os.path.splitext(self.url)[0]+".html"))

    def update_with_data(self, update_data):
        self.load_org_html_file()
        self.buffer_widget.reload()
