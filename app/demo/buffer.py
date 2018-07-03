#!/usr/bin/env python
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

from PyQt5.QtGui import QColor
from PyQt5.QtWidgets import QPushButton
from core.buffer import Buffer

class DemoBuffer(Buffer):
    def __init__(self, buffer_id, url):
        Buffer.__init__(self, buffer_id, url, True, QColor(0, 0, 0, 255))

        self.add_widget(QPushButton("Hello, EAF hacker, it's working!!!"))
        self.buffer_widget.setStyleSheet("font-size: 100px")
