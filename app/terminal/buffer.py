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

from PyQt5.QtGui import QColor, QFont
from core.buffer import Buffer
import QTermWidget

class AppBuffer(Buffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict):
        Buffer.__init__(self, buffer_id, url, arguments, emacs_var_dict, True, QColor(0, 0, 0, 255))

        self.add_widget(QTermWidget.QTermWidget())

        self.buffer_widget.setTerminalFont(QFont('Source Code Pro', 14))
        self.buffer_widget.setColorScheme('Linux')

        self.buffer_widget.finished.connect(self.request_close_buffer)

    def get_key_event_widgets(self):
        return self.buffer_widget.children()

    def fake_key_event_filter(self, event_string):
        if event_string == "RET":
            self.buffer_widget.sendText("\n")

    def zoom_out(self):
        self.buffer_widget.zoomOut()

    def zoom_in(self):
        self.buffer_widget.zoomIn()
