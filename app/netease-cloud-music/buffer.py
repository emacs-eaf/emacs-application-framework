#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2018 Andy Stewart
#
# Author:     SpringHan <springchohaku@qq.com>
# Maintainer: SpringHan <springchohaku@qq.com>
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
from core.webengine import BrowserBuffer
from core.utils import eval_in_emacs, get_emacs_var

class AppBuffer(BrowserBuffer):
    def __init__(self, buffer_id, url, arguments):
        BrowserBuffer.__init__(self, buffer_id, url, arguments, False)

        self.backgroundColor = QColor(get_emacs_var("eaf-emacs-theme-background-color")).darker(110).name()
        
        self.buffer_widget.loadFinished.connect(self.init_app)
        self.load_index_html(__file__)

    def init_app(self):
        self.buffer_widget.execute_js('initColor(\"{}\", \"{}\")'.format(
            get_emacs_var("eaf-emacs-theme-background-color"),
            get_emacs_var("eaf-emacs-theme-foreground-color")
        ))

        # Init
        eval_in_emacs('''eaf--netease-cloud-music-init''', [])
