#! /usr/bin/env python
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
from PyQt5.QtCore import Qt, QEvent
from PyQt5.QtGui import QPainter
from PyQt5.QtWidgets import QWidget
from xutils import get_xlib_display

class View(QWidget):

    trigger_mouse_event = QtCore.pyqtSignal(str, int, int, int, int, QEvent)
    trigger_focus_event = QtCore.pyqtSignal(str)

    def __init__(self, emacs_xid, view_info):
        super(View, self).__init__()

        self.emacs_xid = emacs_xid

        # Init widget attributes.
        self.setWindowFlags(Qt.FramelessWindowHint)
        self.setAttribute(Qt.WA_X11DoNotAcceptFocus, True)
        self.setContentsMargins(0, 0, 0, 0)

        # Init attributes.
        self.view_info = view_info
        (self.buffer_id, self.x, self.y, self.width, self.height) = view_info.split(":")
        self.x = int(self.x)
        self.y = int(self.y)
        self.width = int(self.width)
        self.height = int(self.height)

        self.qimage = None
        self.background_color = None

        # Show and resize.
        self.show()
        self.resize(self.width, self.height)

        self.installEventFilter(self)

        print("Create view: %s" % self.view_info)

    def eventFilter(self, obj, event):
        if event.type() in [QEvent.MouseButtonPress, QEvent.MouseButtonRelease,
                            QEvent.MouseMove, QEvent.MouseButtonDblClick, QEvent.Wheel]:
            self.trigger_mouse_event.emit(self.buffer_id, self.width, self.height, self.qimage.width(), self.qimage.height(), event)
            self.trigger_focus_event.emit("{0},{1}".format(event.globalX(), event.globalY()))

        return False

    def paintEvent(self, event):
        # Init painter.
        painter = QPainter(self)

        # Render background.
        if self.background_color != None:
            painter.setBrush(self.background_color)
            painter.drawRect(0, 0, self.width, self.height)

        # Render buffer image in center of view.
        if self.qimage != None:
            render_x = (self.width - self.qimage.width()) / 2
            render_y = (self.height - self.qimage.height()) / 2
            painter.drawImage(QtCore.QRect(render_x, render_y, self.qimage.width(), self.qimage.height()), self.qimage)

        # End painter.
        painter.end()

    def showEvent(self, event):
        # NOTE: we must reparent after widget show, otherwise reparent operation maybe failed.
        self.reparent()

    def reparent(self):
        xlib_display = get_xlib_display()

        view_xid = self.winId().__int__()
        view_xwindow = xlib_display.create_resource_object("window", view_xid)
        emacs_xwindow = xlib_display.create_resource_object("window", self.emacs_xid)

        view_xwindow.reparent(emacs_xwindow, self.x, self.y)

        xlib_display.sync()

    def handle_destroy(self):
        if self.qimage != None:
            del self.qimage

        self.destroy()

        print("Destroy view: %s" % self.view_info)


