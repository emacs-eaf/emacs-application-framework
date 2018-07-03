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

from PyQt5 import QtCore
from PyQt5.QtGui import QPainter
from PyQt5.QtCore import Qt, QEvent
from PyQt5.QtWidgets import QWidget, QVBoxLayout, QGraphicsView
from core.xutils import get_xlib_display

class View(QWidget):

    trigger_focus_event = QtCore.pyqtSignal(str)

    def __init__(self, emacs_xid, buffer, view_info):
        super(View, self).__init__()

        self.buffer = buffer

        self.emacs_xid = emacs_xid

        # Init widget attributes.
        self.setWindowFlags(Qt.FramelessWindowHint)
        self.setAttribute(Qt.WA_X11DoNotAcceptFocus, True)
        self.setContentsMargins(0, 0, 0, 0)
        self.installEventFilter(self)

        # Init attributes.
        self.view_info = view_info
        (self.buffer_id, self.x, self.y, self.width, self.height) = view_info.split(":")
        self.x = int(self.x)
        self.y = int(self.y)
        self.width = int(self.width)
        self.height = int(self.height)

        # Build QGraphicsView.
        self.layout = QVBoxLayout(self)
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.graphics_view = QGraphicsView(buffer, self)
        self.graphics_view.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.graphics_view.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.graphics_view.setRenderHints(QPainter.Antialiasing | QPainter.SmoothPixmapTransform | QPainter.TextAntialiasing)
        # Remove damn border from QGraphicsView.
        self.graphics_view.setFrameStyle(0)
        self.graphics_view.setStyleSheet("QGraphicsView {background: transparent; border: 3px; outline: none;}")
        self.layout.addWidget(self.graphics_view)

        # NOTE: show function must start before resize to trigger *first* resizeEvent after show.
        self.show()

        # Resize after show to trigger fit view operation.
        self.resize(self.width, self.height)

    def resizeEvent(self, event):
        # Fit content to view rect just when buffer fit_to_view option is enable.
        if self.buffer.fit_to_view:
            if event.oldSize().isValid():
                self.graphics_view.fitInView(self.graphics_view.scene().sceneRect(), Qt.KeepAspectRatio)
        QWidget.resizeEvent(self, event)

    def eventFilter(self, obj, event):
        # Focus emacs buffer when user click view.
        if event.type() in [QEvent.MouseButtonPress, QEvent.MouseButtonRelease,
                            QEvent.MouseMove, QEvent.MouseButtonDblClick, QEvent.Wheel]:
            self.trigger_focus_event.emit("{0},{1}".format(event.globalX(), event.globalY()))

        return False

    def showEvent(self, event):
        # NOTE: we must reparent after widget show, otherwise reparent operation maybe failed.
        self.reparent()

        # Make graphics view at left-top corner after show.
        self.graphics_view.verticalScrollBar().setValue(0)
        self.graphics_view.horizontalScrollBar().setValue(0)

    def reparent(self):
        xlib_display = get_xlib_display()

        view_xid = self.winId().__int__()
        view_xwindow = xlib_display.create_resource_object("window", view_xid)
        emacs_xwindow = xlib_display.create_resource_object("window", self.emacs_xid)

        view_xwindow.reparent(emacs_xwindow, self.x, self.y)

        xlib_display.sync()

    def handle_destroy(self):
        self.destroy()
