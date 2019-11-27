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

from PyQt5.QtCore import Qt, QSizeF
from PyQt5.QtGui import QBrush
from PyQt5.QtGui import QColor
from PyQt5.QtMultimedia import QCameraInfo, QCamera, QCameraImageCapture
from PyQt5.QtMultimediaWidgets import QGraphicsVideoItem
from PyQt5.QtWidgets import QGraphicsScene, QGraphicsView
from PyQt5.QtWidgets import QWidget, QVBoxLayout
from core.buffer import Buffer
from pathlib import Path
import time
import os

class AppBuffer(Buffer):
    def __init__(self, buffer_id, url, arguments):
        Buffer.__init__(self, buffer_id, url, arguments, True, QColor(0, 0, 0, 255))
        self.add_widget(CameraWidget(QColor(0, 0, 0, 255)))

    def all_views_hide(self):
        # Need stop camera if all view will hide, otherwise camera will crash.
        self.buffer_widget.camera.stop()

    def some_view_show(self):
        # Re-start camero after some view show.
        self.buffer_widget.camera.start()

    def take_photo(self):
        self.buffer_widget.take_photo()

class CameraWidget(QWidget):

    def __init__(self, background_color):
        QWidget.__init__(self)

        self.scene = QGraphicsScene(self)
        self.scene.setBackgroundBrush(QBrush(background_color))
        self.graphics_view = QGraphicsView(self.scene)
        self.graphics_view.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.graphics_view.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.graphics_view.setFrameStyle(0)
        self.graphics_view.setStyleSheet("QGraphicsView {background: transparent; border: 3px; outline: none;}")
        self.graphics_view.scale(-1, 1) # this make live video from camero mirror.
        self.video_item = QGraphicsVideoItem()
        self.scene.addItem(self.video_item)

        self.layout = QVBoxLayout(self)
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.layout.addWidget(self.graphics_view)

        self.available_cameras = QCameraInfo.availableCameras()

        # Set the default camera.
        self.select_camera(0)

    def resizeEvent(self, event):
        self.video_item.setSize(QSizeF(event.size().width(), event.size().height()))
        QWidget.resizeEvent(self, event)

    def select_camera(self, i):
        self.camera = QCamera(self.available_cameras[i])
        self.camera.setViewfinder(self.video_item)
        self.camera.setCaptureMode(QCamera.CaptureStillImage)
        self.camera.start()

    def take_photo(self):
        photo_path = os.path.join(str(Path.home()), "EAF_Camera_Photo_" + time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(int(time.time()))))

        image_capture = QCameraImageCapture(self.camera)
        image_capture.capture(photo_path)

        self.message_to_emacs.emit("Save photo at: " + photo_path)

if __name__ == "__main__":
    from PyQt5.QtWidgets import QApplication
    import sys
    import signal
    app = QApplication(sys.argv)

    test = CameraWidget(QColor(0, 0, 0, 255))
    test.show()

    signal.signal(signal.SIGINT, signal.SIG_DFL)
    sys.exit(app.exec_())
