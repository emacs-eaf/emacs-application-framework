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

from PyQt5 import QtCore
from PyQt5.QtGui import QColor, QPixmap, QPainter
from PyQt5.QtCore import QRect, Qt
from PyQt5.QtWidgets import QWidget
import os
from core.buffer import Buffer

class AppBuffer(Buffer):
    def __init__(self, buffer_id, url):
        Buffer.__init__(self, buffer_id, url, False, QColor(0, 0, 0, 255))

        self.add_widget(ImageViewerWidget(url, QColor(0, 0, 0, 255)))
        self.buffer_widget.render_image.connect(self.change_title)

class ImageViewerWidget(QWidget):

    render_image = QtCore.pyqtSignal(str)

    def __init__(self, image_path, background_color):
        QWidget.__init__(self)

        self.background_color = background_color
        self.load_image(image_path)

    def load_image(self, image_path):
        self.parent_dir = os.path.abspath(os.path.join(image_path, os.pardir))
        self.image_name = os.path.basename(image_path)
        self.qimage = QPixmap(image_path)
        self.update()

        self.render_image.emit(self.image_name)

    def load_next_image(self):
        files = [f for f in os.listdir(self.parent_dir) if os.path.isfile(os.path.join(self.parent_dir, f))]
        images = list(filter(lambda f: f.endswith(".jpg") or f.endswith(".png"), files))
        if self.image_name in images:
            image_index = images.index(self.image_name)
            if image_index == len(images) - 1:
                image_index = 0
            else:
                image_index += 1

            self.load_image(os.path.join(self.parent_dir, images[image_index]))

    def load_prev_image(self):
        files = [f for f in os.listdir(self.parent_dir) if os.path.isfile(os.path.join(self.parent_dir, f))]
        images = list(filter(lambda f: f.endswith(".jpg") or f.endswith(".png"), files))
        if self.image_name in images:
            image_index = images.index(self.image_name)
            if image_index == 0:
                image_index = len(images) - 1
            else:
                image_index -= 1

            self.load_image(os.path.join(self.parent_dir, images[image_index]))

    def paintEvent(self, event):
        painter = QPainter(self)

        painter.setBrush(self.background_color)
        painter.drawRect(0, 0, self.rect().width(), self.rect().height())

        width_scale = self.rect().width() * 1.0 / self.qimage.width()
        height_scale = self.rect().height() * 1.0 / self.qimage.height()
        image_scale = 1.0
        if width_scale < height_scale:
            image_scale = width_scale
        else:
            image_scale = height_scale

        render_width = image_scale * self.qimage.width()
        render_height = image_scale * self.qimage.height()
        render_x = (self.rect().width() - render_width) / 2
        render_y = (self.rect().height() - render_height) / 2

        painter.drawPixmap(QRect(render_x, render_y, render_width, render_height), self.qimage)

        painter.end()

    def keyPressEvent(self, event):
        if event.key() == Qt.Key_J:
            self.load_next_image()
        elif event.key() == Qt.Key_K:
            self.load_prev_image()

if __name__ == "__main__":
    from PyQt5.QtWidgets import QApplication
    import sys
    import signal
    app = QApplication(sys.argv)

    test = ImageViewerWidget("/home/andy/rms/1.jpg", QColor(0, 0, 0, 255))
    test.show()

    signal.signal(signal.SIGINT, signal.SIG_DFL)
    sys.exit(app.exec_())
