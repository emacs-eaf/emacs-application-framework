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

from PyQt5 import QtGui, QtCore
from PyQt5.QtCore import Qt
from PyQt5.QtGui import QColor, QFont
from PyQt5.QtWidgets import QWidget, QLabel, QVBoxLayout
from core.buffer import Buffer
from core.utils import get_emacs_var
import qrcode

class AppBuffer(Buffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path):
        Buffer.__init__(self, buffer_id, url, arguments, emacs_var_dict, module_path, False)

        self.add_widget(AirShareWidget(url, QColor(get_emacs_var("eaf-buffer-background-color"))))

class Image(qrcode.image.base.BaseImage):
    def __init__(self, border, width, box_size):
        self.border = border
        self.width = width
        self.box_size = box_size
        size = (width + border * 2) * box_size
        self._image = QtGui.QImage(size, size, QtGui.QImage.Format_RGB16)
        self._image.fill(QtCore.Qt.white)

    def pixmap(self):
        return QtGui.QPixmap.fromImage(self._image)

    def drawrect(self, row, col):
        painter = QtGui.QPainter(self._image)
        painter.fillRect(
            (col + self.border) * self.box_size,
            (row + self.border) * self.box_size,
            self.box_size, self.box_size,
            QtCore.Qt.black)

    def save(self, stream, kind=None):
        pass

class AirShareWidget(QWidget):
    def __init__(self, url, color):
        QWidget.__init__(self)
        self.setStyleSheet("background-color: black")

        self.file_name_font = QFont()
        self.file_name_font.setPointSize(24)

        self.file_name_label = QLabel(self)
        self.file_name_label.setText(url)
        self.file_name_label.setFont(self.file_name_font)
        self.file_name_label.setAlignment(Qt.AlignCenter)
        self.file_name_label.setStyleSheet("color: #eee")

        self.qrcode_label = QLabel(self)

        self.notify_font = QFont()
        self.notify_font.setPointSize(12)
        self.notify_label = QLabel(self)
        self.notify_label.setText("Scan QR code above to copy data.")
        self.notify_label.setFont(self.notify_font)
        self.notify_label.setAlignment(Qt.AlignCenter)
        self.notify_label.setStyleSheet("color: #eee")

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addStretch()
        layout.addWidget(self.qrcode_label, 0, Qt.AlignCenter)
        layout.addSpacing(20)
        layout.addWidget(self.file_name_label, 0, Qt.AlignCenter)
        layout.addSpacing(40)
        layout.addWidget(self.notify_label, 0, Qt.AlignCenter)
        layout.addStretch()

        self.qrcode_label.setPixmap(qrcode.make(url, image_factory=Image).pixmap())

if __name__ == "__main__":
    from PyQt5.QtWidgets import QApplication
    import sys
    import signal
    app = QApplication(sys.argv)

    test = AirShareWidget("/home/andy/rms/1.jpg", QColor(0, 0, 0, 255))
    test.show()

    signal.signal(signal.SIGINT, signal.SIG_DFL)
    sys.exit(app.exec_())
