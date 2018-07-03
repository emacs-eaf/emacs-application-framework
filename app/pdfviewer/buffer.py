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
from PyQt5.QtCore import Qt, QRect
from PyQt5.QtGui import QColor, QPixmap, QImage
from PyQt5.QtGui import QPainter
from PyQt5.QtWidgets import QWidget
import fitz
from core.buffer import Buffer

class PdfViewerBuffer(Buffer):
    def __init__(self, buffer_id, url):
        Buffer.__init__(self, buffer_id, url, False, QColor(0, 0, 0, 255))

        self.add_widget(PdfViewerWidget(url, QColor(0, 0, 0, 255)))
        self.buffer_widget.send_jump_page_message.connect(self.send_jump_page_message)

    def send_jump_page_message(self):
        self.send_input_message("Jump to: ", "jump_page")

    def handle_input_message(self, result_type, result_content):
        if result_type == "jump_page":
            self.buffer_widget.jump_to_page(int(result_content))

class PdfViewerWidget(QWidget):

    send_jump_page_message = QtCore.pyqtSignal()

    def __init__(self, url, background_color):
        super(PdfViewerWidget, self).__init__()

        self.background_color = background_color

        # Load document first.
        self.document = fitz.open(url)

        # Get document's page information.
        self.first_pixmap = self.document.getPagePixmap(0)
        self.page_width = self.first_pixmap.width
        self.page_height = self.first_pixmap.height
        self.page_total_number = self.document.pageCount

        # Init scale and scale mode.
        self.scale = 1.0
        self.read_mode = "fit_to_width"

        # Init scroll attributes.
        self.scroll_step = 20
        self.scroll_offset = 0

        # Padding between pages.
        self.page_padding = 10

    def resizeEvent(self, event):
        # Update scale attributes after widget resize.
        self.update_scale()

        QWidget.resizeEvent(self, event)

    def paintEvent(self, event):
        # Init painter.
        painter = QPainter(self)
        painter.save()

        # Draw background.
        painter.setBrush(self.background_color)
        painter.drawRect(0, 0, self.rect().width(), self.rect().height())

        # Get start/last render index.
        start_page_index = int(self.scroll_offset * 1.0 / self.scale / self.page_height)
        last_page_index = int((self.scroll_offset + self.rect().height()) * 1.0 / self.scale / self.page_height)

        # Translate painter at y coordinate.
        translate_y = (start_page_index * self.scale * self.page_height) - self.scroll_offset
        painter.translate(0, translate_y)

        # Render pages in visible area.
        for index in list(range(start_page_index, last_page_index + 1)):
            if index < self.page_total_number:
                # Get page image.
                page = self.document[index]
                trans = fitz.Matrix(self.scale, self.scale)
                pixmap = page.getPixmap(matrix = trans, alpha = False)
                img = QImage(pixmap.samples, pixmap.width, pixmap.height, pixmap.stride, QImage.Format_RGB888)
                qpixmap = QPixmap.fromImage(img)

                # Init render rect.
                render_width = self.page_width * self.scale
                render_height = self.page_height * self.scale
                render_x = (self.rect().width() - render_width) / 2
                render_y = (index - start_page_index) * self.scale * self.page_height

                # Add padding between pages.
                if (index - start_page_index) > 0:
                    painter.translate(0, self.page_padding)

                # Draw page image.
                painter.drawPixmap(QRect(render_x, render_y, render_width, render_height), qpixmap)

        painter.restore()

    def keyPressEvent(self, event):
        if event.key() == Qt.Key_J:
            self.scroll_up()
        elif event.key() == Qt.Key_K:
            self.scroll_down()
        elif event.key() == Qt.Key_Space:
            self.scroll_up_page()
        elif event.key() == Qt.Key_B:
            self.scroll_down_page()
        elif event.key() == Qt.Key_T:
            self.switch_to_read_mode()
        elif event.key() == Qt.Key_Period:
            self.scroll_to_home()
        elif event.key() == Qt.Key_Comma:
            self.scroll_to_end()
        elif event.key() == Qt.Key_0:
            self.zoom_reset()
        elif event.key() == Qt.Key_Equal:
            self.zoom_in()
        elif event.key() == Qt.Key_Minus:
            self.zoom_out()
        elif event.key() == Qt.Key_G:
            self.send_jump_page_message.emit()

    def update_scale(self):
        if self.read_mode == "fit_to_width":
            new_scale = self.rect().width() * 1.0 / self.page_width
            self.scroll_offset = new_scale * 1.0 / self.scale * self.scroll_offset
            self.scale = new_scale
        elif self.read_mode == "fit_to_height":
            new_scale = self.rect().size().height() * 1.0 / self.page_height
            self.scroll_offset = new_scale * 1.0 / self.scale * self.scroll_offset
            self.scale = new_scale

    def max_scroll_offset(self):
        return self.scale * self.page_height * self.page_total_number - self.rect().height()

    def switch_to_read_mode(self):
        if self.read_mode == "fit_to_width":
            self.read_mode = "fit_to_height"
        else:
            self.read_mode = "fit_to_width"

        self.update_scale()
        self.update()

    def scroll_up(self):
        self.scroll_offset = min(self.scroll_offset + self.scale * self.scroll_step, self.max_scroll_offset())
        self.update()

    def scroll_down(self):
        self.scroll_offset = max(self.scroll_offset - self.scale * self.scroll_step, 0)
        self.update()

    def scroll_up_page(self):
        self.scroll_offset = min(self.scroll_offset + self.rect().height(), self.max_scroll_offset())
        self.update()

    def scroll_down_page(self):
        self.scroll_offset = max(self.scroll_offset - self.rect().height(), 0)
        self.update()

    def scroll_to_home(self):
        self.scroll_offset = 0
        self.update()

    def scroll_to_end(self):
        self.scroll_offset = self.max_scroll_offset()
        self.update()

    def zoom_in(self):
        self.scale = min(10, self.scale + 0.2)
        self.update()

    def zoom_out(self):
        self.scale = max(1, self.scale - 0.2)
        self.update()

    def zoom_reset(self):
        self.update_scale()
        self.update()

    def jump_to_page(self, page_num):
        self.scroll_offset = min(max(self.scale * (int(page_num) - 1) * self.page_height, 0), self.max_scroll_offset())
        self.update()

if __name__ == '__main__':
    import sys
    from PyQt5.QtWidgets import QApplication

    app = QApplication(sys.argv)

    w = PdfViewerWidget(sys.argv[1], QColor(0, 0, 0, 255))
    w.resize(1920, 1080)
    w.show()

    sys.exit(app.exec_())
