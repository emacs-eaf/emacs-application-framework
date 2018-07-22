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
from PyQt5.QtCore import Qt, QRect
from PyQt5.QtGui import QColor, QPixmap, QImage, QFont
from PyQt5.QtGui import QPainter
from PyQt5.QtWidgets import QWidget
import fitz
from core.buffer import Buffer

class AppBuffer(Buffer):
    def __init__(self, buffer_id, url, arguments):
        Buffer.__init__(self, buffer_id, url, arguments, False, QColor(0, 0, 0, 255))

        self.add_widget(PdfViewerWidget(url, QColor(0, 0, 0, 255)))
        self.buffer_widget.send_input_message = self.send_input_message

    def handle_input_message(self, result_type, result_content):
        if result_type == "jump_page":
            self.buffer_widget.jump_to_page(int(result_content))
        elif result_type == "jump_percent"    :
            self.buffer_widget.jump_to_percent(int(result_content))

    def scroll(self, scroll_direction, scroll_type):
        if scroll_type == "page":
            if scroll_direction == "up":
                self.buffer_widget.scroll_up_page()
            else:
                self.buffer_widget.scroll_down_page()
        else:
            if scroll_direction == "up":
                self.buffer_widget.scroll_up()
            else:
                self.buffer_widget.scroll_down()

    def save_session_data(self):
        return "{0}:{1}:{2}".format(self.buffer_widget.scroll_offset, self.buffer_widget.scale, self.buffer_widget.read_mode)

    def restore_session_data(self, session_data):
        (scroll_offset, scale, read_mode) = session_data.split(":")
        self.buffer_widget.scroll_offset = float(scroll_offset)
        self.buffer_widget.scale = float(scale)
        self.buffer_widget.read_mode = read_mode
        self.buffer_widget.update()

class PdfViewerWidget(QWidget):

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
        self.mouse_scroll_offset = 20

        # Padding between pages.
        self.page_padding = 10

        # Init font.
        self.page_annotate_height = 22
        self.page_annotate_padding_right = 10
        self.page_annotate_padding_bottom = 10
        self.page_annotate_color = QColor("#333333");
        self.font = QFont()
        self.font.setPointSize(12)

        # Page cache.
        self.page_cache_pixmap_dict = {}
        self.page_cache_scale = self.scale

    def get_page_pixmap(self, index, scale):
        # Just return cache pixmap when found match index and scale in cache dict.
        if self.page_cache_scale == scale:
            if index in self.page_cache_pixmap_dict.keys():
                return self.page_cache_pixmap_dict[index]
        # Clear dict if page scale changed.
        else:
            self.page_cache_pixmap_dict.clear()
            self.page_cache_scale = scale

        page = self.document[index]
        trans = fitz.Matrix(scale, scale)
        pixmap = page.getPixmap(matrix = trans, alpha = False)
        img = QImage(pixmap.samples, pixmap.width, pixmap.height, pixmap.stride, QImage.Format_RGB888)
        qpixmap = QPixmap.fromImage(img)

        self.page_cache_pixmap_dict[index] = qpixmap

        return qpixmap

    def clean_unused_page_cache_pixmap(self, index_list):
        cache_index_list = list(self.page_cache_pixmap_dict.keys())

        for cache_index in cache_index_list:
            if cache_index not in index_list:
                self.page_cache_pixmap_dict.pop(cache_index)

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
        painter.setPen(self.background_color)
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
                qpixmap = self.get_page_pixmap(index, self.scale)
                # page = self.document[index]
                # trans = fitz.Matrix(self.scale, self.scale)
                # pixmap = page.getPixmap(matrix = trans, alpha = False)
                # img = QImage(pixmap.samples, pixmap.width, pixmap.height, pixmap.stride, QImage.Format_RGB888)
                # qpixmap = QPixmap.fromImage(img)

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

        # Clean unused pixmap cache that avoid use too much memory.
        self.clean_unused_page_cache_pixmap(list(range(start_page_index, last_page_index + 1)))

        painter.restore()

        # Render current page.
        painter.setFont(self.font)
        painter.setPen(self.page_annotate_color)
        painter.drawText(QRect(self.rect().x(),
                               self.rect().y() + self.rect().height() - self.page_annotate_height - self.page_annotate_padding_bottom,
                               self.rect().width() - self.page_annotate_padding_right,
                               self.page_annotate_height),
                         Qt.AlignRight,
                         "{0}% ({1}/{2})".format(int((start_page_index + 1) * 100 / self.page_total_number), start_page_index + 1, self.page_total_number))

    def wheelEvent(self, event):
        self.scroll_offset = max(min(self.scroll_offset - self.scale * event.angleDelta().y() / 120 * self.mouse_scroll_offset, self.max_scroll_offset()), 0)
        self.update()

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
            self.send_input_message("Jump to: ", "jump_page")
        elif event.key() == Qt.Key_P:
            self.send_input_message("Jump to percent: ", "jump_percent")

    def scale_to(self, new_scale):
        self.scroll_offset = new_scale * 1.0 / self.scale * self.scroll_offset
        self.scale = new_scale

    def scale_to_width(self):
        self.scale_to(self.rect().width() * 1.0 / self.page_width)

    def scale_to_height(self):
        self.scale_to(self.rect().size().height() * 1.0 / self.page_height)

    def update_scale(self):
        if self.read_mode == "fit_to_width":
            self.scale_to_width()
        elif self.read_mode == "fit_to_height":
            self.scale_to_height()

    def max_scroll_offset(self):
        return self.scale * self.page_height * self.page_total_number - self.rect().height()

    def switch_to_read_mode(self):
        if self.read_mode == "fit_to_customize":
            self.read_mode = "fit_to_width"
        elif self.read_mode == "fit_to_width":
            self.read_mode = "fit_to_height"
        elif self.read_mode == "fit_to_height":
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
        # Adjust scroll step to make users continue reading fluently.
        self.scroll_offset = min(self.scroll_offset + self.rect().height() - self.scroll_step, self.max_scroll_offset())
        self.update()

    def scroll_down_page(self):
        # Adjust scroll step to make users continue reading fluently.
        self.scroll_offset = max(self.scroll_offset - self.rect().height() + self.scroll_step, 0)
        self.update()

    def scroll_to_home(self):
        self.scroll_offset = 0
        self.update()

    def scroll_to_end(self):
        self.scroll_offset = self.max_scroll_offset()
        self.update()

    def zoom_in(self):
        self.read_mode = "fit_to_customize"
        self.scale_to(min(10, self.scale + 0.2))
        self.update()

    def zoom_out(self):
        self.read_mode = "fit_to_customize"
        self.scale_to(max(1, self.scale - 0.2))
        self.update()

    def zoom_reset(self):
        self.read_mode = "fit_to_width"
        self.update_scale()
        self.update()

    def jump_to_page(self, page_num):
        self.scroll_offset = min(max(self.scale * (int(page_num) - 1) * self.page_height, 0), self.max_scroll_offset())
        self.update()

    def jump_to_percent(self, percent):
        self.scroll_offset = min(max(self.scale * (int(self.page_total_number * percent / 100.0) - 1) * self.page_height, 0), self.max_scroll_offset())
        self.update()

if __name__ == '__main__':
    import sys
    from PyQt5.QtWidgets import QApplication

    app = QApplication(sys.argv)

    w = PdfViewerWidget(sys.argv[1], QColor(0, 0, 0, 255))
    w.resize(1920, 1080)
    w.show()

    sys.exit(app.exec_())
