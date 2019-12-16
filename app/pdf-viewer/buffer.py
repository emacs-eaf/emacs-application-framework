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
from PyQt5.QtCore import Qt, QRect, QEvent
from PyQt5.QtGui import QColor, QPixmap, QImage, QFont, QCursor
from PyQt5.QtGui import QPainter
from PyQt5.QtWidgets import QWidget
from core.buffer import Buffer
import fitz
import time
import random

class AppBuffer(Buffer):
    def __init__(self, buffer_id, url, arguments):
        Buffer.__init__(self, buffer_id, url, arguments, False, QColor(0, 0, 0, 255))

        self.add_widget(PdfViewerWidget(url, QColor(0, 0, 0, 255)))
        self.buffer_widget.send_input_message = self.send_input_message
        self.buffer_widget.translate_double_click_word.connect(self.translate_text)

    def handle_input_message(self, result_type, result_content):
        if result_type == "jump_page":
            self.buffer_widget.jump_to_page(int(result_content))
        elif result_type == "jump_percent":
            self.buffer_widget.jump_to_percent(int(result_content))
        elif result_type == "jump_link":
            self.buffer_widget.jump_to_link(str(result_content))
        elif result_type == "search_text":
            self.buffer_widget.search_text(str(result_content))

    def cancel_input_message(self, result_type):
        if result_type == "jump_link":
            self.buffer_widget.clean_links()

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
        return "{0}:{1}:{2}:{3}".format(self.buffer_widget.scroll_offset,
                                        self.buffer_widget.scale,
                                        self.buffer_widget.read_mode,
                                        self.buffer_widget.inverted_mode)

    def restore_session_data(self, session_data):
        (scroll_offset, scale, read_mode, inverted_mode) = session_data.split(":")
        self.buffer_widget.scroll_offset = float(scroll_offset)
        self.buffer_widget.scale = float(scale)
        self.buffer_widget.read_mode = read_mode
        self.buffer_widget.inverted_mode = inverted_mode == "True"
        self.buffer_widget.update()

    def scroll_up(self):
        self.buffer_widget.scroll_up()

    def scroll_down(self):
        self.buffer_widget.scroll_down()

    def scroll_up_page(self):
        self.buffer_widget.scroll_up_page()

    def scroll_down_page(self):
        self.buffer_widget.scroll_down_page()

    def swtich_to_read_mode(self):
        self.buffer_widget.switch_to_read_mode()

    def scroll_to_home(self):
        self.buffer_widget.scroll_to_home()

    def scroll_to_end(self):
        self.buffer_widget.scroll_to_end()

    def zoom_reset(self):
        self.buffer_widget.zoom_reset()

    def zoom_in(self):
        self.buffer_widget.zoom_in()

    def zoom_out(self):
        self.buffer_widget.zoom_out()

    def jump_to_page(self):
        self.buffer_widget.send_input_message("Jump to: ", "jump_page")

    def jump_to_percent(self):
        self.buffer_widget.send_input_message("Jump to percent: ", "jump_percent")

    def remember_current_position(self):
        self.buffer_widget.remember_current_position()

    def remember_jump(self):
        self.buffer_widget.remember_jump()

    def toggle_inverted_mode(self):
        self.buffer_widget.toggle_inverted_mode()

    def toggle_mark_link(self):
        self.buffer_widget.toggle_mark_link()

    def jump_to_link(self):
        self.buffer_widget.add_mark_jump_link_tips()
        self.buffer_widget.send_input_message("Jump to link: ", "jump_link")

    def search_text(self):
        if self.buffer_widget.is_search_text:
            self.buffer_widget.delete_all_mark_search_text()
        else:
            self.buffer_widget.send_input_message("Search text: ", "search_text")

    def jump_next_match(self):
        if self.buffer_widget.is_search_text:
            self.buffer_widget.jump_next_match()

    def jump_last_match(self):
        if self.buffer_widget.is_search_text:
            self.buffer_widget.jump_last_match()

class PdfViewerWidget(QWidget):
    translate_double_click_word = QtCore.pyqtSignal(str)

    def __init__(self, url, background_color):
        super(PdfViewerWidget, self).__init__()

        self.url = url
        self.background_color = background_color
        self.installEventFilter(self)

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

        # Inverted mode.
        self.inverted_mode = False

        # mark link
        self.is_mark_link = False
        self.mark_link_annot_cache_dict = {}

        #jump link
        self.jump_link_key_cache_dict = {}
        self.jump_link_annot_cache_dict = {}

        # local search text
        self.is_local_search_text = False
        self.local_search_text_annot_cache_dict = {}

        #global search text
        self.is_search_text = False
        self.search_text_offset_list = []

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
        self.page_annotate_light_color = QColor("#333333")
        self.page_annotate_dark_color = QColor("#999999")
        self.font = QFont()
        self.font.setPointSize(12)

        # Page cache.
        self.page_cache_pixmap_dict = {}
        self.page_cache_scale = self.scale
        self.page_cache_trans = None
        self.page_cache_context_delay = 1000

        self.last_action_time = 0

        self.is_page_just_changed = False

        self.remember_offset = None

    def remember_current_position(self):
        self.remember_offset = self.scroll_offset
        self.message_to_emacs.emit("EAF PDF Viewer: Remember Position.")

    def remember_jump(self):
        if self.remember_offset is None:
            self.message_to_emacs.emit("EAF PDF Viewer: Cannot Jump From This Position.")
        else:
            current_scroll_offset = self.scroll_offset
            self.scroll_offset = self.remember_offset
            self.update()

            self.remember_offset = current_scroll_offset

    def get_page_pixmap(self, index, scale):
        # Just return cache pixmap when found match index and scale in cache dict.
        if self.page_cache_scale == scale:
            if index in self.page_cache_pixmap_dict.keys():
                return self.page_cache_pixmap_dict[index]
        # Clear dict if page scale changed.
        else:
            self.page_cache_pixmap_dict.clear()
            self.page_cache_scale = scale
            self.page_cache_trans = fitz.Matrix(scale, scale)

        if self.is_mark_link:
            page = self.add_mark_link(index)
        else:
            self.delete_all_mark_link()
            page = self.document[index]

        # follow page search text
        if self.is_local_search_text:
            page = self.add_mark_local_search_text(page, index)

        trans = self.page_cache_trans if self.page_cache_trans is not None else fitz.Matrix(scale, scale)
        pixmap = page.getPixmap(matrix=trans, alpha=False)

        if self.inverted_mode:
            pixmap.invertIRect(pixmap.irect)

        img = QImage(pixmap.samples, pixmap.width, pixmap.height, pixmap.stride, QImage.Format_RGB888)
        qpixmap = QPixmap.fromImage(img)

        self.page_cache_pixmap_dict[index] = qpixmap

        return qpixmap

    def clean_unused_page_cache_pixmap(self):
        # We need expand render index bound that avoid clean cache around current index.
        start_page_index = max(0, self.get_start_page_index() - 1)
        last_page_index = min(self.page_total_number, self.get_last_page_index() + 1)
        index_list = list(range(start_page_index, last_page_index))

        # Try to clean unused cache.
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
        background_color = self.background_color
        if self.inverted_mode: # change color of background if inverted mode is enable
            background_color = QColor(20, 20, 20, 255)
        painter.setBrush(background_color)
        painter.setPen(background_color)
        painter.drawRect(0, 0, self.rect().width(), self.rect().height())

        # Get start/last render index.
        start_page_index = self.get_start_page_index()
        last_page_index = self.get_last_page_index()

        # Translate painter at y coordinate.
        translate_y = (start_page_index * self.scale * self.page_height) - self.scroll_offset
        painter.translate(0, translate_y)

        # Render pages in visible area.
        for index in list(range(start_page_index, last_page_index)):
            if index < self.page_total_number:
                # Get page image.
                qpixmap = self.get_page_pixmap(index, self.scale)

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
        self.clean_unused_page_cache_pixmap()

        painter.restore()

        # Render current page.
        painter.setFont(self.font)

        if self.inverted_mode:
            painter.setPen(self.page_annotate_dark_color)
        else:
            painter.setPen(self.page_annotate_light_color)

        painter.drawText(QRect(self.rect().x(),
                               self.rect().y() + self.rect().height() - self.page_annotate_height - self.page_annotate_padding_bottom,
                               self.rect().width() - self.page_annotate_padding_right,
                               self.page_annotate_height),
                         Qt.AlignRight,
                         "{0}% ({1}/{2})".format(int((start_page_index + 1) * 100 / self.page_total_number), start_page_index + 1, self.page_total_number))

    def build_context_wrap(f):
        def wrapper(*args):
            # Get self instance object.
            self_obj = args[0]

            # Record page before action.
            page_before_action = self_obj.get_start_page_index()

            # Do action.
            ret = f(*args)

            # Record page after action.
            page_after_action = self_obj.get_start_page_index()
            self_obj.is_page_just_changed = (page_before_action != page_after_action)

            # Start build context timer.
            self_obj.last_action_time = time.time()
            QtCore.QTimer().singleShot(self_obj.page_cache_context_delay, self_obj.build_context_cache)

            return ret

        return wrapper

    @build_context_wrap
    def wheelEvent(self, event):
        if not event.accept():
            self.update_scroll_offset(max(min(self.scroll_offset - self.scale * event.angleDelta().y() / 120 * self.mouse_scroll_offset, self.max_scroll_offset()), 0))

    def get_start_page_index(self):
        return int(self.scroll_offset * 1.0 / self.scale / self.page_height)

    def get_last_page_index(self):
        return int((self.scroll_offset + self.rect().height()) * 1.0 / self.scale / self.page_height) + 1

    def build_context_cache(self):
        # Just build context cache when action duration longer than delay
        # Don't build contexnt cache when is_page_just_changed is True, avoid flickr when user change page.
        last_action_duration = (time.time() - self.last_action_time) * 1000
        if last_action_duration > self.page_cache_context_delay and not self.is_page_just_changed:
            start_page_index = max(0, self.get_start_page_index() - 1)
            last_page_index = min(self.page_total_number, self.get_last_page_index() + 1)

            for index in list(range(start_page_index, last_page_index)):
                self.get_page_pixmap(index, self.scale)

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
        self.update_scroll_offset(min(self.scroll_offset + self.scale * self.scroll_step, self.max_scroll_offset()))

    def scroll_down(self):
        self.update_scroll_offset(max(self.scroll_offset - self.scale * self.scroll_step, 0))

    def scroll_up_page(self):
        # Adjust scroll step to make users continue reading fluently.
        self.update_scroll_offset(min(self.scroll_offset + self.rect().height() - self.scroll_step, self.max_scroll_offset()))

    def scroll_down_page(self):
        # Adjust scroll step to make users continue reading fluently.
        self.update_scroll_offset(max(self.scroll_offset - self.rect().height() + self.scroll_step, 0))

    def scroll_to_home(self):
        self.update_scroll_offset(0)

    def scroll_to_end(self):
        self.update_scroll_offset(self.max_scroll_offset())

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

    def toggle_inverted_mode(self):
        # Need clear page cache first, otherwise current page will not inverted until next page.
        self.page_cache_pixmap_dict.clear()

        # Toggle inverted status.
        self.inverted_mode = not self.inverted_mode

        # Re-render page.
        self.update()

    def toggle_mark_link(self):
        self.is_mark_link = not self.is_mark_link
        self.page_cache_pixmap_dict.clear()
        self.update()

    def add_mark_link(self, index):
        annot_list = []
        page = self.document[index]
        if page.firstLink:
            for link in page.getLinks():
                annot = page.addUnderlineAnnot(link["from"])
                annot.parent = page # Must assign annot parent, else deleteAnnot cause parent is None problem.
                annot_list.append(annot)
            self.mark_link_annot_cache_dict[index] = annot_list
        return page

    def delete_all_mark_link(self):
        if (not self.is_mark_link) and self.mark_link_annot_cache_dict:
            for index in self.mark_link_annot_cache_dict.keys():
                page = self.document[index]
                for annot in self.mark_link_annot_cache_dict[index]:
                    page.deleteAnnot(annot)
        self.mark_link_annot_cache_dict.clear()
        self.update()

    def generate_random_key(self, count):
        letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        key_list = []
        while count > 0:
            key = ''.join(random.choices(letters, k=2))
            if key not in key_list:
                key_list.append(key)
                count -= 1
        return key_list

    def add_mark_jump_link_tips(self):
        # Only mark display page
        start_page_index = self.get_start_page_index()
        last_page_index = self.get_last_page_index()
        tips_size = 7
        annot_list = []

        for page_index in range(start_page_index, last_page_index):
            page = self.document[page_index]
            annot_list = []
            if page.firstLink:
                links = page.getLinks()
                key_list = self.generate_random_key(len(links))
                for index, link in enumerate(links):
                    key = key_list[index]
                    link_rect = link["from"]
                    annot_rect = fitz.Rect(link_rect.top_left, link_rect.x0 + tips_size, link_rect.y0 + tips_size)
                    annot = page.addFreetextAnnot(annot_rect, str(key), fontsize=6, fontname="Cour", \
                                                  text_color=[0.0, 0.0, 0.0], fill_color=[255/255.0, 197/255.0, 36/255.0])
                    annot.parent = page
                    annot_list.append(annot)
                    self.jump_link_key_cache_dict[key] = link

            self.jump_link_annot_cache_dict[page_index] = annot_list

        self.page_cache_pixmap_dict.clear()
        self.update()

    def delete_all_mark_jump_link_tips(self):
        if self.jump_link_annot_cache_dict:
            for index in self.jump_link_annot_cache_dict.keys():
                page = self.document[index]
                for annot in self.jump_link_annot_cache_dict[index]:
                    page.deleteAnnot(annot)
        self.jump_link_key_cache_dict.clear()
        self.jump_link_annot_cache_dict.clear()

    def jump_to_link(self, key):
        key = str(key).upper()
        if key in self.jump_link_key_cache_dict:
            link = self.jump_link_key_cache_dict[key]
            self.remember_current_position()
            self.jump_to_page(link["page"] + 1)
        self.delete_all_mark_jump_link_tips()

        self.update()

    def clean_links(self):
        self.is_mark_link = False
        self.delete_all_mark_jump_link_tips()
        self.page_cache_pixmap_dict.clear()

        self.update()

    def add_mark_local_search_text(self, page, page_index):
        quads_list = page.searchFor(self.search_text, hit_max=999, quads=True)
        annot_list = []
        if quads_list:
            for quads in quads_list:
                annot = page.addHighlightAnnot(quads)
                annot.parent = page
                annot_list.append(annot)
        self.local_search_text_annot_cache_dict[page_index] = annot_list
        return page

    def local_search_text(self, search_text):
        # begin local search
        self.is_local_search_text = True
        self.search_text = search_text
        self.page_cache_pixmap_dict.clear()
        self.update()

    def delete_all_mark_local_search_text(self):
        if self.local_search_text_annot_cache_dict:
            for page_index in self.local_search_text_annot_cache_dict.keys():
                page = self.document[page_index]
                for annot in self.local_search_text_annot_cache_dict[page_index]:
                    page.deleteAnnot(annot)
        self.is_local_search_text = False
        self.search_text = None
        self.local_search_text_annot_cache_dict.clear()
        self.page_cache_pixmap_dict.clear()
        self.update()

    def search_text(self, text):
        self.is_search_text = True
        self.local_search_text(text)

        search_text_index = 0
        self.search_text_index = 0
        for page_index in range(self.page_total_number):
            quads_list = self.document.searchPageFor(page_index, text, hit_max=999, quads=True)
            if quads_list:
                for quad in quads_list:
                    search_text_offset = (page_index * self.page_height + quad.ul.y) * self.scale

                    self.search_text_offset_list.append(search_text_offset)
                    if search_text_offset > self.scroll_offset and search_text_offset < (self.scroll_offset + self.rect().height()):
                        self.search_text_index = search_text_index
                    search_text_index += 1

    def jump_next_match(self):
        if len(self.search_text_offset_list) > 0:
            self.search_text_index = (self.search_text_index + 1) % len(self.search_text_offset_list)
            self.update_scroll_offset(self.search_text_offset_list[self.search_text_index])

    def jump_last_match(self):
        if len(self.search_text_offset_list) > 0:
            self.search_text_index = (self.search_text_index - 1) % len(self.search_text_offset_list)
            self.update_scroll_offset(self.search_text_offset_list[self.search_text_index])

    def delete_all_mark_search_text(self):
        self.is_search_text = False
        self.is_local_search_text = False
        self.delete_all_mark_local_search_text()
        self.search_text_offset_list.clear()

    def jump_to_page(self, page_num):
        self.update_scroll_offset(min(max(self.scale * (int(page_num) - 1) * self.page_height, 0), self.max_scroll_offset()))

    def jump_to_percent(self, percent):
        self.update_scroll_offset(min(max(self.scale * (self.page_total_number * self.page_height * percent / 100.0), 0), self.max_scroll_offset()))

    def update_scroll_offset(self, new_offset):
        if self.scroll_offset != new_offset:
            self.scroll_offset = new_offset
            self.update()

    def get_event_absolute_position(self, event):
        start_page_index = self.get_start_page_index()
        last_page_index = self.get_last_page_index()
        pos = event.pos()

        for index in list(range(start_page_index, last_page_index)):
            if index < self.page_total_number:
                render_width = self.page_width * self.scale
                render_x = int((self.rect().width() - render_width) / 2)

                # computer absolute coordinate of page
                x = int((pos.x() - render_x) * 1.0 / self.scale)
                if pos.y() + self.scroll_offset < (start_page_index + 1) * self.scale * self.page_height:
                    page_offset = self.scroll_offset - start_page_index * self.scale * self.page_height
                    page_index = index
                else:
                    # if display two pages, pos.y() will add page_padding
                    page_offset = self.scroll_offset - (start_page_index + 1) * self.scale * self.page_height - self.page_padding
                    page_index = index + 1
                y = int((pos.y() + page_offset) * 1.0 / self.scale)

                return x, y, page_index
        return None, None, None

    def get_event_link(self, event):
        ex, ey, page_index = self.get_event_absolute_position(event)
        if page_index is None:
            return None

        page = self.document[page_index]
        for link in page.getLinks():
            rect = link["from"]
            if ex >= rect.x0 and ex <= rect.x1 and ey >= rect.y0 and ey <= rect.y1:
                if link["page"]:
                    return link

        return None

    def get_double_click_word(self, event):
        ex, ey, page_index = self.get_event_absolute_position(event)
        if page_index is None:
            return None
        page = self.document[page_index]
        word_offset = 10 # 10 pixel is enough for word intersect operation
        draw_rect = fitz.Rect(ex, ey, ex + word_offset, ey + word_offset)

        page.setCropBox(page.rect)
        page_words = page.getTextWords()
        rect_words = [w for w in page_words if fitz.Rect(w[:4]).intersect(draw_rect)]
        if rect_words:
            return rect_words[0][4]

    def eventFilter(self, obj, event):
        if event.type() == QEvent.MouseButtonPress:
            event_link = self.get_event_link(event)
            if event_link:
                self.jump_to_page(event_link["page"] + 1)
        elif event.type() == QEvent.MouseButtonDblClick:
            double_click_word = self.get_double_click_word(event)
            if double_click_word:
                self.translate_double_click_word.emit(double_click_word)

        return False

if __name__ == '__main__':
    import sys
    from PyQt5.QtWidgets import QApplication

    app = QApplication(sys.argv)

    w = PdfViewerWidget(sys.argv[1], QColor(0, 0, 0, 255))
    w.resize(1920, 1080)
    w.show()

    sys.exit(app.exec_())
