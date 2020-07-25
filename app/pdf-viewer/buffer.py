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
from core.utils import touch, interactive
import fitz
import time
import random
import math
import os
import hashlib

class AppBuffer(Buffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path):
        Buffer.__init__(self, buffer_id, url, arguments, emacs_var_dict, module_path, False)

        self.delete_temp_file = arguments == "temp_pdf_file"
        self.add_widget(PdfViewerWidget(url, config_dir, QColor(0, 0, 0, 255), buffer_id, emacs_var_dict))
        self.buffer_widget.translate_double_click_word.connect(self.translate_text)

        self.build_all_methods(self.buffer_widget)

    def destroy_buffer(self):
        if self.delete_temp_file:
            if os.path.exists(self.url):
                os.remove(self.url)

        super().destroy_buffer()

    def get_table_file(self):
        return self.buffer_widget.table_file_path

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
            self.buffer_widget.cleanup_links()

    def scroll_other_buffer(self, scroll_direction, scroll_type):
        if scroll_type == "page":
            if scroll_direction == "up":
                self.scroll_up_page()
            else:
                self.scroll_down_page()
        else:
            if scroll_direction == "up":
                self.scroll_up()
            else:
                self.scroll_down()

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
        if self.emacs_var_dict["eaf-pdf-dark-mode"] == "ignore":
            self.buffer_widget.inverted_mode = inverted_mode == "True"
        self.buffer_widget.update()

    def jump_to_page(self):
        self.send_input_message("Jump to Page: ", "jump_page")

    def jump_to_page_with_num(self, page_num):
        self.buffer_widget.jump_to_page(int(page_num))
        return ""

    def jump_to_percent(self):
        self.send_input_message("Jump to Percent: ", "jump_percent")

    def jump_to_link(self):
        self.buffer_widget.add_mark_jump_link_tips()
        self.send_input_message("Jump to Link: ", "jump_link")

    def action_quit(self):
        if self.buffer_widget.is_mark_search:
            self.buffer_widget.cleanup_search()
        if self.buffer_widget.is_jump_link:
            self.buffer_widget.cleanup_links()
        if self.buffer_widget.is_select_mode:
            self.buffer_widget.cleanup_select()

    def search_text_forward(self):
        if self.buffer_widget.is_mark_search:
            self.buffer_widget.jump_next_match()
        else:
            self.send_input_message("Search Text: ", "search_text")

    def search_text_backward(self):
        if self.buffer_widget.is_mark_search:
            self.buffer_widget.jump_last_match()
        else:
            self.send_input_message("Search Text: ", "search_text")

    def copy_select(self):
        if self.buffer_widget.is_select_mode:
            content = self.buffer_widget.parse_select_char_list()
            self.eval_in_emacs.emit('''(kill-new "{}")'''.format(content))
            self.message_to_emacs.emit(content)
            self.buffer_widget.cleanup_select()
        else:
            self.message_to_emacs.emit("Cannot copy, you should double click your mouse and hover through the text on the PDF. Don't click and drag!")

    def current_page(self):
        return str(self.buffer_widget.get_start_page_index() + 1)

    def add_annot_highlight(self):
        if self.buffer_widget.is_select_mode:
            self.buffer_widget.annot_select_char_area("highlight")

    def add_annot_strikeout_or_delete_annot(self):
        if self.buffer_widget.is_select_mode:
            self.buffer_widget.annot_select_char_area("strikeout")
        elif self.buffer_widget.is_hover_annot:
            self.buffer_widget.annot_handler("delete")

    def add_annot_underline(self):
        if self.buffer_widget.is_select_mode:
            self.buffer_widget.annot_select_char_area("underline")

    def add_annot_squiggly(self):
        if self.buffer_widget.is_select_mode:
            self.buffer_widget.annot_select_char_area("squiggly")

    def add_annot_text_or_edit_annot(self):
        if self.buffer_widget.is_select_mode:
            self.buffer_widget.get_focus_text.emit(self.buffer_id, "")
        elif self.buffer_widget.is_hover_annot:
            self.buffer_widget.annot_handler("edit")

    def set_focus_text(self, new_text):
        if self.buffer_widget.is_select_mode:
            self.buffer_widget.annot_select_char_area("text", new_text)
        elif self.buffer_widget.is_hover_annot:
            self.buffer_widget.update_annot_text(new_text)

    def get_toc(self):
        result = ""
        toc = self.buffer_widget.document.getToC()
        for line in toc:
            result += "{0}{1} {2}\n".format("".join("    " * (line[0] - 1)), line[1], line[2])
        return result

class PdfViewerWidget(QWidget):
    translate_double_click_word = QtCore.pyqtSignal(str)
    get_focus_text = QtCore.pyqtSignal(str, str)

    def __init__(self, url, config_dir, background_color, buffer_id, emacs_var_dict):
        super(PdfViewerWidget, self).__init__()

        self.url = url
        self.config_dir = config_dir
        self.background_color = background_color
        self.buffer_id = buffer_id
        self.installEventFilter(self)
        self.setMouseTracking(True)
        self.emacs_var_dict = emacs_var_dict

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
        # Simple string comparation. 
        if (self.emacs_var_dict["eaf-pdf-default-zoom"] != "1.0"):
            self.read_mode = "fit_to_customize"
            self.scale = float(self.emacs_var_dict["eaf-pdf-default-zoom"])
        self.horizontal_offset = 0

        # Inverted mode.
        self.inverted_mode = False
        if (self.emacs_var_dict["eaf-pdf-dark-mode"] == "true" or \
            ((self.emacs_var_dict["eaf-pdf-dark-mode"] == "follow" or self.emacs_var_dict["eaf-pdf-dark-mode"] == "ignore") and \
             self.emacs_var_dict["eaf-emacs-theme-mode"] == "dark")):
            self.inverted_mode = True

        # mark link
        self.is_mark_link = False
        self.mark_link_annot_cache_dict = {}

        #jump link
        self.is_jump_link = False
        self.jump_link_key_cache_dict = {}
        self.jump_link_annot_cache_dict = {}

        #global search text
        self.is_mark_search = False
        self.search_text_offset_list = []
        self.search_text_annot_cache_dict = {}

        # select text
        self.is_select_mode = False
        self.start_char_rect_index = None
        self.start_char_page_index = None
        self.last_char_rect_index = None
        self.last_char_page_index = None
        self.select_area_annot_cache_dict = {}
        self.select_area_annot_quad_cache_dict = {}
        self.char_dict = {k:None for k in range(self.page_total_number)}

        # annot
        self.is_hover_annot = False

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

    def repeat_to_length(self, string_to_expand, length):
        return (string_to_expand * (int(length/len(string_to_expand))+1))[:length]

    @interactive()
    def save_current_pos(self):
        self.remember_offset = self.scroll_offset
        self.buffer.message_to_emacs.emit("Saved current position.")

    @interactive()
    def jump_to_saved_pos(self):
        if self.remember_offset is None:
            self.buffer.message_to_emacs.emit("Cannot jump from this position.")
        else:
            current_scroll_offset = self.scroll_offset
            self.scroll_offset = self.remember_offset
            self.update()
            self.remember_offset = current_scroll_offset
            self.buffer.message_to_emacs.emit("Jumped to saved position.")

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

        page = self.document[index]
        if self.is_mark_link:
            page = self.add_mark_link(index)

        # follow page search text
        if self.is_mark_search:
            page = self.add_mark_search_text(page, index)

        # cache page char_dict
        if self.char_dict[index] is None:
            self.char_dict[index] = self.get_page_char_rect_list(index)
            self.select_area_annot_cache_dict[index] = None

        trans = self.page_cache_trans if self.page_cache_trans is not None else fitz.Matrix(scale, scale)
        pixmap = page.getPixmap(matrix=trans, alpha=False)

        if self.inverted_mode:
            pixmap.invertIRect(pixmap.irect)

            # exclude images
            imagelist = page.getImageList()
            for image in imagelist:
                try:
                    # image[7] is the name of the picture
                    imagerect = page.getImageBbox(image[7])
                    if imagerect.isInfinite or imagerect.isEmpty:
                        continue
                    pixmap.invertIRect(imagerect * self.scale)
                except Exception:
                    pass

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
                if self.read_mode == "fit_to_customize" and render_width >= self.rect().width():
                    render_x = max(min(render_x + self.horizontal_offset, 0), self.rect().width() - render_width) # limit the visiable area size
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
            if event.angleDelta().y():
                self.update_vertical_offset(max(min(self.scroll_offset - self.scale * event.angleDelta().y() / 120 * self.mouse_scroll_offset, self.max_scroll_offset()), 0))
            if event.angleDelta().x():
                new_pos = (self.horizontal_offset + self.scale * event.angleDelta().x() / 120 * self.mouse_scroll_offset)
                max_pos = (self.page_width * self.scale - self.rect().width())
                self.update_horizontal_offset(max(min(new_pos , max_pos), -max_pos))

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

    @interactive()
    def toggle_read_mode(self):
        if self.read_mode == "fit_to_customize":
            self.read_mode = "fit_to_width"
        elif self.read_mode == "fit_to_width":
            self.read_mode = "fit_to_height"
        elif self.read_mode == "fit_to_height":
            self.read_mode = "fit_to_width"

        self.update_scale()
        self.update()

    @interactive()
    def scroll_up(self):
        self.update_vertical_offset(min(self.scroll_offset + self.scale * self.scroll_step, self.max_scroll_offset()))

    @interactive()
    def scroll_down(self):
        self.update_vertical_offset(max(self.scroll_offset - self.scale * self.scroll_step, 0))

    @interactive()
    def scroll_right(self):
        self.update_horizontal_offset(max(self.horizontal_offset - self.scale * 30, (self.rect().width() - self.page_width * self.scale) / 2))

    @interactive()
    def scroll_left(self):
        self.update_horizontal_offset(min(self.horizontal_offset + (self.scale * 30), (self.page_width * self.scale - self.rect().width()) / 2))

    @interactive()
    def scroll_up_page(self):
        # Adjust scroll step to make users continue reading fluently.
        self.update_vertical_offset(min(self.scroll_offset + self.rect().height() - self.scroll_step, self.max_scroll_offset()))

    @interactive()
    def scroll_down_page(self):
        # Adjust scroll step to make users continue reading fluently.
        self.update_vertical_offset(max(self.scroll_offset - self.rect().height() + self.scroll_step, 0))

    @interactive()
    def scroll_to_begin(self):
        self.update_vertical_offset(0)

    @interactive()
    def scroll_to_end(self):
        self.update_vertical_offset(self.max_scroll_offset())

    @interactive()
    def zoom_in(self):
        if self.is_mark_search:
            self.cleanup_search()
        self.read_mode = "fit_to_customize"
        self.scale_to(min(10, self.scale + 0.2))
        self.update()

    @interactive()
    def zoom_out(self):
        if self.is_mark_search:
            self.cleanup_search()
        self.read_mode = "fit_to_customize"
        self.scale_to(max(1, self.scale - 0.2))
        self.update()

    @interactive()
    def zoom_reset(self):
        if self.is_mark_search:
            self.cleanup_search()
        self.read_mode = "fit_to_width"
        self.update_scale()
        self.update()

    @interactive()
    def toggle_inverted_mode(self):
        # Need clear page cache first, otherwise current page will not inverted until next page.
        self.page_cache_pixmap_dict.clear()

        # Toggle inverted status.
        self.inverted_mode = not self.inverted_mode

        # Re-render page.
        self.update()

    @interactive()
    def toggle_mark_link(self): #  mark_link will add underline mark on link, using prompt link position.
        if self.is_mark_link:
            self.cleanup_mark_link()
        else:
            self.is_mark_link = True

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

    def cleanup_mark_link(self):
        if self.mark_link_annot_cache_dict:
            for index in self.mark_link_annot_cache_dict.keys():
                page = self.document[index]
                for annot in self.mark_link_annot_cache_dict[index]:
                    page.deleteAnnot(annot)
        self.is_mark_link = False
        self.mark_link_annot_cache_dict.clear()

    def generate_random_key(self, count):
        letters = self.emacs_var_dict["eaf-marker-letters"]
        key_list = []
        key_len = 1 if count == 1 else math.ceil(math.log(count) / math.log(len(letters)))
        while count > 0:
            key = ''.join(random.choices(letters, k=key_len))
            if key not in key_list:
                key_list.append(key)
                count -= 1
        return key_list

    def add_mark_jump_link_tips(self):
        # Only mark display page
        start_page_index = self.get_start_page_index()
        last_page_index = self.get_last_page_index()
        tips_size = 4
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
                    annot_rect = fitz.Rect(link_rect.top_left, link_rect.x0 + (tips_size * len(key)), link_rect.y0 + 7)
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
        self.is_jump_link = True
        key = key.upper()
        if key in self.jump_link_key_cache_dict:
            self.handle_jump_to_link(self.jump_link_key_cache_dict[key])

    def handle_jump_to_link(self, link):
        if "page" in link:
            self.cleanup_links()

            self.save_current_pos()
            self.jump_to_page(link["page"] + 1)

            self.buffer.message_to_emacs.emit("Landed on Page " + str(link["page"] + 1))
        elif "uri" in link:
            self.cleanup_links()

            self.buffer.open_url_in_new_tab.emit(link["uri"])
            self.buffer.message_to_emacs.emit("Open " + link["uri"])

    def cleanup_links(self):
        self.is_jump_link = False
        self.delete_all_mark_jump_link_tips()
        self.page_cache_pixmap_dict.clear()

        self.update()

    def add_mark_search_text(self, page, page_index):
        quads_list = page.searchFor(self.search_term, hit_max=999, quads=True)
        annot_list = []
        if quads_list:
            for quads in quads_list:
                annot = page.addHighlightAnnot(quads)
                annot.parent = page
                annot_list.append(annot)
        self.search_text_annot_cache_dict[page_index] = annot_list

        return page

    def search_text(self, text):
        self.is_mark_search = True
        self.search_term = text
        self.page_cache_pixmap_dict.clear()

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
        self.update()
        if(len(self.search_text_offset_list) == 0):
            self.buffer.message_to_emacs.emit("No results found with \"" + text + "\".")
            self.is_mark_search = False
        else:
            self.update_vertical_offset(self.search_text_offset_list[self.search_text_index])
            self.buffer.message_to_emacs.emit("Found " + str(len(self.search_text_offset_list)) + " results with \"" + text + "\".")

    def jump_next_match(self):
        if len(self.search_text_offset_list) > 0:
            self.search_text_index = (self.search_text_index + 1) % len(self.search_text_offset_list)
            self.update_vertical_offset(self.search_text_offset_list[self.search_text_index])
            self.buffer.message_to_emacs.emit("Match " + str(self.search_text_index + 1) + "/" + str(len(self.search_text_offset_list)))

    def jump_last_match(self):
        if len(self.search_text_offset_list) > 0:
            self.search_text_index = (self.search_text_index - 1) % len(self.search_text_offset_list)
            self.update_vertical_offset(self.search_text_offset_list[self.search_text_index])
            self.buffer.message_to_emacs.emit("Match " + str(self.search_text_index + 1) + "/" + str(len(self.search_text_offset_list)))

    def cleanup_search(self):
        self.buffer.message_to_emacs.emit("Unmarked all matched results.")
        if self.search_text_annot_cache_dict:
            for page_index in self.search_text_annot_cache_dict.keys():
                page = self.document[page_index]
                for annot in self.search_text_annot_cache_dict[page_index]:
                    page.deleteAnnot(annot)
        self.is_mark_search = False
        self.search_term = None
        self.search_text_annot_cache_dict.clear()
        self.page_cache_pixmap_dict.clear()
        self.search_text_offset_list.clear()
        self.update()

    def get_page_char_rect_list(self, page_index):
        lines_list = []
        spans_list = []
        chars_list = []

        page_rawdict = self.document[page_index].getText("rawdict")
        for block in page_rawdict["blocks"]:
            if "lines" in block:
                lines_list += block["lines"]

        for line in lines_list:
            if "spans" in line:
                spans_list += line["spans"]

        for span in spans_list:
            if "chars" in span:
                chars_list += span["chars"]

        return chars_list

    def get_char_rect_index(self):
        offset = 15
        ex, ey, page_index = self.get_cursor_absolute_position()
        if ex and ey and page_index is not None:
            rect = fitz.Rect(ex, ey, ex + offset, ey + offset)
            for char_index, char in enumerate(self.char_dict[page_index]):
                if fitz.Rect(char["bbox"]).intersect(rect):
                    return char_index, page_index
        return None, None

    def get_select_char_list(self):
        page_dict = {}
        if self.start_char_rect_index and self.last_char_rect_index:
            # start and last page
            sp_index = min(self.start_char_page_index, self.last_char_page_index)
            lp_index = max(self.start_char_page_index, self.last_char_page_index)
            for page_index in range(sp_index, lp_index + 1):
                page_char_list = self.char_dict[page_index]

                if page_char_list:
                # handle forward select and backward select on multi page.
                # backward select on multi page.
                    if self.start_char_page_index > self.last_char_page_index:
                        sc = self.last_char_rect_index if page_index == sp_index else 0
                        lc = self.start_char_rect_index if page_index == lp_index else len(page_char_list)
                    else:
                        # forward select on multi page.
                        sc = self.start_char_rect_index if page_index == sp_index else 0
                        lc = self.last_char_rect_index if page_index == lp_index else len(page_char_list)

                    # handle forward select and backward select on same page.
                    sc_index = min(sc, lc)
                    lc_index = max(sc, lc)

                    page_dict[page_index] = page_char_list[sc_index : lc_index + 1]

        return page_dict

    def parse_select_char_list(self):
        string = ""
        page_dict = self.get_select_char_list()
        for index, chars_list in enumerate(page_dict.values()):
            if chars_list:
                string += "".join(list(map(lambda x: x["c"], chars_list)))

                if index != 0:
                    string += "\n\n"    # add new line on page end.
        return string

    def annot_select_char_area(self, annot_type="highlight", text=None):
        self.cleanup_select()   # needs first cleanup select highlight mark.
        for page_index, quad_list in self.select_area_annot_quad_cache_dict.items():
            page = self.document[page_index]

            if annot_type == "highlight":
                new_annot = page.addHighlightAnnot(quad_list)
            elif annot_type == "strikeout":
                new_annot = page.addStrikeoutAnnot(quad_list)
            elif annot_type == "underline":
                new_annot = page.addUnderlineAnnot(quad_list)
            elif annot_type == "squiggly":
                new_annot = page.addSquigglyAnnot(quad_list)
            elif annot_type == "text":
                point = quad_list[-1].lr # lower right point
                new_annot = page.addTextAnnot(point, text, icon="Note")

            new_annot.parent = page
        self.document.saveIncr()
        self.select_area_annot_quad_cache_dict.clear()

    def cleanup_select(self):
        self.is_select_mode = False
        self.delete_all_mark_select_area()
        self.page_cache_pixmap_dict.clear()
        self.update()

    def mark_select_char_area(self):
        page_dict = self.get_select_char_list()
        for page_index, chars_list in page_dict.items():
            # Using multi line rect make of abnormity select area.
            line_rect_list = []
            if chars_list:
                # every char has bbox property store char rect.
                bbox_list = list(map(lambda x: x["bbox"], chars_list))

                # With char order is left to right, if the after char x-axis more than before
                # char x-axis, will determine have "\n" between on both.
                if len(bbox_list) >= 2:
                    tl_x, tl_y = 0, 0 # top left point
                    for index, bbox in enumerate(bbox_list[:-1]):
                        if (tl_x == 0) or (tl_y == 0):
                            tl_x, tl_y = bbox[:2]
                        if bbox[0] > bbox_list[index + 1][2]:
                            br_x, br_y = bbox[2:] # bottom right
                            line_rect_list.append((tl_x, tl_y, br_x, br_y))
                            tl_x, tl_y = 0, 0

                    lc = bbox_list[-1]  # The last char
                    line_rect_list.append((tl_x, tl_y, lc[2], lc[3]))
                else:
                    # if only one char selected.
                    line_rect_list.append(bbox_list[0])

            line_rect_list = list(map(lambda x: fitz.Rect(x), line_rect_list))

            page = self.document[page_index]
            old_annot = self.select_area_annot_cache_dict[page_index]
            if old_annot:
                page.deleteAnnot(old_annot)

            quad_list = list(map(lambda x: x.quad, line_rect_list))
            annot = page.addHighlightAnnot(quad_list)
            annot.parent = page

            # refresh annot
            self.select_area_annot_cache_dict[page_index] = annot
            self.select_area_annot_quad_cache_dict[page_index] = quad_list

        self.page_cache_pixmap_dict.clear()
        self.update()

    def delete_all_mark_select_area(self):
        if self.select_area_annot_cache_dict:
            for page_index, annot in self.select_area_annot_cache_dict.items():
                page = self.document[page_index]
                if annot and annot.parent:
                        page.deleteAnnot(annot)
                self.select_area_annot_cache_dict[page_index] = None # restore cache
        self.last_char_page_index = None
        self.last_char_rect_index = None
        self.start_char_page_index = None
        self.start_char_rect_index = None

    def hover_annot(self):
        ex, ey, page_index = self.get_cursor_absolute_position()
        page = self.document[page_index]
        annot = page.firstAnnot
        if not annot:
            return None, None

        annots = []
        while annot:
            annots.append(annot)
            annot = annot.next

        for annot in annots:
            if fitz.Point(ex, ey) in annot.rect:
                self.is_hover_annot = True
                annot.setOpacity(0.5)
                self.buffer.message_to_emacs.emit("[d]Delete Annot [e]Edit Annot")
            else:
                annot.setOpacity(1) # restore annot
                self.is_hover_annot = False
            annot.update()

        self.page_cache_pixmap_dict.clear()
        self.update()
        return page, annot

    def save_annot(self):
        self.document.saveIncr()
        self.page_cache_pixmap_dict.clear()
        self.update()

    def annot_handler(self, action=None):
        page, annot = self.hover_annot()
        if annot.parent:
            if action == "delete":
                page.deleteAnnot(annot)
                self.save_annot()
            if action == "edit":
                if annot.type[0] == 0:
                    self.get_focus_text.emit(self.buffer_id, annot.info["content"])
                else:
                    self.buffer.message_to_emacs.emit("Cannot edit. Only support text annot type.")

    def update_annot_text(self, annot_text):
        page, annot = self.hover_annot()
        if annot.parent:
            annot.setInfo(content=annot_text)
            annot.update()
        self.save_annot()

    def jump_to_page(self, page_num):
        self.update_vertical_offset(min(max(self.scale * (int(page_num) - 1) * self.page_height, 0), self.max_scroll_offset()))

    def jump_to_percent(self, percent):
        self.update_vertical_offset(min(max(self.scale * (self.page_total_number * self.page_height * percent / 100.0), 0), self.max_scroll_offset()))

    def update_vertical_offset(self, new_offset):
        if self.scroll_offset != new_offset:
            self.scroll_offset = new_offset
            self.update()

    def update_horizontal_offset(self, new_offset):
        if self.horizontal_offset != new_offset:
            self.horizontal_offset = new_offset
            self.update()

    def get_cursor_absolute_position(self):
        start_page_index = self.get_start_page_index()
        last_page_index = self.get_last_page_index()
        pos = self.mapFromGlobal(QCursor.pos()) # map global coordinate to widget coordinate.
        ex, ey = pos.x(), pos.y()

        for index in list(range(start_page_index, last_page_index)):
            if index < self.page_total_number:
                render_width = self.page_width * self.scale
                render_x = int((self.rect().width() - render_width) / 2)
                if self.read_mode == "fit_to_customize" and render_width >= self.rect().width():
                    render_x = max(min(render_x + self.horizontal_offset, 0), self.rect().width() - render_width)

                # computer absolute coordinate of page
                x = (ex - render_x) * 1.0 / self.scale
                if ey + self.scroll_offset < (start_page_index + 1) * self.scale * self.page_height:
                    page_offset = self.scroll_offset - start_page_index * self.scale * self.page_height
                    page_index = index
                else:
                    # if display two pages, pos.y() will add page_padding
                    page_offset = self.scroll_offset - (start_page_index + 1) * self.scale * self.page_height - self.page_padding
                    page_index = index + 1
                y = (ey + page_offset) * 1.0 / self.scale

                return x, y, page_index
        return None, None, None

    def get_event_link(self):
        ex, ey, page_index = self.get_cursor_absolute_position()
        if page_index is None:
            return None

        page = self.document[page_index]
        for link in page.getLinks():
            rect = link["from"]
            if ex >= rect.x0 and ex <= rect.x1 and ey >= rect.y0 and ey <= rect.y1:
                return link

        return None

    def get_double_click_word(self):
        ex, ey, page_index = self.get_cursor_absolute_position()
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
        if event.type() in [QEvent.MouseMove, QEvent.MouseButtonDblClick, QEvent.MouseButtonPress]:
            if not self.document.isPDF:
                return False

        if event.type() == QEvent.MouseMove:
            if self.hasMouseTracking():
                self.hover_annot()
            else:
                self.handle_select_mode()

        elif event.type() == QEvent.MouseButtonPress:
            # add this detect release mouse event
            self.grabMouse()

            # cleanup select mode on another click
            if self.is_select_mode:
                self.cleanup_select()

            if event.button() == Qt.LeftButton:
                # In order to catch mouse move event when drap mouse.
                self.setMouseTracking(False)
            elif event.button() == Qt.RightButton:
                self.handle_click_link()

        elif event.type() == QEvent.MouseButtonRelease:
            # Capture move event, event without holding down the mouse.
            self.setMouseTracking(True)
            self.releaseMouse()

        elif event.type() == QEvent.MouseButtonDblClick:
            if self.is_mark_search:
                self.cleanup_search()
            if event.button() == Qt.RightButton:
                self.handle_translate_word()

        return False

    def handle_select_mode(self):
        self.is_select_mode = True
        rect_index, page_index = self.get_char_rect_index()
        if rect_index and page_index is not None:
            if self.start_char_rect_index is None or self.start_char_page_index is None:
                self.start_char_rect_index, self.start_char_page_index = rect_index, page_index
            else:
                self.last_char_rect_index, self.last_char_page_index = rect_index, page_index
                self.mark_select_char_area()

    def handle_click_link(self):
        event_link = self.get_event_link()
        if event_link:
            self.handle_jump_to_link(event_link)

    def handle_translate_word(self):
        double_click_word = self.get_double_click_word()
        if double_click_word:
            self.translate_double_click_word.emit(double_click_word)
