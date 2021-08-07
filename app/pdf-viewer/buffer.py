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
from PyQt5.QtCore import Qt, QRect, QEvent, QTimer, QFileSystemWatcher
from PyQt5.QtGui import QColor, QPixmap, QImage, QFont, QCursor
from PyQt5.QtGui import QPainter
from PyQt5.QtWidgets import QWidget
from PyQt5.QtWidgets import QToolTip
from core.buffer import Buffer
from core.utils import touch, interactive, eval_in_emacs, message_to_emacs, open_url_in_new_tab, translate_text, atomic_edit, get_emacs_var, get_emacs_var, get_emacs_config_dir
import fitz
import time
import random
import math
import os
import hashlib
import json
import platform

class AppBuffer(Buffer):
    def __init__(self, buffer_id, url, arguments):
        Buffer.__init__(self, buffer_id, url, arguments, False)

        self.delete_temp_file = arguments == "temp_pdf_file"
        self.add_widget(PdfViewerWidget(url, QColor(get_emacs_var("eaf-buffer-background-color")), buffer_id))
        self.buffer_widget.translate_double_click_word.connect(translate_text)

        self.build_all_methods(self.buffer_widget)

    def destroy_buffer(self):
        if self.delete_temp_file:
            if os.path.exists(self.url):
                os.remove(self.url)

        super().destroy_buffer()

    def get_table_file(self):
        return self.buffer_widget.table_file_path

    def handle_input_response(self, callback_tag, result_content):
        if callback_tag == "jump_page":
            self.buffer_widget.jump_to_page(int(result_content))
        elif callback_tag == "jump_percent":
            self.buffer_widget.jump_to_percent(int(result_content))
        elif callback_tag == "jump_link":
            self.buffer_widget.jump_to_link(str(result_content))
        elif callback_tag == "search_text":
            self.buffer_widget.search_text(str(result_content))

    def cancel_input_response(self, callback_tag):
        if callback_tag == "jump_link":
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
        return "{0}:{1}:{2}:{3}:{4}".format(self.buffer_widget.scroll_offset,
                                        self.buffer_widget.scale,
                                        self.buffer_widget.read_mode,
                                        self.buffer_widget.inverted_mode,
                                        self.buffer_widget.rotation)

    def restore_session_data(self, session_data):
        (scroll_offset, scale, read_mode, inverted_mode, rotation) = ("", "", "", "", "0")
        if session_data.count(":") == 3:
            (scroll_offset, scale, read_mode, inverted_mode) = session_data.split(":")
        else:
            (scroll_offset, scale, read_mode, inverted_mode, rotation) = session_data.split(":")
        self.buffer_widget.scroll_offset = float(scroll_offset)
        self.buffer_widget.scale = float(scale)
        self.buffer_widget.read_mode = read_mode
        self.buffer_widget.rotation = int(rotation)
        if get_emacs_var("eaf-pdf-dark-mode") == "ignore":
            self.buffer_widget.inverted_mode = inverted_mode == "True"
        self.buffer_widget.update()

    def jump_to_page(self):
        self.send_input_message("Jump to Page: ", "jump_page")

    def jump_to_page_with_num(self, page_num):
        self.buffer_widget.jump_to_page(int(page_num))
        return ""

    def jump_to_percent(self):
        self.send_input_message("Jump to Percent: ", "jump_percent")

    def jump_to_percent_with_num(self, percent):
        self.buffer_widget.jump_to_percent(float(percent))
        return ""

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
            eval_in_emacs('kill-new', [content])
            message_to_emacs(content)
            self.buffer_widget.cleanup_select()
        else:
            message_to_emacs("Cannot copy, you should double click your mouse and hover through the text on the PDF. Don't click and drag!")

    def get_select(self):
        if self.buffer_widget.is_select_mode:
            content = self.buffer_widget.parse_select_char_list()
            self.buffer_widget.cleanup_select()
            return content
        else:
            return ""

    def page_total_number(self):
        return str(self.buffer_widget.page_total_number)

    def current_page(self):
        return str(self.buffer_widget.start_page_index + 1)

    def current_percent(self):
        return str(self.buffer_widget.current_percent())

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
            atomic_edit(self.buffer_id, "")
        elif self.buffer_widget.is_hover_annot:
            self.buffer_widget.annot_handler("edit")
        else:
            self.buffer_widget.enable_free_text_annot_mode()

    def set_focus_text(self, new_text):
        if self.buffer_widget.is_select_mode:
            self.buffer_widget.annot_select_char_area("text", new_text)
        elif self.buffer_widget.is_hover_annot:
            self.buffer_widget.update_annot_text(new_text)
        else:
            self.buffer_widget.annot_free_text_annot(new_text)

    def get_toc(self):
        result = ""
        toc = self.buffer_widget.document.get_toc()
        for line in toc:
            result += "{0}{1} {2}\n".format("".join("    " * (line[0] - 1)), line[1], line[2])
        return result

    def get_annots(self, page_index):
        '''
        Return a list of annotations on page_index of types.
        '''
        # Notes: annots need the pymupdf above 1.16.4 version.
        annots = self.buffer_widget.get_annots(int(page_index))
        result = {}
        for annot in annots:
            id = annot.info["id"]
            rect = annot.rect
            type = annot.type
            if len(type) != 2:
                continue
            result[id] = {"page": page_index, "type_int": type[0], "type_name": type[1], "rect": "%s:%s:%s:%s" %(rect.x0, rect.y0, rect.x1, rect.y1)}
        return json.dumps(result)

    def jump_to_rect(self, page_index, rect):
        arr = rect.split(":")
        if len(arr) != 4:
            return ""
        rect = fitz.Rect(float(arr[0]), float(arr[1]), float(arr[2]), float(arr[3]))
        self.buffer_widget.jump_to_rect(int(page_index), rect)
        return ""

class PdfDocument(fitz.Document):
    def __init__(self, document):
        self.document = document
        self._is_trim_margin = False
        self._page_cache_dict = {}
        self._document_page_clip = None
        self._document_page_change = lambda rect: None

    def __getattr__(self, attr):
        return getattr(self.document, attr)

    def __getitem__(self, index):
        if index in self._page_cache_dict:
            page = self._page_cache_dict[index]
            if not self._is_trim_margin:
                return page

            if page.cropbox == self._document_page_clip:
                return page

        page = PdfPage(self.document[index], self.document.isPDF)

        # udpate the page clip
        new_rect_clip = self.computer_page_clip(page.get_tight_margin_rect(), self._document_page_clip)
        if new_rect_clip != self._document_page_clip:
            self._document_page_clip = new_rect_clip
            if self._is_trim_margin:
                self._document_page_change(new_rect_clip)

        if self._is_trim_margin:
            return PdfPage(self.document[index], self.document.isPDF, self._document_page_clip)

        return page

    def computer_page_clip(self, *args):
        '''Update the bestest max page clip.'''
        dr = None
        for r in args:
            if r is None:
                continue
            if dr is None:
                dr = r
                continue
            x0 = min(r.x0, dr.x0)
            y0 = min(r.y0, dr.y0)
            x1 = max(r.x1, dr.x1)
            y1 = max(r.y1, dr.y1)
            dr = fitz.Rect(x0, y0, x1, y1)
        return dr

    def reload_document(self, url):
        self._page_cache_dict = {}
        self.document = fitz.open(url)

    def cache_page(self, index, page):
        self._page_cache_dict[index] = page

    def watch_file(self, path, callback):
        '''
        Refresh content with PDF file changed.
        '''
        self.watch_file_path = path
        self.watch_callback = callback
        self.file_changed_wacher = QFileSystemWatcher()
        if self.file_changed_wacher.addPath(path):
            self.file_changed_wacher.fileChanged.connect(self.handle_file_changed)

    def handle_file_changed(self, path):
        '''
        Use the QFileSystemWatcher watch file changed. If the watch file have been remove or rename,
        this watch will auto remove.
        '''
        if path == self.watch_file_path:
            try:
                # Some program will generate `middle` file, but file already changed, fitz try to
                # open the `middle` file caused error.
                time.sleep(0.1)
                self.reload_document(path)
            except:
                return

            message_to_emacs("Detected that %s has been changed. Refreshing buffer..." %path)
            self.watch_callback()
            # if the file have been renew save, file_changed_watcher will remove the path form monitor list.
            if len(self.file_changed_wacher.files()) == 0 :
                self.file_changed_wacher.addPath(path)

    def toggle_trim_margin(self):
        self._is_trim_margin = not self._is_trim_margin

    def get_page_width(self):
        if self.isPDF:
            if self._is_trim_margin:
                return self._document_page_clip.width
            return self.document.pageCropBox(0).width
        else:
            return self[0].clip.width

    def get_page_height(self):
        if self.isPDF:
            if self._is_trim_margin:
                return self._document_page_clip.height
            return self.document.pageCropBox(0).height
        else:
            return self[0].clip.height

    def watch_page_size_change(self, callback):
        self._document_page_change = callback

class PdfPage(fitz.Page):
    def __init__(self, page, isPDF, clip=None):
        self.page = page
        self.isPDF = isPDF
        self.clip = clip or page.cropbox

        self._mark_link_annot_list = []
        self._mark_search_annot_list = []
        self._mark_jump_annot_list = []

        self._page_rawdict = self._init_page_rawdict()
        self._page_char_rect_list = self._init_page_char_rect_list()
        self._tight_margin_rect = self._init_tight_margin()

    def __getattr__(self, attr):
        return getattr(self.page, attr)

    def _init_page_rawdict(self):
        if self.isPDF:
            # Must set CropBox before get page rawdict , if no,
            # the rawdict bbox coordinate is wrong
            # cause the select text failed
            self.page.set_cropbox(self.clip)
            d = self.page.get_text("rawdict")
            # cancel the cropbox, if not, will cause the pixmap set cropbox
            # don't begin on top-left(0, 0), page display black margin
            self.page.set_cropbox(self.page.mediabox)
            return d
        else:
            return self.page.get_text("rawdict")

    def _init_page_char_rect_list(self):
        '''Collection page char rect list when page init'''
        lines_list = []
        spans_list = []
        chars_list = []

        for block in self._page_rawdict["blocks"]:
            if "lines" in block:
                lines_list += block["lines"]

        for line in lines_list:
            if "spans" in line:
                spans_list += line["spans"]

        for span in spans_list:
            if "chars" in span:
                chars_list += span["chars"]

        return chars_list

    def _init_tight_margin(self):
        r = None
        for block in self._page_rawdict["blocks"]:
            # ignore image bbox
            if block["type"] != 0:
                continue

            x0, y0, x1, y1 = block["bbox"]
            if r is None:
                r = fitz.Rect(x0, y0, x1, y1)
                continue
            x0 = min(x0, r.x0)
            y0 = min(y0, r.y0)
            x1 = max(x1, r.x1)
            y1 = max(y1, r.y1)
            r = fitz.Rect(x0, y0, x1, y1)
        if r is None:
            return self.page.cropbox
        return r

    def get_tight_margin_rect(self):
        # if current page don't computer tight rect
        # return None
        if self._tight_margin_rect == self.page.mediabox:
            return None
        return self._tight_margin_rect

    def get_page_char_rect_list(self):
        return self._page_char_rect_list

    def get_page_char_rect_index(self, x, y):
        '''According X and Y coordinate return index of char in char rect list.'''
        if x and y is None:
            return None

        offset = 15
        rect = fitz.Rect(x, y, x + offset, y + offset)
        for char_index, char in enumerate(self._page_char_rect_list):
            if fitz.Rect(char["bbox"]).intersect(rect):
                return char_index
        return None

    def set_rotation(self, rotation):
        self.page.set_rotation(rotation)
        if rotation % 180 != 0:
            self.page_width = self.page.cropbox.height
            self.page_height = self.page.cropbox.width
        else:
            self.page_width = self.page.cropbox.width
            self.page_height = self.page.cropbox.height

    def get_qpixmap(self, scale, *args):
        if self.isPDF:
            self.page.set_cropbox(self.clip)
        pixmap = self.page.get_pixmap(matrix=fitz.Matrix(scale, scale), alpha=False)
        for fn in args:
            fn(self.page, pixmap, scale)

        img = QImage(pixmap.samples, pixmap.width, pixmap.height, pixmap.stride, QImage.Format_RGB888)
        qpixmap = QPixmap.fromImage(img)
        return qpixmap

    def with_invert(self, invert, exclude_image=True):
        if not invert:
            return lambda page, pixmap, scale: None
        # steps:
        # First, make page all content is invert, include image and text
        # if exclude image is True, will find the page all image, then get
        # each image rect. Finally, again invert all image rect.
        def fn(page, pixmap, scale):
            pixmap.invertIRect(pixmap.irect)
            if not exclude_image:
                return pixmap

            # exclude image only support PDF document
            imagelist = None
            try:
                imagelist = page.get_images(full=True)
            except Exception:
                # PyMupdf 1.14 not include argument 'full'.
                imagelist = page.get_images()

            imagebboxlist = []
            for image in imagelist:
                try:
                    imagerect = page.getImageBbox(image)
                    if imagerect.isInfinite or imagerect.isEmpty:
                        continue
                    else:
                        imagebboxlist.append(imagerect)
                except Exception:
                    pass

            for bbox in imagebboxlist:
                pixmap.invertIRect(bbox * page.rotationMatrix * scale)

        return fn

    def add_mark_link(self):
        if self.page.first_link:
            for link in self.page.get_links():
                annot = self.page.addUnderlineAnnot(link["from"])
                annot.parent = self.page # Must assign annot parent, else deleteAnnot cause parent is None problem.
                self._mark_link_annot_list.append(annot)

    def cleanup_mark_link(self):
        if self._mark_link_annot_list:
            for annot in self._mark_link_annot_list:
                self.page.deleteAnnot(annot)
            self._mark_link_annot_list = []

    def mark_search_text(self, keyword):
        quads_list = self.page.search_for(keyword, hit_max=999, quads=True)
        if quads_list:
            for quads in quads_list:
                annot = self.page.addHighlightAnnot(quads)
                annot.parent = self.page
                self._mark_search_annot_list.append(annot)

    def cleanup_search_text(self):
        if self._mark_search_annot_list:
            message_to_emacs("Unmarked all matched results.")
            for annot in self._mark_search_annot_list:
                self.page.deleteAnnot(annot)
            self._mark_search_annot_list = []

    def mark_jump_link_tips(self, letters):
        tips_size = 4
        cache_dict = {}
        if self.page.first_link:
            links = self.page.get_links()
            key_list = generate_random_key(len(links), letters)
            for index, link in enumerate(links):
                key = key_list[index]
                link_rect = link["from"]
                annot_rect = fitz.Rect(link_rect.top_left, link_rect.x0 + (tips_size * len(key)), link_rect.y0 + 7)
                annot = self.page.addFreetextAnnot(annot_rect, str(key), fontsize=6, fontname="Cour", \
                                              text_color=[0.0, 0.0, 0.0], fill_color=[255/255.0, 197/255.0, 36/255.0])
                annot.parent = self.page
                self._mark_jump_annot_list.append(annot)
                cache_dict[key] = link
        return cache_dict

    def cleanup_jump_link_tips(self):
        for annot in self._mark_jump_annot_list:
            self.page.deleteAnnot(annot)
        self._mark_jump_annot_list = []


class PdfViewerWidget(QWidget):

    translate_double_click_word = QtCore.pyqtSignal(str)

    def __init__(self, url, background_color, buffer_id):
        super(PdfViewerWidget, self).__init__()

        self.url = url
        self.config_dir = get_emacs_config_dir()
        self.background_color = background_color
        self.buffer_id = buffer_id
        self.installEventFilter(self)
        self.setMouseTracking(True)

        # Load document first.
        self.document = PdfDocument(fitz.open(url))
        self.document.watch_file(url, lambda: (self.page_cache_pixmap_dict.clear(), self.update()))

        # Get document's page information.
        self.document.watch_page_size_change(self.update_page_size)
        self.page_width = self.document.get_page_width()
        self.page_height = self.document.get_page_height()
        self.page_total_number = self.document.pageCount

        # Init scale and scale mode.
        self.scale = 1.0
        self.read_mode = "fit_to_width"

        self.rotation = 0

        # Simple string comparation.
        if (get_emacs_var("eaf-pdf-default-zoom") != 1.0):
            self.read_mode = "fit_to_customize"
            self.scale = get_emacs_var("eaf-pdf-default-zoom")
        self.horizontal_offset = 0

        # Inverted mode.
        self.inverted_mode = False
        if (get_emacs_var("eaf-pdf-dark-mode") == "force" or \
            get_emacs_var("eaf-pdf-dark-mode") == True or \
            ((get_emacs_var("eaf-pdf-dark-mode") == "follow" or get_emacs_var("eaf-pdf-dark-mode") == "ignore") and \
             get_emacs_var("eaf-emacs-theme-mode") == "dark")):
            self.inverted_mode = True

        # Inverted mode exclude image. (current exclude image inner implement use PDF Only method)
        self.inverted_mode_exclude_image = get_emacs_var("eaf-pdf-dark-exclude-image") and self.document.isPDF

        # mark link
        self.is_mark_link = False

        #jump link
        self.is_jump_link = False
        self.jump_link_key_cache_dict = {}

        #global search text
        self.is_mark_search = False
        self.search_text_offset_list = []

        # select text
        self.is_select_mode = False
        self.start_char_rect_index = None
        self.start_char_page_index = None
        self.last_char_rect_index = None
        self.last_char_page_index = None
        self.select_area_annot_cache_dict = {k:None for k in range(self.page_total_number)}
        self.select_area_annot_quad_cache_dict = {}

        # annot
        self.is_hover_annot = False
        self.free_text_annot_timer = QTimer()
        self.free_text_annot_timer.setInterval(300)
        self.free_text_annot_timer.setSingleShot(True)
        self.free_text_annot_timer.timeout.connect(self.handle_free_text_annot_mode)
        self.is_free_text_annot_mode = False
        self.free_text_annot_pos = (None, None)
        self.edited_page_annot = (None, None)

        # Init scroll attributes.
        self.scroll_offset = 0
        self.scroll_ratio = 0.05
        self.scroll_wheel_lasttime = time.time()
        if get_emacs_var("eaf-pdf-scroll-ratio") != 0.05:
            self.scroll_ratio = get_emacs_var("eaf-pdf-scroll-ratio")

        # Default presentation mode
        self.presentation_mode = False

        # Padding between pages.
        self.page_padding = 10

        # Init font.
        self.page_annotate_padding_right = 10
        self.page_annotate_padding_bottom = 10
        self.page_annotate_light_color = QColor(get_emacs_var("eaf-emacs-theme-foreground-color"))

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

        self.last_hover_annot_id = None

        self.start_page_index = 0
        self.last_page_index = 0

    @interactive
    def toggle_presentation_mode(self):
        '''
        Toggle presentation mode.
        '''
        self.presentation_mode = not self.presentation_mode
        if self.presentation_mode:
            # Make current page fill the view.
            self.zoom_reset("fit_to_height")
            self.jump_to_page(self.start_page_index + 1)

            message_to_emacs("Presentation Mode.")
        else:
            message_to_emacs("Continuous Mode.")

    @property
    def scroll_step(self):
        return self.rect().height() if self.presentation_mode else self.rect().size().height() * self.scroll_ratio

    @interactive
    def save_current_pos(self):
        self.remember_offset = self.scroll_offset
        message_to_emacs("Saved current position.")

    @interactive
    def jump_to_saved_pos(self):
        if self.remember_offset is None:
            message_to_emacs("Cannot jump from this position.")
        else:
            current_scroll_offset = self.scroll_offset
            self.scroll_offset = self.remember_offset
            self.update()
            self.remember_offset = current_scroll_offset
            message_to_emacs("Jumped to saved position.")

    def get_page_pixmap(self, index, scale, rotation=0):
        # Just return cache pixmap when found match index and scale in cache dict.
        if self.page_cache_scale == scale:
            if index in self.page_cache_pixmap_dict.keys():
                return self.page_cache_pixmap_dict[index]
        # Clear dict if page scale changed.
        else:
            self.page_cache_pixmap_dict.clear()
            self.page_cache_scale = scale

        page = self.document[index]
        if self.document.isPDF:
            page.set_rotation(rotation)

        if self.is_mark_link:
            page.add_mark_link()
        else:
            page.cleanup_mark_link()

        # follow page search text
        if self.is_mark_search:
            page.mark_search_text(self.search_term)
        else:
            page.cleanup_search_text()

        if self.is_jump_link:
            self.jump_link_key_cache_dict.update(page.mark_jump_link_tips(get_emacs_var("eaf-marker-letters")))
        else:
            page.cleanup_jump_link_tips()
            self.jump_link_key_cache_dict.clear()

        if get_emacs_var("eaf-pdf-dark-mode") == "follow" and self.document.isPDF:
            color = inverted_color(get_emacs_var("eaf-emacs-theme-background-color"), self.inverted_mode)
            col = (color.redF(), color.greenF(), color.blueF())
            page.draw_rect(page.cropbox, color=col, fill=col, overlay=False)

        qpixmap = page.get_qpixmap(scale, page.with_invert(self.inverted_mode, self.inverted_mode_exclude_image))

        self.page_cache_pixmap_dict[index] = qpixmap
        self.document.cache_page(index, page)

        return qpixmap

    def clean_unused_page_cache_pixmap(self):
        # We need expand render index bound that avoid clean cache around current index.
        index_list = list(range(self.start_page_index, self.last_page_index))

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
        # update page base information
        self.update_page_index()

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

        if self.scroll_offset > self.max_scroll_offset():
            self.update_vertical_offset(self.max_scroll_offset())

        # Translate painter at y coordinate.
        translate_y = (self.start_page_index * self.scale * self.page_height) - self.scroll_offset
        painter.translate(0, translate_y)

        # Render pages in visible area.
        (render_x, render_y, render_width, render_height) = 0, 0, 0, 0
        for index in list(range(self.start_page_index, self.last_page_index)):
            # Get page image.
            hidpi_scale_factor = self.devicePixelRatioF()
            qpixmap = self.get_page_pixmap(index, self.scale * hidpi_scale_factor, self.rotation)

            # Init render rect.
            render_width = qpixmap.width() / hidpi_scale_factor
            render_height = qpixmap.height() / hidpi_scale_factor
            render_x = (self.rect().width() - render_width) / 2

            # Add padding between pages.
            if (index - self.start_page_index) > 0:
                painter.translate(0, self.page_padding)

            # Draw page image.
            if self.read_mode == "fit_to_customize" and render_width >= self.rect().width():
                render_x = max(min(render_x + self.horizontal_offset, 0), self.rect().width() - render_width) # limit the visiable area size

            painter.drawPixmap(QRect(render_x, render_y, render_width, render_height), qpixmap)

            render_y += render_height

        # Clean unused pixmap cache that avoid use too much memory.
        self.clean_unused_page_cache_pixmap()

        painter.restore()

        # Render current page.
        painter.setFont(self.font)

        if self.rect().width() <= render_width and not self.inverted_mode:
            painter.setPen(inverted_color((get_emacs_var("eaf-emacs-theme-foreground-color")), True))
        else:
            painter.setPen(inverted_color((get_emacs_var("eaf-emacs-theme-foreground-color"))))

        # Draw progress.
        progress_percent = int((self.start_page_index + 1) * 100 / self.page_total_number)
        current_page = self.start_page_index + 1
        painter.drawText(QRect(self.rect().x(),
                               self.rect().y(),
                               self.rect().width() - self.page_annotate_padding_right,
                               self.rect().height() - self.page_annotate_padding_bottom),
                         Qt.AlignRight | Qt.AlignBottom,
                         "{0}% ({1}/{2})".format(progress_percent, current_page, self.page_total_number))

    def build_context_wrap(f):
        def wrapper(*args):
            # Get self instance object.
            self_obj = args[0]

            # Record page before action.
            page_before_action = self_obj.start_page_index

            # Do action.
            ret = f(*args)

            # Record page after action.
            page_after_action = self_obj.start_page_index
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
                numSteps = event.angleDelta().y()
                if self.presentation_mode:
                    # page scrolling
                    curtime = time.time()
                    if curtime - self.scroll_wheel_lasttime > 0.1:
                        numSteps = 1 if numSteps > 0 else -1
                        self.scroll_wheel_lasttime = curtime
                    else:
                        numSteps = 0
                else:
                    # fixed pixel scrolling
                    numSteps = numSteps / 120
                self.update_vertical_offset(max(min(self.scroll_offset - numSteps * self.scroll_step, self.max_scroll_offset()), 0))
            if event.angleDelta().x():
                new_pos = (self.horizontal_offset + event.angleDelta().x() / 120 * self.scroll_step)
                max_pos = (self.page_width * self.scale - self.rect().width())
                self.update_horizontal_offset(max(min(new_pos , max_pos), -max_pos))

    def update_page_index(self):
        self.start_page_index = min(int(self.scroll_offset * 1.0 / self.scale / self.page_height),
                                    self.page_total_number - 1)
        self.last_page_index = min(int((self.scroll_offset + self.rect().height()) * 1.0 / self.scale / self.page_height) + 1,
                                   self.page_total_number)
    def update_page_size(self, rect):
        current_page_index = self.start_page_index
        self.page_width = rect.width
        self.page_height = rect.height
        self.jump_to_page(current_page_index)

    def build_context_cache(self):
        # Just build context cache when action duration longer than delay
        # Don't build contexnt cache when is_page_just_changed is True, avoid flickr when user change page.
        last_action_duration = (time.time() - self.last_action_time) * 1000
        if last_action_duration > self.page_cache_context_delay and not self.is_page_just_changed:
            for index in list(range(self.start_page_index, self.last_page_index)):
                self.get_page_pixmap(index, self.scale, self.rotation)

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

    @interactive
    def toggle_read_mode(self):
        if self.read_mode == "fit_to_customize":
            self.read_mode = "fit_to_width"
        elif self.read_mode == "fit_to_width":
            self.read_mode = "fit_to_height"
        elif self.read_mode == "fit_to_height":
            self.read_mode = "fit_to_width"

        self.update_scale()
        self.update()

    @interactive
    def scroll_up(self):
        self.update_vertical_offset(min(self.scroll_offset + self.scroll_step, self.max_scroll_offset()))

    @interactive
    def scroll_down(self):
        self.update_vertical_offset(max(self.scroll_offset - self.scroll_step, 0))

    @interactive
    def scroll_right(self):
        self.update_horizontal_offset(max(self.horizontal_offset - self.scroll_step, (self.rect().width() - self.page_width * self.scale) / 2))

    @interactive
    def scroll_left(self):
        self.update_horizontal_offset(min(self.horizontal_offset + self.scroll_step, (self.page_width * self.scale - self.rect().width()) / 2))

    @interactive
    def scroll_up_page(self):
        # Adjust scroll step to make users continue reading fluently.
        self.update_vertical_offset(min(self.scroll_offset + self.rect().height() - self.scroll_step, self.max_scroll_offset()))

    @interactive
    def scroll_down_page(self):
        # Adjust scroll step to make users continue reading fluently.
        self.update_vertical_offset(max(self.scroll_offset - self.rect().height() + self.scroll_step, 0))

    @interactive
    def scroll_to_begin(self):
        self.update_vertical_offset(0)

    @interactive
    def scroll_to_end(self):
        self.update_vertical_offset(self.max_scroll_offset())

    @interactive
    def zoom_in(self):
        self.read_mode = "fit_to_customize"
        self.scale_to(min(10, self.scale + 0.2))
        self.update()

    @interactive
    def zoom_out(self):
        self.read_mode = "fit_to_customize"
        self.scale_to(max(1, self.scale - 0.2))
        self.update()

    @interactive
    def zoom_reset(self, read_mode="fit_to_width"):
        if self.is_mark_search:
            self.cleanup_search()
        self.read_mode = read_mode
        self.update_scale()
        self.update()

    @interactive
    def toggle_trim_white_margin(self):
        current_page_index = self.start_page_index
        self.document.toggle_trim_margin()
        self.page_cache_pixmap_dict.clear()
        self.update()
        self.jump_to_page(current_page_index)

    @interactive
    def toggle_inverted_mode(self):
        # Need clear page cache first, otherwise current page will not inverted until next page.
        self.page_cache_pixmap_dict.clear()

        # Toggle inverted status.
        if not self.document.isPDF:
            self.inverted_mode = not self.inverted_mode
            self.update()
            return

        if self.inverted_mode_exclude_image:
            self.inverted_mode_exclude_image = False
        elif self.inverted_mode:
            self.inverted_mode = False
        else:
            self.inverted_mode_exclude_image = True
            self.inverted_mode = True

        # Re-render page.
        self.update()

    @interactive
    def toggle_mark_link(self): #  mark_link will add underline mark on link, using prompt link position.
        self.is_mark_link = not self.is_mark_link and self.document.isPDF
        self.page_cache_pixmap_dict.clear()
        self.update()

    def update_rotate(self, rotate):
        if self.document.isPDF:
            current_page_index = self.start_page_index
            self.rotation = rotate
            self.page_width, self.page_height = self.page_height, self.page_width

            # Need clear page cache first, otherwise current page will not inverted until next page.
            self.page_cache_pixmap_dict.clear()
            self.update_scale()
            self.update()
            self.jump_to_page(current_page_index)
        else:
            message_to_emacs("Only support PDF!")

    @interactive
    def rotate_clockwise(self):
        self.update_rotate((self.rotation + 90) % 360)

    @interactive
    def rotate_counterclockwise(self):
        self.update_rotate((self.rotation - 90) % 360)

    def add_mark_jump_link_tips(self):
        self.is_jump_link = True and self.document.isPDF
        self.page_cache_pixmap_dict.clear()
        self.update()

    def jump_to_link(self, key):
        key = key.upper()
        if key in self.jump_link_key_cache_dict:
            self.handle_jump_to_link(self.jump_link_key_cache_dict[key])

    def handle_jump_to_link(self, link):
        if "page" in link:
            self.cleanup_links()

            self.save_current_pos()
            self.jump_to_page(link["page"] + 1)

            message_to_emacs("Landed on Page " + str(link["page"] + 1))
        elif "uri" in link:
            self.cleanup_links()

            open_url_in_new_tab(link["uri"])
            message_to_emacs("Open " + link["uri"])

    def cleanup_links(self):
        self.is_jump_link = False
        self.page_cache_pixmap_dict.clear()
        self.update()

    def search_text(self, text):
        self.is_mark_search = True
        self.search_term = text

        self.search_text_index = 0
        for page_index in range(self.page_total_number):
            quads_list = self.document.searchPageFor(page_index, text, hit_max=999, quads=True)
            if quads_list:
                for index, quad in enumerate(quads_list):
                    search_text_offset = (page_index * self.page_height + quad.ul.y) * self.scale

                    self.search_text_offset_list.append(search_text_offset)
                    if search_text_offset > self.scroll_offset and search_text_offset < (self.scroll_offset + self.rect().height()):
                        self.search_text_index = index

        if(len(self.search_text_offset_list) == 0):
            message_to_emacs("No results found with \"" + text + "\".")
            self.is_mark_search = False
        else:
            self.page_cache_pixmap_dict.clear()
            self.update()
            self.update_vertical_offset(self.search_text_offset_list[self.search_text_index])
            message_to_emacs("Found " + str(len(self.search_text_offset_list)) + " results with \"" + text + "\".")

    def jump_next_match(self):
        if len(self.search_text_offset_list) > 0:
            self.search_text_index = (self.search_text_index + 1) % len(self.search_text_offset_list)
            self.update_vertical_offset(self.search_text_offset_list[self.search_text_index])
            message_to_emacs("Match " + str(self.search_text_index + 1) + "/" + str(len(self.search_text_offset_list)))

    def jump_last_match(self):
        if len(self.search_text_offset_list) > 0:
            self.search_text_index = (self.search_text_index - 1) % len(self.search_text_offset_list)
            self.update_vertical_offset(self.search_text_offset_list[self.search_text_index])
            message_to_emacs("Match " + str(self.search_text_index + 1) + "/" + str(len(self.search_text_offset_list)))

    def cleanup_search(self):
        self.is_mark_search = False
        self.search_term = None
        self.page_cache_pixmap_dict.clear()
        self.search_text_offset_list.clear()
        self.update()

    def get_select_char_list(self):
        page_dict = {}
        if self.start_char_rect_index and self.last_char_rect_index:
            # start and last page
            sp_index = min(self.start_char_page_index, self.last_char_page_index)
            lp_index = max(self.start_char_page_index, self.last_char_page_index)
            for page_index in range(sp_index, lp_index + 1):
                page_char_list = self.document[page_index].get_page_char_rect_list()

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

    def annot_free_text_annot(self, text=None):
        (point, page_index) = self.free_text_annot_pos
        if point == None or page_index == None:
            return

        page = self.document[page_index]
        new_annot = page.addTextAnnot(point, text, icon="Note")
        new_annot.parent = page

        self.save_annot()

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

            def check_rect(rect):
                tl_x, tl_y, br_x, br_y = rect
                if tl_x <= br_x and tl_y <= br_y:
                    return fitz.Rect(rect)
                # discard the illegal rect. return a micro rect
                return fitz.Rect(tl_x, tl_y, tl_x+1, tl_y+1)

            line_rect_list = list(map(check_rect, line_rect_list))

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
                if annot and annot.parent:
                        annot.parent.deleteAnnot(annot)
                self.select_area_annot_cache_dict[page_index] = None # restore cache
        self.last_char_page_index = None
        self.last_char_rect_index = None
        self.start_char_page_index = None
        self.start_char_rect_index = None

    def get_annots(self, page_index, types=None):
        '''
        Return a list of annotations on page_index of types.
        '''
        # Notes: annots need the pymupdf above 1.16.4 version.
        page = self.document[page_index]
        return page.annots(types)

    def hover_annot(self):
        try:
            ex, ey, page_index = self.get_cursor_absolute_position()
            page = self.document[page_index]
            annot = page.firstAnnot
            if not annot:
                return None, None

            annots = []
            while annot:
                annots.append(annot)
                annot = annot.next

            is_hover_annot = False
            current_annot = None
            for annot in annots:
                if fitz.Point(ex, ey) in annot.rect:
                    # message_to_emacs(annot.info["content"])
                    is_hover_annot = True
                    current_annot = annot
                    opacity = 0.5
                    message_to_emacs("[d]Delete Annot [e]Edit Annot")
                else:
                    opacity = 1.0
                if opacity != annot.opacity:
                    annot.setOpacity(opacity)
                    annot.update()

            # update only if changed
            if is_hover_annot != self.is_hover_annot:
                self.is_hover_annot = is_hover_annot
                self.page_cache_pixmap_dict.clear()
                self.update()

            if current_annot and current_annot.info["content"]:
                if current_annot.info["id"] != self.last_hover_annot_id or not QToolTip.isVisible():
                    QToolTip.showText(QCursor.pos(), current_annot.info["content"], None, QRect(), 10 * 1000)
                self.last_hover_annot_id = current_annot.info["id"]
            else:
                if QToolTip.isVisible():
                    QToolTip.hideText()

            return page, current_annot
        except Exception as e:
            print("Hove Annot: ", e)
            return None, None

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
                self.edited_page_annot = (page, annot)
                atomic_edit(self.buffer_id, annot.info["content"].replace("\r", "\n"))

    def update_annot_text(self, annot_text):
        page, annot = self.edited_page_annot
        if annot.parent:
            annot.setInfo(content=annot_text)
            annot.update()
        self.save_annot()
        self.edited_page_annot = (None, None)

    def jump_to_page(self, page_num):
        self.update_vertical_offset(min(max(self.scale * (int(page_num) - 1) * self.page_height, 0), self.max_scroll_offset()))

    def jump_to_percent(self, percent):
        self.update_vertical_offset(min(max(self.scale * (self.page_total_number * self.page_height * percent / 100.0), 0), self.max_scroll_offset()))

    def jump_to_rect(self, page_index, rect):
        quad = rect.quad
        self.update_vertical_offset((page_index * self.page_height + quad.ul.y) * self.scale)

    def current_percent(self):
        return 100.0 * self.scroll_offset / (self.max_scroll_offset() + self.rect().height())

    def update_vertical_offset(self, new_offset):
        if self.scroll_offset != new_offset:
            self.scroll_offset = new_offset
            self.update()

            eval_in_emacs("eaf--pdf-update-position", [self.start_page_index + 1, self.page_total_number])

    def update_horizontal_offset(self, new_offset):
        if self.horizontal_offset != new_offset:
            self.horizontal_offset = new_offset
            self.update()

    def get_cursor_absolute_position(self):
        pos = self.mapFromGlobal(QCursor.pos()) # map global coordinate to widget coordinate.
        ex, ey = pos.x(), pos.y()

        # set page coordinate
        render_width = self.page_width * self.scale
        render_x = int((self.rect().width() - render_width) / 2)
        if self.read_mode == "fit_to_customize" and render_width >= self.rect().width():
            render_x = max(min(render_x + self.horizontal_offset, 0), self.rect().width() - render_width)

        # computer absolute coordinate of page
        x = (ex - render_x) * 1.0 / self.scale
        if ey + self.scroll_offset < (self.start_page_index + 1) * self.scale * self.page_height:
            page_offset = self.scroll_offset - self.start_page_index * self.scale * self.page_height
            page_index = self.start_page_index
        else:
            # if display two pages, pos.y() will add page_padding
            page_offset = self.scroll_offset - (self.start_page_index + 1) * self.scale * self.page_height - self.page_padding
            page_index = self.start_page_index + 1
        y = (ey + page_offset) * 1.0 / self.scale

        temp = x
        if self.rotation == 90:
            x = y
            y = self.page_width - temp
        elif self.rotation == 180:
            x = self.page_width - x
            y = self.page_height - y
        elif self.rotation == 270:
            x = self.page_height - y
            y = temp

        return x, y, page_index

    def get_event_link(self):
        ex, ey, page_index = self.get_cursor_absolute_position()
        if page_index is None:
            return None

        page = self.document[page_index]
        for link in page.get_links():
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

        page.set_cropbox(page.rect)
        page_words = page.get_text_words()
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

            if self.is_free_text_annot_mode:
                if event.button() != Qt.LeftButton:
                    self.disable_free_text_annot_mode()
            else:
                if event.button() == Qt.LeftButton:
                    # In order to catch mouse move event when drap mouse.
                    self.setMouseTracking(False)
                elif event.button() == Qt.RightButton:
                    self.handle_click_link()

        elif event.type() == QEvent.MouseButtonRelease:
            # Capture move event, event without holding down the mouse.
            self.setMouseTracking(True)
            self.releaseMouse()
            if not self.free_text_annot_timer.isActive():
                self.free_text_annot_timer.start()

            if platform.system() == "Darwin":
                eval_in_emacs('eaf-activate-emacs-window', [])

        elif event.type() == QEvent.MouseButtonDblClick:
            self.disable_free_text_annot_mode()
            if event.button() == Qt.RightButton:
                self.handle_translate_word()

        return False

    def enable_free_text_annot_mode(self):
        self.is_free_text_annot_mode = True
        self.free_text_annot_pos = (None, None)

    def disable_free_text_annot_mode(self):
        self.is_free_text_annot_mode = False

    def handle_free_text_annot_mode(self):
        if self.is_free_text_annot_mode:
            self.disable_free_text_annot_mode()
            ex, ey, page_index = self.get_cursor_absolute_position()
            self.free_text_annot_pos = (fitz.Point(ex, ey), page_index)

            atomic_edit(self.buffer_id, "")

    def handle_select_mode(self):
        self.is_select_mode = True
        ex, ey, page_index = self.get_cursor_absolute_position()
        rect_index = self.document[page_index].get_page_char_rect_index(ex, ey)
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


# utils function
def inverted_color(color, inverted=False):
    color = QColor(color)
    if not inverted:
        return color

    r = 1.0 - float(color.redF())
    g = 1.0 - float(color.greenF())
    b = 1.0 - float(color.blueF())

    col = QColor()
    col.setRgbF(r, g, b)
    return col

def generate_random_key(count, letters):
    key_list = []
    key_len = 1 if count == 1 else math.ceil(math.log(count) / math.log(len(letters)))
    while count > 0:
        key = ''.join(random.choices(letters, k=key_len))
        if key not in key_list:
            key_list.append(key)
            count -= 1
    return key_list
