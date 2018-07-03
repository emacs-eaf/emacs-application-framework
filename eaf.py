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

from PyQt5.QtWidgets import QApplication
from core.fake_key_event import fake_key_event
from core.utils import file_is_image, file_is_video
from core.view import View
from dbus.mainloop.glib import DBusGMainLoop
from pymediainfo import MediaInfo
import dbus
import dbus.service
import os

EAF_DBUS_NAME = "com.lazycat.eaf"
EAF_OBJECT_NAME = "/com/lazycat/eaf"

class EAF(dbus.service.Object):
    def __init__(self, args):
        global emacs_xid, emacs_width, emacs_height

        dbus.service.Object.__init__(
            self,
            dbus.service.BusName(EAF_DBUS_NAME, bus=dbus.SessionBus()),
            EAF_OBJECT_NAME)

        (emacs_xid, emacs_width, emacs_height) = (map(lambda x: int(x), args))
        self.buffer_dict = {}
        self.view_dict = {}

        self.start_finish()

    @dbus.service.method(EAF_DBUS_NAME, in_signature="ss", out_signature="s")
    def new_buffer(self, buffer_id, url):
        if url == "eaf-demo":
            try:
                from app.demo.buffer import DemoBuffer
                self.create_buffer(buffer_id, DemoBuffer(buffer_id, url))
            except ImportError:
                return "Something wrong when import app.demo.buffer"
        else:
            url = os.path.expanduser(url)

            if url.startswith("/"):
                if os.path.exists(url):
                    (_, extension) = os.path.splitext(url)

                    if extension in [".pdf", ".xps", ".oxps", ".cbz", ".epub", ".fb2", "fbz"]:
                        try:
                            from app.pdfviewer.buffer import PdfViewerBuffer
                            self.create_buffer(buffer_id, PdfViewerBuffer(buffer_id, url))
                        except ImportError:
                            return "Something wrong when import app.pdfviewer.buffer"
                    else:
                        file_info = MediaInfo.parse(url)
                        if file_is_image(file_info):
                            try:
                                from app.imageviewer.buffer import ImageViewerBuffer
                                self.create_buffer(buffer_id, ImageViewerBuffer(buffer_id, url))
                            except ImportError:
                                return "Something wrong when import app.imageviewer.buffer"
                        elif file_is_video(file_info):
                            try:
                                from app.videoplayer.buffer import VideoPlayerBuffer
                                self.create_buffer(buffer_id, VideoPlayerBuffer(buffer_id, url))
                            except ImportError:
                                return "Something wrong when import app.videoplayer.buffer"
                        else:
                            return "Don't know how to open {0}".format(url)
                else:
                    return "Path {0} not exists.".format(url)
            else:
                try:
                    from app.browser.buffer import BrowserBuffer
                    from urllib.parse import urlparse
                    result = urlparse(url)
                    if len(result.scheme) != 0:
                        self.create_buffer(buffer_id, BrowserBuffer(buffer_id, result.geturl()))
                    else:
                        result = urlparse("{0}:{1}".format("http", url))
                        if result.scheme != "":
                            self.create_buffer(buffer_id, BrowserBuffer(buffer_id, result.geturl()))
                        else:
                            return "{0} is not valid url".format(url)
                except ImportError:
                    return "Something wrong when import app.browser.buffer"
        return ""

    def create_buffer(self, buffer_id, app_buffer):
        global emacs_width, emacs_height

        # Add buffer to buffer dict.
        self.buffer_dict[buffer_id] = app_buffer

        # Resize buffer with emacs max window size,
        # view (QGraphicsView) will adjust visual area along with emacs window changed.
        app_buffer.buffer_widget.resize(emacs_width, emacs_height)

        # Monitor buffer signals.
        app_buffer.update_title.connect(self.update_buffer_title)
        app_buffer.open_url.connect(self.open_buffer_url)

        # Send message to emacs.
        app_buffer.input_message.connect(self.input_message)

    @dbus.service.method(EAF_DBUS_NAME, in_signature="s", out_signature="")
    def update_views(self, args):
        global emacs_xid

        view_infos = args.split(",")

        # Do something if buffer's all view hide after update_views operation.
        old_view_buffer_ids = list(set(map(lambda v: v.buffer_id, self.view_dict.values())))
        new_view_buffer_ids = list(set(map(lambda v: v.split(":")[0], view_infos)))

        # Call all_views_hide interface when buffer's all views will hide.
        # We do something in app's buffer interface, such as videoplayer will pause video when all views hide.
        # Note, we must call this function before last view destroy,
        # such as QGraphicsVideoItem will report "Internal data stream error" error.
        for old_view_buffer_id in old_view_buffer_ids:
            if old_view_buffer_id not in new_view_buffer_ids:
                self.buffer_dict[old_view_buffer_id].all_views_hide()

        # Remove old key from view dict and destroy old view.
        for key in list(self.view_dict):
            if key not in view_infos:
                self.view_dict[key].handle_destroy()
                self.view_dict.pop(key, None)

        # Create new view and udpate in view dict.
        if view_infos != ['']:
            for view_info in view_infos:
                if view_info not in self.view_dict:
                    (buffer_id, _, _, _, _) = view_info.split(":")
                    view = View(emacs_xid, self.buffer_dict[buffer_id], view_info)
                    self.view_dict[view_info] = view

                    view.trigger_focus_event.connect(self.focus_emacs_buffer)

        # Call some_view_show interface when buffer's view switch back.
        # Note, this must call after new view create, otherwise some buffer,
        # such as QGraphicsVideoItem will report "Internal data stream error" error.
        if view_infos != ['']:
            for new_view_buffer_id in new_view_buffer_ids:
                if new_view_buffer_id not in old_view_buffer_ids:
                    self.buffer_dict[new_view_buffer_id].some_view_show()

        # Adjust buffer size along with views change.
        # Note: just buffer that option `fit_to_view' is False need to adjust,
        # if buffer option fit_to_view is True, buffer render adjust by view.resizeEvent()
        for buffer in list(self.buffer_dict.values()):
            if not buffer.fit_to_view:
                buffer_views = list(filter(lambda v: v.buffer_id == buffer.buffer_id, list(self.view_dict.values())))

                # Adjust buffer size to max view's size.
                if len(buffer_views) > 0:
                    max_view = max(buffer_views, key=lambda v: v.width * v.height)

                    buffer.buffer_widget.resize(max_view.width, max_view.height)
                # Adjust buffer size to emacs window size if not match view found.
                else:
                    buffer.buffer_widget.resize(emacs_width, emacs_height)

    @dbus.service.method(EAF_DBUS_NAME, in_signature="s", out_signature="")
    def kill_buffer(self, buffer_id):
        # Kill all view base on buffer_id.
        for key in list(self.view_dict):
            if buffer_id == self.view_dict[key].buffer_id:
                self.view_dict[key].handle_destroy()
                self.view_dict.pop(key, None)

        # Clean buffer from buffer dict.
        if buffer_id in self.buffer_dict:
            self.buffer_dict[buffer_id].handle_destroy()
            self.buffer_dict.pop(buffer_id, None)

    @dbus.service.method(EAF_DBUS_NAME, in_signature="s", out_signature="")
    def send_key(self, args):
        # Get buffer id and event string.
        (buffer_id, event_string) = args.split(":")

        # Send event to buffer when found match buffer.
        if buffer_id in self.buffer_dict:
            self.buffer_dict[buffer_id].send_key_event(fake_key_event(event_string))

    @dbus.service.method(EAF_DBUS_NAME, in_signature="sss", out_signature="")
    def handle_input_message(self, buffer_id, callback_type, callback_result):
        for buffer in list(self.buffer_dict.values()):
            if buffer.buffer_id == buffer_id:
                buffer.handle_input_message(callback_type, callback_result)

    @dbus.service.signal("com.lazycat.eaf")
    def focus_emacs_buffer(self, message):
        pass

    @dbus.service.signal("com.lazycat.eaf")
    def start_finish(self):
        pass

    @dbus.service.signal("com.lazycat.eaf")
    def update_buffer_title(self, buffer_id, title):
        pass

    @dbus.service.signal("com.lazycat.eaf")
    def open_buffer_url(self, url):
        pass

    @dbus.service.signal("com.lazycat.eaf")
    def input_message(self, buffer_id, message, callback_type):
        pass

if __name__ == "__main__":
    import sys
    import signal

    DBusGMainLoop(set_as_default=True) # WARING: only use once in one process

    bus = dbus.SessionBus()
    if bus.request_name(EAF_DBUS_NAME) != dbus.bus.REQUEST_NAME_REPLY_PRIMARY_OWNER:
        print("EAF process has startup.")
    else:
        emacs_xid = 0
        emacs_width = emacs_height = 0

        app = QApplication(sys.argv)

        eaf = EAF(sys.argv[1:])

        print("EAF process start.")

        signal.signal(signal.SIGINT, signal.SIG_DFL)
        sys.exit(app.exec_())
