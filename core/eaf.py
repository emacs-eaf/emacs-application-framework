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
from dbus.mainloop.glib import DBusGMainLoop
from fake_key_event import fake_key_event
from pymediainfo import MediaInfo
from utils import file_is_image, file_is_video
from view import View
import dbus
import dbus.service

import os,sys,inspect
current_dir = os.path.dirname(os.path.abspath(inspect.getfile(inspect.currentframe())))
parent_dir = os.path.dirname(current_dir)
sys.path.insert(0, parent_dir)
from app.browser.buffer import BrowserBuffer
from app.imageviewer.buffer import ImageViewerBuffer
from app.videoplayer.buffer import VideoPlayerBuffer
from app.demo.buffer import DemoBuffer

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
        global emacs_width, emacs_height

        if url == "eaf rocks!":
            self.create_buffer(buffer_id, DemoBuffer(buffer_id, url, emacs_width, emacs_height))
        elif url.startswith("/"):
            if os.path.exists(url):
                file_info = MediaInfo.parse(url)
                if file_is_image(file_info):
                    self.create_buffer(buffer_id, ImageViewerBuffer(buffer_id, url, emacs_width, emacs_height))
                elif file_is_video(file_info):
                    self.create_buffer(buffer_id, VideoPlayerBuffer(buffer_id, url, emacs_width, emacs_height))
                else:
                    return "Don't know how to open {0}".format(url)
            else:
                return "Path {0} not exists.".format(url)
        else:
            from urllib.parse import urlparse
            result = urlparse(url)
            if len(result.scheme) != 0:
                self.create_buffer(buffer_id, BrowserBuffer(buffer_id, result.geturl(), emacs_width, emacs_height))
            else:
                result = urlparse("{0}:{1}".format("http", url))
                if result.scheme != "":
                    self.create_buffer(buffer_id, BrowserBuffer(buffer_id, result.geturl(), emacs_width, emacs_height))
                else:
                    return "{0} is not valid url".format(url)

        return ""

    def create_buffer(self, buffer_id, app_buffer):
        # Add buffer to buffer dict.
        self.buffer_dict[buffer_id] = app_buffer

        # Monitor buffer signals.
        app_buffer.update_title.connect(self.update_buffer_title)
        app_buffer.open_url.connect(self.open_buffer_url)

    @dbus.service.method(EAF_DBUS_NAME, in_signature="s", out_signature="")
    def update_views(self, args):
        global emacs_xid

        view_infos = args.split(",")

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
            QApplication.sendEvent(self.buffer_dict[buffer_id].buffer_widget, fake_key_event(event_string))

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
