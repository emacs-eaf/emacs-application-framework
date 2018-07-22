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

# NOTE
# QtWebEngine will throw error "ImportError: QtWebEngineWidgets must be imported before a QCoreApplication instance is created"
# So we import browser module before start Qt application instance to avoid this error, but we never use this module.
from app.browser.buffer import AppBuffer as NeverUsed

from PyQt5.QtWidgets import QApplication
from PyQt5.QtNetwork import QNetworkProxy
from core.fake_key_event import fake_key_event
from core.utils import file_is_image, file_is_video
from core.view import View
from dbus.mainloop.glib import DBusGMainLoop
import importlib
import dbus
import dbus.service
import os
import json

EAF_DBUS_NAME = "com.lazycat.eaf"
EAF_OBJECT_NAME = "/com/lazycat/eaf"

class EAF(dbus.service.Object):
    def __init__(self, args):
        global emacs_xid, emacs_width, emacs_height

        dbus.service.Object.__init__(
            self,
            dbus.service.BusName(EAF_DBUS_NAME, bus=dbus.SessionBus()),
            EAF_OBJECT_NAME)

        # (emacs_xid, emacs_width, emacs_height, proxy_host, proxy_port) = (map(lambda x: int(x), args))
        (emacs_xid, emacs_width, emacs_height, proxy_host, proxy_port) = args
        emacs_xid = int(emacs_xid)
        emacs_width = int(emacs_width)
        emacs_height = int(emacs_height)

        self.buffer_dict = {}
        self.view_dict = {}

        self.start_finish()

        self.session_file_path = os.path.expanduser("~/.emacs.d/eaf/session.json")

        # Set HTTP proxy.
        if proxy_host != "" and proxy_port != "":
            proxy = QNetworkProxy()
            proxy.setType(QNetworkProxy.HttpProxy)
            proxy.setHostName(proxy_host)
            proxy.setPort(int(proxy_port))
            QNetworkProxy.setApplicationProxy(proxy)

    @dbus.service.method(EAF_DBUS_NAME, in_signature="ssss", out_signature="s")
    def new_buffer(self, buffer_id, url, app_name, arguments):
        # NOTE: We need use function str convert dbus.String to String,
        # otherwise some library will throw error, such as fitz library.
        return self.create_app(buffer_id, str(url), "app.{0}.buffer".format(str(app_name)), str(arguments))

    @dbus.service.method(EAF_DBUS_NAME, in_signature="sss", out_signature="")
    def update_buffer_with_url(self, module_path, buffer_url, update_data):
        for buffer in list(self.buffer_dict.values()):
            if buffer.module_path == module_path and buffer.url == buffer_url:
                buffer.update_with_data(update_data)
                break

    @dbus.service.method(EAF_DBUS_NAME, in_signature="sss", out_signature="")
    def scroll_buffer(self, view_info, scroll_direction, scroll_type):
        (buffer_id, _, _, _, _) = view_info.split(":")
        if buffer_id in self.buffer_dict:
            self.buffer_dict[buffer_id].scroll(scroll_direction, scroll_type)

    def get_new_browser_window_buffer_id(self):
        import secrets

        return "{0}-{1}-{2}-{3}-{4}-{5}-{6}".format(
            secrets.token_hex(2),
            secrets.token_hex(2),
            secrets.token_hex(2),
            secrets.token_hex(2),
            secrets.token_hex(2),
            secrets.token_hex(2),
            secrets.token_hex(2))

    def create_new_browser_window(self):
        # Generate buffer id same as eaf.el does.
        buffer_id = self.get_new_browser_window_buffer_id()

        # Create buffer for create new browser window.
        app_buffer = self.create_buffer(buffer_id, "http://0.0.0.0", "app.browser.buffer", "")

        # Create emacs buffer with buffer id.
        self.create_new_browser_buffer(buffer_id)

        # Return new QWebEngineView for create new browser window.
        return app_buffer.buffer_widget

    def create_app(self, buffer_id, url, module_path, arguments):
        try:
            self.create_buffer(buffer_id, url, module_path, arguments)

            return ""
        except ImportError:
            import traceback
            traceback.print_exc()
            return "Something wrong when import {0}".format(module_path)

    def create_buffer(self, buffer_id, url, module_path, arguments):
        global emacs_width, emacs_height

        # Create application buffer.
        module = importlib.import_module(module_path)
        app_buffer = module.AppBuffer(buffer_id, url, arguments)
        app_buffer.module_path = module_path

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

        # Handle buffer close request.
        app_buffer.close_buffer.connect(self.request_kill_buffer)

        # Handle message to emacs.
        app_buffer.message_to_emacs.connect(self.message_to_emacs)

        # Add create new window callback if module is browser
        if module_path == "app.browser.buffer":
            app_buffer.buffer_widget.create_new_browser_window_callback = self.create_new_browser_window

        # Restore buffer session.
        self.restore_buffer_session(app_buffer)

        return app_buffer

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
            # Save buffer session.
            self.save_buffer_session(self.buffer_dict[buffer_id])

            self.buffer_dict[buffer_id].handle_destroy()
            self.buffer_dict.pop(buffer_id, None)

    @dbus.service.method(EAF_DBUS_NAME, in_signature="ss", out_signature="")
    def send_key(self, buffer_id, event_string):
        # Send event to buffer when found match buffer.
        if buffer_id in self.buffer_dict:
            fake_key_event(event_string, self.buffer_dict[buffer_id])

    @dbus.service.method(EAF_DBUS_NAME, in_signature="ss", out_signature="")
    def send_keystroke(self, buffer_id, keystroke):
        if buffer_id in self.buffer_dict:
            self.buffer_dict[buffer_id].send_keystroke(keystroke)

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

    @dbus.service.signal("com.lazycat.eaf")
    def create_new_browser_buffer(self, buffer_id):
        pass

    @dbus.service.signal("com.lazycat.eaf")
    def request_kill_buffer(self, buffer_id):
        pass

    @dbus.service.signal("com.lazycat.eaf")
    def message_to_emacs(self, message):
        pass

    def save_buffer_session(self, buf):
        # Create config file it not exist.
        if not os.path.exists(self.session_file_path):
            basedir = os.path.dirname(self.session_file_path)
            if not os.path.exists(basedir):
                os.makedirs(basedir)

            with open(self.session_file_path, 'a'):
                os.utime(self.session_file_path, None)

            print("Create session file %s" % (self.session_file_path))

        # Save buffer session to file.
        if buf.save_session_data() != "":
            with open(self.session_file_path, "r+") as session_file:
                # Init session dict.
                session_dict = {}
                try:
                    session_dict = json.load(session_file)
                except ValueError:
                    pass

                # Init module path dict.
                if buf.module_path not in session_dict:
                    session_dict[buf.module_path] = {}

                # Update session data.
                if buf.url in session_dict[buf.module_path]:
                    session_dict[buf.module_path].update({buf.url: buf.save_session_data()})
                else:
                    session_dict.update({buf.module_path: {buf.url: buf.save_session_data()}})

                # Clean session file and update new content.
                session_file.seek(0)
                session_file.truncate(0)
                json.dump(session_dict, session_file)

                print("Save session: ", buf.module_path, buf.url, buf.save_session_data())

    def restore_buffer_session(self, buf):
        if os.path.exists(self.session_file_path):
            with open(self.session_file_path, "r+") as session_file:
                session_dict = {}
                try:
                    session_dict = json.load(session_file)
                except ValueError:
                    import traceback
                    traceback.print_exc()

                if buf.module_path in session_dict:
                    if buf.url in session_dict[buf.module_path]:
                        buf.restore_session_data(session_dict[buf.module_path][buf.url])

                        print("Restore session: ", buf.buffer_id, buf.module_path, self.session_file_path)
                    else:
                        print("No session data about %s, no need restore session." % (buf.url))
                else:
                    print("No data in session file, no need restore session.")
        else:
            print("Not found %s, no need restore session." % (self.session_file_path))

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
