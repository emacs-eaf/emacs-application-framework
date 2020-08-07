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
from app.browser.buffer import AppBuffer as NeverUsed # noqa
from sys import version_info

from PyQt5.QtCore import QLibraryInfo, QTimer
from PyQt5.QtNetwork import QNetworkProxy
from PyQt5.QtWidgets import QApplication
from core.view import View
from dbus.mainloop.glib import DBusGMainLoop
import dbus
import dbus.service
import importlib
import json
import os
import subprocess

EAF_DBUS_NAME = "com.lazycat.eaf"
EAF_OBJECT_NAME = "/com/lazycat/eaf"

class EAF(dbus.service.Object):
    def __init__(self, args):
        global emacs_width, emacs_height, eaf_config_dir, proxy_string

        dbus.service.Object.__init__(
            self,
            dbus.service.BusName(EAF_DBUS_NAME, bus=dbus.SessionBus()),
            EAF_OBJECT_NAME)

        (emacs_width, emacs_height, proxy_host, proxy_port, proxy_type, config_dir, var_dict_string) = args
        emacs_width = int(emacs_width)
        emacs_height = int(emacs_height)
        eaf_config_dir = os.path.expanduser(config_dir)

        self.buffer_dict = {}
        self.view_dict = {}
        self.emacs_var_dict = {}

        self.update_emacs_var_dict(var_dict_string)

        self.first_start(self.webengine_include_private_codec())

        self.session_file = os.path.join(eaf_config_dir, "session.json")

        # Set Network proxy.
        if proxy_host != "" and proxy_port != "":
            proxy_string = "{0}://{1}:{2}".format(proxy_type, proxy_host, proxy_port)

            proxy = QNetworkProxy()
            if proxy_type == "socks5":
                proxy.setType(QNetworkProxy.Socks5Proxy)
            elif proxy_type == "http":
                proxy.setType(QNetworkProxy.HttpProxy)

            proxy.setHostName(proxy_host)
            proxy.setPort(int(proxy_port))
            QNetworkProxy.setApplicationProxy(proxy)

    def get_command_result(self, command):
        ''' Execute the command and return the result. '''
        if version_info >= (3,7):
            return subprocess.run(command, check=False, shell=True, stdout=subprocess.PIPE, text=True).stdout
        else:
            return subprocess.run(command, check=False, shell=True, stdout=subprocess.PIPE).stdout

    def webengine_include_private_codec(self):
        ''' Return bool of whether the QtWebEngineProcess include private codec. '''
        path = os.path.join(QLibraryInfo.location(QLibraryInfo.LibraryExecutablesPath), "QtWebEngineProcess")
        return self.get_command_result("ldd {} | grep libavformat".format(path)) != ""

    @dbus.service.method(EAF_DBUS_NAME, in_signature="s", out_signature="")
    def update_emacs_var_dict(self, var_dict_string):
        ''' Update Python side emacs_var_dict.(Fix issue #206) '''
        self.emacs_var_dict = json.loads(var_dict_string)

        for buffer in list(self.buffer_dict.values()):
            buffer.emacs_var_dict = self.emacs_var_dict

    @dbus.service.method(EAF_DBUS_NAME, in_signature="ssss", out_signature="s")
    def new_buffer(self, buffer_id, url, app_name, arguments):
        ''' Create new buffer. '''
        # NOTE: We need use function str convert dbus.String to String,
        # otherwise some library will throw error, such as fitz library.
        return self.create_app(buffer_id, str(url), "app.{0}.buffer".format(str(app_name)), str(arguments))

    @dbus.service.method(EAF_DBUS_NAME, in_signature="sss", out_signature="")
    def update_buffer_with_url(self, module_path, buffer_url, update_data):
        ''' Update buffer with url '''
        for buffer in list(self.buffer_dict.values()):
            if buffer.module_path == module_path and buffer.url == buffer_url:
                buffer.update_with_data(update_data)
                break

    @dbus.service.method(EAF_DBUS_NAME, in_signature="sss", out_signature="")
    def scroll_other_buffer(self, view_info, scroll_direction, scroll_type):
        ''' Scroll to other buffer '''
        (buffer_id, _, _, _, _) = view_info.split(":")
        if buffer_id in self.buffer_dict:
            self.buffer_dict[buffer_id].scroll_other_buffer(scroll_direction, scroll_type)

    def get_new_browser_window_buffer_id(self):
        ''' Return new browser window's buffer ID. '''
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
        ''' Create new browser window.'''
        # Generate buffer id same as eaf.el does.
        buffer_id = self.get_new_browser_window_buffer_id()

        # Create buffer for create new browser window.
        app_buffer = self.create_buffer(buffer_id, "http://0.0.0.0", "app.browser.buffer", "")

        # Create emacs buffer with buffer id.
        self.create_new_browser_buffer(buffer_id)

        # Return new QWebEngineView for create new browser window.
        return app_buffer.buffer_widget

    def create_app(self, buffer_id, url, module_path, arguments):
        ''' Create app using create_buffer.'''
        try:
            self.create_buffer(buffer_id, url, module_path, arguments)

            return ""
        except ImportError:
            import traceback
            traceback.print_exc()
            return "EAF: Something went wrong when trying to import {0}".format(module_path)

    def create_buffer(self, buffer_id, url, module_path, arguments):
        ''' Create buffer.'''
        global emacs_width, emacs_height, eaf_config_dir, proxy_string

        # Create application buffer.
        module = importlib.import_module(module_path)
        app_buffer = module.AppBuffer(buffer_id, url, eaf_config_dir, arguments, self.emacs_var_dict, module_path)

        # Add buffer to buffer dict.
        self.buffer_dict[buffer_id] = app_buffer

        # Resize buffer with emacs max window size,
        # view (QGraphicsView) will adjust visual area along with emacs window changed.
        app_buffer.buffer_widget.resize(emacs_width, emacs_height)

        # Monitor buffer signals.
        app_buffer.update_buffer_details.connect(self.update_buffer_details)
        app_buffer.translate_text.connect(self.translate_text)
        app_buffer.open_url_in_new_tab.connect(self.open_url_in_new_tab)
        app_buffer.open_url_in_background_tab.connect(self.open_url_in_background_tab)
        app_buffer.goto_left_tab.connect(self.goto_left_tab)
        app_buffer.goto_right_tab.connect(self.goto_right_tab)

        # Send message to emacs.
        app_buffer.input_message.connect(self.input_message)

        # Handle buffer close request.
        app_buffer.request_close_buffer.connect(self.request_kill_buffer)

        # Handle message to emacs.
        app_buffer.message_to_emacs.connect(self.message_to_emacs)

        # Handle set emacs var signal.
        app_buffer.set_emacs_var.connect(self.set_emacs_var)

        # Handle eval form in emacs.
        app_buffer.eval_in_emacs.connect(self.eval_in_emacs)

        # Handle get_focus_text signal.
        if getattr(app_buffer, "get_focus_text", False) and getattr(app_buffer.get_focus_text, "connect", False):
            app_buffer.get_focus_text.connect(self.edit_focus_text)

        if getattr(app_buffer.buffer_widget, "get_focus_text", False) and getattr(app_buffer.buffer_widget.get_focus_text, "connect", False):
            app_buffer.buffer_widget.get_focus_text.connect(self.edit_focus_text)

        # Handle get_sub_node_id signal.
        if getattr(app_buffer, "get_sub_node_id", False) and getattr(app_buffer.get_sub_node_id, "connect", False):
            app_buffer.get_sub_node_id.connect(self.add_multiple_sub_nodes)

        # Handle get_brother_node_id signal.
        if getattr(app_buffer, "get_brother_node_id", False) and getattr(app_buffer.get_brother_node_id, "connect", False):
            app_buffer.get_brother_node_id.connect(self.add_multiple_brother_nodes)

        # Handle get_middle_node_id signal.
        if getattr(app_buffer, "get_middle_node_id", False) and getattr(app_buffer.get_middle_node_id, "connect", False):
            app_buffer.get_middle_node_id.connect(self.add_multiple_middle_nodes)

        # Handle trigger_focus_event signal.
        if getattr(app_buffer.buffer_widget, "trigger_focus_event", False) and getattr(app_buffer.buffer_widget.trigger_focus_event, "connect", False):
            app_buffer.buffer_widget.trigger_focus_event.connect(self.focus_emacs_buffer)

        # Handle export_org_json signal.
        if getattr(app_buffer, "export_org_json", False) and getattr(app_buffer.export_org_json, "connect", False):
            app_buffer.export_org_json.connect(self.export_org_json)

        # Handle dev tools signal.
        if getattr(app_buffer, "open_dev_tools_tab", False) and getattr(app_buffer.open_dev_tools_tab, "connect", False):
            app_buffer.open_dev_tools_tab.connect(self.open_dev_tools_tab)

        # Handle fulllscreen signal.
        if getattr(app_buffer, "enter_fullscreen_request", False) and getattr(app_buffer.enter_fullscreen_request, "connect", False):
            app_buffer.enter_fullscreen_request.connect(self.enter_fullscreen_request)

        if getattr(app_buffer, "exit_fullscreen_request", False) and getattr(app_buffer.exit_fullscreen_request, "connect", False):
            app_buffer.exit_fullscreen_request.connect(self.exit_fullscreen_request)

        # Add create new window when create_new_browser_window_callback is call.
        if module_path == "app.browser.buffer" or module_path == "app.terminal.buffer":
            app_buffer.buffer_widget.create_new_browser_window_callback = self.create_new_browser_window

        elif module_path == "app.rss-reader.buffer":
            app_buffer.buffer_widget.browser.create_new_browser_window_callback = self.create_new_browser_window

        if module_path == "app.browser.buffer":
            app_buffer.proxy_string = proxy_string

        # If arguments is dev_tools, create dev tools page.
        if module_path == "app.browser.buffer" and arguments == "dev_tools" and self.dev_tools_page:
            self.dev_tools_page.setDevToolsPage(app_buffer.buffer_widget.web_page)
            self.dev_tools_page = None

        # Restore buffer session.
        self.restore_buffer_session(app_buffer)

        return app_buffer

    @dbus.service.method(EAF_DBUS_NAME, in_signature="s", out_signature="")
    def update_views(self, args):
        ''' Update views.'''
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
                self.view_dict[key].destroy_view()
                self.view_dict.pop(key, None)

        # Create new view and udpate in view dict.
        if view_infos != ['']:
            for view_info in view_infos:
                if view_info not in self.view_dict:
                    (buffer_id, _, _, _, _, _) = view_info.split(":")
                    view = View(self.buffer_dict[buffer_id], view_info)
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

                # Send resize signal to buffer.
                buffer.resize_view()

    @dbus.service.method(EAF_DBUS_NAME, in_signature="", out_signature="")
    def kill_emacs(self):
        ''' Kill all buffurs from buffer dict.'''
        tmp_buffer_dict = {}
        for buffer_id in self.buffer_dict:
            tmp_buffer_dict[buffer_id] = self.buffer_dict[buffer_id]

        for buffer_id in tmp_buffer_dict:
            self.kill_buffer(buffer_id)

    @dbus.service.method(EAF_DBUS_NAME, in_signature="s", out_signature="")
    def kill_buffer(self, buffer_id):
        ''' Kill all view based on buffer_id and clean buffer from buffer dict.'''
        # Kill all view base on buffer_id.
        for key in list(self.view_dict):
            if buffer_id == self.view_dict[key].buffer_id:
                self.view_dict[key].destroy_view()
                self.view_dict.pop(key, None)

        # Clean buffer from buffer dict.
        if buffer_id in self.buffer_dict:
            # Save buffer session.
            self.save_buffer_session(self.buffer_dict[buffer_id])

            self.buffer_dict[buffer_id].destroy_buffer()
            self.buffer_dict.pop(buffer_id, None)

    @dbus.service.method(EAF_DBUS_NAME, in_signature="sss", out_signature="")
    def execute_function(self, buffer_id, function_name, event_string):
        ''' Execute function and do not return anything. '''
        if buffer_id in self.buffer_dict:
            try:
                buffer = self.buffer_dict[buffer_id]
                buffer.current_event_string = event_string
                buffer.execute_function(function_name)
            except AttributeError:
                import traceback
                traceback.print_exc()
                self.message_to_emacs("Cannot execute function: " + function_name + " (" + buffer_id + ")")

    @dbus.service.method(EAF_DBUS_NAME, in_signature="ss", out_signature="s")
    def call_function(self, buffer_id, function_name):
        ''' Call function and return the result. '''
        if buffer_id in self.buffer_dict:
            try:
                return str(self.buffer_dict[buffer_id].call_function(function_name))
            except AttributeError:
                import traceback
                traceback.print_exc()
                self.message_to_emacs("Cannot call function: " + function_name)
                return ""

    @dbus.service.method(EAF_DBUS_NAME, in_signature="sss", out_signature="s")
    def call_function_with_args(self, buffer_id, function_name, args_string):
        ''' Call function with arguments and return the result. '''
        if buffer_id in self.buffer_dict:
            try:
                return str(self.buffer_dict[buffer_id].call_function_with_args(function_name, args_string))
            except AttributeError:
                import traceback
                traceback.print_exc()
                self.message_to_emacs("Cannot call function: " + function_name)
                return ""

    @dbus.service.method(EAF_DBUS_NAME, in_signature="s", out_signature="")
    def action_quit(self, buffer_id):
        ''' Execute action_quit() for specified buffer.'''
        if buffer_id in self.buffer_dict:
            self.buffer_dict[buffer_id].action_quit()

    @dbus.service.method(EAF_DBUS_NAME, in_signature="ss", out_signature="")
    def send_key(self, buffer_id, event_string):
        ''' Send event to buffer when found match buffer.'''
        if buffer_id in self.buffer_dict:
            self.buffer_dict[buffer_id].fake_key_event(event_string)

    @dbus.service.method(EAF_DBUS_NAME, in_signature="ss", out_signature="")
    def send_key_sequence(self, buffer_id, event_string):
        ''' Send event to buffer when found match buffer.'''
        if buffer_id in self.buffer_dict:
            self.buffer_dict[buffer_id].fake_key_sequence(event_string)

    @dbus.service.method(EAF_DBUS_NAME, in_signature="sss", out_signature="")
    def handle_input_response(self, buffer_id, callback_tag, callback_result):
        ''' Handle input message for specified buffer.'''
        for buffer in list(self.buffer_dict.values()):
            if buffer.buffer_id == buffer_id:
                buffer.handle_input_response(callback_tag, callback_result)

    @dbus.service.method(EAF_DBUS_NAME, in_signature="ss", out_signature="")
    def cancel_input_response(self, buffer_id, callback_tag):
        ''' Cancel input message for specified buffer.'''
        for buffer in list(self.buffer_dict.values()):
            if buffer.buffer_id == buffer_id:
                buffer.cancel_input_response(callback_tag)

    @dbus.service.method(EAF_DBUS_NAME, in_signature="ss", out_signature="")
    def update_focus_text(self, buffer_id, new_text):
        ''' Update focus text for specified buffer.'''
        for buffer in list(self.buffer_dict.values()):
            if buffer.buffer_id == buffer_id:
                buffer.set_focus_text(new_text)

    @dbus.service.method(EAF_DBUS_NAME, in_signature="ss", out_signature="")
    def update_multiple_sub_nodes(self, buffer_id, new_text):
        ''' Update multiplt sub nodes.'''
        for buffer in list(self.buffer_dict.values()):
            if buffer.buffer_id == buffer_id:
                for line in str(new_text).split("\n"):
                    buffer.add_texted_sub_node(line)

    @dbus.service.method(EAF_DBUS_NAME, in_signature="ss", out_signature="")
    def update_multiple_brother_nodes(self, buffer_id, new_text):
        ''' Update multiplt brother nodes.'''
        for buffer in list(self.buffer_dict.values()):
            if buffer.buffer_id == buffer_id:
                for line in str(new_text).split("\n"):
                    buffer.add_texted_brother_node(line)

    @dbus.service.method(EAF_DBUS_NAME, in_signature="ss", out_signature="")
    def update_multiple_middle_nodes(self, buffer_id, new_text):
        ''' Update multiplt middle nodes.'''
        for buffer in list(self.buffer_dict.values()):
            if buffer.buffer_id == buffer_id:
                for line in str(new_text).split("\n"):
                    buffer.add_texted_middle_node(line)

    @dbus.service.signal(EAF_DBUS_NAME)
    def add_multiple_sub_nodes(self, buffer_id):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def add_multiple_brother_nodes(self, buffer_id):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def add_multiple_middle_nodes(self, buffer_id):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def focus_emacs_buffer(self, message):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def first_start(self, webengine_include_private_codec):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def update_buffer_details(self, buffer_id, title, url):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def open_url_in_new_tab(self, url):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def open_dev_tools_page(self):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def open_url_in_background_tab(self, url):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def goto_left_tab(self):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def goto_right_tab(self):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def translate_text(self, text):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def input_message(self, buffer_id, message, callback_tag, input_type, input_content):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def create_new_browser_buffer(self, buffer_id):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def request_kill_buffer(self, buffer_id):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def message_to_emacs(self, message):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def set_emacs_var(self, var_name, var_value, eaf_specific):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def eval_in_emacs(self, elisp_code_string):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def edit_focus_text(self, buffer_id, focus_text):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def export_org_json(self, org_json_content, org_file_path):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def enter_fullscreen_request(self):
        pass

    @dbus.service.signal(EAF_DBUS_NAME)
    def exit_fullscreen_request(self):
        pass

    def open_dev_tools_tab(self, web_page):
        ''' Open dev-tools tab'''
        self.dev_tools_page = web_page
        self.open_dev_tools_page()

    def save_buffer_session(self, buf):
        ''' Save buffer session to file.'''
        # Create config file it not exist.
        if not os.path.exists(self.session_file):
            basedir = os.path.dirname(self.session_file)
            if not os.path.exists(basedir):
                os.makedirs(basedir)

            with open(self.session_file, 'a'):
                os.utime(self.session_file, None)

            print("Create session file %s" % (self.session_file))

        # Save buffer session to file.
        buf_session_data = buf.save_session_data()
        if buf_session_data != "":
            with open(self.session_file, "r+") as session_file:
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
                session_dict[buf.module_path].update({buf.url: buf_session_data})

                # Clean session file and update new content.
                session_file.seek(0)
                session_file.truncate(0)
                json.dump(session_dict, session_file)

                print("Saved session: ", buf.module_path, buf.url, buf_session_data)

    def restore_buffer_session(self, buf):
        ''' Restore buffer session from file.'''
        if os.path.exists(self.session_file):
            with open(self.session_file, "r+") as session_file:
                session_dict = {}
                try:
                    session_dict = json.load(session_file)
                except ValueError:
                    pass

                if buf.module_path in session_dict:
                    if buf.url in session_dict[buf.module_path]:
                        buf.restore_session_data(session_dict[buf.module_path][buf.url])

                        print("Session restored: ", buf.buffer_id, buf.module_path, self.session_file)
                    else:
                        print("Session is not restored, as no data about %s." % (buf.url))
                else:
                    print("Session is not restored, as no data present in session file.")
        else:
            print("Session is not restored, as %s cannot be found." % (self.session_file))

if __name__ == "__main__":
    import sys
    import signal

    proxy_string = ""

    DBusGMainLoop(set_as_default=True) # WARING: only use once in one process

    bus = dbus.SessionBus()
    if bus.request_name(EAF_DBUS_NAME) != dbus.bus.REQUEST_NAME_REPLY_PRIMARY_OWNER:
        print("EAF process is already running.")
    else:
        emacs_width = emacs_height = 0
        eaf_config_dir = ""

        app = QApplication(sys.argv + ["--disable-web-security"])

        eaf = EAF(sys.argv[1:])

        print("EAF process starting...")

        signal.signal(signal.SIGINT, signal.SIG_DFL)
        sys.exit(app.exec_())
