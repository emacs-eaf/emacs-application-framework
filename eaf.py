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
from PyQt6 import QtWebEngineWidgets as NeverUsed # noqa

from PyQt6.QtNetwork import QNetworkProxy, QNetworkProxyFactory
from PyQt6.QtWidgets import QApplication
from PyQt6.QtCore import QTimer, QThread
from core.utils import PostGui, eval_in_emacs, init_epc_client, close_epc_client, message_to_emacs, get_emacs_vars, get_emacs_config_dir
from epc.server import ThreadingEPCServer
import json
import os
import platform
import threading

if platform.system() == "Windows":
    import pygetwindow as gw    # type: ignore

class EAF(object):
    def __init__(self, args):
        global emacs_width, emacs_height, proxy_string

        # Parse init arguments.
        (emacs_width, emacs_height, emacs_server_port) = args
        emacs_width = int(emacs_width)
        emacs_height = int(emacs_height)

        # Init variables.
        self.buffer_dict = {}
        self.view_dict = {}
        
        self.thread_queue = []

        for name in ["scroll_other_buffer", "eval_js_function", "eval_js_code", "action_quit", "send_key", "send_key_sequence",
                     "handle_search_forward", "handle_search_backward", "set_focus_text"]:
            self.build_buffer_function(name)

        for name in ["execute_js_function", "execute_js_code", "execute_function", "execute_function_with_args"]:
            self.build_buffer_return_function(name)

        # Init EPC client port.
        init_epc_client(int(emacs_server_port))

        # Build EPC server.
        self.server = ThreadingEPCServer(('localhost', 0), log_traceback=True)
        self.server.allow_reuse_address = True
        
        # import logging
        # self.server = ThreadingEPCServer(('localhost', 0)
        # self.server.logger.setLevel(logging.DEBUG)

        eaf_config_dir = get_emacs_config_dir()
        self.session_file = os.path.join(eaf_config_dir, "session.json")

        if not os.path.exists(eaf_config_dir):
            os.makedirs(eaf_config_dir);

        # ch = logging.FileHandler(filename=os.path.join(eaf_config_dir, 'epc_log.txt'), mode='w')
        # formatter = logging.Formatter('%(asctime)s | %(levelname)-8s | %(lineno)04d | %(message)s')
        # ch.setFormatter(formatter)
        # ch.setLevel(logging.DEBUG)
        # self.server.logger.addHandler(ch)

        self.server.register_instance(self) # register instance functions let elisp side call

        # Start EPC server with sub-thread, avoid block Qt main loop.
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.start()

        # Pass epc port and webengine codec information to Emacs when first start EAF.
        eval_in_emacs('eaf--first-start', [self.server.server_address[1]])

        # Disable use system proxy, avoid page slow when no network connected.
        QNetworkProxyFactory.setUseSystemConfiguration(False)

        # Set Network proxy.
        (proxy_host, proxy_port, proxy_type) = get_emacs_vars([
            "eaf-proxy-host",
            "eaf-proxy-port",
            "eaf-proxy-type"])

        self.proxy = (proxy_type, proxy_host, proxy_port)
        self.is_proxy = False

        if proxy_type != "" and proxy_host != "" and proxy_port != "":
            self.enable_proxy()

    def enable_proxy(self):
        global proxy_string

        proxy_string = "{0}://{1}:{2}".format(self.proxy[0], self.proxy[1], self.proxy[2])

        proxy = QNetworkProxy()
        if self.proxy[0] == "socks5":
            proxy.setType(QNetworkProxy.ProxyType.Socks5Proxy)
        elif self.proxy[0] == "http":
            proxy.setType(QNetworkProxy.ProxyType.HttpProxy)
        proxy.setHostName(self.proxy[1])
        proxy.setPort(int(self.proxy[2]))

        self.is_proxy = True
        QNetworkProxy.setApplicationProxy(proxy)

    def disable_proxy(self):
        global proxy_string

        proxy_string = ""

        proxy = QNetworkProxy()
        proxy.setType(QNetworkProxy.ProxyType.NoProxy)

        self.is_proxy = False
        QNetworkProxy.setApplicationProxy(proxy)

    def toggle_proxy(self):
        if self.is_proxy:
            self.disable_proxy()
        else:
            self.enable_proxy()

    @PostGui()
    def update_buffer_with_url(self, module_path, buffer_url, update_data):
        ''' Update buffer with url '''
        for buffer in list(self.buffer_dict.values()):
            if buffer.module_path == module_path and buffer.url == buffer_url:
                buffer.update_with_data(update_data)
                break

    @PostGui()
    def new_buffer(self, buffer_id, url, module_path, arguments):
        ''' New buffer.
        new_buffer just clone of create_buffer with @PostGui elisp call asynchronously.
        '''
        self.create_buffer(buffer_id, url, module_path, arguments)

    def create_buffer(self, buffer_id, url, module_path, arguments):
        ''' Create buffer.
        create_buffer can't wrap with @PostGui, because need call by createNewWindow signal of browser.'''
        import importlib
        
        global emacs_width, emacs_height, proxy_string

        # Always load module with app absolute path.
        # 
        # Don't cache module in memory, 
        # this is very convenient for EAF to load the latest application code in real time without the need for kill EAF process.
        spec = importlib.util.spec_from_file_location("AppBuffer", module_path) # type: ignore
        module = importlib.util.module_from_spec(spec) # type: ignore
        spec.loader.exec_module(module)

        # Create application buffer.
        app_buffer = module.AppBuffer(buffer_id, url, arguments)

        # Add module_path.
        app_buffer.module_path = module_path

        # Add buffer to buffer dict.
        self.buffer_dict[buffer_id] = app_buffer

        # Resize buffer with emacs max window size,
        # view (QGraphicsView) will adjust visual area along with emacs window changed.
        app_buffer.buffer_widget.resize(emacs_width, emacs_height)

        # Handle dev tools signal.
        if getattr(app_buffer, "open_devtools_tab", False) and getattr(app_buffer.open_devtools_tab, "connect", False):
            app_buffer.open_devtools_tab.connect(self.open_devtools_tab)

        # Add create buffer interface for createWindow signal.
        if app_buffer.base_class_name() == "BrowserBuffer":
            app_buffer.create_buffer = self.create_buffer

        # Set proxy for browser.
        if app_buffer.base_class_name() == "BrowserBuffer":
            app_buffer.proxy_string = proxy_string

        # If arguments is devtools, create devtools page.
        if app_buffer.base_class_name() == "BrowserBuffer" and arguments == "devtools" and self.devtools_page:
            self.devtools_page.setDevToolsPage(app_buffer.buffer_widget.web_page)
            self.devtools_page = None

        # Restore buffer session.
        self.restore_buffer_session(app_buffer)

        return app_buffer

    @PostGui()
    def update_views(self, args):
        ''' Update views.'''
        from core.view import View

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
                if old_view_buffer_id in self.buffer_dict:
                    self.buffer_dict[old_view_buffer_id].all_views_hide()

        # Remove old key from view dict and destroy old view.
        for key in list(self.view_dict):
            if key not in view_infos:
                self.destroy_view_later(key)

        # NOTE:
        # Create new view and REPARENT view to Emacs window.
        if view_infos != ['']:
            for view_info in view_infos:
                if view_info not in self.view_dict:
                    (buffer_id, _, _, _, _, _) = view_info.split(":")
                    try:
                        view = View(self.buffer_dict[buffer_id], view_info)
                        self.view_dict[view_info] = view
                    except KeyError:
                        eval_in_emacs('eaf--rebuild-buffer', [])
                        message_to_emacs("Buffer id '{}' not exists, rebuild EAF buffer.".format(buffer_id))

        # Call some_view_show interface when buffer's view switch back.
        # Note, this must call after new view create, otherwise some buffer,
        # such as QGraphicsVideoItem will report "Internal data stream error" error.
        if view_infos != ['']:
            for new_view_buffer_id in new_view_buffer_ids:
                if new_view_buffer_id not in old_view_buffer_ids:
                    if new_view_buffer_id in self.buffer_dict:
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

        # NOTE:
        # When you do switch buffer or kill buffer in Emacs, will call Python function 'update_views.
        # Screen will flick if destroy old view BEFORE reparent new view.
        #
        # So we call function 'destroy_view_now' at last to make sure destroy old view AFTER reparent new view.
        # Then screen won't flick.
        self.destroy_view_now()

    def destroy_view_later(self, key):
        '''Just record view id in global list 'destroy_view_list', and not destroy old view immediately.'''
        global destroy_view_list

        destroy_view_list.append(key)

    def destroy_view_now(self):
        '''Destroy all old view immediately.'''
        global destroy_view_list

        for key in destroy_view_list:
            if key in self.view_dict:
                self.view_dict[key].destroy_view()
            self.view_dict.pop(key, None)

        destroy_view_list = []

    def button_press_on_eaf_window(self):
        for key in self.buffer_dict:
            buffer_widget = self.buffer_dict[key].buffer_widget
            
            if hasattr(buffer_widget, "is_button_press") and buffer_widget.is_button_press:
                return True
            
        return False    
        
    @PostGui()
    def kill_buffer(self, buffer_id):
        ''' Kill all view based on buffer_id and clean buffer from buffer dict.'''
        # Kill all view base on buffer_id.
        for key in list(self.view_dict):
            if buffer_id == self.view_dict[key].buffer_id:
                self.destroy_view_later(key)

        # Clean buffer from buffer dict.
        if buffer_id in self.buffer_dict:
            # Save buffer session.
            self.save_buffer_session(self.buffer_dict[buffer_id])

            self.buffer_dict[buffer_id].destroy_buffer()
            self.buffer_dict.pop(buffer_id, None)
    
    @PostGui()
    def clip_buffer(self, buffer_id):
        '''Clip the image of buffer for display.'''
        eaf_config_dir = get_emacs_config_dir()
        for key in list(self.view_dict):
            view = self.view_dict[key]
            if buffer_id == view.buffer_id:
                image = view.screen_shot().save(os.path.join(eaf_config_dir, buffer_id + ".jpeg"))

    @PostGui()
    def ocr_buffer(self, buffer_id):
        try:
            import easyocr
            import tempfile

            for key in list(self.view_dict):
                view = self.view_dict[key]
                if buffer_id == view.buffer_id:
                    message_to_emacs("Start OCR current buffer, it's need few seconds to analyze...")
                    
                    import tempfile
                    image_path = os.path.join(tempfile.gettempdir(), buffer_id + ".png")
                    image = view.screen_shot().save(image_path)
                    
                    thread = OCRThread(image_path)
                    self.thread_queue.append(thread)
                    thread.start()
        except:
            import traceback
            traceback.print_exc()
            message_to_emacs("Please execute command `pip3 install easyocr` to install easyocr first.")
    
    @PostGui()
    def show_buffer_view(self, buffer_id):
        '''Show the single buffer view.'''
        for key in list(self.view_dict):
            view = self.view_dict[key]
            if buffer_id == view.buffer_id:
               view.try_show_top_view() 
    
    @PostGui()
    def hide_buffer_view(self, buffer_id):
        '''Hide the single buffer view.'''
        for key in list(self.view_dict):
            view = self.view_dict[key]
            if buffer_id == view.buffer_id:
               view.try_hide_top_view() 

    @PostGui()
    def kill_emacs(self):
        ''' Kill all buffurs from buffer dict.'''
        tmp_buffer_dict = {}
        for buffer_id in self.buffer_dict:
            tmp_buffer_dict[buffer_id] = self.buffer_dict[buffer_id]

        for buffer_id in tmp_buffer_dict:
            self.kill_buffer(buffer_id)

    def build_buffer_function(self, name):
        @PostGui()
        def _do(*args):
            buffer_id = args[0]

            if type(buffer_id) == str and buffer_id in self.buffer_dict:
                try:
                    getattr(self.buffer_dict[buffer_id], name)(*args[1:])
                except AttributeError:
                    import traceback
                    traceback.print_exc()
                    message_to_emacs("Got error with : " + name + " (" + buffer_id + ")")

        setattr(self, name, _do)

    def build_buffer_return_function(self, name):
        def _do(*args):
            buffer_id = args[0]

            if type(buffer_id) == str and buffer_id in self.buffer_dict:
                try:
                    return getattr(self.buffer_dict[buffer_id], name)(*args[1:])
                except AttributeError:
                    import traceback
                    traceback.print_exc()
                    message_to_emacs("Got error with : " + name + " (" + buffer_id + ")")
                    return None

        setattr(self, name, _do)

    @PostGui()
    def eval_function(self, buffer_id, function_name, event_string):
        ''' Execute function and do not return anything. '''
        if type(buffer_id) == str and buffer_id in self.buffer_dict:
            try:
                buffer = self.buffer_dict[buffer_id]
                buffer.current_event_string = event_string
                getattr(buffer, function_name)()
            except AttributeError:
                import traceback
                traceback.print_exc()
                message_to_emacs("Cannot execute function: " + function_name + " (" + buffer_id + ")")

    def get_emacs_wsl_window_id(self):
        if platform.system() == "Windows":
            return gw.getActiveWindow()._hWnd

    def activate_emacs_win32_window(self, frame_title):
        if platform.system() == "Windows":
            try:
                w = gw.getWindowsWithTitle(frame_title)
                w[0].activate()
            except:
                message_to_emacs('''Emacs title is empty cause EAF can not active Emacs window, plase add (setq frame-title-format "Emacs") in your config to active Emacs window.''')

    @PostGui()
    def handle_input_response(self, buffer_id, callback_tag, callback_result):
        ''' Handle input message for specified buffer.'''
        if type(buffer_id) == str and buffer_id in self.buffer_dict:
            buffer = self.buffer_dict[buffer_id]

            buffer.handle_input_response(callback_tag, callback_result)
            buffer.stop_search_input_monitor_thread()

    @PostGui()
    def cancel_input_response(self, buffer_id, callback_tag):
        ''' Cancel input message for specified buffer.'''
        if type(buffer_id) == str and buffer_id in self.buffer_dict:
            buffer = self.buffer_dict[buffer_id]

            buffer.cancel_input_response(callback_tag)
            buffer.stop_marker_input_monitor_thread()
            buffer.stop_search_input_monitor_thread()

    @PostGui()
    def show_top_views(self):
        for key in list(self.view_dict):
            self.view_dict[key].try_show_top_view()

    @PostGui()
    def hide_top_views(self):
        for key in list(self.view_dict):
            self.view_dict[key].try_hide_top_view()

    def open_devtools_tab(self, web_page):
        ''' Open devtools tab'''
        self.devtools_page = web_page
        eval_in_emacs('eaf-open-devtool-page', [])
        
        # We need adjust web window size after open developer tool.
        QTimer().singleShot(1000, lambda : eval_in_emacs('eaf-monitor-configuration-change', []))

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

    def cleanup(self):
        '''Do some cleanup before exit python process.'''
        close_epc_client()
        
class OCRThread(QThread):

    def __init__(self, image_path):
        QThread.__init__(self)

        self.image_path = image_path

    def run(self):
        import easyocr
        reader = easyocr.Reader(['ch_sim','en']) 
        result = reader.readtext(self.image_path)
        eval_in_emacs("eaf-ocr-buffer-record", [''.join(list(map(lambda r: r[1], result)))])
        
        import os
        os.remove(self.image_path)

if __name__ == "__main__":
    import sys
    import signal

    proxy_string = ""

    emacs_width = emacs_height = 0

    destroy_view_list = []

    hardware_acceleration_args = []
    if platform.system() != "Windows":
        hardware_acceleration_args += [
            "--ignore-gpu-blocklist",
            "--enable-gpu-rasterization",
            "--enable-native-gpu-memory-buffers"]

    app = QApplication(sys.argv + ["--disable-web-security"] + hardware_acceleration_args)
    app.setApplicationName("eaf.py")

    eaf = EAF(sys.argv[1:])

    signal.signal(signal.SIGINT, signal.SIG_DFL)
    sys.exit(app.exec())
