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

import os

import sexpdata
from PyQt6 import QtGui
from PyQt6.QtCore import QObject, pyqtSignal
from PyQt6.QtWidgets import QApplication


class PostGui(QObject):

    through_thread = pyqtSignal(object, object)

    def __init__(self, inclass=True):
        super(PostGui, self).__init__()
        self.through_thread.connect(self.on_signal_received)
        self.inclass = inclass

    def __call__(self, func):
        self._func = func

        from functools import wraps

        @wraps(func)
        def obj_call(*args, **kwargs):
            self.emit_signal(args, kwargs)
        return obj_call

    def emit_signal(self, args, kwargs):
        self.through_thread.emit(args, kwargs)

    def on_signal_received(self, args, kwargs):
        try:
            if self.inclass:
                obj, args = args[0], args[1:]
                self._func(obj, *args, **kwargs)
            else:
                self._func(*args, **kwargs)
        except Exception:
            import traceback
            traceback.print_exc()


def touch(path):
    import os

    if not os.path.exists(path):
        basedir = os.path.dirname(path)

        if not os.path.exists(basedir):
            os.makedirs(basedir)

        with open(path, 'a'):
            os.utime(path)

def get_free_port():
    """
    Determines a free port using sockets.
    """
    import socket

    free_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    free_socket.bind(('0.0.0.0', 0))
    free_socket.listen(5)
    port = free_socket.getsockname()[1]
    free_socket.close()

    return port

def is_port_in_use(port):
    import socket

    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        return s.connect_ex(('127.0.0.1', port)) == 0

def string_to_base64(text):
    import base64
    return str(base64.b64encode(str(text).encode("utf-8")), "utf-8")

def get_local_ip():
    try:
        import socket

        s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        s.connect(("8.8.8.8", 80))
        return s.getsockname()[0]
    except OSError:
        import sys
        print("Network is unreachable")
        sys.exit()

def popen_and_call(popen_args, on_exit):
    """
    Runs the given args in a subprocess.Popen, and then calls the function
    on_exit when the subprocess completes.
    on_exit is a callable object, and popen_args is a list/tuple of args that
    would give to subprocess.Popen.
    """
    def run_in_thread(on_exit, popen_args):
        import subprocess

        try:
            proc = subprocess.Popen(popen_args,
                                    stdin=subprocess.PIPE,
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.STDOUT)
            proc.wait()
        except OSError:
            import traceback
            traceback.print_exc()
        on_exit()
        return
    import threading

    thread = threading.Thread(target=run_in_thread, args=(on_exit, popen_args))
    thread.start()
    # returns immediately after the thread starts
    return thread

def call_and_check_code(popen_args, on_exit):
    """
    Runs the given args in a subprocess.Popen, and then calls the function
    on_exit when the subprocess completes.
    on_exit is a callable object, and popen_args is a list/tuple of args that
    would give to subprocess.Popen.
    """
    def run_in_thread(on_exit, popen_args):
        import subprocess

        retcode = subprocess.call(popen_args,
                                  stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE,
                                  stderr=subprocess.STDOUT)
        on_exit(retcode)
        return

    import threading

    thread = threading.Thread(target=run_in_thread, args=(on_exit, popen_args))
    thread.start()
    # returns immediately after the thread starts
    return thread

def get_clipboard_text():
    ''' Get text from system clipboard.'''
    from PyQt6.QtGui import QClipboard
    from PyQt6.QtWidgets import QApplication

    clipboard = QApplication.clipboard()
    text = clipboard.text()
    if text:
        return text

    if clipboard.supportsSelection():
        return clipboard.text(QClipboard.Mode.Selection)

    return ""

def set_clipboard_text(text):
    ''' Set text to system clipboard.'''
    from PyQt6.QtGui import QClipboard
    from PyQt6.QtWidgets import QApplication

    clipboard = QApplication.clipboard()
    clipboard.setText(text)

    if clipboard.supportsSelection():
        clipboard.setText(text, QClipboard.Mode.Selection)


def interactive(insert_or_do = False, msg_emacs = None, new_name = None):
    """
    Defines an interactive command invoked from Emacs.
    """
    def wrap(f, insert_or_do = insert_or_do, msg_emacs = msg_emacs, new_name = new_name):
        from functools import wraps

        f.interactive = True
        f.insert_or_do = insert_or_do
        f.msg_emacs = msg_emacs
        f.new_name = new_name

        @wraps(f)
        def wrapped_f(*args, **kwargs):
            return f(*args, **kwargs)
        return wrapped_f

    # Support both @interactive and @interactive() as valid syntax.
    if callable(insert_or_do):
        return wrap(insert_or_do, insert_or_do = False, msg_emacs = None, new_name = None)
    else:
        return wrap


def abstract(f):
    """
    Add a `abstract` flag to a method,

    We don't use abs.abstractmethod cause we don't need strict
    implementation check.
    """
    from functools import wraps

    f.abstract = True
    @wraps(f)
    def wrap(*args, **kwargs):
        return f(*args, **kwargs)
    return wrap

epc_client = None

def init_epc_client(emacs_server_port):
    from epc.client import EPCClient

    global epc_client

    if epc_client is None:
        try:
            epc_client = EPCClient(("127.0.0.1", emacs_server_port), log_traceback=True)
        except ConnectionRefusedError:
            import traceback
            traceback.print_exc()

def close_epc_client():
    global epc_client

    if epc_client is not None:
        epc_client.close()


def handle_arg_types(arg):
    if type(arg) is str and arg.startswith("'"):
        arg = sexpdata.Symbol(arg.partition("'")[2])

    return sexpdata.Quoted(arg)

def eval_in_emacs(method_name, args):
    global epc_client

    args = [sexpdata.Symbol(method_name)] + list(map(handle_arg_types, args))    # type: ignore
    sexp = sexpdata.dumps(args)

    epc_client.call("eval-in-emacs", [sexp])    # type: ignore


def get_emacs_func_result(method_name, args):
    global epc_client

    args = [sexpdata.Symbol(method_name)] + list(map(handle_arg_types, args))    # type: ignore
    sexp = sexpdata.dumps(args)

    result = epc_client.call_sync("get-emacs-func-result", [sexp])    # type: ignore
    return result if result != [] else False

def get_app_dark_mode(app_dark_mode_var):
    app_dark_mode = get_emacs_var(app_dark_mode_var)
    return (app_dark_mode == "force" or \
            app_dark_mode is True or \
            (app_dark_mode == "follow" and \
             get_emacs_theme_mode() == "dark"))

def get_emacs_theme_mode():
    return get_emacs_func_result("eaf-get-theme-mode", [])

def get_emacs_theme_background():
    return get_emacs_func_result("eaf-get-theme-background-color", [])

def get_emacs_theme_foreground():
    return get_emacs_func_result("eaf-get-theme-foreground-color", [])

def message_to_emacs(message, prefix=True, logging=True):
    eval_in_emacs('eaf--show-message', [message, prefix, logging])

def clear_emacs_message():
    eval_in_emacs('eaf--clear-message', [])

def set_emacs_var(var_name, var_value):
    eval_in_emacs('eaf--set-emacs-var', [var_name, var_value])

def open_url_in_background_tab(url):
    eval_in_emacs('eaf-open-browser-in-background', [url])

def duplicate_page_in_new_tab(url):
    eval_in_emacs('eaf-browser--duplicate-page-in-new-tab', [url])

def open_url_in_new_tab(url):
    eval_in_emacs('eaf-open-browser', [url])

def open_url_in_new_tab_same_window(url, current_url):
    eval_in_emacs("eaf-open-browser-same-window", [url, current_url])

def open_url_in_new_tab_other_window(url):
    eval_in_emacs('eaf-open-browser-other-window', [url])

def translate_text(text):
    eval_in_emacs('eaf-translate-text', [text])

def input_message(buffer_id, message, callback_tag, input_type, input_content, completion_list):
    eval_in_emacs('eaf--input-message', [buffer_id, message, callback_tag, input_type, input_content, completion_list])

def focus_emacs_buffer(buffer_id):
    eval_in_emacs('eaf-focus-buffer', [buffer_id])

def atomic_edit(buffer_id, focus_text):
    eval_in_emacs('eaf--atomic-edit', [buffer_id, focus_text])

def convert_emacs_bool(symbol_value, symbol_is_boolean):
    if symbol_is_boolean == "t":
        return symbol_value is True
    else:
        return symbol_value

def get_emacs_vars(args):
    global epc_client

    return list(map(lambda result: convert_emacs_bool(result[0], result[1]) if result != [] else False, epc_client.call_sync("get-emacs-vars", args))) # type: ignore

def get_emacs_var(var_name):
    global epc_client

    (symbol_value, symbol_is_boolean) = epc_client.call_sync("get-emacs-var", [var_name]) # type: ignore

    return convert_emacs_bool(symbol_value, symbol_is_boolean)

emacs_config_dir = ""

def get_emacs_config_dir():
    import os

    global emacs_config_dir

    if emacs_config_dir == "":
        emacs_config_dir = os.path.join(os.path.expanduser(get_emacs_var("eaf-config-location")), '')

    if not os.path.exists(emacs_config_dir):
        os.makedirs(emacs_config_dir)

    return emacs_config_dir

def to_camel_case(string):
    components = string.split('_')
    return components[0] + ''.join(x.title() for x in components[1:])

emacs_func_cache_dict = {}

def get_emacs_func_cache_result(func_name, func_args):
    global emacs_func_cache_dict

    if func_name in emacs_func_cache_dict:
        return emacs_func_cache_dict[func_name]
    else:
        result = get_emacs_func_result(func_name, func_args)
        emacs_func_cache_dict[func_name] = result

        return result

current_desktop = os.getenv("XDG_CURRENT_DESKTOP") or os.getenv("XDG_SESSION_DESKTOP")

def post_event(widget, event):
    try:
        QApplication.postEvent(widget, event)
    except:
        import traceback
        print("post_event error: " + traceback.format_exc())

def get_qrcode_pixmap(content):
    import tempfile

    import qrcode

    img = qrcode.make(content)
    temp_qrcode_file = tempfile.NamedTemporaryFile(mode="w", delete=False)
    temp_qrcode_file_path = temp_qrcode_file.name
    img.save(temp_qrcode_file_path)

    pixmap = QtGui.QPixmap(temp_qrcode_file_path)
    os.remove(temp_qrcode_file_path)

    return pixmap
