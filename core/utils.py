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

from PyQt5.QtCore import QObject, pyqtSignal

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
        return s.connect_ex(('localhost', port)) == 0

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

def popen_and_call(popen_args, on_exit, stdout_file=None):
    """
    Runs the given args in a subprocess.Popen, and then calls the function
    on_exit when the subprocess completes.
    on_exit is a callable object, and popen_args is a list/tuple of args that
    would give to subprocess.Popen.
    """
    def run_in_thread(on_exit, popen_args):
        import subprocess

        proc = subprocess.Popen(popen_args, stdout=stdout_file)
        proc.wait()
        on_exit()
        return
    import threading

    thread = threading.Thread(target=run_in_thread, args=(on_exit, popen_args))
    thread.start()
    # returns immediately after the thread starts
    return thread

def call_and_check_code(popen_args, on_exit, stdout_file=None):
    """
    Runs the given args in a subprocess.Popen, and then calls the function
    on_exit when the subprocess completes.
    on_exit is a callable object, and popen_args is a list/tuple of args that
    would give to subprocess.Popen.
    """
    def run_in_thread(on_exit, popen_args):
        import subprocess

        retcode = subprocess.call(popen_args, stdout=stdout_file)
        on_exit(retcode)
        return
    
    import threading

    thread = threading.Thread(target=run_in_thread, args=(on_exit, popen_args))
    thread.start()
    # returns immediately after the thread starts
    return thread

def get_clipboard_text():
    ''' Get text from system clipboard.'''
    from PyQt5.QtWidgets import QApplication
    from PyQt5.QtGui import QClipboard

    clipboard = QApplication.clipboard()
    text = clipboard.text()
    if text:
        return text

    if clipboard.supportsSelection():
        return clipboard.text(QClipboard.Selection)

    return ""

def set_clipboard_text(text):
    ''' Set text to system clipboard.'''
    from PyQt5.QtWidgets import QApplication
    from PyQt5.QtGui import QClipboard
    
    clipboard = QApplication.clipboard()
    clipboard.setText(text)

    if clipboard.supportsSelection():
        clipboard.setText(text, QClipboard.Selection)


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

    if epc_client == None:
        try:
            epc_client = EPCClient(("localhost", emacs_server_port), log_traceback=True)
        except ConnectionRefusedError:
            import traceback
            traceback.print_exc()

def close_epc_client():
    global epc_client

    if epc_client != None:
        epc_client.close()

def convert_arg_to_str(arg):
    if type(arg) == str:
        return arg
    elif type(arg) == bool:
        arg = str(arg).upper()
    elif type(arg) == list:
        new_arg = ""
        for a in arg:
            new_arg = new_arg + " " + convert_arg_to_str(a)
        arg = "(" + new_arg[1:] + ")"
    return arg

def eval_in_emacs(method_name, args):
    global epc_client

    if epc_client == None:
        print("Please call init_epc_client first before callling eval_in_emacs.")
    else:
        args = list(map(convert_arg_to_str, args))
        # Make argument encode with Base64, avoid string quote problem pass to elisp side.
        args = list(map(string_to_base64, args))

        args.insert(0, method_name)

        # Call eval-in-emacs elisp function.
        epc_client.call("eval-in-emacs", args)

def get_app_dark_mode(app_dark_mode_var):
    app_dark_mode = get_emacs_var(app_dark_mode_var)
    return (app_dark_mode == "force" or \
            app_dark_mode == True or \
            (app_dark_mode == "follow" and \
             get_emacs_theme_mode() == "dark"))

def get_emacs_theme_mode():
    return get_emacs_func_result("eaf-get-theme-mode", [])

def get_emacs_theme_background():
    return get_emacs_func_result("eaf-get-theme-background-color", [])

def get_emacs_theme_foreground():
    return get_emacs_func_result("eaf-get-theme-foreground-color", [])

def get_emacs_func_result(method_name, args):
    global epc_client

    if epc_client == None:
        print("Please call init_epc_client first before callling eval_in_emacs.")
    else:
        args = list(map(convert_arg_to_str, args))
        # Make argument encode with Base64, avoid string quote problem pass to elisp side.
        args = list(map(string_to_base64, args))

        args.insert(0, method_name)

        # Call eval-in-emacs elisp function synchronously and return the result
        result = epc_client.call_sync("eval-in-emacs", args)
        return result if result != [] else False

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
        return symbol_value == True
    else:
        return symbol_value

def get_emacs_vars(args):
    global epc_client

    return list(map(lambda result: convert_emacs_bool(result[0], result[1]) if result != [] else False, epc_client.call_sync("get-emacs-vars", args)))

def get_emacs_var(var_name):
    global epc_client

    (symbol_value, symbol_is_boolean) = epc_client.call_sync("get-emacs-var", [var_name])

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
