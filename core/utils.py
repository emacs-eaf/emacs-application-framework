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
from PyQt5.QtGui import QClipboard
from PyQt5.QtWidgets import QApplication
from epc.client import EPCClient
from functools import wraps
from ast import literal_eval
import base64
import functools
import os
import socket
import subprocess
import sys
import threading
import re

class PostGui(QtCore.QObject):

    through_thread = QtCore.pyqtSignal(object, object)

    def __init__(self, inclass=True):
        super(PostGui, self).__init__()
        self.through_thread.connect(self.on_signal_received)
        self.inclass = inclass

    def __call__(self, func):
        self._func = func

        @functools.wraps(func)
        def obj_call(*args, **kwargs):
            self.emit_signal(args, kwargs)
        return obj_call

    def emit_signal(self, args, kwargs):
        self.through_thread.emit(args, kwargs)

    def on_signal_received(self, args, kwargs):
        if self.inclass:
            obj, args = args[0], args[1:]
            self._func(obj, *args, **kwargs)
        else:
            self._func(*args, **kwargs)

def touch(path):
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
    return str(base64.b64encode(str(text).encode("utf-8")), "utf-8")

def get_local_ip():
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        s.connect(("8.8.8.8", 80))
        return s.getsockname()[0]
    except OSError:
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
        proc = subprocess.Popen(popen_args, stdout=stdout_file)
        proc.wait()
        on_exit()
        return
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
        retcode = subprocess.call(popen_args, stdout=stdout_file)
        on_exit(retcode)
        return
    thread = threading.Thread(target=run_in_thread, args=(on_exit, popen_args))
    thread.start()
    # returns immediately after the thread starts
    return thread

def get_clipboard_text():
    ''' Get text from system clipboard.'''
    clipboard = QApplication.clipboard()
    text = clipboard.text()
    if text:
        return text

    if clipboard.supportsSelection():
        return clipboard.text(QClipboard.Selection)

    return ""

def set_clipboard_text(text):
    ''' Set text to system clipboard.'''
    clipboard = QApplication.clipboard()
    clipboard.setText(text)

    if clipboard.supportsSelection():
        clipboard.setText(text, QClipboard.Selection)


def interactive(insert_or_do = False, msg_emacs = None, new_name = None):
    """
    Defines an interactive command invoked from Emacs.
    """
    def wrap(f, insert_or_do = insert_or_do, msg_emacs = msg_emacs, new_name = new_name):
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
    f.abstract = True
    @wraps(f)
    def wrap(*args, **kwargs):
        return f(*args, **kwargs)
    return wrap

epc_client = None

def init_epc_client(emacs_server_port):
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

def eval_in_emacs(method_name, args):
    global epc_client

    if epc_client == None:
        print("Please call init_epc_client first before call eval_in_emacs.")
    else:
        # Make argument encode with Base64, avoid string quote problem pass to elisp side.
        args = list(map(string_to_base64, args))
        args.insert(0, method_name)

        # Call eval-in-emacs elisp function.
        epc_client.call("eval-in-emacs", args)

def message_to_emacs(message):
    eval_in_emacs('eaf--show-message', [message])

def set_emacs_var(var_name, var_value, eaf_specific):
    eval_in_emacs('eaf--set-emacs-var', [var_name, var_value, eaf_specific])

def open_url_in_background_tab(url):
    eval_in_emacs('eaf-open-browser-in-background', [url])

def duplicate_page_in_new_tab(url):
    eval_in_emacs('eaf-browser--duplicate-page-in-new-tab', [url])

def open_url_in_new_tab(url):
    eval_in_emacs('eaf-open-browser', [url])

def translate_text(text):
    eval_in_emacs('eaf-translate-text', [text])

def input_message(buffer_id, message, callback_tag, input_type, input_content):
    eval_in_emacs('eaf--input-message', [buffer_id, message, callback_tag, input_type, input_content])

def focus_emacs_buffer(message):
    eval_in_emacs('eaf-focus-buffer', [message])

def atomic_edit(buffer_id, focus_text):
    eval_in_emacs('eaf--atomic-edit', [buffer_id, focus_text])

def list_string_to_list(list_string):
    '''Convert the list string from emacs var to list type.'''
    list_var = list_string.removeprefix('(').removesuffix(')')
    quote = 0
    extra_char_num = 0
    for x in range(len(list_var)):
        x += extra_char_num
        if list_var[x] == '"':
            if quote == 1:
                quote = 0
            else:
                quote = 1

        if (list_var[x] == '(') and (quote == 0):
            list_var = list_var[:x] + '[' + list_var[x + 1:]
        elif (list_var[x] == ')') and (quote == 0):
            list_var = list_var[:x] + ']' + list_var[x + 1:]
        elif (list_var[x] == ' ') and (quote == 0):
            list_var = list_var[:x] + '{split}' + list_var[x + 1:]
            extra_char_num += 6
        elif (list_var[x] == '.') and (quote == 0):
            list_var = list_var[:x] + '' + list_var[x + 2:]
            extra_char_num -= 2

    list_var = str(list_var.split('{split}')).replace("', '[", "', ['").replace("]'", "']").replace("'[", "['").replace("'\"", "'").replace("\"'", "'")
    
    if list_var[0] == "'" and list_var[1] == "[":
        list_var = "['" + list_var[2:]

    list_var = literal_eval(str(list_var))
    return list_var
