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
import functools
import os
import socket
import sys
import base64
import threading
import subprocess
from functools import wraps

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
