#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import ctypes
import os
from ctypes import byref, c_bool, c_double, c_int32, c_long, c_uint32, c_void_p

from PyQt6.QtCore import QPoint, QTimer, Qt

from core.utils import eval_in_emacs


class CGPoint(ctypes.Structure):
    _fields_ = [("x", c_double), ("y", c_double)]


class CGSize(ctypes.Structure):
    _fields_ = [("width", c_double), ("height", c_double)]


class CGRect(ctypes.Structure):
    _fields_ = [("origin", CGPoint), ("size", CGSize)]


class MacOSWindowBridge:
    """Read native macOS window geometry and foreground application state."""

    window_list_options = 1 | 16  # On-screen windows, excluding desktop elements.
    cf_number_sint32_type = 3

    def __init__(self):
        self.core_graphics = ctypes.CDLL(
            "/System/Library/Frameworks/CoreGraphics.framework/CoreGraphics")
        self.core_foundation = ctypes.CDLL(
            "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation")
        ctypes.CDLL("/System/Library/Frameworks/AppKit.framework/AppKit")
        self.objc = ctypes.CDLL("/usr/lib/libobjc.A.dylib")

        self._configure_core_graphics()
        self._configure_objc()

    def _configure_core_graphics(self):
        cg = self.core_graphics
        cf = self.core_foundation

        cg.CGWindowListCopyWindowInfo.argtypes = [c_uint32, c_uint32]
        cg.CGWindowListCopyWindowInfo.restype = c_void_p
        cg.CGRectMakeWithDictionaryRepresentation.argtypes = [
            c_void_p, ctypes.POINTER(CGRect)]
        cg.CGRectMakeWithDictionaryRepresentation.restype = c_bool

        cf.CFArrayGetCount.argtypes = [c_void_p]
        cf.CFArrayGetCount.restype = c_long
        cf.CFArrayGetValueAtIndex.argtypes = [c_void_p, c_long]
        cf.CFArrayGetValueAtIndex.restype = c_void_p
        cf.CFDictionaryGetValue.argtypes = [c_void_p, c_void_p]
        cf.CFDictionaryGetValue.restype = c_void_p
        cf.CFNumberGetValue.argtypes = [c_void_p, c_long, c_void_p]
        cf.CFNumberGetValue.restype = c_bool
        cf.CFRelease.argtypes = [c_void_p]

        self.owner_pid_key = c_void_p.in_dll(cg, "kCGWindowOwnerPID").value
        self.window_number_key = c_void_p.in_dll(cg, "kCGWindowNumber").value
        self.window_layer_key = c_void_p.in_dll(cg, "kCGWindowLayer").value
        self.bounds_key = c_void_p.in_dll(cg, "kCGWindowBounds").value

    def _configure_objc(self):
        self.objc.objc_getClass.argtypes = [ctypes.c_char_p]
        self.objc.objc_getClass.restype = c_void_p
        self.objc.sel_registerName.argtypes = [ctypes.c_char_p]
        self.objc.sel_registerName.restype = c_void_p

        self.send_object = ctypes.CFUNCTYPE(
            c_void_p, c_void_p, c_void_p)(("objc_msgSend", self.objc))
        self.send_pid = ctypes.CFUNCTYPE(
            c_int32, c_void_p, c_void_p)(("objc_msgSend", self.objc))
        self.send_object_pid = ctypes.CFUNCTYPE(
            c_void_p, c_void_p, c_void_p, c_int32)(
                ("objc_msgSend", self.objc))
        self.send_bool_options = ctypes.CFUNCTYPE(
            c_bool, c_void_p, c_void_p, ctypes.c_ulong)(
                ("objc_msgSend", self.objc))

        workspace_class = self.objc.objc_getClass(b"NSWorkspace")
        self.workspace = self.send_object(
            workspace_class, self.objc.sel_registerName(b"sharedWorkspace"))
        self.frontmost_application_selector = self.objc.sel_registerName(
            b"frontmostApplication")
        self.process_identifier_selector = self.objc.sel_registerName(
            b"processIdentifier")
        self.running_application_class = self.objc.objc_getClass(
            b"NSRunningApplication")
        self.running_application_selector = self.objc.sel_registerName(
            b"runningApplicationWithProcessIdentifier:")
        self.activate_selector = self.objc.sel_registerName(
            b"activateWithOptions:")

    def _dictionary_int(self, dictionary, key):
        value = self.core_foundation.CFDictionaryGetValue(dictionary, key)
        if not value:
            return None

        result = c_int32()
        if self.core_foundation.CFNumberGetValue(
                value, self.cf_number_sint32_type, byref(result)):
            return result.value
        return None

    def emacs_windows(self, emacs_pid):
        """Return on-screen, normal-layer windows owned by Emacs."""
        windows = self.core_graphics.CGWindowListCopyWindowInfo(
            self.window_list_options, 0)
        if not windows:
            return {}

        result = {}
        try:
            for index in range(self.core_foundation.CFArrayGetCount(windows)):
                info = self.core_foundation.CFArrayGetValueAtIndex(windows, index)
                if self._dictionary_int(info, self.owner_pid_key) != emacs_pid:
                    continue
                if self._dictionary_int(info, self.window_layer_key) != 0:
                    continue

                window_number = self._dictionary_int(
                    info, self.window_number_key)
                bounds_dictionary = self.core_foundation.CFDictionaryGetValue(
                    info, self.bounds_key)
                bounds = CGRect()
                if (window_number is not None and bounds_dictionary and
                        self.core_graphics.CGRectMakeWithDictionaryRepresentation(
                            bounds_dictionary, byref(bounds))):
                    result[window_number] = (
                        bounds.origin.x,
                        bounds.origin.y,
                        bounds.size.width,
                        bounds.size.height)
        finally:
            self.core_foundation.CFRelease(windows)

        return result

    def frontmost_pid(self):
        """Return the process identifier of the frontmost macOS application."""
        application = self.send_object(
            self.workspace, self.frontmost_application_selector)
        if not application:
            return None
        return self.send_pid(application, self.process_identifier_selector)

    def activate_application(self, pid):
        """Activate a running application immediately through AppKit."""
        application = self.send_object_pid(
            self.running_application_class,
            self.running_application_selector,
            int(pid))
        if not application:
            return False
        return self.send_bool_options(
            application, self.activate_selector, 1)  # Activate all windows.


class MacOSWindowTracker:
    """Keep top-level EAF views aligned with their macOS Emacs windows."""

    edge_anchor_margin = 100

    def __init__(self, emacs_pid, views):
        self.emacs_pid = int(emacs_pid)
        self.eaf_pid = os.getpid()
        self.views = views
        self.bridge = MacOSWindowBridge()
        self.last_frontmost_pid = None

        self.timer = QTimer()
        self.timer.setTimerType(Qt.TimerType.PreciseTimer)
        self.timer.timeout.connect(self.update)
        self.timer.start(16)

    @staticmethod
    def _contains(bounds, x, y):
        left, top, width, height = bounds
        return left <= x <= left + width and top <= y <= top + height

    @staticmethod
    def _distance_to_bounds(bounds, x, y):
        left, top, width, height = bounds
        nearest_x = min(max(x, left), left + width)
        nearest_y = min(max(y, top), top + height)
        return (x - nearest_x) ** 2 + (y - nearest_y) ** 2

    def _attach_view(self, view, windows):
        if not windows:
            return

        if view.emacs_frame_geometry is not None:
            frame_x, frame_y, frame_width, frame_height = \
                view.emacs_frame_geometry
            window_number, bounds = min(
                windows.items(),
                key=lambda candidate: (
                    (candidate[1][0] - frame_x) ** 2 +
                    (candidate[1][1] - frame_y) ** 2 +
                    (candidate[1][2] - frame_width) ** 2 +
                    (candidate[1][3] - frame_height) ** 2))
            metrics_bounds = view.emacs_frame_geometry
        else:
            center_x = view.x + view.width / 2
            center_y = view.y + view.height / 2
            containing_candidates = [
                (window_number, bounds)
                for window_number, bounds in windows.items()
                if self._contains(bounds, center_x, center_y)
            ]
            candidates = containing_candidates or list(windows.items())
            if containing_candidates:
                window_number, bounds = min(
                    candidates,
                    key=lambda candidate: candidate[1][2] * candidate[1][3])
            else:
                window_number, bounds = min(
                    candidates,
                    key=lambda candidate: self._distance_to_bounds(
                        candidate[1], center_x, center_y))
            metrics_bounds = bounds

        view.macos_window_number = window_number
        view.macos_window_metrics = (
            view.x - metrics_bounds[0],
            view.y - metrics_bounds[1],
            metrics_bounds[0] + metrics_bounds[2] - view.x - view.width,
            metrics_bounds[1] + metrics_bounds[3] - view.y - view.height,
            view.width,
            view.height)

    def _axis_geometry(self, start, extent, leading, trailing, view_extent):
        leading_anchored = 0 <= leading <= self.edge_anchor_margin
        trailing_anchored = 0 <= trailing <= self.edge_anchor_margin

        if leading_anchored and trailing_anchored:
            target_start = start + leading
            target_extent = max(1, extent - leading - trailing)
        elif leading_anchored:
            target_start = start + leading
            target_extent = view_extent
        elif trailing_anchored:
            target_extent = view_extent
            target_start = start + extent - trailing - target_extent
        else:
            target_extent = view_extent
            free_space = max(0, extent - target_extent)
            original_free_space = max(1, leading + trailing)
            target_start = start + free_space * leading / original_free_space

        return round(target_start), round(target_extent)

    def _update_view_position(self, view, windows):
        if view.macos_window_number is None:
            self._attach_view(view, windows)

        if (view.macos_window_number is None or
                view.macos_window_metrics is None or
                view.macos_window_number not in windows):
            return False

        bounds = windows[view.macos_window_number]
        left, top, right, bottom, view_width, view_height = \
            view.macos_window_metrics
        target_x, target_width = self._axis_geometry(
            bounds[0], bounds[2], left, right, view_width)
        target_y, target_height = self._axis_geometry(
            bounds[1], bounds[3], top, bottom, view_height)

        if target_x != view.x or target_y != view.y:
            view.x = target_x
            view.y = target_y
            view.windowHandle().setPosition(QPoint(target_x, target_y))
        if target_width != view.width or target_height != view.height:
            view.width = target_width
            view.height = target_height
            view.resize(target_width, target_height)
            return True
        return False

    def _update_frontmost_application(self):
        frontmost_pid = self.bridge.frontmost_pid()
        if frontmost_pid is None:
            return

        if frontmost_pid == self.emacs_pid:
            for view in self.views():
                if not view.isVisible():
                    view.try_show_top_view()

        external_application = (
            frontmost_pid != self.emacs_pid and
            frontmost_pid != self.eaf_pid)
        if external_application:
            # Hide immediately in the Qt process.  Waiting for the Emacs RPC
            # screenshot path can race with app switching and leave an
            # always-on-top EAF window visible until the next focus change.
            for view in self.views():
                if view.isVisible():
                    view.try_hide_top_view()

        if frontmost_pid == self.last_frontmost_pid:
            return

        previous_pid = self.last_frontmost_pid
        self.last_frontmost_pid = frontmost_pid
        if (external_application and
                (previous_pid is None or
                 previous_pid in (self.emacs_pid, self.eaf_pid))):
            eval_in_emacs('eaf--topmost-macos-focus-out', [])

    def activate_emacs_after_mouse_release(self):
        """Return focus only if EAF is still the frontmost application."""
        if self.bridge.frontmost_pid() == self.eaf_pid:
            self.bridge.activate_application(self.emacs_pid)

    def update(self):
        """Synchronize EAF position and visibility with native macOS state."""
        windows = self.bridge.emacs_windows(self.emacs_pid)
        views = self.views()
        for view in views:
            self._update_view_position(view, windows)
        self._update_frontmost_application()
