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
from PyQt5.QtCore import QUrl, Qt, QEvent, QPointF, QEventLoop, QVariant, QTimer
from PyQt5.QtNetwork import QNetworkCookie
from PyQt5.QtWebEngineWidgets import QWebEngineView, QWebEnginePage, QWebEngineContextMenuData
from PyQt5.QtWidgets import QApplication, QWidget
from core.utils import touch
import os

MOUSE_BACK_BUTTON = 8
MOUSE_FORWARD_BUTTON = 16

class BrowserView(QWebEngineView):

    open_url_in_new_tab = QtCore.pyqtSignal(str)

    def __init__(self):
        super(QWebEngineView, self).__init__()

        self.installEventFilter(self)

        self.web_page = BrowserPage()
        self.setPage(self.web_page)

        self.cookie_store = self.page().profile().cookieStore()
        self.cookie_storage = BrowserCookieStorage()
        self.cookie_store.cookieAdded.connect(self.cookie_storage.add_cookie)

        self.load_cookie()

    def load_cookie(self):
        for cookie in self.cookie_storage.load_cookie():
            self.cookie_store.setCookie(cookie)

    def clean_cookie(self):
        self.cookie_storage.clean_cookie(self.cookie_store)

    def createWindow(self, window_type):
        return self.create_new_browser_window_callback()

    def event(self, event):
        if event.type() == QEvent.ChildAdded:
            obj = event.child()
            if isinstance(obj, QWidget):
                obj.installEventFilter(self)

        return QWebEngineView.event(self, event)

    def eventFilter(self, obj, event):
        if event.type() == QEvent.MouseButtonRelease:
            hit = self.web_page.hitTestContent(event.pos())
            clicked_url = hit.linkUrl()
            base_url = hit.baseUrl()

            if clicked_url is not None and base_url is not None and clicked_url != base_url and clicked_url != '':
                result = ""

                if 'http://' in clicked_url or 'https://' in clicked_url:
                    result = clicked_url
                elif clicked_url == "#":
                    result = base_url + clicked_url
                else:
                    # Don't open url in EAF if clicked_url is not start with http/ftp or #
                    result = "http://" + base_url.split("/")[2] + clicked_url

                    event.accept()
                    return False

                modifiers = QApplication.keyboardModifiers()

                if modifiers != Qt.ControlModifier:
                    # Load url in current tab.
                    self.setUrl(QUrl(result))
                else:
                    # Load url in new tab if user press ctrl modifier.
                    self.open_url_in_new_tab.emit(result)

                return True

            event.accept()
            return False

        elif event.type() == QEvent.MouseButtonPress:
            if event.button() == MOUSE_FORWARD_BUTTON:
                self.forward()

                event.accept()
                return True
            elif event.button() == MOUSE_BACK_BUTTON:
                self.back()

                event.accept()
                return True

        return super(QWebEngineView, self).eventFilter(obj, event)

    def zoom_in(self):
        self.setZoomFactor(min(5, self.zoomFactor() + 0.25))

    def zoom_out(self):
        self.setZoomFactor(max(0.25, self.zoomFactor() - 0.25))

    def zoom_reset(self):
        self.setZoomFactor(1)

class BrowserPage(QWebEnginePage):

    def __init__(self):
        QWebEnginePage.__init__(self)

    def hitTestContent(self, pos):
        return WebHitTestResult(self, pos)

    def mapToViewport(self, pos):
        return QPointF(pos.x(), pos.y())

    def executeJavaScript(self, scriptSrc):
        self.loop = QEventLoop()
        self.result = QVariant()
        QTimer.singleShot(250, self.loop.quit)

        self.runJavaScript(scriptSrc, self.callbackJS)
        self.loop.exec_()
        self.loop = None
        return self.result

    def callbackJS(self, res):
        if self.loop is not None and self.loop.isRunning():
            self.result = res
            self.loop.quit()

class WebHitTestResult():
    def __init__(self, page, pos):
        self.page = page
        self.pos = pos
        self.m_linkUrl = self.page.url().toString()
        self.m_baseUrl = self.page.url().toString()
        self.viewportPos = self.page.mapToViewport(self.pos)
        self.source = """(function() {
        let e = document.elementFromPoint(%1, %2);
        if (!e)
            return;
        function isMediaElement(e) {
            return e.tagName == 'AUDIO' || e.tagName == 'VIDEO';
        };
        function isEditableElement(e) {
            if (e.isContentEditable)
                return true;
            if (e.tagName === 'INPUT' || e.tagName === 'TEXTAREA')
                return e.getAttribute('readonly') != 'readonly';
            return false;
        };
        function isSelected(e) {
            let selection = window.getSelection();
            if (selection.type !== 'Range')
                return false;
            return window.getSelection().containsNode(e, true);
        };
        let res = {
            baseUrl: document.baseURI,
            alternateText: e.getAttribute('alt'),
            boundingRect: '',
            imageUrl: '',
            contentEditable: isEditableElement(e),
            contentSelected: isSelected(e),
            linkTitle: '',
            linkUrl: '',
            mediaUrl: '',
            tagName: e.tagName.toLowerCase()
        };
        let r = e.getBoundingClientRect();
        res.boundingRect = [r.top, r.left, r.width, r.height];
        if (e.tagName == 'IMG')
            res.imageUrl = e.getAttribute('src');
        if (e.tagName == 'A') {
            res.linkTitle = e.text;
            res.linkUrl = e.getAttribute('href');
        }
        while (e) {
            if (res.linkTitle === '' && e.tagName === 'A') {
                res.linkTitle = e.text;
                if(res.linkUrl === '') {
                res.linkUrl = e.getAttribute('href');
                }
            }
            if (res.mediaUrl === '' && isMediaElement(e)) {
                res.mediaUrl = e.currentSrc;
                res.mediaPaused = e.paused;
                res.mediaMuted = e.muted;
            }
            e = e.parentElement;
        }
        return res;
        })()"""

        self.js = self.source.replace("%1", str(self.viewportPos.x())).replace("%2", str(self.viewportPos.y()))
        self.dic = self.page.executeJavaScript(self.js)
        if self.dic is None:
            return

        self.m_isNull = False
        self.m_baseUrl = self.dic["baseUrl"]
        self.m_alternateText = self.dic["alternateText"]
        self.m_imageUrl = self.dic["imageUrl"]
        self.m_isContentEditable = self.dic["contentEditable"]
        self.m_isContentSelected = self.dic["contentSelected"]
        self.m_linkTitle = self.dic["linkTitle"]
        self.m_linkUrl = self.dic["linkUrl"]
        self.m_mediaUrl = self.dic["mediaUrl"]
        try:
            self.m_mediaPaused = self.dic["mediaPaused"]
            self.m_mediaMuted = self.dic["mediaMuted"]
        except Exception:
            pass
        self.m_tagName = self.dic["tagName"]

    def linkUrl(self):
        return self.m_linkUrl

    def isContentEditable(self):
        return self.m_isContentEditable

    def isContentSelected(self):
        return self.m_isContentSelected

    def imageUrl(self):
        try:
            return self.m_imageUrl
        except Exception:
            return ""

    def mediaUrl(self):
        return self.m_mediaUrl

    def baseUrl(self):
        return self.m_baseUrl

    def updateWithContextMenuData(self, data):
        if data.isValid():
            pass
        else:
            return

        self.m_linkTitle = data.linkText()
        self.m_linkUrl = data.linkUrl().toString()
        self.m_isContentEditable = data.isContentEditable()
        if data.selectedText() == "":
            self.m_isContentSelected = False
        else:
            self.m_isContentSelected = True

        if data.mediaType() == QWebEngineContextMenuData.MediaTypeImage:
            self.m_imageUrl = data.mediaUrl().toString()
        elif data.mediaType() == QWebEngineContextMenuData.MediaTypeAudio or data.mediaType() == QWebEngineContextMenuData.MediaTypeVideo:
            self.m_mediaUrl = data.mediaUrl().toString()

def webview_scroll(webview, scroll_direction, scroll_type):
    line_offset = 50

    if scroll_type == "page":
        if scroll_direction == "up":
            webview.buffer_widget.web_page.runJavaScript("window.scrollBy(0, screen.height)")
        else:
            webview.buffer_widget.web_page.runJavaScript("window.scrollBy(0, -screen.height)")
    else:
        if scroll_direction == "up":
            webview.buffer_widget.web_page.runJavaScript("window.scrollBy({0}, {1});".format(0, line_offset))
        else:
            webview.buffer_widget.web_page.runJavaScript("window.scrollBy({0}, {1});".format(0, -line_offset))

class BrowserCookieStorage:
    def __init__(self):
        self.cookie_file = os.path.expanduser("~/.emacs.d/eaf/browser/cookie/cookie")

        touch(self.cookie_file)

    def load_cookie(self):
        with open(self.cookie_file, 'rb+') as store:
            cookies = store.read()
            return QNetworkCookie.parseCookies(cookies)

    def save_cookie(self, cookie):
        with open(self.cookie_file, 'wb+') as store:
            store.write(cookie + b'\n' if cookie is not None else b'')

    def add_cookie(self, cookie):
        raw = cookie.toRawForm()
        self.save_cookie(raw)

    def clean_cookie(self, cookie_store):
        cookie_store.deleteAllCookies()

        open(self.cookie_file, 'w').close()
