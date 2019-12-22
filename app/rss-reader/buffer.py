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

from PyQt5.QtCore import QUrl
from PyQt5.QtGui import QColor, QPainter, QFont, QTextDocument
from PyQt5.QtWidgets import QPushButton, QHBoxLayout, QVBoxLayout, QWidget, QApplication, QWidget, QListWidget, QVBoxLayout, QLabel, QPushButton, QListWidgetItem
from core.buffer import Buffer
from PyQt5 import QtWidgets, QtCore
from core.browser import BrowserView
import feedparser

class AppBuffer(Buffer):
    def __init__(self, buffer_id, url, arguments):
        Buffer.__init__(self, buffer_id, url, arguments, True, QColor(0, 0, 0, 255))

        self.add_widget(RSSReaderWidget())

        self.worker = FetchRSSThread()
        self.workerThread = QtCore.QThread()
        self.workerThread.started.connect(self.worker.run)
        self.worker.fetchRSS.connect(self.buffer_widget.handle_rss)
        self.worker.moveToThread(self.workerThread)
        self.workerThread.start()

class RSSReaderWidget(QWidget):

    def __init__(self):
        super(RSSReaderWidget, self).__init__()

        self.feed_area = QWidget()
        self.feed_list = QListWidget()
        self.feed_list.setStyleSheet( """QListWidget{background: #4D5250;}""")
        panel_layout = QVBoxLayout()
        panel_layout.setSpacing(0)
        panel_layout.setContentsMargins(0, 0, 0, 0)
        panel_layout.addWidget(self.feed_list)
        self.feed_area.setLayout(panel_layout)

        self.article_area = QWidget()
        self.article_list = QListWidget()
        article_layout = QVBoxLayout()
        article_layout.setSpacing(0)
        article_layout.setContentsMargins(0, 0, 0, 0)

        self.browser = BrowserView()

        article_layout.addWidget(self.article_list)
        article_layout.addWidget(self.browser)

        article_layout.setStretchFactor(self.article_list, 1)
        article_layout.setStretchFactor(self.browser, 3)


        self.article_area.setLayout(article_layout)

        hbox = QHBoxLayout()
        hbox.setSpacing(0)
        hbox.setContentsMargins(0, 0, 0, 0)

        hbox.addWidget(self.feed_area)
        hbox.addWidget(self.article_area)

        hbox.setStretchFactor(self.feed_area, 1)
        hbox.setStretchFactor(self.article_area, 3)

        self.setLayout(hbox)

        self.article_list.itemActivated.connect(self.handle_article)

    def handle_article(self, article_item):
        self.browser.setUrl(QUrl(article_item.post_link))

    def handle_rss(self, feed_object):
        feed_item = QListWidgetItem(self.feed_list)
        feed_item_widget = RSSFeedItem(feed_object, len(feed_object.entries))
        feed_item.setSizeHint(feed_item_widget.sizeHint())
        self.feed_list.addItem(feed_item)
        self.feed_list.setItemWidget(feed_item, feed_item_widget)

        self.browser.setUrl(QUrl(feed_object.entries[0].link))

        for post in feed_object.entries:
            item_widget = RSSArticleItem(post)
            item = QListWidgetItem(self.article_list)
            item.post_link = item_widget.post_link
            item.setSizeHint(item_widget.sizeHint())
            self.article_list.addItem(item)
            self.article_list.setItemWidget(item, item_widget)

class FetchRSSThread(QtCore.QObject):
    fetchRSS = QtCore.pyqtSignal(object)

    def __init__(self):
        super().__init__()

    @QtCore.pyqtSlot()
    def run(self):
        d = feedparser.parse('https://sachachua.com/blog/feed/')
        # d = feedparser.parse('https://planet.emacslife.com/atom.xml')
        self.fetchRSS.emit(d)

class RSSFeedItem(QWidget):

    def __init__(self, feed_object, post_num):
        super(RSSFeedItem, self).__init__()

        layout = QHBoxLayout()
        layout.setContentsMargins(10, 10, 10, 10)

        title_label = QLabel(feed_object.feed.title)
        title_label.setFont(QFont('Arial', 18))
        title_label.setStyleSheet("color: #DDD")
        layout.addWidget(title_label)

        number_label = QLabel(str(post_num))
        number_label.setFont(QFont('Arial', 16))
        number_label.setStyleSheet("color: #AAA")
        layout.addStretch(1)
        layout.addWidget(number_label)

        self.setLayout(layout)

class RSSArticleItem(QWidget):

    def __init__(self, post):
        super(RSSArticleItem, self).__init__()

        self.post_id = post.id
        self.post_link = post.link

        date = ""
        try:
            date = "%d-%02d-%02d" % (post.published_parsed.tm_year, post.published_parsed.tm_mon, post.published_parsed.tm_mday)
        except Exception:
            pass

        layout = QVBoxLayout()
        layout.setContentsMargins(20, 20, 20, 20)

        title_label = QLabel(post.title)
        title_label.setFont(QFont('Arial', 18))
        layout.addWidget(title_label)

        description_doc = QTextDocument()
        description_doc.setHtml(post.description)
        description_label = QLabel(self.truncate_description(description_doc.toPlainText()))
        description_label.setWordWrap(True)
        description_label.setStyleSheet("color: #333")
        description_label.setFont(QFont("Arial", 16))
        layout.addWidget(description_label)

        post_info_widget = QWidget()
        post_box = QHBoxLayout()
        post_box.setSpacing(0)
        post_box.setContentsMargins(0, 0, 0, 0)

        author_label = QLabel(post.author)
        author_label.setStyleSheet("color: #666")
        post_box.addWidget(author_label)
        post_box.addStretch(1)
        date_label = QLabel(date)
        date_label.setStyleSheet("color: #666")
        post_box.addWidget(date_label)

        post_info_widget.setLayout(post_box)

        layout.addWidget(post_info_widget)

        self.setLayout(layout)

    def truncate_description(self, text):
        return (text[:90] + ' ...') if len(text) > 90 else text
