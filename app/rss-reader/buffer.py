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

from PyQt5.QtCore import QUrl, Qt
from PyQt5.QtGui import QColor, QFont
from PyQt5.QtWidgets import QPushButton, QHBoxLayout, QWidget, QWidget, QListWidget, QVBoxLayout, QLabel, QListWidgetItem, QStackedWidget
from core.buffer import Buffer
from PyQt5 import QtCore
from core.browser import BrowserView
from core.utils import touch
import feedparser
import json
import os

class AppBuffer(Buffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict):
        Buffer.__init__(self, buffer_id, url, arguments, emacs_var_dict, True, QColor(0, 0, 0, 255))

        self.add_widget(RSSReaderWidget(config_dir))

        self.buffer_widget.browser.message_to_emacs = self.message_to_emacs
        self.buffer_widget.browser.set_emacs_var = self.set_emacs_var
        self.buffer_widget.browser.eval_in_emacs = self.eval_in_emacs
        self.buffer_widget.browser.send_input_message = self.send_input_message

    def add_subscription(self):
        self.buffer_widget.send_input_message("Subscribe to RSS feed: ", "add_subscription")

    def delete_subscription(self):
        self.buffer_widget.send_input_message("Are you sure you want to delete the current feed? (y or n): ", "delete_subscription")

    def next_subscription(self):
        self.buffer_widget.next_subscription()

    def prev_subscription(self):
        self.buffer_widget.prev_subscription()

    def next_article(self):
        self.buffer_widget.next_article()

    def prev_article(self):
        self.buffer_widget.prev_article()

    def first_subscription(self):
        self.buffer_widget.first_subscription()

    def last_subscription(self):
        self.buffer_widget.last_subscription()

    def first_article(self):
        self.buffer_widget.first_article()

    def last_article(self):
        self.buffer_widget.last_article()

    def scroll_up(self):
        self.buffer_widget.browser.scroll_up()

    def scroll_down(self):
        self.buffer_widget.browser.scroll_down()

    def scroll_up_page(self):
        self.buffer_widget.browser.scroll_up_page()

    def scroll_down_page(self):
        self.buffer_widget.browser.scroll_down_page()

    def scroll_to_begin(self):
        self.buffer_widget.browser.scroll_to_begin()

    def scroll_to_bottom(self):
        self.buffer_widget.browser.scroll_to_bottom()

    def handle_input_message(self, result_type, result_content):
        if result_type == "search_text_forward":
            self.buffer_widget.browser._search_text(str(result_content))
        elif result_type == "search_text_backward":
            self.buffer_widget.browser._search_text(str(result_content), True)
        elif result_type == "jump_link":
            self.buffer_widget.browser.jump_to_link(str(result_content))
        elif result_type == "jump_link_new_buffer":
            self.buffer_widget.browser.jump_to_link(str(result_content), "true")
        elif result_type == "add_subscription":
            self.buffer_widget.add_subscription(result_content)
        elif result_type == "delete_subscription":
            if result_content == "y":
                self.buffer_widget.delete_subscription()

    def cancel_input_message(self, result_type):
        if result_type == "jump_link" or result_type == "jump_link_new_buffer":
            self.buffer_widget.browser.cleanup_links()

    def action_quit(self):
        self.buffer_widget.browser.search_quit()

    def open_link(self):
        self.buffer_widget.browser.open_link()
        self.send_input_message("Open Link: ", "jump_link");

    def open_link_new_buffer(self):
        self.buffer_widget.browser.open_link_new_buffer()
        self.send_input_message("Open Link in New Buffer: ", "jump_link_new_buffer");

    def search_text_forward(self):
        self.buffer_widget.browser.search_text_forward()

    def search_text_backward(self):
        self.buffer_widget.browser.search_text_backward()

class RSSReaderWidget(QWidget):

    def __init__(self, config_dir):
        super(RSSReaderWidget, self).__init__()

        self.feed_file_path = os.path.join(config_dir, "rss-reader", "feeds.json")

        self.feed_area = QWidget()
        self.feed_list = QListWidget()
        self.feed_list.setStyleSheet( """QListView {background: #4D5250; show-decoration-selected: 1; selection-background-color: #464646;}""")
        panel_layout = QVBoxLayout()
        panel_layout.setSpacing(0)
        panel_layout.setContentsMargins(0, 0, 0, 0)
        panel_layout.addWidget(self.feed_list)
        self.feed_area.setLayout(panel_layout)

        self.article_area = QWidget()
        self.article_list = QListWidget()
        self.article_list.setStyleSheet( """QListView {background: #FFF; show-decoration-selected: 1; selection-background-color: #EEE;}""")
        self.article_list.verticalScrollBar().setStyleSheet("QScrollBar {width:0px;}");
        article_layout = QVBoxLayout()
        article_layout.setSpacing(0)
        article_layout.setContentsMargins(0, 0, 0, 0)

        self.browser = BrowserView(config_dir)

        article_layout.addWidget(self.article_list)
        article_layout.addWidget(self.browser)

        article_layout.setStretchFactor(self.article_list, 1)
        article_layout.setStretchFactor(self.browser, 3)

        self.article_area.setLayout(article_layout)

        self.welcome_page = QWidget()
        self.welcome_page_box = QVBoxLayout()
        self.welcome_page_box.setSpacing(10)
        self.welcome_page_box.setContentsMargins(0, 0, 0, 0)

        welcome_title_label = QLabel("Welcome to EAF RSS Reader!")
        welcome_title_label.setFont(QFont('Arial', 24))
        welcome_title_label.setStyleSheet("QLabel {color: white; font-weight: bold; margin: 20px;}")
        welcome_title_label.setAlignment(Qt.AlignHCenter)

        add_subscription_label = QLabel("Press 'a' to subscribe to a feed!")
        add_subscription_label.setFont(QFont('Arial', 20))
        add_subscription_label.setStyleSheet("QLabel {color: #grey;}")
        add_subscription_label.setAlignment(Qt.AlignHCenter)

        self.welcome_page_box.addStretch(1)
        self.welcome_page_box.addWidget(welcome_title_label)
        self.welcome_page_box.addWidget(add_subscription_label)
        self.welcome_page_box.addStretch(1)

        self.welcome_page.setLayout(self.welcome_page_box)

        self.right_area = QStackedWidget()
        self.right_area.addWidget(self.welcome_page)
        self.right_area.addWidget(self.article_area)

        if self.has_feed():
            self.right_area.setCurrentIndex(1)
        else:
            self.right_area.setCurrentIndex(0)

        hbox = QHBoxLayout()
        hbox.setSpacing(0)
        hbox.setContentsMargins(0, 0, 0, 0)

        hbox.addWidget(self.feed_area)
        hbox.addWidget(self.right_area)

        hbox.setStretchFactor(self.feed_area, 1)
        hbox.setStretchFactor(self.right_area, 3)

        self.setLayout(hbox)

        self.feed_list.itemActivated.connect(self.handle_feed)
        self.article_list.itemActivated.connect(self.handle_article)

        self.feed_object_dict = {}

        self.init_select_line = False

        self.fetch_feeds()

    def has_feed(self):
        if os.path.exists(self.feed_file_path):
            try:
                with open(self.feed_file_path, "r") as feed_file:
                    feed_dict = json.loads(feed_file.read())
                    return len(feed_dict.keys()) > 0
            except Exception:
                return False

        return False

    def fetch_feeds(self):
        if os.path.exists(self.feed_file_path):
            try:
                with open(self.feed_file_path, "r") as feed_file:
                    feed_dict = json.loads(feed_file.read())
                    for index, feed_link in enumerate(feed_dict):
                        self.fetch_feed(feed_link, index == 0)
            except Exception:
                pass

    def handle_feed(self, feed_item):
        if feed_item.feed_link in self.feed_object_dict:
            self.init_article_area(self.feed_object_dict[feed_item.feed_link])

    def handle_article(self, article_item):
        article_item.mark_as_read()

        self.browser.setUrl(QUrl(article_item.post_link))

    def fetch_feed(self, feed_link, refresh_ui):
        fetchThread = FetchRSSThread(feed_link)
        fetchThread.fetch_rss.connect(lambda f_object, f_link, f_title: self.handle_rss(f_object, f_link, f_title, refresh_ui))
        fetchThread.invalid_rss.connect(self.handle_invalid_rss)

        object_name = "feed_thread_" + feed_link
        setattr(self, object_name, fetchThread)
        getattr(self, object_name).start()

    def add_subscription(self, feed_link):
        if not self.feed_exists(feed_link):
            self.fetch_feed(feed_link, True)
        else:
            self.message_to_emacs.emit("Feed already exists: " + feed_link)

    def delete_subscription(self):
        feed_count = self.feed_list.count()
        current_row = self.feed_list.currentRow()
        feed_link = self.feed_list.currentItem().feed_link
        feed_title = self.feed_list.currentItem().feed_title

        self.feed_list.takeItem(current_row)

        with open(self.feed_file_path, "r") as feed_file:
            feed_dict = json.loads(feed_file.read())
            if feed_link in feed_dict:
                del feed_dict[feed_link]

                with open(self.feed_file_path, "w") as f:
                    f.write(json.dumps(feed_dict))

        if feed_count <= 1:
            self.feed_list.clear()
            self.article_list.clear()
            self.browser.setUrl(QUrl(""))
            self.right_area.setCurrentIndex(0)
        else:
            if current_row < feed_count - 1:
                self.feed_list.setCurrentRow(current_row)
            else:
                self.feed_list.setCurrentRow(feed_count - 2)
            self.handle_feed(self.feed_list.currentItem())
            self.message_to_emacs.emit("Removed feed: " + feed_title)

    def feed_exists(self, feed_link):
        if not os.path.exists(self.feed_file_path):
            return False

        try:
            with open(self.feed_file_path, "r") as feed_file:
                feed_dict = json.loads(feed_file.read())
                return feed_link in feed_dict
        except Exception:
            import traceback
            traceback.print_exc()

            return False

    def save_feed(self, feed_object, feed_link, feed_title):
        touch(self.feed_file_path)

        article_ids = list(map(lambda post: post.id, feed_object.entries))

        try:
            with open(self.feed_file_path, "r") as feed_file:
                feed_dict = json.loads(feed_file.read())
                if feed_link not in feed_dict:
                    feed_dict[feed_link] = {
                        "title": feed_title,
                        "unread_articles": article_ids
                    }

                    self.save_feed_dict(feed_dict)
                    self.message_to_emacs.emit("Add feed: " + feed_link)
        except Exception:
            import traceback
            traceback.print_exc()

            self.save_feed_dict({feed_link : {
                "title": feed_title,
                "unread_articles": article_ids
            }})
            self.message_to_emacs.emit("Add feed: " + feed_link)

    def save_feed_dict(self, feed_dict):
        with open(self.feed_file_path, "w") as f:
            f.write(json.dumps(feed_dict))

    def handle_rss(self, feed_object, feed_link, feed_title, refresh_ui):
        feed_object.feed_link = feed_link
        self.feed_object_dict[feed_link] = feed_object

        self.save_feed(feed_object, feed_link, feed_title)

        self.right_area.setCurrentIndex(1)

        feed_item = QListWidgetItem(self.feed_list)
        feed_item.feed_link = feed_link
        feed_item.feed_title = feed_title
        feed_item_widget = RSSFeedItem(feed_object, len(feed_object.entries))
        feed_item.update_article_num = feed_item_widget.update_article_num
        feed_item.setSizeHint(feed_item_widget.sizeHint())
        self.feed_list.addItem(feed_item)
        self.feed_list.setItemWidget(feed_item, feed_item_widget)

        unread_articles = []
        with open(self.feed_file_path, "r") as feed_file:
            feed_dict = json.loads(feed_file.read())
            if feed_object.feed_link in feed_dict:
                if "unread_articles" in feed_dict[feed_object.feed_link]:
                    unread_articles = ["unread_articles"]

                for index in range(self.feed_list.count()):
                    feed_item = self.feed_list.item(index)
                    if feed_item.feed_link == feed_object.feed_link:
                        feed_item.update_article_num(len(unread_articles))
                        break

        if refresh_ui:
            self.init_article_area(feed_object)

    def init_article_area(self, feed_object):
        self.browser.setUrl(QUrl(feed_object.entries[0].link))

        self.article_list.clear()

        unread_articles = []
        with open(self.feed_file_path, "r") as feed_file:
            feed_dict = json.loads(feed_file.read())
            if feed_object.feed_link in feed_dict and "unread_articles" in feed_dict[feed_object.feed_link]:
                unread_articles = feed_dict[feed_object.feed_link]["unread_articles"]

        for index, post in enumerate(feed_object.entries):
            item_widget = RSSArticleItemWidget(feed_object, post, unread_articles)
            item = QListWidgetItem(self.article_list)
            item.mark_as_read = item_widget.mark_as_read
            item.post_link = item_widget.post_link
            item.setSizeHint(item_widget.sizeHint())
            item_widget.mark_article_read.connect(self.mark_article_read)
            self.article_list.addItem(item)
            self.article_list.setItemWidget(item, item_widget)

            if index == 0:
                item.mark_as_read()

        self.article_list.setCurrentRow(0)

        if not self.init_select_line:
            self.init_select_line = True
            self.feed_list.setCurrentRow(0)

    def handle_invalid_rss(self, feed_link):
        self.message_to_emacs.emit("Invalid feed link: " + feed_link)

    def mark_article_read(self, feed_link, post_link):
        if os.path.exists(self.feed_file_path):
            try:
                with open(self.feed_file_path, "r") as feed_file:
                    feed_dict = json.loads(feed_file.read())
                    if feed_link in feed_dict:
                        unread_articles = feed_dict[feed_link]["unread_articles"]

                        if post_link in unread_articles:
                            unread_articles.remove(post_link)
                            feed_dict[feed_link]["unread_articles"] = unread_articles

                            with open(self.feed_file_path, "w") as f:
                                f.write(json.dumps(feed_dict))

                        for index in range(self.feed_list.count()):
                            feed_item = self.feed_list.item(index)
                            if feed_item.feed_link == feed_link:
                                feed_item.update_article_num(len(unread_articles))
                                break
            except Exception:
                pass

    def next_subscription(self):
        feed_count = self.feed_list.count()
        current_row = self.feed_list.currentRow()

        if current_row < feed_count - 1:
            self.feed_list.setCurrentRow(current_row + 1)
            self.feed_list.scrollToItem(self.feed_list.currentItem())
            self.handle_feed(self.feed_list.currentItem())
        else:
            self.message_to_emacs.emit("End of subscribed feeds")

    def prev_subscription(self):
        current_row = self.feed_list.currentRow()

        if current_row > 0:
            self.feed_list.setCurrentRow(current_row - 1)
            self.feed_list.scrollToItem(self.feed_list.currentItem())
            self.handle_feed(self.feed_list.currentItem())
        else:
            self.message_to_emacs.emit("Beginning of subscribed feeds")

    def first_subscription(self):
        self.feed_list.setCurrentRow(0)
        self.feed_list.scrollToItem(self.feed_list.currentItem())
        self.handle_feed(self.feed_list.currentItem())

    def last_subscription(self):
        feed_count = self.feed_list.count()

        self.feed_list.setCurrentRow(feed_count - 1)
        self.feed_list.scrollToItem(self.feed_list.currentItem())
        self.handle_feed(self.feed_list.currentItem())

    def next_article(self):
        article_count = self.article_list.count()
        current_row = self.article_list.currentRow()

        if current_row < article_count - 1:
            self.article_list.setCurrentRow(current_row + 1)
            self.article_list.scrollToItem(self.article_list.currentItem())
            self.handle_article(self.article_list.currentItem())
        else:
            self.message_to_emacs.emit("End of articles")

    def prev_article(self):
        current_row = self.article_list.currentRow()

        if current_row > 0:
            self.article_list.setCurrentRow(current_row - 1)
            self.article_list.scrollToItem(self.article_list.currentItem())
            self.handle_article(self.article_list.currentItem())
        else:
            self.message_to_emacs.emit("Beginning of articles")

    def first_article(self):
        self.article_list.setCurrentRow(0)
        self.article_list.scrollToItem(self.article_list.currentItem())
        self.handle_article(self.article_list.currentItem())

    def last_article(self):
        article_count = self.article_list.count()

        self.article_list.setCurrentRow(article_count - 1)
        self.article_list.scrollToItem(self.article_list.currentItem())
        self.handle_article(self.article_list.currentItem())

class FetchRSSThread(QtCore.QThread):
    fetch_rss = QtCore.pyqtSignal(object, str, str)
    invalid_rss = QtCore.pyqtSignal(str)

    def __init__(self, feed_link):
        super().__init__()
        self.feed_link = feed_link

    def run(self):
        try:
            d = feedparser.parse(self.feed_link)
            self.fetch_rss.emit(d, self.feed_link, d.feed.title)
        except Exception:
            import traceback
            traceback.print_exc()

            self.invalid_rss.emit(self.feed_link)

class RSSFeedItem(QWidget):

    def __init__(self, feed_object, post_num):
        super(RSSFeedItem, self).__init__()

        layout = QHBoxLayout()
        layout.setContentsMargins(10, 10, 10, 10)

        feed_title_label = QLabel(feed_object.feed.title)
        feed_title_label.setFont(QFont('Arial', 18))
        feed_title_label.setStyleSheet("color: #DDD")
        layout.addWidget(feed_title_label)

        self.number_label = QLabel(str(post_num))
        self.number_label.setFont(QFont('Arial', 16))
        self.number_label.setStyleSheet("color: #AAA")
        layout.addStretch(1)
        layout.addWidget(self.number_label)

        self.setLayout(layout)

    def update_article_num(self, num):
        self.number_label.setText(str(num))

class RSSArticleItemWidget(QWidget):
    mark_article_read = QtCore.pyqtSignal(str, str)

    def __init__(self, feed_object, post, unread_articles):
        super(RSSArticleItemWidget, self).__init__()

        self.feed_object = feed_object
        self.post_id = post.id
        self.post_link = post.link

        date = ""
        try:
            date = "[%d-%02d-%02d]" % (post.published_parsed.tm_year, post.published_parsed.tm_mon, post.published_parsed.tm_mday)
        except Exception:
            pass

        article_layout = QHBoxLayout()
        article_layout.setContentsMargins(10, 10, 0, 0)

        self.article_date_label = QLabel(date)
        self.article_date_label.setFont(QFont('Arial', 16))

        self.article_title_label = QLabel(post.title)
        self.article_title_label.setFont(QFont('Arial', 16))

        self.status_label = QLabel("*")
        self.status_label.setFont(QFont('Arial', 20))

        if self.post_id in unread_articles:
            self.status_label.setStyleSheet("color: #008DFF")
            self.article_date_label.setStyleSheet("color: #464646")
            self.article_title_label.setStyleSheet("color: #464646")
        else:
            self.status_label.setStyleSheet("color: #9e9e9e")
            self.article_date_label.setStyleSheet("color: #9e9e9e")
            self.article_title_label.setStyleSheet("color: #9e9e9e")

        article_layout.addWidget(self.status_label)
        article_layout.addWidget(self.article_date_label)
        article_layout.addWidget(self.article_title_label)
        article_layout.addStretch(1)

        self.setLayout(article_layout)

    def truncate_description(self, text):
        return (text[:90] + ' ...') if len(text) > 90 else text

    def mark_as_read(self):
        self.status_label.setStyleSheet("color: #9e9e9e")
        self.article_date_label.setStyleSheet("color: #9e9e9e")
        self.article_title_label.setStyleSheet("color: #9e9e9e")

        self.mark_article_read.emit(self.feed_object.feed_link, self.post_id)
