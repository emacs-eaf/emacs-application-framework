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

from PyQt5.QtGui import QColor, QFont
from PyQt5.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QLabel, QListView, QStackedWidget, QPushButton, QTextEdit, QTableWidget, QTableWidgetItem, QFrame, QHeaderView
from PyQt5.QtCore import QStringListModel
from PyQt5 import QtCore, QtGui, QtWidgets
from PyQt5.QtCore import Qt, QTimer

from core.buffer import Buffer
from core.utils import interactive

from pygit2 import Repository, discover_repository
from pygit2 import GIT_SORT_TOPOLOGICAL, GIT_SORT_REVERSE, GIT_STATUS_WT_NEW, GIT_STATUS_WT_MODIFIED, GIT_STATUS_WT_DELETED, GIT_STATUS_WT_TYPECHANGE, GIT_STATUS_WT_RENAMED, GIT_STATUS_WT_UNREADABLE

from datetime import datetime

import json
import os

class AppBuffer(Buffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path):
        Buffer.__init__(self, buffer_id, url, arguments, emacs_var_dict, module_path, False)

        self.background_color = QColor(0, 0, 0)

        arguments_dict = json.loads(arguments)

        self.add_widget(GitViewerWidget(buffer_id, config_dir, arguments_dict["directory"]))

        self.build_all_methods(self.buffer_widget)

class GitViewerWidget(QWidget):

    def __init__(self, buffer_id, config_dir, directory):
        super(GitViewerWidget, self).__init__()

        repo_path = discover_repository(directory)
        if repo_path:
            self.repo = Repository(repo_path)
            commit_size = 0
            lastest_commit = None
            for commit in self.repo.walk(self.repo.head.target, GIT_SORT_TOPOLOGICAL):
                commit_size += 1

                if not lastest_commit:
                    lastest_commit = commit

            remote_branch_size = 0
            for branch in self.repo.branches.remote:
                if branch != "origin/HEAD" and branch != "origin/master":
                    remote_branch_size += 1

            # Change background.
            self.setStyleSheet("background-color: #000000");

            # Add main box.
            main_layout = QVBoxLayout()
            main_layout.setSpacing(0)
            main_layout.setContentsMargins(0, 0, 0, 0)

            self.setLayout(main_layout)

            # Add repo top area.
            self.repo_top_font_size = 16
            self.lastest_commit_font_size = 16
            self.repo_top_area = QWidget()
            self.repo_top_layout = QVBoxLayout()
            self.repo_top_layout.setSpacing(0)
            self.repo_top_layout.setContentsMargins(30, 30, 30, 30)
            self.repo_top_area.setLayout(self.repo_top_layout)
            main_layout.addWidget(self.repo_top_area)

            # Add repo title.
            self.repo_title = QLabel(self.repo.path)
            self.repo_title.setStyleSheet("QLabel {color: #E98123;}")
            self.repo_title.setFont(QFont('Arial', 20))
            self.repo_top_layout.addWidget(self.repo_title)

            # Add summary info.
            self.repo_summary_area = QWidget()
            self.repo_summary_layout = QHBoxLayout()
            self.repo_summary_layout.setSpacing(30)
            self.repo_summary_layout.setContentsMargins(0, 0, 0, 0)
            self.repo_summary_area.setLayout(self.repo_summary_layout)
            self.repo_top_layout.addWidget(self.repo_summary_area)

            # Add head info.
            self.head_info = QLabel("{}    {}    {}    {}".format(
                self.repo.head.shorthand,
                str(remote_branch_size) + " branches",
                str(commit_size) + " commits",
                format_bytes(get_dir_size(repo_path))
            ))
            self.head_info.setStyleSheet("QLabel {color: #C46C1D;}")
            self.head_info.setFont(QFont('Arial', self.repo_top_font_size))
            self.repo_summary_layout.addWidget(self.head_info)

            self.repo_summary_layout.addStretch(1)

            # Add lastest commit info.
            self.lastest_commit_info = QLabel("Head: {}".format(lastest_commit.message))
            self.lastest_commit_info.setStyleSheet("QLabel {color: #6C6C6C;}")
            self.lastest_commit_info.setFont(QFont('Arial', self.lastest_commit_font_size))
            self.repo_top_layout.addSpacing(40)
            self.repo_top_layout.addWidget(self.lastest_commit_info)

            # Add info box.
            info_box = QWidget()
            info_area_layout = QHBoxLayout()
            info_area_layout.setSpacing(30)
            info_area_layout.setContentsMargins(30, 0, 30, 30)
            info_box.setLayout(info_area_layout)

            # Add category panel.
            self.category_panel_listview = QListView()
            self.category_panel_listview.setSpacing(10)
            self.category_panel_listview.setStyleSheet("QListView {font-size: 40px; padding: 10px;}")
            category_panel_model = QStringListModel()
            category_panel_list = ["1: Status", "2: Commit", "3: Branch", "4: Submodule"]
            category_panel_model.setStringList(category_panel_list)
            self.category_panel_listview.setModel(category_panel_model)

            info_area_layout.addWidget(self.category_panel_listview)
            info_area_layout.setStretchFactor(self.category_panel_listview, 1)

            # Add content widget.
            self.info_stacked_widget = QStackedWidget()

            self.git_status_widget = GitStatusWidget(self.repo)
            self.git_commit_widget = GitCommitWidget(self.repo)
            self.git_branch_widget = QPushButton("Git branch")
            self.git_submodule_widget = QPushButton("Git submodule")

            self.info_stacked_widget.addWidget(self.git_status_widget)
            self.info_stacked_widget.addWidget(self.git_commit_widget)
            self.info_stacked_widget.addWidget(self.git_branch_widget)
            self.info_stacked_widget.addWidget(self.git_submodule_widget)

            self.info_stacked_widget.setCurrentIndex(0)

            info_area_layout.addWidget(self.info_stacked_widget)
            info_area_layout.setStretchFactor(self.info_stacked_widget, 4)

            # Add help panel.
            help_panel_listview = QListView()
            help_panel_listview.setSpacing(10)
            help_panel_listview.setStyleSheet("QListView {font-size: 40px; padding: 10px;}")
            help_panel_model = QStringListModel()
            help_panel_list = ["Press l: Git pull",
                               "Press u: Git push",
                               "Press a: Commit all",
                               "Press s: Commit file",
                               "Press k: Cancel file"]
            help_panel_model.setStringList(help_panel_list)
            help_panel_listview.setModel(help_panel_model)

            info_area_layout.addWidget(help_panel_listview)
            info_area_layout.setStretchFactor(help_panel_listview, 1)

            main_layout.addWidget(info_box)

    @interactive()
    def show_status_info(self):
        self.info_stacked_widget.setCurrentIndex(0)

    @interactive()
    def show_commit_info(self):
        self.info_stacked_widget.setCurrentIndex(1)

    @interactive()
    def show_branch_info(self):
        self.info_stacked_widget.setCurrentIndex(2)

    @interactive()
    def show_submodule_info(self):
        self.info_stacked_widget.setCurrentIndex(3)

class GitStatusWidget(QFrame):

    def __init__(self, repo):
        super(GitStatusWidget, self).__init__()
        self.setStyleSheet("font-size: 40px; padding: 10px; border: 1px solid rgb(30, 30, 30);")

        layout = QVBoxLayout()
        layout.setSpacing(0)
        layout.setContentsMargins(0, 0, 0, 0)

        status_data = []
        for filepath, flag in repo.status().items():
            if flag == GIT_STATUS_WT_MODIFIED:
                status_data.append(["Modified", filepath])
            elif flag == GIT_STATUS_WT_DELETED:
                status_data.append(["Deleted", filepath])
            elif flag == GIT_STATUS_WT_NEW:
                status_data.append(["New", filepath])
            elif flag == GIT_STATUS_WT_RENAMED:
                status_data.append(["Renamed", filepath])

        if len(status_data) > 0:
            status_model = TableModel(status_data)

            self.unstage_label = QLabel("Unstaged changes ({})".format(len(status_data)))
            self.unstage_label.setStyleSheet("border: 0px;")

            self.unstaged_view = QtWidgets.QTableView()
            self.unstaged_view.setModel(status_model)
            self.unstaged_view.setStyleSheet(
                """
                QTableView
                {
                border: 0px;
                }

                QTableView::item
                {
                border: 0px;
                padding-left: 60px;
                }
                """)
            self.unstaged_view.verticalHeader().setVisible(False)
            self.unstaged_view.horizontalHeader().setVisible(False)
            self.unstaged_view.horizontalHeader().setSectionResizeMode(0, QHeaderView.ResizeToContents)
            self.unstaged_view.horizontalHeader().setSectionResizeMode(1, QHeaderView.Stretch)
            self.unstaged_view.setShowGrid(False)

            layout.addWidget(self.unstage_label)
            layout.addWidget(self.unstaged_view)
        else:
            nothing_change_label = QLabel("Nothing change in local repo.")
            nothing_change_label.setStyleSheet("border: 0px;")

            layout.addWidget(nothing_change_label)
            layout.addStretch(1)

        self.setLayout(layout)

class GitCommitWidget(QFrame):

    def __init__(self, repo):
        super(GitCommitWidget, self).__init__()
        self.setStyleSheet("font-size: 40px; padding: 10px; border: 1px solid rgb(30, 30, 30);")

        self.repo = repo

        self.layout = QVBoxLayout()
        self.layout.setSpacing(0)
        self.layout.setContentsMargins(0, 0, 0, 0)

        self.setLayout(self.layout)

        self.parse_thread = ParseCommitsThread(self.repo)
        self.parse_thread.parse_finish.connect(self.handle_commits)

        # Wait 1 second to parse commit, acceleration interface display.
        QTimer().singleShot(1000, self.start_parse)

    def start_parse(self):
        self.parse_thread.start()

    def handle_commits(self, commit_data):
        if len(commit_data) > 0:
            commit_model = CommitTableModel(commit_data)

            self.commit_view = QtWidgets.QTableView()
            self.commit_view.setModel(commit_model)
            self.commit_view.setStyleSheet(
                """
                QTableView
                {
                border: 0px;
                }

                QTableView::item
                {
                border: 0px;
                padding-left: 10px;
                }
                """)
            self.commit_view.setVerticalScrollBarPolicy(QtCore.Qt.ScrollBarAlwaysOff)
            self.commit_view.verticalHeader().setVisible(False)
            self.commit_view.horizontalHeader().setVisible(False)
            self.commit_view.horizontalHeader().setSectionResizeMode(0, QHeaderView.ResizeToContents)
            self.commit_view.horizontalHeader().setSectionResizeMode(1, QHeaderView.Stretch)
            self.commit_view.horizontalHeader().setSectionResizeMode(2, QHeaderView.ResizeToContents)
            self.commit_view.horizontalHeader().setSectionResizeMode(3, QHeaderView.ResizeToContents)
            self.commit_view.setShowGrid(False)

            self.layout.addWidget(self.commit_view)

class CommitTableModel(QtCore.QAbstractTableModel):

    def __init__(self, data):
        super(CommitTableModel, self).__init__()
        self._data = data

    def data(self, index, role):
        if role == Qt.DisplayRole:
            # See below for the nested-list data structure.
            # .row() indexes into the outer list,
            # .column() indexes into the sub-list
            return self._data[index.row()][index.column()]

        if role == Qt.ForegroundRole:
            if index.column() == 0:
                return QtGui.QColor(80, 80, 80)
            elif index.column() == 1:
                return QtGui.QColor(0, 128, 0)
            elif index.column() == 2:
                return QtGui.QColor(218, 121, 35)
            else:
                return QtGui.QColor(81, 175, 239)

    def rowCount(self, index):
        # The length of the outer list.
        return len(self._data)

    def columnCount(self, index):
        # The following takes the first sub-list, and returns
        # the length (only works if all rows are an equal length)
        return len(self._data[0])

class ParseCommitsThread(QtCore.QThread):
    parse_finish = QtCore.pyqtSignal(list)

    def __init__(self, repo):
        super().__init__()

        self.repo = repo

    def run(self):
        commit_data = []
        for commit in self.repo.walk(self.repo.head.target, GIT_SORT_TOPOLOGICAL):
            commit_data.append([commit.hex[:7],
                                commit.message,
                                commit.author.name,
                                datetime.utcfromtimestamp(commit.author.time).strftime('%Y-%m-%d %H:%M:%S')])

        self.parse_finish.emit(commit_data)

class TableModel(QtCore.QAbstractTableModel):

    def __init__(self, data):
        super(TableModel, self).__init__()
        self._data = data

    def data(self, index, role):
        if role == Qt.DisplayRole:
            # See below for the nested-list data structure.
            # .row() indexes into the outer list,
            # .column() indexes into the sub-list
            return self._data[index.row()][index.column()]

    def rowCount(self, index):
        # The length of the outer list.
        return len(self._data)

    def columnCount(self, index):
        # The following takes the first sub-list, and returns
        # the length (only works if all rows are an equal length)
        return len(self._data[0])

def get_dir_size(start_path = '.'):
    total_size = 0
    for dirpath, dirnames, filenames in os.walk(start_path):
        for f in filenames:
            fp = os.path.join(dirpath, f)
            # skip if it is symbolic link
            if not os.path.islink(fp):
                total_size += os.path.getsize(fp)

    return total_size

def format_bytes(size):
    # 2**10 = 1024
    power = 2**10
    n = 0
    power_labels = {0 : '', 1: 'K', 2: 'M', 3: 'G', 4: 'T'}
    while size > power:
        size /= power
        n += 1
    return str(round(size, 2)) + power_labels[n]+'B'
