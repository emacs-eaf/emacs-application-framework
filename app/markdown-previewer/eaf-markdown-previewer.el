;;; eaf-markdown-previewer.el --- Markdown previewer

;; Filename: eaf-markdown-previewer.el
;; Description: Markdown previewer
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-07-31 17:15:43
;; Version: 0.1
;; Last-Updated: 2021-07-31 17:15:43
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/eaf-markdown-previewer.el
;; Keywords:
;; Compatibility: GNU Emacs 28.0.50
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Markdown previewer
;;

;;; Installation:
;;
;; Put eaf-markdown-previewer.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'eaf-markdown-previewer)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET eaf-markdown-previewer RET
;;

;;; Change log:
;;
;; 2021/07/31
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

(defcustom eaf-markdown-dark-mode "follow"
  ""
  :type 'string)

(defcustom eaf-markdown-previewer-keybinding
  '(("C--" . "zoom_out")
    ("C-=" . "zoom_in")
    ("C-0" . "zoom_reset")
    ("C-s" . "search_text_forward")
    ("C-r" . "search_text_backward")
    ("C-n" . "scroll_up")
    ("C-p" . "scroll_down")
    ("C-f" . "scroll_right")
    ("C-b" . "scroll_left")
    ("C-v" . "scroll_up_page")
    ("C-y" . "yank_text")
    ("C-w" . "kill_text")
    ("M-e" . "atomic_edit")
    ("M-c" . "caret_toggle_browsing")
    ("M-D" . "select_text")
    ("M-s" . "open_link")
    ("M-S" . "open_link_new_buffer")
    ("M-B" . "open_link_background_buffer")
    ("C-/" . "undo_action")
    ("M-_" . "redo_action")
    ("M-w" . "copy_text")
    ("M-f" . "history_forward")
    ("M-b" . "history_backward")
    ("M-q" . "clear_cookies")
    ("C-t" . "toggle_password_autofill")
    ("C-d" . "save_page_password")
    ("M-a" . "toggle_adblocker")
    ("C-M-q" . "clear_history")
    ("C-M-i" . "import_chrome_history")
    ("M-v" . "scroll_down_page")
    ("M-<" . "scroll_to_begin")
    ("M->" . "scroll_to_bottom")
    ("M-p" . "duplicate_page")
    ("M-t" . "new_blank_page")
    ("M-d" . "toggle_dark_mode")
    ("SPC" . "insert_or_scroll_up_page")
    ("J" . "insert_or_select_left_tab")
    ("K" . "insert_or_select_right_tab")
    ("j" . "insert_or_scroll_up")
    ("k" . "insert_or_scroll_down")
    ("h" . "insert_or_scroll_left")
    ("l" . "insert_or_scroll_right")
    ("f" . "insert_or_open_link")
    ("F" . "insert_or_open_link_new_buffer")
    ("B" . "insert_or_open_link_background_buffer")
    ("c" . "insert_or_caret_at_line")
    ("u" . "insert_or_scroll_down_page")
    ("d" . "insert_or_scroll_up_page")
    ("H" . "insert_or_history_backward")
    ("L" . "insert_or_history_forward")
    ("t" . "insert_or_new_blank_page")
    ("T" . "insert_or_recover_prev_close_page")
    ("i" . "insert_or_focus_input")
    ("I" . "insert_or_open_downloads_setting")
    ("r" . "insert_or_refresh_page")
    ("g" . "insert_or_scroll_to_begin")
    ("x" . "insert_or_close_buffer")
    ("G" . "insert_or_scroll_to_bottom")
    ("-" . "insert_or_zoom_out")
    ("=" . "insert_or_zoom_in")
    ("0" . "insert_or_zoom_reset")
    ("m" . "insert_or_save_as_bookmark")
    ("o" . "insert_or_open_browser")
    ("y" . "insert_or_download_youtube_video")
    ("Y" . "insert_or_download_youtube_audio")
    ("p" . "insert_or_toggle_device")
    ("P" . "insert_or_duplicate_page")
    ("1" . "insert_or_save_as_pdf")
    ("2" . "insert_or_save_as_single_file")
    ("3" . "insert_or_save_as_screenshot")
    ("v" . "insert_or_view_source")
    ("e" . "insert_or_edit_url")
    ("n" . "insert_or_export_text")
    ("," . "insert_or_switch_to_reader_mode")
    ("." . "insert_or_translate_text")
    (";" . "insert_or_translate_page")
    ("C-M-c" . "copy_code")
    ("C-M-l" . "copy_link")
    ("C-a" . "select_all_or_input_text")
    ("M-u" . "clear_focus")
    ("C-j" . "open_downloads_setting")
    ("M-o" . "eval_js")
    ("M-O" . "eval_js_file")
    ("<escape>" . "eaf-browser-send-esc-or-exit-fullscreen")
    ("M-," . "eaf-send-down-key")
    ("M-." . "eaf-send-up-key")
    ("M-m" . "eaf-send-return-key")
    ("<f5>" . "refresh_page")
    ("<f12>" . "open_devtools")
    ("<C-return>" . "eaf-send-ctrl-return-sequence")
    )
  "The keybinding of EAF Markdown Previewer."
  :type 'cons)

(add-to-list 'eaf-app-binding-alist '("markdown-previewer" . eaf-markdown-previewer-keybinding))

(setq eaf-markdown-previewer-path (concat (file-name-directory load-file-name) "buffer.py"))
(add-to-list 'eaf-app-module-path-alist '("markdown-previewer" . eaf-markdown-previewer-path))

(defun eaf--markdown-preview-display (buf)
  "Given BUF, split window to show file and previewer."
  (eaf-split-preview-windows
   (buffer-local-value
    'eaf--buffer-url buf))
  (switch-to-buffer buf)
  (other-window +1))

(provide 'eaf-markdown-previewer)

;;; eaf-markdown-previewer.el ends here
