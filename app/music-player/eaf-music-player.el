;;; eaf-music-player.el --- Music player

;; Filename: eaf-music-player.el
;; Description: Music player
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-07-31 17:23:55
;; Version: 0.1
;; Last-Updated: 2021-07-31 17:23:55
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/eaf-music-player.el
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
;; Music player
;;

;;; Installation:
;;
;; Put eaf-music-player.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'eaf-music-player)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET eaf-music-player RET
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

(defcustom eaf-music-play-order "list"
  ""
  :type 'string)

(defcustom eaf-music-player-keybinding
  '(("<f12>" . "open_devtools")
    ("j" . "js_play_next")
    ("k" . "js_play_prev")
    ("h" . "js_play_random")
    ("," . "js_backward")
    ("." . "js_forward")
    ("SPC" . "js_toggle")
    ("C-n" . "js_scroll_up")
    ("C-p" . "js_scroll_down")
    ("C-v" . "js_scroll_up_page")
    ("M-v" . "js_scroll_down_page")
    ("M-<" . "js_scroll_to_begin")
    ("M->" . "js_scroll_to_bottom")
    ("g" . "js_jump_to_file")
    ("t" . "js_toggle_play_order")
    )
  "The keybinding of EAF Music Player."
  :type 'cons)

(defcustom eaf-music-extension-list
  '("mp3")
  "The extension list of music application."
  :type 'cons)

;;;###autoload
(defun eaf-open-music-player (music-file)
  "Open EAF music player."
  (interactive "fOpen music: ")
  (eaf-open "eaf-music-player" "music-player" music-file))

(add-to-list 'eaf-app-extensions-alist '("music-player" . eaf-music-extension-list))
(add-to-list 'eaf-app-binding-alist '("music-player" . eaf-music-player-keybinding))

(setq eaf-music-player-module-path (concat (file-name-directory load-file-name) "buffer.py"))
(add-to-list 'eaf-app-module-path-alist '("music-player" . eaf-music-player-module-path))

(provide 'eaf-music-player)
;;; eaf-music-player.el ends here
