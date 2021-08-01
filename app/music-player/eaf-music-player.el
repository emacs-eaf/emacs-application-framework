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
    ("j" . "play_next")
    ("k" . "play_prev")
    ("h" . "play_random")
    ("," . "backward")
    ("." . "forward")
    ("SPC" . "toggle")
    ("C-n" . "scroll_up")
    ("C-p" . "scroll_down")
    ("C-v" . "scroll_up_page")
    ("M-v" . "scroll_down_page")
    ("M-<" . "scroll_to_begin")
    ("M->" . "scroll_to_bottom")
    ("g" . "jump_to_file")
    ("t" . "toggle_play_order")
    )
  "The keybinding of EAF Music Player."
  :type 'cons)

;;;###autoload
(defun eaf-open-music-player (music-file)
  "Open EAF music player."
  (interactive "fOpen music: ")
  (eaf-open "eaf-music-player" "music-player" music-file))

(provide 'eaf-music-player)
;;; eaf-music-player.el ends here
