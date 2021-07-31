;;; eaf-js-video-player.el --- Javascript video player

;; Filename: eaf-js-video-player.el
;; Description: Javascript video player
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-07-31 20:42:37
;; Version: 0.1
;; Last-Updated: 2021-07-31 20:42:37
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/eaf-js-video-player.el
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
;; Javascript video player
;;

;;; Installation:
;;
;; Put eaf-js-video-player.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'eaf-js-video-player)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET eaf-js-video-player RET
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

(defcustom eaf-js-video-player-keybinding
  '(("SPC" . "toggle_play")
    ("M-g" . "exit_fullscreen")
    ("<f12>" . "open_devtools")
    ("h" . "backward")
    ("f" . "toggle_fullscreen")
    ("l" . "forward")
    ("r" . "restart")
    ("j" . "decrease_volume")
    ("k" . "increase_volume")
    ("x" . "close_buffer")
    ("c--" . "zoom_out")
    ("C-=" . "zoom_in")
    ("C-0" . "zoom_reset")
    )
  "The keybinding of EAF JS Video Player."
  :type 'cons)

(provide 'eaf-js-video-player)

;;; eaf-js-video-player.el ends here
