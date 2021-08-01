;;; eaf-image-viewer.el --- Image viewer

;; Filename: eaf-image-viewer.el
;; Description: Image viewer
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-07-31 20:43:15
;; Version: 0.1
;; Last-Updated: 2021-07-31 20:43:15
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/eaf-image-viewer.el
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
;; Image viewer
;;

;;; Installation:
;;
;; Put eaf-image-viewer.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'eaf-image-viewer)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET eaf-image-viewer RET
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

(defcustom eaf-image-viewer-keybinding
  '(("n" . "load_next_image")
    ("p" . "load_prev_image")
    ("SPC" . "load_prev_image")
    ("," . "zoom_out")
    ("." . "zoom_in")
    ("/" . "zoom_reset")
    ("-" . "zoom_out")
    ("=" . "zoom_in")
    ("0" . "zoom_reset")
    ("9" . "zoom_toggle")
    ("x" . "close_buffer")
    ("u" . "rotate_left")
    ("i" . "rotate_right")
    ("y" . "flip_horizontal")
    ("o" . "flip_vertical")
    ("j" . "move_down")
    ("k" . "move_up")
    ("h" . "move_left")
    ("l" . "move_right")
    ("d" . "delete_current_image")
    ("<f12>" . "open_devtools")
    )
  "The keybinding of EAF Image Viewer."
  :type 'cons)

(add-to-list 'eaf-app-binding-alist '("image-viewer" . eaf-image-viewer-keybinding))

(provide 'eaf-image-viewer)

;;; eaf-image-viewer.el ends here
