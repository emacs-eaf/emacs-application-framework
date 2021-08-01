;;; eaf-elfeed.el --- Elfeed plugin

;; Filename: eaf-elfeed.el
;; Description: Elfeed plugin
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-07-20 22:23:46
;; Version: 0.1
;; Last-Updated: 2021-07-20 22:23:46
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/eaf-elfeed.el
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
;; Elfeed plugin
;;

;;; Installation:
;;
;; Put eaf-elfeed.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'eaf-elfeed)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET eaf-elfeed RET
;;

;;; Change log:
;;
;; 2021/07/20
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

(defcustom eaf-elfeed-split-direction "below"
  "Elfeed browser page display location.
Default is `below', you can chang it with `right'."
  :type 'string)

(defun eaf-elfeed-open-url ()
  "Display the currently selected item in an eaf buffer."
  (interactive)
  (if (featurep 'elfeed)
      (let ((entry (elfeed-search-selected :ignore-region)))
        (require 'elfeed-show)
        (when (elfeed-entry-p entry)
          ;; Move to next feed item.
          (elfeed-untag entry 'unread)
          (elfeed-search-update-entry entry)
          (unless elfeed-search-remain-on-entry (forward-line))

          ;; Open elfeed item in other window,
          ;; and scroll EAF browser content by command `scroll-other-window'.
          (delete-other-windows)
          (pcase eaf-elfeed-split-direction
            ("below"
             (split-window-no-error nil 30 'up)
             (eaf--select-window-by-direction "down")
             (eaf-open-browser (elfeed-entry-link entry))
             (eaf--select-window-by-direction "up"))
            ("right"
             (split-window-no-error nil 60 'right)
             (eaf--select-window-by-direction "right")
             (eaf-open-browser (elfeed-entry-link entry))
             (eaf--select-window-by-direction "left")))
          ))
    (message "Please install elfeed first.")))

(defun eaf--select-window-by-direction (direction)
  "Select the most on the side according to the direction."
  (ignore-errors
    (dotimes (_ 50)
      (pcase direction
        ("left" (windmove-left))
        ("right" (windmove-right))
        ("up" (windmove-up))
        ("below" (windmove-down))
        ))))

(provide 'eaf-elfeed)

;;; eaf-elfeed.el ends here
