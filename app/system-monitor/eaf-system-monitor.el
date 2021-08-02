;;; eaf-system-monitor.el --- System monitor

;; Filename: eaf-system-monitor.el
;; Description: System monitor
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-07-31 20:45:45
;; Version: 0.1
;; Last-Updated: 2021-07-31 20:45:45
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/eaf-system-monitor.el
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
;; System monitor
;;

;;; Installation:
;;
;; Put eaf-system-monitor.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'eaf-system-monitor)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET eaf-system-monitor RET
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

(defcustom eaf-system-monitor-keybinding
  '(("<f12>" . "open_devtools")
    ("C-n" . "scroll_up")
    ("C-p" . "scroll_down")
    ("C-v" . "scroll_up_page")
    ("M-v" . "scroll_down_page")
    ("M-<" . "scroll_to_begin")
    ("M->" . "scroll_to_bottom")
    )
  "The keybinding of EAF System Monitor."
  :type 'cons)

(add-to-list 'eaf-app-binding-alist '("system-monitor" . eaf-system-monitor-keybinding))

(setq eaf-system-monitor-module-path (concat (file-name-directory load-file-name) "buffer.py"))
(add-to-list 'eaf-app-module-path-alist '("system-monitor" . eaf-system-monitor-module-path))

;;;###autoload
(defun eaf-open-system-monitor ()
  "Open EAF system monitor."
  (interactive)
  (eaf-open "eaf-system-monitor" "system-monitor"))

(provide 'eaf-system-monitor)

;;; eaf-system-monitor.el ends here
