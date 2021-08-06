;;; eaf-file-manager.el --- File manager

;; Filename: eaf-file-manager.el
;; Description: File manager
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-07-31 20:45:09
;; Version: 0.1
;; Last-Updated: 2021-07-31 20:45:09
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/eaf-file-manager.el
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
;; File manager
;;

;;; Installation:
;;
;; Put eaf-file-manager.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'eaf-file-manager)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET eaf-file-manager RET
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

(defcustom eaf-file-manager-keybinding
  '(("<f12>" . "open_devtools")
    ("h" . "up_directory")
    ("j" . "select_next_file")
    ("k" . "select_prev_file")
    ("l" . "open_file")
    ("f" . "open_file")
    ("'" . "up_directory")
    ("q" . "bury-buffer")
    )
  "The keybinding of EAF File Manager."
  :type 'cons)

(add-to-list 'eaf-app-binding-alist '("file-manager" . eaf-file-manager-keybinding))

(setq eaf-file-manager-module-path (concat (file-name-directory load-file-name) "buffer.py"))
(add-to-list 'eaf-app-module-path-alist '("file-manager" . eaf-file-manager-module-path))

;;;###autoload
(defun eaf-open-file-manager ()
  "Open EAF file manager."
  (interactive)
  (eaf-open "~" "file-manager"))

(defcustom eaf-file-manager-dark-header-color "#EEC900"
  ""
  :type 'string)

(defcustom eaf-file-manager-dark-directory-color "#00B8FF"
  ""
  :type 'string)

(defcustom eaf-file-manager-dark-symlink-color "#46D9FF"
  ""
  :type 'string)

(defcustom eaf-file-manager-light-header-color "#7E4C8D"
  ""
  :type 'string)

(defcustom eaf-file-manager-light-directory-color "#2257A0"
  ""
  :type 'string)

(defcustom eaf-file-manager-light-symlink-color "#46D9FF"
  ""
  :type 'string)

(provide 'eaf-file-manager)

;;; eaf-file-manager.el ends here
