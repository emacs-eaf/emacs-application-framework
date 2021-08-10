;;; eaf-file-browser.el --- File manager

;; Filename: eaf-file-browser.el
;; Description: File manager
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-07-31 20:45:09
;; Version: 0.1
;; Last-Updated: 2021-07-31 20:45:09
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/eaf-file-browser.el
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
;; Put eaf-file-browser.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'eaf-file-browser)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET eaf-file-browser RET
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

(defun eaf-file-browser-qrcode (dir)
  "Open EAF File Browser application.

Select directory DIR to share file from the smartphone.

Make sure that your smartphone is connected to the same WiFi network as this computer."
  (interactive "D[EAF/file-browser] Specify Destination: ")
  (eaf-open dir "file-browser"))

(setq eaf-file-browser-module-path (concat (file-name-directory load-file-name) "buffer.py"))
(add-to-list 'eaf-app-module-path-alist '("file-browser" . eaf-file-browser-module-path))

(provide 'eaf-file-browser)

;;; eaf-file-browser.el ends here
