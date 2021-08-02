;;; eaf-jupyter.el --- Jupyter

;; Filename: eaf-jupyter.el
;; Description: Jupyter
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-07-31 17:21:08
;; Version: 0.1
;; Last-Updated: 2021-07-31 17:21:08
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/eaf-jupyter.el
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
;; Jupyter
;;

;;; Installation:
;;
;; Put eaf-jupyter.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'eaf-jupyter)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET eaf-jupyter RET
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

(defcustom eaf-jupyter-font-size 13
  ""
  :type 'integer)

(defcustom eaf-jupyter-font-family ""
  ""
  :type 'string)

(defcustom eaf-jupyter-dark-mode "follow"
  ""
  :type 'string)

(defcustom eaf-jupyter-keybinding
  '(("C-+" . "zoom_in")
    ("C--" . "zoom_out")
    ("C-0" . "zoom_reset")
    ("C-l" . "eaf-send-key-sequence")
    ("C-a" . "eaf-send-key-sequence")
    ("C-e" . "eaf-send-key-sequence")
    ("C-u" . "eaf-send-key-sequence")
    ("C-k" . "eaf-send-key-sequence")
    ("C-y" . "eaf-send-key-sequence")
    ("C-p" . "eaf-send-key-sequence")
    ("C-n" . "eaf-send-key-sequence")
    ("C-f" . "eaf-send-key-sequence")
    ("C-b" . "eaf-send-key-sequence")
    ("C-d" . "eaf-send-key-sequence")
    ("M-b" . "eaf-send-key-sequence")
    ("M-f" . "eaf-send-key-sequence")
    ("M-d" . "eaf-send-key-sequence")
    ("M-<" . "eaf-send-key-sequence")
    ("M->" . "eaf-send-key-sequence")
    ("<C-return>" . "eaf-send-ctrl-return-sequence")
    ("<S-return>" . "eaf-send-shift-return-sequence")
    )
  "The keybinding of EAF Jupyter."
  :type 'cons)

(add-to-list 'eaf-app-binding-alist '("jupyter" . eaf-jupyter-keybinding))

(setq eaf-jupyter-module-path (concat (file-name-directory load-file-name) "buffer.py"))
(add-to-list 'eaf-app-module-path-alist '("jupyter" . eaf-jupyter-module-path))

;;;###autoload
(defun eaf-open-jupyter ()
  "Open jupyter."
  (interactive)
  (if (executable-find (if (eaf--called-from-wsl-on-windows-p)
                           "jupyter-qtconsole.exe"
                         "jupyter-qtconsole"))
      (let* ((data (json-read-from-string (shell-command-to-string (if (eaf--called-from-wsl-on-windows-p)
                                                                       "jupyter.exe kernelspec list --json"
                                                                     "jupyter kernelspec list --json"))))
             (kernel (completing-read "Jupyter Kernels: " (mapcar #'car (alist-get 'kernelspecs data))))
             (args (make-hash-table :test 'equal)))
        (puthash "kernel" kernel args)
        (eaf-open (format "eaf-jupyter-%s" kernel) "jupyter" (json-encode-hash-table args) t))
    (message "[EAF/jupyter] Please install qtconsole first.")))

(provide 'eaf-jupyter)

;;; eaf-jupyter.el ends here
