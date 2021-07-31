;;; eaf-terminal.el --- Terminal plugins

;; Filename: eaf-terminal.el
;; Description: Terminal plugins
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-07-20 22:46:47
;; Version: 0.1
;; Last-Updated: 2021-07-20 22:46:47
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/eaf-terminal.el
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
;; Terminal plugins
;;

;;; Installation:
;;
;; Put eaf-terminal.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'eaf-terminal)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET eaf-terminal RET
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

(defcustom eaf-terminal-dark-mode "follow"
  ""
  :type 'boolean)

(defcustom eaf-terminal-font-size 13
  ""
  :type 'integer)

(defcustom eaf-terminal-font-family ""
  ""
  :type 'string)

(defun eaf-send-second-key-sequence ()
  "Send second part of key sequence to terminal."
  (interactive)
  (eaf-call-async "send_key_sequence"
                  eaf--buffer-id
                  (nth 1 (split-string (key-description (this-command-keys-vector))))))

(defun eaf-ipython-command ()
  (if (eaf--called-from-wsl-on-windows-p)
      "ipython.exe"
    "ipython"))

(defun eaf-open-ipython ()
  "Open ipython in terminal."
  (interactive)
  (if (executable-find (eaf-ipython-command))
      (eaf-terminal-run-command-in-dir
       (eaf-ipython-command)
       (eaf--non-remote-default-directory))
    (message "[EAF/terminal] Please install ipython first.")))

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

(defun eaf-terminal-run-command-in-dir (command dir &optional always-new)
  "Run COMMAND in terminal in directory DIR.

If ALWAYS-NEW is non-nil, always open a new terminal for the dedicated DIR."
  (let* ((args (make-hash-table :test 'equal))
         (expand-dir (expand-file-name dir)))
    (puthash "command" command args)
    (puthash "directory"
             (if (eaf--called-from-wsl-on-windows-p)
                 (eaf--translate-wsl-url-to-windows expand-dir)
               expand-dir)
             args)
    (eaf-open dir "terminal" (json-encode-hash-table args) always-new)))

(defun eaf--generate-terminal-command ()
  (if (or (eaf--called-from-wsl-on-windows-p)
          (eq system-type 'windows-nt))
      "powershell.exe"
    (getenv "SHELL")))

(provide 'eaf-terminal)

;;; eaf-terminal.el ends here
