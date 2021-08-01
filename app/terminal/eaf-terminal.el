;;; eaf-terminal.el --- Terminal plugins

;; Filename: eaf-terminal.el
;; Description: Terminal plugins
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-07-20 22:46:47
;; Version: 0.1
;; Last-Updated: Sat Jul 31 11:29:58 2021 (-0400)
;;           By: Mingde (Matthew) Zeng
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
  :type 'string)

(defcustom eaf-terminal-font-size 13
  ""
  :type 'integer)

(defcustom eaf-terminal-font-family ""
  ""
  :type 'string)

(defcustom eaf-terminal-keybinding
  '(("M-n" . "scroll_up")
    ("M-p" . "scroll_down")
    ("C-v" . "scroll_up_page")
    ("M-v" . "scroll_down_page")
    ("M-<" . "scroll_to_begin")
    ("M->" . "scroll_to_bottom")
    ("C--" . "zoom_out")
    ("C-=" . "zoom_in")
    ("C-0" . "zoom_reset")
    ("C-S-c" . "copy_text")
    ("C-S-v" . "yank_text")
    ("C-s" . "search_text_forward")
    ("M-s" . "search_text_backward")
    ("C-a" . "eaf-send-key-sequence")
    ("C-e" . "eaf-send-key-sequence")
    ("C-f" . "eaf-send-key-sequence")
    ("C-b" . "eaf-send-key-sequence")
    ("C-d" . "eaf-send-key-sequence")
    ("C-n" . "eaf-send-key-sequence")
    ("C-p" . "eaf-send-key-sequence")
    ("C-r" . "eaf-send-key-sequence")
    ("C-y" . "eaf-send-key-sequence")
    ("C-k" . "eaf-send-key-sequence")
    ("C-o" . "eaf-send-key-sequence")
    ("C-u" . "eaf-send-key-sequence")
    ("C-l" . "eaf-send-key-sequence")
    ("C-w" . "eaf-send-key-sequence")
    ("M-f" . "eaf-send-key-sequence")
    ("M-b" . "eaf-send-key-sequence")
    ("M-d" . "eaf-send-key-sequence")
    ("C-c C-c" . "eaf-send-second-key-sequence")
    ("C-c C-x" . "eaf-send-second-key-sequence")
    ("<f12>" . "open_devtools")
    ("M-w" . "copy_text")
    ("C-y" . "yank_text")
    ("C-S-a" . "select_all")
    ("C-S-l" . "clear_selection")
    ("C-M-l" . "clear")
    ("M-DEL" . "eaf-send-alt-backspace-sequence")
    ("M-<backspace>" . "eaf-send-alt-backspace-sequence"))
  "The keybinding of EAF Terminal."
  :type 'cons)

(add-to-list 'eaf-app-binding-alist '("terminal" . eaf-terminal-keybinding))

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

;;;###autoload
(defun eaf-open-terminal ()
  "Open EAF Terminal, a powerful GUI terminal emulator in Emacs.

The initial directory is `default-directory'.  However, it opens `$HOME'
 when `default-directory' is part of a remote process.

If a buffer of EAF Terminal in `default-directory' exists, switch to the buffer.
To override and open a new terminal regardless, call interactively with prefix arg."
  (interactive)
  (eaf-terminal-run-command-in-dir (eaf--generate-terminal-command) (eaf--non-remote-default-directory) t))

(provide 'eaf-terminal)

;;; eaf-terminal.el ends here
