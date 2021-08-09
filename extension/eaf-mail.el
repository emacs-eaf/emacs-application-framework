;;; eaf-mail.el --- Mail plugins

;; Filename: eaf-mail.el
;; Description: Mail plugins
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-07-20 22:27:26
;; Version: 0.1
;; Last-Updated: 2021-07-20 22:27:26
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/eaf-mail.el
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
;; Mail plugins
;;

;;; Installation:
;;
;; Put eaf-mail.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'eaf-mail)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET eaf-mail RET
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

(defcustom eaf-mua-get-html
  '(("^gnus-" . eaf-gnus-get-html)
    ("^mu4e-" . eaf-mu4e-get-html)
    ("^notmuch-" . eaf-notmuch-get-html))
  "An alist regex mapping a MUA `major-mode' to a function to retrieve HTML part of a mail."
  :type 'alist)

(defun eaf--gnus-htmlp (part)
  "Determine whether the gnus mail PART is HTML."
  (when-let ((type (mm-handle-type part)))
    (string= "text/html" (car type))))

(defun eaf--notmuch-htmlp (part)
  "Determine whether the notmuch mail PART is HTML."
  (when-let ((type (plist-get part :content-type)))
    (string= "text/html" type)))

(defun eaf--get-html-func ()
  "The function returning a function used to extract HTML of different MUAs."
  (catch 'get-html
    (cl-loop for (regex . func) in eaf-mua-get-html
             do (when (string-match regex (symbol-name major-mode))
                  (throw 'get-html func))
             finally return (error "[EAF] You are either not in a MUA buffer or your MUA is not supported!"))))

(defun eaf-gnus-get-html ()
  "Retrieve HTML part of a gnus mail."
  (with-current-buffer gnus-original-article-buffer
    (when-let* ((dissect (mm-dissect-buffer t t))
                (buffer (if (bufferp (car dissect))
                            (when (eaf--gnus-htmlp dissect)
                              (car dissect))
                          (car (cl-find-if #'eaf--gnus-htmlp (cdr dissect))))))
      (with-current-buffer buffer
        (buffer-string)))))

(defun eaf-mu4e-get-html ()
  "Retrieve HTML part of a mu4e mail."
  (let ((msg mu4e~view-message))
    (mu4e-message-field msg :body-html)))

(defun eaf-notmuch-get-html ()
  "Retrieve HTML part of a notmuch mail."
  (when-let* ((msg (cond ((derived-mode-p 'notmuch-show-mode)
                          (notmuch-show-get-message-properties))
                         ((derived-mode-p 'notmuch-tree-mode)
                          (notmuch-tree-get-message-properties))
                         (t nil)))
              (body (plist-get msg :body))
              (parts (car body))
              (content (plist-get parts :content))
              (part (if (listp content)
                        (cl-find-if #'eaf--notmuch-htmlp content)
                      (when (eaf--notmuch-htmlp parts)
                        parts))))
    (notmuch-get-bodypart-text msg part notmuch-show-process-crypto)))

;;;###autoload
(defun eaf-open-mail-as-html ()
  "Open the html mail in EAF Browser.

The value of `mail-user-agent' must be a KEY of the alist `eaf-mua-get-html'.

In that way the corresponding function will be called to retrieve the HTML
 part of the current mail."
  (interactive)
  (when-let* ((html (funcall (eaf--get-html-func)))
              (default-directory (eaf--non-remote-default-directory))
              (file (concat (temporary-file-directory) (make-temp-name "eaf-mail-") ".html")))
    (with-temp-file file
      (insert html))
    (eaf-open file "browser" "temp_html_file")))

(provide 'eaf-mail)

;;; eaf-mail.el ends here
