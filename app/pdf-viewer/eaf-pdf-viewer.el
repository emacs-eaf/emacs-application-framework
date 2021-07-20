;;; eaf-pdf-viewer.el --- PDF Viewer

;; Filename: eaf-pdf-viewer.el
;; Description: PDF Viewer
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-07-20 22:16:40
;; Version: 0.1
;; Last-Updated: 2021-07-20 22:16:40
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/eaf-pdf-viewer.el
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
;; PDF Viewer
;;

;;; Installation:
;;
;; Put eaf-pdf-viewer.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'eaf-pdf-viewer)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET eaf-pdf-viewer RET
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

(defvar eaf-pdf-outline-buffer-name "*eaf pdf outline*"
  "The name of pdf-outline-buffer.")

(defvar eaf-pdf-outline-window-configuration nil
  "Save window configure before popup outline buffer.")

(defcustom eaf-pdf-extension-list
  '("pdf" "xps" "oxps" "cbz" "epub" "fb2" "fbz")
  "The extension list of pdf application."
  :type 'cons)

(defcustom eaf-pdf-store-history t
  "If it is t, the pdf file path will be stored in eaf-config-location/pdf/history/log.txt for eaf-open-pdf-from-history to use"
  :type 'boolean)

(define-minor-mode eaf-pdf-outline-mode
  "EAF pdf outline mode."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "RET") 'eaf-pdf-outline-jump)
            (define-key map (kbd "q") 'quit-window)
            map))

(defun eaf-pdf-outline ()
  "Create PDF outline."
  (interactive)
  (let ((buffer-name (buffer-name (current-buffer)))
        (toc (eaf-call-sync "call_function" eaf--buffer-id "get_toc"))
        (page-number (string-to-number (eaf-call-sync "call_function" eaf--buffer-id "current_page"))))
    ;; Save window configuration before outline.
    (setq eaf-pdf-outline-window-configuration (current-window-configuration))

    ;; Insert outline content.
    (with-current-buffer (get-buffer-create  eaf-pdf-outline-buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert toc)
      (setq toc (mapcar (lambda (line)
                          (string-to-number (car (last (split-string line " ")))))
                        (butlast (split-string (buffer-string) "\n"))))
      (goto-line (seq-count (apply-partially #'>= page-number) toc))
      (set (make-local-variable 'eaf-pdf-outline-original-buffer-name) buffer-name)
      (let ((view-read-only nil))
        (read-only-mode 1))
      (eaf-pdf-outline-mode 1))

    ;; Popup ouline buffer.
    (pop-to-buffer eaf-pdf-outline-buffer-name)))

(defun eaf-pdf-outline-jump ()
  "Jump into specific page."
  (interactive)
  (let* ((line (thing-at-point 'line))
         (page-num (replace-regexp-in-string "\n" "" (car (last (s-split " " line))))))
    ;; Jump to page.
    (switch-to-buffer-other-window eaf-pdf-outline-original-buffer-name)
    (eaf-call-sync "call_function_with_args" eaf--buffer-id "jump_to_page_with_num" (format "%s" page-num))

    ;; Restore window configuration before outline operation.
    (when eaf-pdf-outline-window-configuration
      (set-window-configuration eaf-pdf-outline-window-configuration)
      (setq eaf-pdf-outline-window-configuration nil))))

(defun eaf-pdf-get-annots (page)
  "Return a map of annotations on PAGE.

The key is the annot id on PAGE."
  (eaf-call-sync "call_function_with_args" eaf--buffer-id "get_annots" (format "%s" page)))

(defun eaf-pdf-jump-to-annot (annot)
  "Jump to specifical pdf annot."
  (let ((rect (gethash "rect" annot))
        (page (gethash "page" annot)))
    (eaf-call-sync "call_function_with_args" eaf--buffer-id "jump_to_rect" (format "%s" page) rect)))

(provide 'eaf-pdf-viewer)

;;; eaf-pdf-viewer.el ends here
