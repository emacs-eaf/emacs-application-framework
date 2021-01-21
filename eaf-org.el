;;; eaf-org.el --- Emacs application framework  -*- lexical-binding: t; -*-

;; Filename: eaf-org.el
;; Description: Emacs application framework
;; Author:  stardiviner <numbchild@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2020-05-17 12:31:12
;; Version: 0.5
;; Last-Updated: Wed Jan 20 05:34:13 2021 (-0500)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/manateelazycat/emacs-application-framework
;; Keywords:
;; Compatibility: emacs-version >= 27
;;
;; Features that might be required by this library:
;;
;; Please check README
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

;;; Code:

(if (version< emacs-version "27")
    (require 'org-docview)
  (require 'ol))

(defcustom eaf-org-override-pdf-links nil
  "When enabled, this will override existing PDF file links's open function.

So that every existing PDF org-link that's supposed to be opened
 by something in `eaf-org-override-pdf-links-list' will be opened using EAF.

Enable this when the you want to ensure the PDF link in the org file can be
 opened without EAF enabled."
  :type 'boolean
  :safe #'booleanp
  :group 'org-link)

(defun eaf-org-export-to-pdf-and-open ()
  "Run `org-latex-export-to-pdf', delete the tex file and `eaf-open' pdf in a new buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (when (derived-mode-p 'org-mode)
      (save-buffer)
      (let* ((pdf-name (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))
             (pdf-name-with-ext (concat pdf-name ".pdf"))
             (eaf-pdf-buffer (get-buffer pdf-name-with-ext))
             (pdf-full-path (concat (file-name-directory (buffer-file-name)) pdf-name-with-ext)))
        (let ((exported (org-latex-export-to-pdf)))
          (message (concat "Trying to open " pdf-name-with-ext))
          (delete-file (concat pdf-name ".tex"))
          (delete-other-windows)
          (split-window-right)
          (other-window 1)
          (if (and eaf-pdf-buffer
                   (with-current-buffer eaf-pdf-buffer
                     (derived-mode-p 'eaf-mode)))
              (switch-to-buffer eaf-pdf-buffer)
            (eaf-open pdf-full-path)))))))

(defvar eaf-org-override-pdf-links-list
  '("docview" "pdfview" "pdftools")
  "A list of all PDF file link types which will be override by EAF open function.")

(dolist (type eaf-org-override-pdf-links-list)
  (when (and eaf-org-override-pdf-links
             (org-link-get-parameter type :follow)) ; if `nil' means `ol-<link>' not loaded.
      (org-link-set-parameters         ; store original `:follow' function
       type :orig-follow (org-link-get-parameter type :follow))
      (org-link-set-parameters type :follow #'eaf-org-open)))

(defun eaf-org-store-link ()
  "Store the page of PDF as link support for `org-store-link'.
The raw link looks like this: [[eaf:<app>::<path>::<extra-args>][description]]"
  (interactive)
  (when (eq major-mode 'eaf-mode)
    (let* ((app eaf--buffer-app-name)
           ;; filter temp files which is converted to PDF
           (url (if (string-prefix-p "/tmp/" eaf--buffer-url)
                    (warn "[EAF] doesn't support this application link which is converted to temporary PDF file.")
                  eaf--buffer-url))
           (extra-args (cl-case (intern app)
                         ('pdf-viewer
                          (eaf-epc-call-sync "call_function" eaf--buffer-id "current_page"))
                         ('js-video-player
                          (eaf-epc-call-sync "call_function" eaf--buffer-id "save_session_data"))))
           (link (if extra-args
                     (concat "eaf:" app "::" url "::" extra-args)
                   (concat "eaf:" app "::" url)))
           (description (buffer-name)))
      (cl-case app
        ('pdf-viewer
         (if (and eaf-org-override-pdf-links
                  (or (equal (org-link-get-parameter "docview" :follow) 'eaf-org-open)
                      (equal (org-link-get-parameter "pdfview" :follow) 'eaf-org-open)
                      (equal (org-link-get-parameter "pdftools" :follow) 'eaf-org-open)))
             (progn (require 'ol-docview) ; use `docview' for most wide compatible support.
                    (org-link-store-props
                     :type "docview"
                     :link (concat "docview:" url)
                     :description description))
           (org-link-store-props
            :type "eaf"
            :link link
            :description description)))
        (t (org-link-store-props
            :type "eaf"
            :link link
            :description description))))))

(defun eaf-org-open (link &optional _)
  "Open LINK using EAF on an EAF supported file."
  (if (member (car (split-string link "::")) (mapcar 'car eaf-app-extensions-alist))
      ;; for eaf-org link type spec: "eaf:<app>::<path>::<extra-args>"
      (let* ((list (split-string link "::"))
             (app (car list))
             (url (cadr list))
             (extra-args (caddr list)))
        (cl-case (intern app)
          ('browser
           (eaf-open url "browser"))
          ('pdf-viewer
           (eaf-open url "pdf-viewer")
           (eaf-epc-call-sync "call_function_with_args" eaf--buffer-id
                     "jump_to_page_with_num" (format "%s" extra-args)))
          ('mindmap
           (eaf-open url "mindmap"))
          ('js-video-player
           (eaf-open url "js-video-player")
           (eaf-epc-call-sync "call_function_with_args" eaf--buffer-id
                     "restore_session_data" (format "%s" extra-args)))
          (t (eaf-open url))))
    ;; for other link types spec: "<link-type>:URL:(parameters)"
    ;; NOTE: currently only support override PDF link types.
    (let* ((list (split-string link "::"))
           (url (car list))
           (extra-args (cadr list)))
      (cl-case (intern (file-name-extension url))
        ('pdf
         (if eaf-org-override-pdf-links
             (progn (eaf-open (expand-file-name url) "pdf-viewer")
                    (when extra-args
                      (eaf-epc-call-sync "call_function_with_args" eaf--buffer-id
                                "jump_to_page_with_num" (format "%s" extra-args))))
           (dolist (type eaf-org-override-pdf-links-list)
             ;; restore to original :follow function, since eaf-org-override-pdf-links is nil
             (org-link-set-parameters
              type :follow (org-link-get-parameter type :orig-follow))
             ;; re-open link with original :follow function
             (apply (org-link-get-parameter type :follow) link))))))))

(org-link-set-parameters "eaf"
			             :follow #'eaf-org-open
			             :store #'eaf-org-store-link)


(provide 'eaf-org)
;;; eaf-org.el ends here
