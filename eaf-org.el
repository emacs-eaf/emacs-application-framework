;; eaf-org.el --- Emacs application framework  -*- lexical-binding: t; -*-

;; Filename: eaf-org.el
;; Description: Emacs application framework
;; Author:  stardiviner <numbchild@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2020-05-17 12:31:12
;; Version: 0.5
;; Last-Updated: Wed May 20 11:48:43 2020 (-0400)
;;           By: Mingde (Matthew) Zeng
;; URL: http://www.emacswiki.org/emacs/download/eaf.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
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

(defcustom eaf-org-override-pdf-links nil
  "This options will override existing PDF file links's open function.
 Check out variable `eaf-org-override-pdf-links-list' about link types."
  :type 'boolean
  :safe #'booleanp
  :group 'org-link)

(defvar eaf-org-override-pdf-links-list
  '("docview" "pdfview" "pdftools")
  "A list of all PDF file link types which will be override by EAF open function.")

(defun eaf-org-store-link ()
  "Store the page of PDF as link support for `org-store-link'.

The raw link looks like this: [[eaf:<app>::<path>::<extra-args>]]"
  (interactive)
  (when (eq major-mode 'eaf-mode)
    (let* ((app eaf--buffer-app-name)
           ;; filter temp files which is converted to PDF
           (url (if (string-prefix-p "/tmp/" eaf--buffer-url)
                    (warn "[EAF] don't support this application link which is converted to temporary PDF file.")
                  eaf--buffer-url))
           (extra-args (cl-case (intern app)
                         ('pdf-viewer
                          (eaf-call "call_function" eaf--buffer-id "current_page"))
                         ('js-video-player
                          (eaf-call "call_function" eaf--buffer-id "save_session_data"))))
           (link (if extra-args
                     (concat "eaf:" app "::" url "::" extra-args)
                   (concat "eaf:" app "::" url)))
           (description (buffer-name)))
      (if (and (string-equal app "pdf-viewer")
               eaf-org-override-pdf-links
               (or (equal (org-link-get-parameter "docview" :follow) 'eaf-org-open)
                   (equal (org-link-get-parameter "pdfview" :follow) 'eaf-org-open)
                   (equal (org-link-get-parameter "pdftools" :follow) 'eaf-org-open)))
          (org-link-store-props
           :type "eaf"
           :link link
           :description description)
        (require 'ol-docview) ; use `docview' for most wide compatible support.
        (org-link-store-props
         :type "docview"
         :link url
         :description description)))))

(defun eaf-org-open (link _)
  "Open EAF link with EAF correspoinding application."
  (if (member (car (split-string link "::")) (mapcar 'car eaf-app-extensions-alist))
      ;; for eaf-org link type spec: "eaf:<app>:URL:(parameters)"
      (let* ((list (split-string link "::"))
             (app (car list))
             (url (cadr list))
             (extra-args (caddr list)))
        (cl-case (intern app)
          ('browser
           (eaf-open url "browser"))
          ('pdf-viewer
           (eaf-open url "pdf-viewer")
           (eaf-call "call_function_with_args" eaf--buffer-id
                     "jump_to_page_with_num" (format "%s" extra-args)))
          ('mindmap
           (eaf-open url "mindmap"))
          ('js-video-player
           (eaf-open url "js-video-player")
           (eaf-call "call_function_with_args" eaf--buffer-id
                     "restore_session_data" (format "%s" extra-args)))
          (t (eaf-open url))))
    ;; for other link types spec: "<link-type>:URL:(parameters)"
    ;; NOTE: currently only support override PDF link types.
    (let* ((list (split-string link "::"))
           (url (car list))
           (extra-args (cadr list)))
      (if eaf-org-override-pdf-links
          (cl-case (intern (file-name-extension url))
            ('pdf
             (eaf-open (expand-file-name url) "pdf-viewer")
             (eaf-call "call_function_with_args" eaf--buffer-id
                       "jump_to_page_with_num" (format "%s" extra-args))))
        ;; restore to original :follow function
        (org-link-set-parameters
         type :follow (org-link-get-parameter type :orig-follow))
        ;; re-open link with original :follow function
        (apply (org-link-get-parameter type :follow) link)))))

(org-link-set-parameters "eaf"
			                   :follow #'eaf-org-open
			                   :store #'eaf-org-store-link)

(if eaf-org-override-pdf-links
    (dolist (type eaf-org-override-pdf-links-list)
      (when (org-link-get-parameter type :follow) ; if `nil' means `ol-<link>' not loaded.
        (org-link-set-parameters         ; store original `:follow' function
         type :orig-follow (org-link-get-parameter type :follow))
        (org-link-set-parameters type :follow #'eaf-org-open))))

(provide 'eaf-org)
