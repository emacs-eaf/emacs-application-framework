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

(defun eaf-org-store-link ()
  "Store the page of PDF as link support for `org-store-link'.

The raw link looks like this: [[eaf:<app>::<path>::<extra-args>]]"
  (interactive)
  (when (eq major-mode 'eaf-mode)
    (let* ((app eaf--buffer-app-name)
           (url eaf--buffer-url)
           (extra-args (cl-case (intern app)
                         ('pdf-viewer
                          (eaf-call "call_function" eaf--buffer-id "current_page"))))
           ;; (eaf-call "call_function_with_args" eaf--buffer-id "store_session_data" (format "%s" page-num))
           (link (concat "eaf:" app "::" url "::" extra-args))
           (description (buffer-name)))
      (org-link-store-props
       :type "eaf"
       :link link
       :description description))))

(defun eaf-org-open (link _)
  "Open EAF link with EAF correspoinding application."
  (let* ((list (split-string link "::"))
         (app (intern (car list)))
         (url (cadr list))
         (extra-args (caddr list)))
    (cl-case app
      ('pdf-viewer
       ;; TODO open the PDF file
       (eaf-open url "pdf-viewer")
       (eaf-call "call_function_with_args" eaf--buffer-id
                 "jump_to_page_with_num" (format "%s" extra-args))))))

(org-link-set-parameters "eaf"
			                   :follow #'eaf-org-open
			                   :store #'eaf-org-store-link)

(provide 'eaf-org)
