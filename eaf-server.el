;;; eaf-server.el --- Emacs application framework  -*- lexical-binding: t; -*-

;; Filename: eaf-server.el
;; Description: Emacs application framework
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Version: 0.5
;; Last-Updated: Tue Jan 19 05:02:06 2021 (-0500)
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
(require 'cl-lib)

(defvar eaf-server-clients '()
  "Alist where KEY is a client process and VALUE is the string.")

(defvar eaf-server-servers '()
  "Alist where KEY is the port number the server is listening at.")

(defvar eaf-server-display-buffer-on-update nil
  "If non-nil, force the process buffer to be visible whenever new text arrives.")
(make-variable-buffer-local 'eaf-server-display-buffer-on-update)

(defun eaf-server-make-process-name (port)
  "Return server name of the process listening on PORT."
  (format "eaf-server:%d" port))

(defun eaf-server-get-process (port)
  "Return the server process that is listening on PORT."
  (get-process (eaf-server-make-process-name port)))

(defun eaf-server-process-buffer (port)
  "Return buffer of the server process that is listening on PORT."
  (process-contact (eaf-server-get-process port) :buffer))

(defun eaf-server-delete-clients (server-proc)
  "Delete EAF clients of SERVER-PROC."
  (let ((server-proc-name (process-contact server-proc :name)))
    (cl-loop for client in eaf-server-clients
             if (string= server-proc-name (process-contact client :name))
             do
             (delete-process client)
             (message "Deleted client process %s" client))
    (setq eaf-server-clients
          (cl-delete-if (lambda (client)
                          (string= (process-contact server-proc :name)
                                   (process-contact client :name)))
                        eaf-server-clients))))

(cl-defun eaf-server-start (port &optional (display-buffer-on-update nil)
                                 (buffer-major-mode 'text-mode))
  "Start a TCP server listening at PORT."
  (interactive
   (list (read-number "Enter the port number to listen to: " 9999)))
  (let* ((proc-name (eaf-server-make-process-name port))
         (buffer-name (format "*%s*" proc-name)))
    (unless (process-status proc-name)
      (make-network-process :name proc-name :buffer buffer-name
                            :family 'ipv4 :service port
                            :sentinel 'eaf-server-sentinel
                            :filter 'eaf-server-filter :server 't)
      (with-current-buffer buffer-name
        (funcall buffer-major-mode)
        (setq eaf-server-display-buffer-on-update display-buffer-on-update))
      (setq eaf-server-clients '()))
    ;; (display-buffer buffer-name)
    ))

(defun eaf-server-stop (port)
  "Stop an Emacs TCP server at PORT."
  (interactive
   (list (read-number "Enter the port number the server is listening to: "
                      9999)))
  (let ((server-proc (eaf-server-get-process port)))
    (if (process-live-p server-proc)
        (progn
          (eaf-server-delete-clients server-proc)
          (delete-process server-proc)
          (kill-buffer (concat "*eaf-server:" (number-to-string port) "*"))
          (message "[EAF] Server terminated."))
      (message "[EAF] Server already terminated."))))

(defun eaf-server-append-to-proc-buffer (proc string)
  (let ((buffer (process-contact proc :buffer))
        (inhibit-read-only t))
    (and buffer (get-buffer buffer)
         (with-current-buffer buffer
           (when eaf-server-display-buffer-on-update
             (display-buffer buffer))
           (let ((moving (= (point) (point-max))))
             (save-excursion
               (goto-char (point-max))
               (insert string)
               )
             (if moving (goto-char (point-max))))))))

(defun eaf-server-filter (proc string)
  (tcp-eval string))

(defun tcp-eval (string)
  (eval (car (read-from-string (format "(progn %s)" string)))))

(defun eaf-server-sentinel (proc msg)
  (cond
   ((string-match "open from .*\n" msg)
    (push proc eaf-server-clients)
    (eaf-server-log proc "client connected\n")
    )
   ((string= msg "connection broken by remote peer\n")
    (setq eaf-server-clients (cl-delete proc eaf-server-clients))
    (eaf-server-log proc "client has quit\n")
    )
   ((eq (process-status proc) 'closed)
    (eaf-server-delete-clients proc))))

(defun eaf-server-log (client string)
  "If a server buffer exists, write STRING to it for logging purposes."
  (eaf-server-append-to-proc-buffer client
                                    (format "%s %s: %s"
                                            (current-time-string)
                                            client string)))


(provide 'eaf-server)
;;; eaf-server.el ends here
