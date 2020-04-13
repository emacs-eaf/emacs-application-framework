;;; eaf-websocket.el --- eaf websocket jsonrpc -*- lexical-binding: t no-byte-compile: t; -*-

;; Filename: eaf-websocket.el
;; Description: Emacs application framework websocket jsonrpc
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2020-04-13 14:10:12
;; Version: 0.5
;; Last-Updated: Sat Apr 13 19:32:05 2020 (-0400)
;;           By: xhcoding
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.      See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.


;;; Commentary:

;;; Code:

(require 'json)
(require 'jsonrpc)
(require 'websocket)

(defclass jsonrpc-websocket-connection (jsonrpc-connection)
  ((-url
    :initarg :url
    :initform ""
    :type string
    :accessor jsonrpc-websocket--url
    :documentation "Websocket server url.")
   (-websocket-client
    :accessor jsonrpc-websocket--websocket-client
    :writer jsonrpc-websocket--set-websocket-client
    :initform nil
    :documentation "Websocket client"
    :))
  :documentation "A JSONRPC connection over websocket.
The following initargs are accepted:
:URL (mandatory) , websocket server url.")

(cl-defmethod initialize-instance ((connection jsonrpc-websocket-connection) slots)
  "Construct CONNECTION with SLOTS."
  (cl-call-next-method)
  (let ((url (plist-get slots :url)))
    (jsonrpc-websocket--set-websocket-client
     connection
     (websocket-open
      url
      :on-message (lambda (websocket frame)
                    (let* ((json-object-type 'plist)
                           (json-message (json-read-from-string (websocket-frame-text frame))))
                      (jsonrpc-connection-receive connection json-message)))
      :on-close (lambda (_websocket)
                  ;;TODO: handle close
                  (message "EAF WebSocket closed"))))))

(cl-defmethod jsonrpc-connection-send ((connection jsonrpc-websocket-connection)
                                       &key
                                       id
                                       method
                                       params
                                       result
                                       error
                                       )
  "Send message with websocket CONNECTION."
  (let ((message `(:jsonrpc "2.0" :id ,(if id id json-null))))
    (cond (method (plist-put message :method
                             (cond ((keywordp method) (substring (symbol-name method) 1))
                                   ((and method (symbolp method)) (symbol-name method))
                                   (t method)))
                  (plist-put message :params params))
          (result (plist-put message :result result))
          (error (plist-put message :error error)))
    (websocket-send-text (jsonrpc-websocket--websocket-client connection) (json-encode-plist message))))


(defvar eaf-websocket--jsonrpc-connection nil)

(defun eaf-websocket-start-connection()
  "Start connect websocket server."
  (setq      eaf-websocket--jsonrpc-connection
             (jsonrpc-websocket-connection
              :name "eaf"
              :url "ws://127.0.0.1:12980"
              :notification-dispatcher (lambda (conn method params)
                                         (when (and (symbolp method) (fboundp method))
                                           (apply method (append params nil))))
              :request-dispatcher (lambda (conn method params)

                                    (when (and (symbolp method) (fboundp method))
                                      (apply method (append params nil)))))))

(defun eaf-websocket-stop-connection()
  "Stop webcosket connect."
  (websocket-close eaf-websocket--jsonrpc-connection)
  (setq eaf-websocket--jsonrpc-connection nil))

(defun eaf-websocket-call (method &rest params)
  "Call remote METHOD with PARAMS."
  (jsonrpc-request eaf-websocket--jsonrpc-connection method params))

(defun eaf-websocket-notify (method &rest params)
  "Notify METHOD with PARAMS."
  (jsonrpc-notify eaf-websocket--jsonrpc-connection method params))

(provide 'eaf-websocket)

;;; eaf-websocket.el ends here
