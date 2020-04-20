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

(defclass jsonrpc-websocket-client-connection (jsonrpc-connection)
  ((-url
    :initarg :url
    :initform ""
    :type string
    :accessor jsonrpc-websocket-client--url
    :documentation "Websocket server url.")
   (-socket
    :accessor jsonrpc-websocket-client--socket
    :writer jsonrpc-websocket-client--set-socket
    :initform nil
    :documentation "client socket"
    :))
  :documentation "A JSONRPC connection over websocket.
The following initargs are accepted:
:URL (mandatory) , websocket server url.")

(cl-defmethod initialize-instance ((connection jsonrpc-websocket-client-connection) slots)
  "Construct CONNECTION with SLOTS."
  (cl-call-next-method)
  (let ((url (plist-get slots :url)))
    (jsonrpc-websocket-client--set-socket
     connection
     (websocket-open
      url
      :on-message (lambda (websocket frame)
                    (let* ((json-object-type 'plist)
                           (json-message (json-read-from-string (websocket-frame-text frame))))
                      (jsonrpc-connection-receive connection json-message)))
      :on-close (lambda (_websocket)
                  ;;TODO: handle close
                  (message "eaf client closed"))))))

(cl-defmethod jsonrpc-connection-send ((connection jsonrpc-websocket-client-connection)
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
    (websocket-send-text (jsonrpc-websocket-client--socket connection) (json-encode-plist message))))

(defclass jsonrpc-websocket-server-connection (jsonrpc-connection)
  ((-port
    :initarg :port
    :initform 12981
    :accessor jsonrpc-websocket--port
    :documentation "Websocket server port.")
   (-server
    :accessor jsonrpc-websocket-server--server
    :writer jsonrpc-websocket-server--set-server
    :initform nil
    :documentation "Websocket server"
    :)
   (-socket
    :accessor jsonrpc-websocket-server--socket
    :writer jsonrpc-websocket-server-set-socket
    :iniform nil
    :documentation "socket"))
  :documentation "A JSONRPC connection over websocket.
The following initargs are accepted:
:URL (mandatory) , websocket server url.")

(cl-defmethod initialize-instance ((connection jsonrpc-websocket-server-connection) slots)
  "Construct CONNECTION with SLOTS."
  (cl-call-next-method)
  (let ((port (plist-get slots :port)))
    (jsonrpc-websocket-server--set-server
     connection
     (websocket-server
      port
      :host 'local
      :on-message (lambda (socket frame)
                    (jsonrpc-websocket-server-set-socket connection socket)
                    (let* ((json-object-type 'plist)
                           (json-message (json-read-from-string (websocket-frame-text frame))))
                      (jsonrpc-connection-receive connection json-message)))
      :on-close (lambda (_websocket)
                  ;;TODO: handle close
                  (message "server's client websocket closed"))))))

(cl-defmethod jsonrpc-connection-send ((connection jsonrpc-websocket-server-connection)
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
    (websocket-send-text (jsonrpc-websocket-server--socket connection) (json-encode-plist message))))



(defvar eaf-websocket--jsonrpc-client-connection nil)

(defvar eaf-websocket--jsonrpc-server-connection nil)

(defun eaf-websocket--call-emacs (method params)
  (when (and (symbolp method) (fboundp method))
    (let ((args (mapcar
                 (lambda(param)
                   (unless (or
                          (eq param :json-false)
                          (eq param :json-null))
                     param)
                   ) params)))
      (apply method args))))

(defun eaf-websocket-start-server(name port)
  "Start NAME websocket server with PORT."
  (setq eaf-websocket--jsonrpc-server-connection
        (jsonrpc-websocket-server-connection
         :name name
         :port port
         :notification-dispatcher (lambda (conn method params)
                                    (eaf-websocket--call-emacs method params)
                                    )
         :request-dispatcher (lambda (conn method params)
                               (eaf-websocket--call-emacs method params)
                               ))))

(defun eaf-websocket-stop-server ()
  "Stop websocket server."
  (when eaf-websocket--jsonrpc-server-connection
    (websocket-server-close (jsonrpc-websocket-server--server eaf-websocket--jsonrpc-server-connection))
    (setq eaf-websocket--jsonrpc-server-connection nil)))

(defun eaf-websocket-start-client(name url)
  "Start connect NAME websocket server with URL."
  (setq eaf-websocket--jsonrpc-client-connection
        (jsonrpc-websocket-client-connection
         :name name
         :url url
         :notification-dispatcher (lambda (conn method params)
                                    (message "client don't handle notify!")
                                    )
         :request-dispatcher (lambda (conn method params)
                               (message "client don't handle request!")
                               ))))

(defun eaf-websocket-stop-client()
  "Stop client."
  (when eaf-websocket--jsonrpc-client-connection
    (websocket-close (jsonrpc-websocket-client--socket eaf-websocket--jsonrpc-client-connection))
    (setq eaf-websocket--jsonrpc-client-connection nil)))


(defun eaf-websocket-call (method &rest params)
  "Call remote METHOD with PARAMS."
  (jsonrpc-request eaf-websocket--jsonrpc-client-connection method params))

(cl-defun eaf-websocket-async-call (method params &rest args &key _success-fn _error-fn _timeout-fn)
  "Async call remote METHOD with PARAMS. PARAMS is  a sequence."
  (apply #'jsonrpc-async-request eaf-websocket--jsonrpc-client-connection method params args))

(defun eaf-websocket-notify (method &rest params)
  "Notify METHOD with PARAMS."
  (jsonrpc-notify eaf-websocket--jsonrpc-client-connection method params))

(provide 'eaf-websocket)

;;; eaf-websocket.el ends here
