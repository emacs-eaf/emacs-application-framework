;;; epcs.el --- EPC Server

;; Copyright (C) 2011,2012,2013  Masashi Sakurai

;; Author: Masashi Sakurai <m.sakurai at kiwanami.net>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(eval-when-compile (require 'cl))
(require 'epc)

(defvar epcs:client-processes nil
  "[internal] A list of ([process object] . [`epc:manager' instance]).  
When the server process accepts the client connection, the
`epc:manager' instance is created and stored in this variable
`epcs:client-processes'. This variable is used for the management
purpose.")

;; epcs:server
;;   name    : process name (string)   ex: "EPC Server 1"
;;   process : server process object
;;   port    : port number
;;   connect-function : initialize function for `epc:manager' instances
(defstruct epcs:server name process port connect-function)

(defvar epcs:server-processes nil
  "[internal] A list of ([process object] . [`epcs:server' instance]).
This variable is used for the management purpose.")

(defun epcs:server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (lexical-let* 
      ((connect-function connect-function)
       (name (format "EPC Server %s" (epc:uid)))
       (buf (epc:make-procbuf (format "*%s*" name)))
       (main-process 
        (make-network-process 
         :name name
         :buffer buf
         :family 'ipv4 :server t :nowait t
         :host "127.0.0.1" :service (or port t)
         :sentinel 
         (lambda (process message) 
           (epcs:sentinel process message connect-function)))))
    (unless port
      ;; notify port number to the parent process via STDOUT.
      (message "%s\n" (process-contact main-process :service)))
    (push (cons main-process
                (make-epcs:server 
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          epcs:server-processes)
    main-process))

(defun epcs:server-stop (process)
  "Stop the TCP server process."
  (cond
   ((and process 
         (assq process epcs:server-processes))
    (epc:log "EPCS: Shutdown Server: %S" process)
    (let ((buf (process-buffer process)))
      (delete-process process)
      (kill-buffer buf))
    (setq epcs:server-processes 
          (assq-delete-all process epcs:server-processes)))
   (t (error "Not found in the server process list. [%S]" process))))

(defun epcs:get-manager-by-process (proc)
  "[internal] Return the epc:manager instance for the PROC."
  (loop for (pp . mngr) in epcs:client-processes
        if (eql pp proc)
        do (return mngr)
        finally return nil))

(defun epcs:kill-all-processes ()
  "Kill all child processes for debug purpose."
  (interactive)
  (loop for (proc . mngr) in epcs:client-processes
        do (ignore-errors
             (delete-process proc)
             (kill-buffer (process-buffer proc)))))

(defun epcs:accept (process)
  "[internal] Initialize the process and return epc:manager object."
  (epc:log "EPCS: >> Connection accept: %S" process)
  (lexical-let* ((connection-id (epc:uid))
                 (connection-name (format "epc con %s" connection-id))
                 (channel (cc:signal-channel connection-name))
                 (connection (make-epc:connection 
                              :name connection-name
                              :process process
                              :buffer (process-buffer process)
                              :channel channel)))
    (epc:log "EPCS: >> Connection establish")
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process 
                        (lambda (p m)
                          (epc:process-filter connection p m)))
    (set-process-sentinel process
                          (lambda (p e)
                            (epc:process-sentinel connection p e)))
    (make-epc:manager :server-process process :port t
                      :connection connection)))

(defun epcs:sentinel (process message connect-function)
  "[internal] Process sentinel handler for the server process."
  (epc:log "EPCS: SENTINEL: %S %S" process message)
  (let ((mngr (epcs:get-manager-by-process process)))
    (cond
     ;; new connection
     ((and (string-match "open" message) (null mngr))
      (condition-case err
          (let ((mngr (epcs:accept process)))
            (push (cons process mngr) epcs:client-processes)
            (epc:init-epc-layer mngr)
            (when connect-function (funcall connect-function mngr))
            mngr)
        ('error 
         (epc:log "EPCS: Protocol error: %S" err)
         (epc:log "EPCS: ABORT %S" process)
         (delete-process process))))
     ;; ignore
     ((null mngr) nil )
     ;; disconnect
     (t
      (let ((pair (assq process epcs:client-processes)) d)
        (when pair
          (epc:log "EPCS: DISCONNECT %S" process)
          (epc:stop-epc (cdr pair))
          (setq epcs:client-processes 
                (assq-delete-all process epcs:client-processes))
          ))
      nil))))


;; Management GUI

;; todo...

(provide 'epcs)
;;; epcs.el ends here
