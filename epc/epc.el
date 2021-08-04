;;; epc.el --- A RPC stack for the Emacs Lisp

;; Copyright (C) 2011, 2012, 2013  Masashi Sakurai

;; Author: SAKURAI Masashi <m.sakurai at kiwanami.net>
;; Version: 0.1.1
;; Keywords: lisp, rpc
;; Package-Requires: ((concurrent "0.3.1")
;; URL: https://github.com/kiwanami/emacs-epc

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

;; This program is an asynchronous RPC stack for Emacs.  Using this
;; RPC stack, the Emacs can communicate with the peer process.
;; Because the protocol is S-expression encoding and consists of
;; asynchronous communications, the RPC response is fairly good.
;;
;; Current implementations for the EPC are followings:
;; - epcs.el : Emacs Lisp implementation
;; - RPC::EPC::Service : Perl implementation

;;; Code:

(eval-when-compile (require 'cl))
(require 'concurrent)


;;==================================================
;; Utility

(defvar epc:debug-out nil)
(defvar epc:debug-buffer "*epc log*")

(defvar epc:mngr)

;;(setq epc:debug-out t)
;;(setq epc:debug-out nil)

(defun epc:log-init ()
  (when (get-buffer epc:debug-buffer)
    (kill-buffer epc:debug-buffer)))

(defun epc:log (&rest args)
  (when epc:debug-out
    (with-current-buffer
        (get-buffer-create epc:debug-buffer)
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert (apply 'format args) "\n"))))

(defun epc:make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))

(defun epc:document-function (function docstring)
  "Document FUNCTION with DOCSTRING.  Use this for `defstruct' accessor etc."
  (put function 'function-documentation docstring))
(put 'epc:document-function 'lisp-indent-function 'defun)
(put 'epc:document-function 'doc-string-elt 2)


;;==================================================
;; Low Level Interface

(defvar epc:uid 1)

(defun epc:uid ()
  (incf epc:uid))

(defvar epc:accept-process-timeout 150  "Asynchronous timeout time. (msec)")
(defvar epc:accept-process-timeout-count 100 " Startup function waits (`epc:accept-process-timeout' * `epc:accept-process-timeout-count') msec for the external process getting ready.")

(put 'epc-error 'error-conditions '(error epc-error))
(put 'epc-error 'error-message "EPC Error")

(defstruct epc:connection
  "Set of information for network connection and event handling.

name    : Connection name. This name is used for process and buffer names.
process : Connection process object.
buffer  : Working buffer for the incoming data.
channel : Event channels for incoming messages."
  name process buffer channel)

(epc:document-function 'epc:connection-name
  "[internal] Connection name. This name is used for process and buffer names.

\(fn EPC:CONNECTION)")

(epc:document-function 'epc:connection-process
  "[internal] Connection process object.

\(fn EPC:CONNECTION)")

(epc:document-function 'epc:connection-buffer
  "[internal] Working buffer for the incoming data.

\(fn EPC:CONNECTION)")

(epc:document-function 'epc:connection-channel
  "[internal] Event channels for incoming messages.

\(fn EPC:CONNECTION)")


(defun epc:connect (host port)
  "[internal] Connect the server, initialize the process and
return epc:connection object."
  (epc:log ">> Connection start: %s:%s" host port)
  (lexical-let* ((connection-id (epc:uid))
                 (connection-name (format "epc con %s" connection-id))
                 (connection-buf (epc:make-procbuf (format "*%s*" connection-name)))
                 (connection-process
                  (open-network-stream connection-name connection-buf host port))
                 (channel (cc:signal-channel connection-name))
                 (connection (make-epc:connection
                              :name connection-name
                              :process connection-process
                              :buffer connection-buf
                              :channel channel)))
    (epc:log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process
                        (lambda (p m)
                          (epc:process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e)
                            (epc:process-sentinel connection p e)))
    (set-process-query-on-exit-flag connection-process nil)
    connection))

(defun epc:connection-reset (connection)
  "[internal] Reset the connection for restarting the process."
  (cc:signal-disconnect-all (epc:connection-channel connection))
  connection)

(defun epc:process-sentinel (connection process msg)
  (epc:log "!! Process Sentinel [%s] : %S : %S"
           (epc:connection-name connection) process msg)
  (epc:disconnect connection))

(defun epc:net-send (connection sexp)
  (let* ((msg (encode-coding-string
               (concat (epc:prin1-to-string sexp) "\n") 'utf-8-unix))
         (string (concat (epc:net-encode-length (length msg)) msg))
         (proc (epc:connection-process connection)))
    (epc:log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun epc:disconnect (connection)
  (lexical-let
      ((process (epc:connection-process connection))
       (buf (epc:connection-buffer connection))
       (name (epc:connection-name connection)))
    (epc:log "!! Disconnect [%s]" name)
    (when process
      (set-process-sentinel process nil)
      (delete-process process)
      (when (get-buffer buf) (kill-buffer buf)))
    (epc:log "!! Disconnected finished [%s]" name)))

(defun epc:process-filter (connection process message)
  (epc:log "INCOMING: [%s] [%S]" (epc:connection-name connection) message)
  (with-current-buffer (epc:connection-buffer connection)
    (goto-char (point-max))
    (insert message)
    (epc:process-available-input connection process)))

(defun epc:process-available-input (connection process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (epc:net-have-input-p)
      (let ((event (epc:net-read-or-lose process))
            (ok nil))
        (epc:log "<< RECV [%S]" event)
        (unwind-protect
            (condition-case err
                (progn
                  (apply 'cc:signal-send
                         (cons (epc:connection-channel connection) event))
                  (setq ok t))
              ('error (epc:log "MsgError: %S / <= %S" err event)))
          (unless ok
            (epc:run-when-idle 'epc:process-available-input connection process)))))))

(defun epc:net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (epc:net-decode-length))))

(defun epc:run-when-idle (function &rest args)
  "Call FUNCTION as soon as Emacs is idle."
  (apply #'run-at-time
         (if (featurep 'xemacs) itimer-short-interval 0)
         nil function args))

(defun epc:net-read-or-lose (process)
  (condition-case error
      (epc:net-read)
    (error
     (debug 'error error)
     (error "net-read error: %S" error))))

(defun epc:net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (epc:net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length)) content)
    (assert (plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun epc:net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun epc:net-encode-length (n)
  "Encode an integer into a 24-bit hex string."
  (format "%06x" n))

(defun epc:prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))


;;==================================================
;; High Level Interface

(defstruct epc:manager
  "Root object that holds all information related to an EPC activity.

`epc:start-epc' returns this object.

title          : instance name for displaying on the `epc:controller' UI
server-process : process object for the peer
commands       : a list of (prog . args)
port           : port number
connection     : epc:connection instance
methods        : alist of method (name . function)
sessions       : alist of session (id . deferred)
exit-hook      : functions for after shutdown EPC connection"
  title server-process commands port connection methods sessions exit-hooks)

(epc:document-function 'epc:manager-title
  "Instance name (string) for displaying on the `epc:controller' UI

You can modify this slot using `setf' to change the title column
in the `epc:controller' table UI.

\(fn EPC:MANAGER)")

(epc:document-function 'epc:manager-server-process
  "Process object for the peer.

This is *not* network process but the external program started by
`epc:start-epc'.  For network process, see `epc:connection-process'.

\(fn EPC:MANAGER)")

(epc:document-function 'epc:manager-commands
  "[internal] a list of (prog . args)

\(fn EPC:MANAGER)")

(epc:document-function 'epc:manager-port
  "Port number (integer).

\(fn EPC:MANAGER)")

(epc:document-function 'epc:manager-connection
  "[internal] epc:connection instance

\(fn EPC:MANAGER)")

(epc:document-function 'epc:manager-methods
  "[internal] alist of method (name . function)

\(fn EPC:MANAGER)")

(epc:document-function 'epc:manager-sessions
  "[internal] alist of session (id . deferred)

\(fn EPC:MANAGER)")

(epc:document-function 'epc:manager-exit-hooks
  "Hooks called after shutdown EPC connection.

Use `epc:manager-add-exit-hook' to add hook.

\(fn EPC:MANAGER)")

(defstruct epc:method
  "Object to hold serving method information.

name       : method name (symbol)   ex: 'test
task       : method function (function with one argument)
arg-specs  : arg-specs (one string) ex: \"(A B C D)\"
docstring  : docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"
"
  name task docstring arg-specs)

(epc:document-function 'epc:method-name
  "[internal] method name (symbol)   ex: 'test

\(fn EPC:METHOD)")

(epc:document-function 'epc:method-task
  "[internal] method function (function with one argument)

\(fn EPC:METHOD)")

(epc:document-function 'epc:method-arg-specs
  "[internal] arg-specs (one string) ex: \"(A B C D)\"

\(fn EPC:METHOD)")

(epc:document-function 'epc:method-docstring
  "[internal] docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"

\(fn EPC:METHOD)")


(defvar epc:live-connections nil
  "[internal] A list of `epc:manager' objects those currently connect to the epc peer.
This variable is for debug purpose.")

(defun epc:live-connections-add (mngr)
  "[internal] Add the EPC manager object."
  (push mngr epc:live-connections))

(defun epc:live-connections-delete (mngr)
  "[internal] Remove the EPC manager object."
  (setq epc:live-connections (delete mngr epc:live-connections)))


(defun epc:start-epc (server-prog server-args)
  "Start the epc server program and return an epc:manager object.

Start server program SERVER-PROG with command line arguments
SERVER-ARGS.  The server program must print out the port it is
using at the first line of its stdout.  If the server prints out
non-numeric value in the first line or does not print out the
port number in three seconds, it is regarded as start-up
failure."
  (let ((mngr (epc:start-server server-prog server-args)))
    (epc:init-epc-layer mngr)
    mngr))

(defun epc:start-epc-deferred (server-prog server-args)
  "Deferred version of `epc:start-epc'"
  (deferred:nextc (epc:start-server-deferred server-prog server-args)
    #'epc:init-epc-layer))

(defun epc:server-process-name (uid)
  (format "epc:server:%s" uid))

(defun epc:server-buffer-name (uid)
  (format " *%s*" (epc:server-process-name uid)))

(defun epc:start-server (server-prog server-args)
  "[internal] Start a peer server and return an epc:manager instance which is set up partially."
  (let* ((uid (epc:uid))
         (process-name (epc:server-process-name uid))
         (process-buffer (get-buffer-create (epc:server-buffer-name uid)))
         (process (apply 'start-process
                         process-name process-buffer
                         server-prog server-args))
         (cont 1) port)
    (while cont
      (accept-process-output process 0 epc:accept-process-timeout t)
      (let ((port-str (with-current-buffer process-buffer
                          (buffer-string))))
        (cond
         ((string-match "^[ \n\r]*[0-9]+[ \n\r]*$" port-str)
          (setq port (string-to-number port-str)
                cont nil))
         ((< 0 (length port-str))
          (error "Server may raise an error. \
Use \"M-x epc:pop-to-last-server-process-buffer RET\" \
to see full traceback:\n%s" port-str))
         ((not (eq 'run (process-status process)))
          (setq cont nil))
         (t
          (incf cont)
          (when (< epc:accept-process-timeout-count cont) ; timeout 15 seconds
            (error "Timeout server response."))))))
    (set-process-query-on-exit-flag process nil)
    (make-epc:manager :server-process process
                      :commands (cons server-prog server-args)
                      :title (mapconcat 'identity (cons server-prog server-args) " ")
                      :port port
                      :connection (epc:connect "localhost" port))))

(defun epc:start-server-deferred (server-prog server-args)
  "[internal] Same as `epc:start-server' but start the server asynchronously."
  (lexical-let*
      ((uid (epc:uid))
       (process-name (epc:server-process-name uid))
       (process-buffer (get-buffer-create (epc:server-buffer-name uid)))
       (process (apply 'start-process
                       process-name process-buffer
                       server-prog server-args))
       (mngr (make-epc:manager
              :server-process process
              :commands (cons server-prog server-args)
              :title (mapconcat 'identity (cons server-prog server-args) " ")))
       (cont 1) port)
    (set-process-query-on-exit-flag process nil)
    (deferred:$
      (deferred:next
        (deferred:lambda (_)
          (accept-process-output process 0 nil t)
          (let ((port-str (with-current-buffer process-buffer
                            (buffer-string))))
            (cond
             ((string-match "^[0-9]+$" port-str)
              (setq port (string-to-number port-str)
                    cont nil))
             ((< 0 (length port-str))
              (error "Server may raise an error. \
Use \"M-x epc:pop-to-last-server-process-buffer RET\" \
to see full traceback:\n%s" port-str))
             ((not (eq 'run (process-status process)))
              (setq cont nil))
             (t
              (incf cont)
              (when (< epc:accept-process-timeout-count cont)
                ;; timeout 15 seconds
                (error "Timeout server response."))
              (deferred:nextc (deferred:wait epc:accept-process-timeout)
                self))))))
      (deferred:nextc it
        (lambda (_)
          (setf (epc:manager-port mngr) port)
          (setf (epc:manager-connection mngr) (epc:connect "localhost" port))
          mngr)))))

(defun epc:stop-epc (mngr)
  "Disconnect the connection for the server."
  (let* ((proc (epc:manager-server-process mngr))
         (buf (and proc (process-buffer proc))))
    (epc:disconnect (epc:manager-connection mngr))
    (when proc
      (accept-process-output proc 0 epc:accept-process-timeout t))
    (when (and proc (equal 'run (process-status proc)))
      (kill-process proc))
    (when buf  (kill-buffer buf))
    (condition-case err
        (epc:manager-fire-exit-hook mngr)
      (error (epc:log "Error on exit-hooks : %S / " err mngr)))
    (epc:live-connections-delete mngr)))

(defun epc:start-epc-debug (port)
  "[internal] Return an epc:manager instance which is set up partially."
  (epc:init-epc-layer
   (make-epc:manager :server-process nil
                     :commands (cons "[DEBUG]" nil)
                     :port port
                     :connection (epc:connect "localhost" port))))

(defun epc:args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond
   ((atom args) args)
   (t (cadr args))))

(defun epc:init-epc-layer (mngr)
  "[internal] Connect to the server program and return an epc:connection instance."
  (lexical-let*
      ((mngr mngr)
       (conn (epc:manager-connection mngr))
       (channel (epc:connection-channel conn)))
    ;; dispatch incoming messages with the lexical scope
    (loop for (method . body) in
          `((call
             . (lambda (args)
                 (epc:log "SIG CALL: %S" args)
                 (apply 'epc:handler-called-method ,mngr (epc:args args))))
            (return
             . (lambda (args)
                 (epc:log "SIG RET: %S" args)
                 (apply 'epc:handler-return ,mngr (epc:args args))))
            (return-error
             . (lambda (args)
                 (epc:log "SIG RET-ERROR: %S" args)
                 (apply 'epc:handler-return-error ,mngr (epc:args args))))
            (epc-error
             . (lambda (args)
                 (epc:log "SIG EPC-ERROR: %S" args)
                 (apply 'epc:handler-epc-error ,mngr (epc:args args))))
            (methods
             . (lambda (args)
                 (epc:log "SIG METHODS: %S" args)
                 (epc:handler-methods ,mngr (caadr args))))
            ) do
              (cc:signal-connect channel method body))
    (epc:live-connections-add mngr)
    mngr))



(defun epc:manager-add-exit-hook (mngr hook-function)
  "Register the HOOK-FUNCTION which is called after the EPC connection closed by the EPC controller UI.
HOOK-FUNCTION is a function with no argument."
  (let* ((hooks (epc:manager-exit-hooks mngr)))
    (setf (epc:manager-exit-hooks mngr) (cons hook-function hooks))
    mngr))

(defun epc:manager-fire-exit-hook (mngr)
  "[internal] Call exit-hooks functions of MNGR. After calling hooks, this functions clears the hook slot so as not to call doubly."
  (let* ((hooks (epc:manager-exit-hooks mngr)))
    (run-hooks hooks)
    (setf (epc:manager-exit-hooks mngr) nil)
    mngr))

(defun epc:manager-status-server-process (mngr)
  "[internal] Return the status of the process object for the peer process. If the process is nil, return nil."
  (and mngr
       (epc:manager-server-process mngr)
       (process-status (epc:manager-server-process mngr))))

(defun epc:manager-status-connection-process (mngr)
  "[internal] Return the status of the process object for the connection process."
  (and (epc:manager-connection mngr)
       (process-status (epc:connection-process
                        (epc:manager-connection mngr)))))

(defun epc:manager-restart-process (mngr)
  "[internal] Restart the process and reconnect."
  (cond
   ((null (epc:manager-server-process mngr))
    (error "Cannot restart this EPC process!"))
   (t
    (epc:stop-epc mngr)
    (let* ((cmds (epc:manager-commands mngr))
           (new-mngr (epc:start-server (car cmds) (cdr cmds))))
      (setf (epc:manager-server-process mngr)
            (epc:manager-server-process new-mngr))
      (setf (epc:manager-port mngr)
            (epc:manager-port new-mngr))
      (setf (epc:manager-connection mngr)
            (epc:manager-connection new-mngr))
      (setf (epc:manager-methods mngr)
            (epc:manager-methods new-mngr))
      (setf (epc:manager-sessions mngr)
            (epc:manager-sessions new-mngr))
      (epc:connection-reset (epc:manager-connection mngr))
      (epc:init-epc-layer mngr)
      (epc:live-connections-delete new-mngr)
      (epc:live-connections-add mngr)
      mngr))))

(defun epc:manager-send (mngr method &rest messages)
  "[internal] low-level message sending."
  (let* ((conn (epc:manager-connection mngr)))
    (epc:net-send conn (cons method messages))))

(defun epc:manager-get-method (mngr method-name)
  "[internal] Return a method object. If not found, return nil."
  (loop for i in (epc:manager-methods mngr)
        if (eq method-name (epc:method-name i))
        do (return i)))

(defun epc:handler-methods (mngr uid)
  "[internal] Return a list of information for registered methods."
  (let ((info
         (loop for i in (epc:manager-methods mngr)
               collect
               (list
                (epc:method-name i)
                (or (epc:method-arg-specs i) "")
                (or (epc:method-docstring i) "")))))
    (epc:manager-send mngr 'return uid info)))

(defun epc:handler-called-method (mngr uid name args)
  "[internal] low-level message handler for peer's calling."
  (lexical-let ((mngr mngr) (uid uid))
    (let* ((methods (epc:manager-methods mngr))
           (method (epc:manager-get-method mngr name)))
      (cond
       ((null method)
        (epc:log "ERR: No such method : %s" name)
        (epc:manager-send mngr 'epc-error uid (format "EPC-ERROR: No such method : %s" name)))
       (t
        (condition-case err
            (let* ((f (epc:method-task method))
                   (ret (apply f args)))
              (cond
               ((deferred-p ret)
                (deferred:nextc ret
                  (lambda (xx) (epc:manager-send mngr 'return uid xx))))
               (t (epc:manager-send mngr 'return uid ret))))
            (error
             (epc:log "ERROR : %S" err)
             (epc:manager-send mngr 'return-error uid err))))))))

(defun epc:manager-remove-session (mngr uid)
  "[internal] Remove a session from the epc manager object."
  (loop with ret = nil
        for pair in (epc:manager-sessions mngr)
        unless (eq uid (car pair))
        do (push pair ret)
        finally
        do (setf (epc:manager-sessions mngr) ret)))

(defun epc:handler-return (mngr uid args)
  "[internal] low-level message handler for normal returns."
  (let ((pair (assq uid (epc:manager-sessions mngr))))
    (cond
     (pair
      (epc:log "RET: id:%s [%S]" uid args)
      (epc:manager-remove-session mngr uid)
      (deferred:callback (cdr pair) args))
     (t ; error
      (epc:log "RET: NOT FOUND: id:%s [%S]" uid args)))))

(defun epc:handler-return-error (mngr uid args)
  "[internal] low-level message handler for application errors."
  (let ((pair (assq uid (epc:manager-sessions mngr))))
    (cond
     (pair
      (epc:log "RET-ERR: id:%s [%S]" uid args)
      (epc:manager-remove-session mngr uid)
      (deferred:errorback (cdr pair) (format "%S" args)))
     (t ; error
      (epc:log "RET-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun epc:handler-epc-error (mngr uid args)
  "[internal] low-level message handler for epc errors."
  (let ((pair (assq uid (epc:manager-sessions mngr))))
    (cond
     (pair
      (epc:log "RET-EPC-ERR: id:%s [%S]" uid args)
      (epc:manager-remove-session mngr uid)
      (deferred:errorback (cdr pair) (list 'epc-error args)))
     (t ; error
      (epc:log "RET-EPC-ERR: NOT FOUND: id:%s [%S]" uid args)))))



(defun epc:call-deferred (mngr method-name args)
  "Call peer's method with args asynchronously. Return a deferred
object which is called with the result."
  (let ((uid (epc:uid))
        (sessions (epc:manager-sessions mngr))
        (d (deferred:new)))
    (push (cons uid d) sessions)
    (setf (epc:manager-sessions mngr) sessions)
    (epc:manager-send mngr 'call uid method-name args)
    d))

(defun epc:define-method (mngr method-name task &optional arg-specs docstring)
  "Define a method and return a deferred object which is called by the peer."
  (let* ((method (make-epc:method
                  :name method-name :task task
                  :arg-specs arg-specs :docstring docstring))
         (methods (cons method (epc:manager-methods mngr))))
    (setf (epc:manager-methods mngr) methods)
    method))

(defun epc:query-methods-deferred (mngr)
  "Return a list of information for the peer's methods.
The list is consisted of lists of strings:
 (name arg-specs docstring)."
  (let ((uid (epc:uid))
        (sessions (epc:manager-sessions mngr))
        (d (deferred:new)))
    (push (cons uid d) sessions)
    (setf (epc:manager-sessions mngr) sessions)
    (epc:manager-send mngr 'methods uid)
    d))

(defun epc:sync (mngr d)
  "Wrap deferred methods with synchronous waiting, and return the result.
If an exception is occurred, this function throws the error."
  (lexical-let ((result 'epc:nothing))
    (deferred:$ d
      (deferred:nextc it
        (lambda (x) (setq result x)))
      (deferred:error it
        (lambda (er) (setq result (cons 'error er)))))
    (while (eq result 'epc:nothing)
      (save-current-buffer
        (accept-process-output
         (epc:connection-process (epc:manager-connection mngr))
         0 epc:accept-process-timeout t)))
    (if (and (consp result) (eq 'error (car result)))
        (error (cdr result)) result)))

(defun epc:call-sync (mngr method-name args)
  "Call peer's method with args synchronously and return the result.
If an exception is occurred, this function throws the error."
  (epc:sync mngr (epc:call-deferred mngr method-name args)))

(defun epc:live-p (mngr)
  "Return non-nil when MNGR is an EPC manager object with a live
connection."
  (let ((proc (ignore-errors
                (epc:connection-process (epc:manager-connection mngr)))))
    (and (processp proc)
         ;; Same as `process-live-p' in Emacs >= 24:
         (memq (process-status proc) '(run open listen connect stop)))))


;;==================================================
;; Troubleshooting / Debugging support

(defun epc:pop-to-last-server-process-buffer ()
  "Open the buffer for most recently started server program process.
This is useful when you want to check why the server program
failed to start (e.g., to see its traceback / error message)."
  (interactive)
  (let ((buffer (get-buffer (epc:server-buffer-name epc:uid))))
    (if buffer
        (pop-to-buffer buffer)
      (error "No buffer for the last server process.  \
Probably the EPC connection exits correctly or you didn't start it yet."))))



;;==================================================
;; Management Interface

(defun epc:controller ()
  "Display the management interface for EPC processes and connections.
Process list.
Session status, statistics and uptime.
Peer's method list.
Display process buffer.
Kill sessions and connections.
Restart process."
  (interactive)
  (let* ((buf-name "*EPC Controller*")
         (buf (get-buffer buf-name)))
    (unless (buffer-live-p buf)
      (setq buf (get-buffer-create buf-name)))
    (pop-to-buffer buf)))

(defun epc:controller-methods (mngr)
  "Display a list of methods for the MNGR process."
  (let* ((buf-name "*EPC Controller/Methods*")
         (buf (get-buffer buf-name)))
    (unless (buffer-live-p buf)
      (setq buf (get-buffer-create buf-name))
      (with-current-buffer buf
        (setq buffer-read-only t)))
    (lexical-let ((buf buf) (mngr mngr))
      (deferred:$
        (epc:query-methods-deferred mngr)
        (deferred:nextc it
          (lambda (methods)
            (pop-to-buffer buf)))))))

(defface epc:face-title
  '((((class color) (background light))
     :foreground "Slategray4" :background "Gray90" :weight bold)
    (((class color) (background dark))
     :foreground "maroon2" :weight bold))
  "Face for titles" :group 'epc)

(defun epc:define-keymap (keymap-list &optional prefix)
  "[internal] Keymap utility."
  (let ((map (make-sparse-keymap)))
    (mapc
     (lambda (i)
       (define-key map
         (if (stringp (car i))
             (read-kbd-macro
              (if prefix
                  (replace-regexp-in-string "prefix" prefix (car i))
                (car i)))
           (car i))
         (cdr i)))
     keymap-list)
    map))

(defun epc:add-keymap (keymap keymap-list &optional prefix)
  (loop with nkeymap = (copy-keymap keymap)
        for i in keymap-list
        do
        (define-key nkeymap
          (if (stringp (car i))
              (read-kbd-macro
               (if prefix
                   (replace-regexp-in-string "prefix" prefix (car i))
                 (car i)))
            (car i))
          (cdr i))
        finally return nkeymap))

(provide 'epc)
;;; epc.el ends here
