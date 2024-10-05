;;; epcs.el --- EPC Server              -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'subr-x)

;; deferred
(defmacro eaf-deferred-chain (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form))
           (indent 0))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

;; Debug
(defvar eaf-deferred-debug nil
  "Debug output switch.")

(defvar eaf-deferred-debug-count 0
  "[internal] Debug output counter.")

(defun eaf-deferred-log (&rest args)
  "[internal] Debug log function."
  (when eaf-deferred-debug
    (with-current-buffer (get-buffer-create "*eaf-deferred-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "%5i %s\n\n\n" eaf-deferred-debug-count (apply #'format args)))))
    (cl-incf eaf-deferred-debug-count)))

(defvar eaf-deferred-debug-on-signal nil
  "If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(defmacro eaf-deferred-condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`eaf-deferred-debug-on-signal'."
  (declare (debug condition-case)
           (indent 1))
  `(let ((debug-on-signal
          (or debug-on-signal eaf-deferred-debug-on-signal)))
     (condition-case ,var
         ,protected-form
       ,@handlers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back end functions of deferred tasks

(defvar eaf-deferred-tick-time 0.001
  "Waiting time between asynchronous tasks (second).
The shorter waiting time increases the load of Emacs. The end
user can tune this parameter. However, applications should not
modify it because the applications run on various environments.")

(defvar eaf-deferred-queue nil
  "[internal] The execution queue of deferred objects.
See the functions `eaf-deferred-post-task' and `eaf-deferred-worker'.")

(defun eaf-deferred-post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`eaf-deferred-queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (let ((pack `(,d ,which . ,arg)))
    (push pack eaf-deferred-queue)
    (eaf-deferred-log "QUEUE-POST [%s]: %s" (length eaf-deferred-queue) pack)
    (run-at-time eaf-deferred-tick-time nil 'eaf-deferred-worker)
    d))

(defun eaf-deferred-worker ()
  "[internal] Consume a deferred task.
Mainly this function is called by timer asynchronously."
  (when eaf-deferred-queue
    (let* ((pack (car (last eaf-deferred-queue)))
           (d (car pack))
           (which (cadr pack))
           (arg (cddr pack)) value)
      (setq eaf-deferred-queue (nbutlast eaf-deferred-queue))
      (condition-case err
          (setq value (eaf-deferred-exec-task d which arg))
        (error
         (eaf-deferred-log "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

;; Struct: eaf-deferred-object
;;
;; callback    : a callback function (default `identity')
;; errorback   : an errorback function (default `eaf-deferred-resignal')
;; cancel      : a canceling function (default `eaf-deferred-default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;;
(cl-defstruct eaf-deferred-object
  (callback 'identity)
  (errorback 'eaf-deferred-resignal)
  (cancel 'eaf-deferred-default-cancel)
  next status value)

(defun eaf-deferred-resignal (err)
  "[internal] Safely resignal ERR as an Emacs condition.

If ERR is a cons (ERROR-SYMBOL . DATA) where ERROR-SYMBOL has an
`error-conditions' property, it is re-signaled unchanged. If ERR
is a string, it is signaled as a generic error using `error'.
Otherwise, ERR is formatted into a string as if by `print' before
raising with `error'."
  (cond ((and (listp err)
              (symbolp (car err))
              (get (car err) 'error-conditions))
         (signal (car err) (cdr err)))
        ((stringp err)
         (error "%s" err))
        (t
         (error "%S" err))))

(defun eaf-deferred-default-cancel (d)
  "[internal] Default canceling function."
  (eaf-deferred-log "CANCEL : %s" d)
  (setf (eaf-deferred-object-callback d) 'identity)
  (setf (eaf-deferred-object-errorback d) 'eaf-deferred-resignal)
  (setf (eaf-deferred-object-next d) nil)
  d)

(defun eaf-deferred-exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (eaf-deferred-log "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "eaf-deferred-exec-task was given a nil."))
  (let ((callback (if (eq which 'ok)
                      (eaf-deferred-object-callback d)
                    (eaf-deferred-object-errorback d)))
        (next-deferred (eaf-deferred-object-next d)))
    (cond
     (callback
      (eaf-deferred-condition-case err
        (let ((value (funcall callback arg)))
          (cond
           ((eaf-deferred-object-p value)
            (eaf-deferred-log "WAIT NEST : %s" value)
            (if next-deferred
                (eaf-deferred-set-next value next-deferred)
              value))
           (t
            (if next-deferred
                (eaf-deferred-post-task next-deferred 'ok value)
              (setf (eaf-deferred-object-status d) 'ok)
              (setf (eaf-deferred-object-value d) value)
              value))))
        (error
         (cond
          (next-deferred
           (eaf-deferred-post-task next-deferred 'ng err))
          (t
           (eaf-deferred-log "ERROR : %S" err)
           (message "deferred error : %S" err)
           (setf (eaf-deferred-object-status d) 'ng)
           (setf (eaf-deferred-object-value d) err)
           err)))))
     (t ; <= (null callback)
      (cond
       (next-deferred
        (eaf-deferred-exec-task next-deferred which arg))
       ((eq which 'ok) arg)
       (t                             ; (eq which 'ng)
        (eaf-deferred-resignal arg)))))))

(defun eaf-deferred-set-next (prev next)
  "[internal] Connect deferred objects."
  (setf (eaf-deferred-object-next prev) next)
  (cond
   ((eq 'ok (eaf-deferred-object-status prev))
    (setf (eaf-deferred-object-status prev) nil)
    (let ((ret (eaf-deferred-exec-task
                next 'ok (eaf-deferred-object-value prev))))
      (if (eaf-deferred-object-p ret) ret
        next)))
   ((eq 'ng (eaf-deferred-object-status prev))
    (setf (eaf-deferred-object-status prev) nil)
    (let ((ret (eaf-deferred-exec-task next 'ng (eaf-deferred-object-value prev))))
      (if (eaf-deferred-object-p ret) ret
        next)))
   (t
    next)))

(defun eaf-deferred-new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-eaf-deferred-object :callback callback)
    (make-eaf-deferred-object)))

(defun eaf-deferred-callback (d &optional arg)
  "Start deferred chain with a callback message."
  (eaf-deferred-exec-task d 'ok arg))

(defun eaf-deferred-errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (declare (indent 1))
  (eaf-deferred-exec-task d 'ng arg))

(defun eaf-deferred-callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (declare (indent 1))
  (eaf-deferred-post-task d 'ok arg))

(defun eaf-deferred-next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (eaf-deferred-callback-post (eaf-deferred-new callback))."
  (let ((d (if callback
               (make-eaf-deferred-object :callback callback)
             (make-eaf-deferred-object))))
    (eaf-deferred-callback-post d arg)
    d))

(defun eaf-deferred-nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-eaf-deferred-object :callback callback)))
    (eaf-deferred-set-next d nd)))

(defun eaf-deferred-error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-eaf-deferred-object :errorback callback)))
    (eaf-deferred-set-next d nd)))

(defvar eaf-epc-debug nil)

(defun eaf-epc-log (&rest args)
(when eaf-epc-debug
  (with-current-buffer (get-buffer-create "*eaf-epc-log*")
    (buffer-disable-undo)
    (goto-char (point-max))
    (insert (apply 'format args) "\n\n\n"))))

(defun eaf-epc-make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))

(defvar eaf-epc-uid 1)

(defun eaf-epc-uid ()
  (cl-incf eaf-epc-uid))

(defvar eaf-epc-accept-process-timeout 150
  "Asynchronous timeout time. (msec)")

(put 'epc-error 'error-conditions '(error epc-error))
(put 'epc-error 'error-message "EPC Error")

(cl-defstruct eaf-epc-connection
  "Set of information for network connection and event handling.

name    : Connection name. This name is used for process and buffer names.
process : Connection process object.
buffer  : Working buffer for the incoming data.
channel : Event channels for incoming messages."
  name process buffer channel)

(defun eaf-epc-connect (host port)
  "[internal] Connect the server, initialize the process and
return eaf-epc-connection object."
  (eaf-epc-log ">> Connection start: %s:%s" host port)
  (let* ((connection-id (eaf-epc-uid))
         (connection-name (format "eaf-epc con %s" connection-id))
         (connection-buf (eaf-epc-make-procbuf (format "*%s*" connection-name)))
         (connection-process
          (open-network-stream connection-name connection-buf host port))
         (channel (list connection-name nil))
         (connection (make-eaf-epc-connection
                      :name connection-name
                      :process connection-process
                      :buffer connection-buf
                      :channel channel)))
    (eaf-epc-log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process
                        (lambda (p m)
                          (eaf-epc-process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e)
                            (eaf-epc-process-sentinel connection p e)))
    (set-process-query-on-exit-flag connection-process nil)
    connection))

(defun eaf-epc-process-sentinel (connection process msg)
  (eaf-epc-log "!! Process Sentinel [%s] : %S : %S"
               (eaf-epc-connection-name connection) process msg)
  (eaf-epc-disconnect connection))

(defun eaf-epc-net-send (connection sexp)
  (let* ((msg (encode-coding-string
               (concat (eaf-epc-prin1-to-string sexp) "\n") 'utf-8-unix))
         (string (concat (format "%06x" (length msg)) msg))
         (proc (eaf-epc-connection-process connection)))
    (eaf-epc-log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun eaf-epc-disconnect (connection)
  (let ((process (eaf-epc-connection-process connection))
        (buf (eaf-epc-connection-buffer connection))
        (name (eaf-epc-connection-name connection)))
    (eaf-epc-log "!! Disconnect [%s]" name)
    (when process
      (set-process-sentinel process nil)
      (delete-process process)
      (when (get-buffer buf) (kill-buffer buf)))
    (eaf-epc-log "!! Disconnected finished [%s]" name)))

(defun eaf-epc-process-filter (connection process message)
  (eaf-epc-log "INCOMING: [%s] [%S]" (eaf-epc-connection-name connection) message)
  (with-current-buffer (eaf-epc-connection-buffer connection)
    (goto-char (point-max))
    (insert message)
    (eaf-epc-process-available-input connection process)))

(defun eaf-epc-signal-connect (channel event-sym &optional callback)
  "Append an observer for EVENT-SYM of CHANNEL and return a deferred object.
If EVENT-SYM is `t', the observer receives all signals of the channel.
If CALLBACK function is given, the deferred object executes the
CALLBACK function asynchronously. One can connect subsequent
tasks to the returned deferred object."
  (let ((d (if callback
               (eaf-deferred-new callback)
             (eaf-deferred-new))))
    (push (cons event-sym d)
          (cddr channel))
    d))

(defun eaf-epc-signal-send (channel event-sym &rest args)
  "Send a signal to CHANNEL. If ARGS values are given,
observers can get the values by following code:

  (lambda (event)
    (destructuring-bind
     (event-sym (args))
     event ... ))
"
  (let ((observers (cddr channel))
        (event (list event-sym args)))
    (cl-loop for i in observers
             for name = (car i)
             for d = (cdr i)
             if (or (eq event-sym name) (eq t name))
             do (eaf-deferred-callback-post d event))))

(defun eaf-epc-process-available-input (connection process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (eaf-epc-net-have-input-p)
      (let ((event (eaf-epc-net-read-or-lose process))
            (ok nil))
        (eaf-epc-log "<< RECV [%S]" event)
        (unwind-protect
            (condition-case err
                (progn
                  (apply 'eaf-epc-signal-send
                         (cons (eaf-epc-connection-channel connection) event))
                  (setq ok t))
              ('error (eaf-epc-log "MsgError: %S / <= %S" err event)))
          (unless ok
            (eaf-epc-process-available-input connection process)))))))

(defun eaf-epc-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (eaf-epc-net-decode-length))))

(defun eaf-epc-net-read-or-lose (_process)
  (condition-case error
      (eaf-epc-net-read)
    (error
     (debug 'error error)
     (error "net-read error: %S" error))))

(defun eaf-epc-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (eaf-epc-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length))
         _content)
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun eaf-epc-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun eaf-epc-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(cl-defstruct eaf-epc-manager
  "Root object that holds all information related to an EPC activity.

`eaf-epc-start-epc' returns this object.

title          : instance name for displaying on the `eaf-epc-controller' UI
server-process : process object for the peer
commands       : a list of (prog . args)
port           : port number
connection     : eaf-epc-connection instance
methods        : alist of method (name . function)
sessions       : alist of session (id . deferred)
exit-hook      : functions for after shutdown EPC connection"
  title server-process commands port connection methods sessions exit-hooks)

(cl-defstruct eaf-epc-method
  "Object to hold serving method information.

name       : method name (symbol)   ex: 'test
task       : method function (function with one argument)
arg-specs  : arg-specs (one string) ex: \"(A B C D)\"
docstring  : docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"
"
  name task docstring arg-specs)

(defvar eaf-epc-live-connections nil
  "[internal] A list of `eaf-epc-manager' objects.
those objects currently connect to the epc peer.
This variable is for debug purpose.")

(defun eaf-epc-stop-epc (mngr)
  "Disconnect the connection for the server."
  (let* ((proc (eaf-epc-manager-server-process mngr))
         (buf (and proc (process-buffer proc))))
    (eaf-epc-disconnect (eaf-epc-manager-connection mngr))
    (when proc
      (accept-process-output proc 0 eaf-epc-accept-process-timeout t))
    (when (and proc (equal 'run (process-status proc)))
      (kill-process proc))
    (when buf  (kill-buffer buf))
    (setq eaf-epc-live-connections (delete mngr eaf-epc-live-connections))
    ))

(defun eaf-epc-args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond
   ((atom args) args)
   (t (cadr args))))

(defun eaf-epc-init-epc-layer (mngr)
  "[internal] Connect to the server program and return an eaf-epc-connection instance."
  (let* ((mngr mngr)
         (conn (eaf-epc-manager-connection mngr))
         (channel (eaf-epc-connection-channel conn)))
    ;; dispatch incoming messages with the lexical scope
    (cl-loop for (method . body) in
             `((call
                . (lambda (args)
                    (eaf-epc-log "SIG CALL: %S" args)
                    (apply 'eaf-epc-handler-called-method ,mngr (eaf-epc-args args))))
               (return
                . (lambda (args)
                    (eaf-epc-log "SIG RET: %S" args)
                    (apply 'eaf-epc-handler-return ,mngr (eaf-epc-args args))))
               (return-error
                . (lambda (args)
                    (eaf-epc-log "SIG RET-ERROR: %S" args)
                    (apply 'eaf-epc-handler-return-error ,mngr (eaf-epc-args args))))
               (epc-error
                . (lambda (args)
                    (eaf-epc-log "SIG EPC-ERROR: %S" args)
                    (apply 'eaf-epc-handler-epc-error ,mngr (eaf-epc-args args))))
               (methods
                . (lambda (args)
                    (eaf-epc-log "SIG METHODS: %S" args)
                    (eaf-epc-handler-methods ,mngr (caadr args))))
               ) do
             (eaf-epc-signal-connect channel method body))
    (push mngr eaf-epc-live-connections)
    mngr))

(defun eaf-epc-manager-send (mngr method &rest messages)
  "[internal] low-level message sending."
  (let* ((conn (eaf-epc-manager-connection mngr)))
    (eaf-epc-net-send conn (cons method messages))))

(defun eaf-epc-manager-get-method (mngr method-name)
  "[internal] Return a method object. If not found, return nil."
  (cl-loop for i in (eaf-epc-manager-methods mngr)
           if (eq method-name (eaf-epc-method-name i))
           do (cl-return i)))

(defun eaf-epc-handler-methods (mngr uid)
  "[internal] Return a list of information for registered methods."
  (let ((info
         (cl-loop for i in (eaf-epc-manager-methods mngr)
                  collect
                  (list
                   (eaf-epc-method-name i)
                   (or (eaf-epc-method-arg-specs i) "")
                   (or (eaf-epc-method-docstring i) "")))))
    (eaf-epc-manager-send mngr 'return uid info)))

(defun eaf-epc-handler-called-method (mngr uid name args)
  "[internal] low-level message handler for peer's calling."
  (let ((mngr mngr) (uid uid))
    (let* ((_methods (eaf-epc-manager-methods mngr))
           (method (eaf-epc-manager-get-method mngr name)))
      (cond
       ((null method)
        (eaf-epc-log "ERR: No such method : %s" name)
        (eaf-epc-manager-send mngr 'epc-error uid (format "EPC-ERROR: No such method : %s" name)))
       (t
        (condition-case err
            (let* ((f (eaf-epc-method-task method))
                   (ret (apply f args)))
              (cond
               ((eaf-deferred-object-p ret)
                (eaf-deferred-nextc ret
                  (lambda (xx) (eaf-epc-manager-send mngr 'return uid xx))))
               (t (eaf-epc-manager-send mngr 'return uid ret))))
          (error
           (eaf-epc-log "ERROR : %S" err)
           (eaf-epc-manager-send mngr 'return-error uid err))))))))

(defun eaf-epc-manager-remove-session (mngr uid)
  "[internal] Remove a session from the epc manager object."
  (cl-loop with ret = nil
           for pair in (eaf-epc-manager-sessions mngr)
           unless (eq uid (car pair))
           do (push pair ret)
           finally
           do (setf (eaf-epc-manager-sessions mngr) ret)))

(defun eaf-epc-handler-return (mngr uid args)
  "[internal] low-level message handler for normal returns."
  (let ((pair (assq uid (eaf-epc-manager-sessions mngr))))
    (cond
     (pair
      (eaf-epc-log "RET: id:%s [%S]" uid args)
      (eaf-epc-manager-remove-session mngr uid)
      (eaf-deferred-callback (cdr pair) args))
     (t ; error
      (eaf-epc-log "RET: NOT FOUND: id:%s [%S]" uid args)))))

(defun eaf-epc-handler-return-error (mngr uid args)
  "[internal] low-level message handler for application errors."
  (let ((pair (assq uid (eaf-epc-manager-sessions mngr))))
    (cond
     (pair
      (eaf-epc-log "RET-ERR: id:%s [%S]" uid args)
      (eaf-epc-manager-remove-session mngr uid)
      (eaf-deferred-errorback (cdr pair) (format "%S" args)))
     (t ; error
      (eaf-epc-log "RET-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun eaf-epc-handler-epc-error (mngr uid args)
  "[internal] low-level message handler for epc errors."
  (let ((pair (assq uid (eaf-epc-manager-sessions mngr))))
    (cond
     (pair
      (eaf-epc-log "RET-EPC-ERR: id:%s [%S]" uid args)
      (eaf-epc-manager-remove-session mngr uid)
      (eaf-deferred-errorback (cdr pair) (list 'epc-error args)))
     (t ; error
      (eaf-epc-log "RET-EPC-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun eaf-epc-call-deferred (mngr method-name args)
  "Call peer's method with args asynchronously. Return a deferred
object which is called with the result."
  (let ((uid (eaf-epc-uid))
        (sessions (eaf-epc-manager-sessions mngr))
        (d (eaf-deferred-new)))
    (push (cons uid d) sessions)
    (setf (eaf-epc-manager-sessions mngr) sessions)
    (eaf-epc-manager-send mngr 'call uid method-name args)
    d))

(defun eaf-epc-define-method (mngr method-name task &optional arg-specs docstring)
  "Define a method and return a deferred object which is called by the peer."
  (let* ((method (make-eaf-epc-method
                  :name method-name :task task
                  :arg-specs arg-specs :docstring docstring))
         (methods (cons method (eaf-epc-manager-methods mngr))))
    (setf (eaf-epc-manager-methods mngr) methods)
    method))

(defun eaf-epc-sync (mngr d)
  "Wrap deferred methods with synchronous waiting, and return the result.
If an exception is occurred, this function throws the error."
  (let ((result 'eaf-epc-nothing))
    (eaf-deferred-chain
     d
     (eaf-deferred-nextc it
       (lambda (x) (setq result x)))
     (eaf-deferred-error it
       (lambda (er) (setq result (cons 'error er)))))
    (while (eq result 'eaf-epc-nothing)
      (save-current-buffer
        (accept-process-output
         (eaf-epc-connection-process (eaf-epc-manager-connection mngr))
         0 eaf-epc-accept-process-timeout t)))
    (if (and (consp result) (eq 'error (car result)))
        (error (cdr result)) result)))

(defun eaf-epc-call-sync (mngr method-name args)
  "Call peer's method with args synchronously and return the result.
If an exception is occurred, this function throws the error."
  (eaf-epc-sync mngr (eaf-epc-call-deferred mngr method-name args)))

(defun eaf-epc-live-p (mngr)
  "Return non-nil when MNGR is an EPC manager object with a live
connection."
  (let ((proc (ignore-errors
                (eaf-epc-connection-process (eaf-epc-manager-connection mngr)))))
    (and (processp proc)
         ;; Same as `process-live-p' in Emacs >= 24:
         (memq (process-status proc) '(run open listen connect stop)))))

;; epcs
(defvar eaf-epc-server-client-processes nil
  "[internal] A list of ([process object] . [`eaf-epc-manager' instance]).
When the server process accepts the client connection, the
`eaf-epc-manager' instance is created and stored in this variable
`eaf-epc-server-client-processes'. This variable is used for the management
purpose.")

;; eaf-epc-server
;;   name    : process name (string)   ex: "EPC Server 1"
;;   process : server process object
;;   port    : port number
;;   connect-function : initialize function for `eaf-epc-manager' instances
(cl-defstruct eaf-epc-server name process port connect-function)

(defvar eaf-epc-server-processes nil
  "[internal] A list of ([process object] . [`eaf-epc-server' instance]).
This variable is used for the management purpose.")

(defun eaf-epc-server-get-manager-by-process (proc)
  "[internal] Return the eaf-epc-manager instance for the PROC."
  (cl-loop for (pp . mngr) in eaf-epc-server-client-processes
           if (eql pp proc)
           do (cl-return mngr)
           finally return nil))

(defun eaf-epc-server-accept (process)
  "[internal] Initialize the process and return eaf-epc-manager object."
  (eaf-epc-log "EAF-EPC-SERVER- >> Connection accept: %S" process)
  (let* ((connection-id (eaf-epc-uid))
         (connection-name (format "eaf-epc con %s" connection-id))
         (channel (list connection-name nil))
         (connection (make-eaf-epc-connection
                      :name connection-name
                      :process process
                      :buffer (process-buffer process)
                      :channel channel)))
    (eaf-epc-log "EAF-EPC-SERVER- >> Connection establish")
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process
                        (lambda (p m)
                          (eaf-epc-process-filter connection p m)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process
                          (lambda (p e)
                            (eaf-epc-process-sentinel connection p e)))
    (make-eaf-epc-manager :server-process process :port t
                          :connection connection)))

(defun eaf-epc-server-sentinel (process message connect-function)
  "[internal] Process sentinel handler for the server process."
  (eaf-epc-log "EAF-EPC-SERVER- SENTINEL: %S %S" process message)
  (let ((mngr (eaf-epc-server-get-manager-by-process process)))
    (cond
     ;; new connection
     ((and (string-match "open" message) (null mngr))
      (condition-case err
          (let ((mngr (eaf-epc-server-accept process)))
            (push (cons process mngr) eaf-epc-server-client-processes)
            (eaf-epc-init-epc-layer mngr)
            (when connect-function (funcall connect-function mngr))
            mngr)
        ('error
         (eaf-epc-log "EAF-EPC-SERVER- Protocol error: %S" err)
         (eaf-epc-log "EAF-EPC-SERVER- ABORT %S" process)
         (delete-process process))))
     ;; ignore
     ((null mngr) nil )
     ;; disconnect
     (t
      (let ((pair (assq process eaf-epc-server-client-processes)) _d)
        (when pair
          (eaf-epc-log "EAF-EPC-SERVER- DISCONNECT %S" process)
          (eaf-epc-stop-epc (cdr pair))
          (setq eaf-epc-server-client-processes
                (assq-delete-all process eaf-epc-server-client-processes))
          ))
      nil))))

(defun eaf-epc-server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (let*
      ((connect-function connect-function)
       (name (format "EAF EPC Server %s" (eaf-epc-uid)))
       (buf (eaf-epc-make-procbuf (format " *%s*" name)))
       (main-process
        (make-network-process
         :name name
         :buffer buf
         :family 'ipv4
         :server t
         :host "127.0.0.1"
         :service (or port t)
         :noquery t
         :sentinel
         (lambda (process message)
           (eaf-epc-server-sentinel process message connect-function)))))
    (push (cons main-process
                (make-eaf-epc-server
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          eaf-epc-server-processes)
    main-process))

(provide 'eaf-epc)
;;; eaf-epc.el ends here
