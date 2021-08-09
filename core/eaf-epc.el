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
(declare-function pp-display-expression 'pp)

(defvar eaf-deferred-version nil "deferred.el version")
(setq eaf-deferred-version "0.5.0")

;;; Code:

(defmacro eaf-deferred-aand (test &rest rest)
  "[internal] Anaphoric AND."
  (declare (debug ("test" form &rest form)))
  `(let ((it ,test))
     (if it ,(if rest `(eaf-deferred-aand ,@rest) 'it))))

(defmacro eaf-deferred-$ (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form)))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

(defmacro eaf-deferred-lambda (args &rest body)
  "Anaphoric lambda macro for self recursion."
  (declare (debug ("args" form &rest form)))
  (let ((argsyms (cl-loop repeat (length args) collect (cl-gensym))))
  `(lambda (,@argsyms)
     (let (self)
       (setq self (lambda( ,@args ) ,@body))
       (funcall self ,@argsyms)))))

(cl-defmacro eaf-deferred-try (d &key catch finally)
  "Try-catch-finally macro. This macro simulates the
try-catch-finally block asynchronously. CATCH and FINALLY can be
nil. Because of asynchrony, this macro does not ensure that the
task FINALLY should be called."
  (let ((chain
         (if catch `((eaf-deferred-error it ,catch)))))
    (when finally
      (setq chain (append chain `((eaf-deferred-watch it ,finally)))))
    `(eaf-deferred-$ ,d ,@chain)))

(defun eaf-deferred-setTimeout (f msec)
  "[internal] Timer function that emulates the `setTimeout' function in JS."
  (run-at-time (/ msec 1000.0) nil f))

(defun eaf-deferred-cancelTimeout (id)
  "[internal] Timer cancellation function that emulates the `cancelTimeout' function in JS."
  (cancel-timer id))

(defun eaf-deferred-run-with-idle-timer (sec f)
  "[internal] Wrapper function for run-with-idle-timer."
  (run-with-idle-timer sec nil f))

(defun eaf-deferred-call-lambda (f &optional arg)
  "[internal] Call a function with one or zero argument safely.
The lambda function can define with zero and one argument."
  (condition-case err
      (funcall f arg)
    ('wrong-number-of-arguments
     (display-warning 'deferred "\
Callback that takes no argument may be specified.
Passing callback with no argument is deprecated.
Callback must take one argument.
Or, this error is coming from somewhere inside of the callback: %S" err)
     (condition-case nil
         (funcall f)
       ('wrong-number-of-arguments
        (signal 'wrong-number-of-arguments (cdr err))))))) ; return the first error

;; debug

(eval-and-compile
  (defvar eaf-deferred-debug nil "Debug output switch."))
(defvar eaf-deferred-debug-count 0 "[internal] Debug output counter.")

(defmacro eaf-deferred-message (&rest args)
  "[internal] Debug log function."
  (when eaf-deferred-debug
    `(progn
       (with-current-buffer (get-buffer-create "*eaf-deferred-debug*")
         (save-excursion
           (goto-char (point-max))
           (insert (format "%5i %s\n" eaf-deferred-debug-count (format ,@args)))))
       (cl-incf eaf-deferred-debug-count))))

(defun eaf-deferred-message-mark ()
  "[internal] Debug log function."
  (interactive)
  (eaf-deferred-message "==================== mark ==== %s"
    (format-time-string "%H:%M:%S" (current-time))))

(defun eaf-deferred-pp (d)
  (require 'pp)
  (eaf-deferred-$
    (eaf-deferred-nextc d
      (lambda (x)
        (pp-display-expression x "*eaf-deferred-pp*")))
    (eaf-deferred-error it
      (lambda (e)
        (pp-display-expression e "*eaf-deferred-pp*")))
    (eaf-deferred-nextc it
      (lambda (_x) (pop-to-buffer "*eaf-deferred-pp*")))))

(defvar eaf-deferred-debug-on-signal nil
"If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(defmacro eaf-deferred-condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`eaf-deferred-debug-on-signal'."
  (declare (debug condition-case)
           (indent 2))
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

(defmacro eaf-deferred-pack (a b c)
  `(cons ,a (cons ,b ,c)))

(defun eaf-deferred-schedule-worker ()
  "[internal] Schedule consuming a deferred task in the execution queue."
  (run-at-time eaf-deferred-tick-time nil 'eaf-deferred-worker))

(defun eaf-deferred-post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`eaf-deferred-queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (push (eaf-deferred-pack d which arg) eaf-deferred-queue)
  (eaf-deferred-message "QUEUE-POST [%s]: %s"
    (length eaf-deferred-queue) (eaf-deferred-pack d which arg))
  (eaf-deferred-schedule-worker)
  d)

(defun eaf-deferred-clear-queue ()
  "Clear the execution queue. For test and debugging."
  (interactive)
  (eaf-deferred-message "QUEUE-CLEAR [%s -> 0]" (length eaf-deferred-queue))
  (setq eaf-deferred-queue nil))

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
         (eaf-deferred-message "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

(defun eaf-deferred-flush-queue! ()
  "Call all deferred tasks synchronously. For test and debugging."
  (let (value)
    (while eaf-deferred-queue
      (setq value (eaf-deferred-worker)))
    value))

(defun eaf-deferred-sync! (d)
  "Wait for the given deferred task. For test and debugging.
Error is raised if it is not processed within deferred chain D."
  (progn
    (let ((last-value 'eaf-deferred-undefined*)
          uncaught-error)
      (eaf-deferred-try
        (eaf-deferred-nextc d
          (lambda (x) (setq last-value x)))
        :catch
        (lambda (err) (setq uncaught-error err)))
      (while (and (eq 'eaf-deferred-undefined* last-value)
                  (not uncaught-error))
        (sit-for 0.05)
        (sleep-for 0.05))
      (when uncaught-error
        (eaf-deferred-resignal uncaught-error))
      last-value)))


;; Struct: deferred
;;
;; callback    : a callback function (default `eaf-deferred-default-callback')
;; errorback   : an errorback function (default `eaf-deferred-default-errorback')
;; cancel      : a canceling function (default `eaf-deferred-default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;;
(cl-defstruct deferred
  (callback 'eaf-deferred-default-callback)
  (errorback 'eaf-deferred-default-errorback)
  (cancel 'eaf-deferred-default-cancel)
  next status value)

(defun eaf-deferred-default-callback (i)
  "[internal] Default callback function."
  (identity i))

(defun eaf-deferred-default-errorback (err)
  "[internal] Default errorback function."
  (eaf-deferred-resignal err))

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
  (eaf-deferred-message "CANCEL : %s" d)
  (setf (deferred-callback d) 'eaf-deferred-default-callback)
  (setf (deferred-errorback d) 'eaf-deferred-default-errorback)
  (setf (deferred-next d) nil)
  d)

(defvar eaf-deferred-onerror nil
  "Default error handler. This value is nil or a function that
  have one argument for the error message.")

(defun eaf-deferred-exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (eaf-deferred-message "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "eaf-deferred-exec-task was given a nil."))
  (let ((callback (if (eq which 'ok)
                      (deferred-callback d)
                    (deferred-errorback d)))
        (next-deferred (deferred-next d)))
    (cond
     (callback
      (eaf-deferred-condition-case err
          (let ((value (eaf-deferred-call-lambda callback arg)))
            (cond
             ((deferred-p value)
              (eaf-deferred-message "WAIT NEST : %s" value)
              (if next-deferred
                  (eaf-deferred-set-next value next-deferred)
                value))
             (t
              (if next-deferred
                  (eaf-deferred-post-task next-deferred 'ok value)
                (setf (deferred-status d) 'ok)
                (setf (deferred-value d) value)
                value))))
        (error
         (cond
          (next-deferred
           (eaf-deferred-post-task next-deferred 'ng err))
          (eaf-deferred-onerror
            (eaf-deferred-call-lambda eaf-deferred-onerror err))
          (t
           (eaf-deferred-message "ERROR : %S" err)
           (message "deferred error : %S" err)
           (setf (deferred-status d) 'ng)
           (setf (deferred-value d) err)
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
  (setf (deferred-next prev) next)
  (cond
   ((eq 'ok (deferred-status prev))
    (setf (deferred-status prev) nil)
    (let ((ret (eaf-deferred-exec-task
                 next 'ok (deferred-value prev))))
      (if (deferred-p ret) ret
        next)))
   ((eq 'ng (deferred-status prev))
    (setf (deferred-status prev) nil)
    (let ((ret (eaf-deferred-exec-task next 'ng (deferred-value prev))))
      (if (deferred-p ret) ret
        next)))
   (t
    next)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic functions for deferred objects

(defun eaf-deferred-new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-deferred :callback callback)
    (make-deferred)))

(defun eaf-deferred-callback (d &optional arg)
  "Start deferred chain with a callback message."
  (eaf-deferred-exec-task d 'ok arg))

(defun eaf-deferred-errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (eaf-deferred-exec-task d 'ng arg))

(defun eaf-deferred-callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (eaf-deferred-post-task d 'ok arg))

(defun eaf-deferred-errorback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (eaf-deferred-post-task d 'ng arg))

(defun eaf-deferred-cancel (d)
  "Cancel all callbacks and deferred chain in the deferred object."
  (eaf-deferred-message "CANCEL : %s" d)
  (funcall (deferred-cancel d) d)
  d)

(defun eaf-deferred-status (d)
  "Return a current status of the deferred object. The returned value means following:
`ok': the callback was called and waiting for next deferred.
`ng': the errorback was called and waiting for next deferred.
 nil: The neither callback nor errorback was not called."
  (deferred-status d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic utility functions

(defun eaf-deferred-succeed (&optional arg)
  "Create a synchronous deferred object."
  (let ((d (eaf-deferred-new)))
    (eaf-deferred-exec-task d 'ok arg)
    d))

(defun eaf-deferred-fail (&optional arg)
  "Create a synchronous deferred object."
  (let ((d (eaf-deferred-new)))
    (eaf-deferred-exec-task d 'ng arg)
    d))

(defun eaf-deferred-next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (eaf-deferred-callback-post (eaf-deferred-new callback))."
  (let ((d (if callback
               (make-deferred :callback callback)
             (make-deferred))))
    (eaf-deferred-callback-post d arg)
    d))

(defun eaf-deferred-nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (let ((nd (make-deferred :callback callback)))
    (eaf-deferred-set-next d nd)))

(defun eaf-deferred-error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (let ((nd (make-deferred :errorback callback)))
    (eaf-deferred-set-next d nd)))

(defun eaf-deferred-watch (d callback)
  "Create a deferred object with watch task and connect it to the given deferred object.
The watch task CALLBACK can not affect deferred chains with
return values. This function is used in following purposes,
simulation of try-finally block in asynchronous tasks, progress
monitoring of tasks."
  (let* ((callback callback)
         (normal (lambda (x) (ignore-errors (eaf-deferred-call-lambda callback x)) x))
         (err    (lambda (e)
                   (ignore-errors (eaf-deferred-call-lambda callback e))
                   (eaf-deferred-resignal e))))
    (let ((nd (make-deferred :callback normal :errorback err)))
      (eaf-deferred-set-next d nd))))

(defun eaf-deferred-wait (msec)
  "Return a deferred object scheduled at MSEC millisecond later."
  (let ((d (eaf-deferred-new)) (start-time (float-time)) timer)
    (eaf-deferred-message "WAIT : %s" msec)
    (setq timer (eaf-deferred-setTimeout
                  (lambda ()
                    (eaf-deferred-exec-task d 'ok
                      (* 1000.0 (- (float-time) start-time)))
                    nil) msec))
    (setf (deferred-cancel d)
          (lambda (x)
            (eaf-deferred-cancelTimeout timer)
            (eaf-deferred-default-cancel x)))
    d))

(defun eaf-deferred-wait-idle (msec)
  "Return a deferred object which will run when Emacs has been
idle for MSEC millisecond."
  (let ((d (eaf-deferred-new)) (start-time (float-time)) timer)
    (eaf-deferred-message "WAIT-IDLE : %s" msec)
    (setq timer
          (eaf-deferred-run-with-idle-timer
            (/ msec 1000.0)
            (lambda ()
              (eaf-deferred-exec-task d 'ok
                (* 1000.0 (- (float-time) start-time)))
              nil)))
    (setf (deferred-cancel d)
          (lambda (x)
            (eaf-deferred-cancelTimeout timer)
            (eaf-deferred-default-cancel x)))
    d))

(defun eaf-deferred-call (f &rest args)
  "Call the given function asynchronously."
  (eaf-deferred-next
    (lambda (_x)
      (apply f args))))

(defun eaf-deferred-apply (f &optional args)
  "Call the given function asynchronously."
  (eaf-deferred-next
    (lambda (_x)
      (apply f args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

(defun eaf-deferred-empty-p (times-or-seq)
  "[internal] Return non-nil if TIMES-OR-SEQ is the number zero or nil."
  (or (and (numberp times-or-seq) (<= times-or-seq 0))
      (and (sequencep times-or-seq) (= (length times-or-seq) 0))))

(defun eaf-deferred-loop (times-or-seq func)
  "Return a iteration deferred object."
  (eaf-deferred-message "LOOP : %s" times-or-seq)
  (if (eaf-deferred-empty-p times-or-seq) (eaf-deferred-next)
    (let* (items (rd
                  (cond
                   ((numberp times-or-seq)
                    (cl-loop for i from 0 below times-or-seq
                             with ld = (eaf-deferred-next)
                             do
                             (push ld items)
                             (setq ld
                                   (let ((i i))
                                     (eaf-deferred-nextc ld
                                       (lambda (_x) (eaf-deferred-call-lambda func i)))))
                             finally return ld))
                   ((sequencep times-or-seq)
                    (cl-loop for i in (append times-or-seq nil) ; seq->list
                             with ld = (eaf-deferred-next)
                             do
                             (push ld items)
                             (setq ld
                                   (let ((i i))
                                     (eaf-deferred-nextc ld
                                       (lambda (_x) (eaf-deferred-call-lambda func i)))))
                             finally return ld)))))
      (setf (deferred-cancel rd)
            (lambda (x) (eaf-deferred-default-cancel x)
              (cl-loop for i in items
                       do (eaf-deferred-cancel i))))
      rd)))

(defun eaf-deferred-trans-multi-args (args self-func list-func main-func)
  "[internal] Check the argument values and dispatch to methods."
  (cond
   ((and (= 1 (length args)) (consp (car args)) (not (functionp (car args))))
    (let ((lst (car args)))
      (cond
       ((or (null lst) (null (car lst)))
        (eaf-deferred-next))
       ((eaf-deferred-aand lst (car it) (or (functionp it) (deferred-p it)))
        ;; a list of deferred objects
        (funcall list-func lst))
       ((eaf-deferred-aand lst (consp it))
        ;; an alist of deferred objects
        (funcall main-func lst))
       (t (error "Wrong argument type. %s" args)))))
   (t (funcall self-func args))))

(defun eaf-deferred-parallel-array-to-alist (lst)
  "[internal] Translation array to alist."
  (cl-loop for d in lst
           for i from 0 below (length lst)
           collect (cons i d)))

(defun eaf-deferred-parallel-alist-to-array (alst)
  "[internal] Translation alist to array."
  (cl-loop for pair in
           (sort alst (lambda (x y)
                        (< (car x) (car y))))
           collect (cdr pair)))

(defun eaf-deferred-parallel-func-to-deferred (alst)
  "[internal] Normalization for parallel and earlier arguments."
  (cl-loop for pair in alst
           for d = (cdr pair)
           collect
           (progn
             (unless (deferred-p d)
               (setf (cdr pair) (eaf-deferred-next d)))
             pair)))

(defun eaf-deferred-parallel-main (alst)
  "[internal] Deferred alist implementation for `eaf-deferred-parallel'. "
  (eaf-deferred-message "PARALLEL<KEY . VALUE>" )
  (let ((nd (eaf-deferred-new))
        (len (length alst))
        values)
    (cl-loop for pair in
             (eaf-deferred-parallel-func-to-deferred alst)
             with cd ; current child deferred
             do
             (let ((name (car pair)))
               (setq cd
                     (eaf-deferred-nextc (cdr pair)
                       (lambda (x)
                         (push (cons name x) values)
                         (eaf-deferred-message "PARALLEL VALUE [%s/%s] %s"
                           (length values) len (cons name x))
                         (when (= len (length values))
                           (eaf-deferred-message "PARALLEL COLLECTED")
                           (eaf-deferred-post-task nd 'ok (nreverse values)))
                         nil)))
               (eaf-deferred-error cd
                 (lambda (e)
                   (push (cons name e) values)
                   (eaf-deferred-message "PARALLEL ERROR [%s/%s] %s"
                     (length values) len (cons name e))
                   (when (= (length values) len)
                     (eaf-deferred-message "PARALLEL COLLECTED")
                     (eaf-deferred-post-task nd 'ok (nreverse values)))
                   nil))))
    nd))

(defun eaf-deferred-parallel-list (lst)
  "[internal] Deferred list implementation for `eaf-deferred-parallel'. "
  (eaf-deferred-message "PARALLEL<LIST>" )
  (let* ((pd (eaf-deferred-parallel-main (eaf-deferred-parallel-array-to-alist lst)))
         (rd (eaf-deferred-nextc pd 'eaf-deferred-parallel-alist-to-array)))
    (setf (deferred-cancel rd)
          (lambda (x) (eaf-deferred-default-cancel x)
            (eaf-deferred-cancel pd)))
    rd))

(defun eaf-deferred-parallel (&rest args)
  "Return a deferred object that calls given deferred objects or
functions in parallel and wait for all callbacks. The following
deferred task will be called with an array of the return
values. ARGS can be a list or an alist of deferred objects or
functions."
  (eaf-deferred-message "PARALLEL : %s" args)
  (eaf-deferred-trans-multi-args args
    'eaf-deferred-parallel 'eaf-deferred-parallel-list 'eaf-deferred-parallel-main))

(defun eaf-deferred-earlier-main (alst)
  "[internal] Deferred alist implementation for `eaf-deferred-earlier'. "
  (eaf-deferred-message "EARLIER<KEY . VALUE>" )
  (let ((nd (eaf-deferred-new))
        (len (length alst))
        value results)
    (cl-loop for pair in
             (eaf-deferred-parallel-func-to-deferred alst)
             with cd ; current child deferred
             do
             (let ((name (car pair)))
               (setq cd
                     (eaf-deferred-nextc (cdr pair)
                       (lambda (x)
                         (push (cons name x) results)
                         (cond
                          ((null value)
                           (setq value (cons name x))
                           (eaf-deferred-message "EARLIER VALUE %s" (cons name value))
                           (eaf-deferred-post-task nd 'ok value))
                          (t
                           (eaf-deferred-message "EARLIER MISS [%s/%s] %s" (length results) len (cons name value))
                           (when (eql (length results) len)
                             (eaf-deferred-message "EARLIER COLLECTED"))))
                         nil)))
               (eaf-deferred-error cd
                 (lambda (e)
                   (push (cons name e) results)
                   (eaf-deferred-message "EARLIER ERROR [%s/%s] %s" (length results) len (cons name e))
                   (when (and (eql (length results) len) (null value))
                     (eaf-deferred-message "EARLIER FAILED")
                     (eaf-deferred-post-task nd 'ok nil))
                   nil))))
    nd))

(defun eaf-deferred-earlier-list (lst)
  "[internal] Deferred list implementation for `eaf-deferred-earlier'. "
  (eaf-deferred-message "EARLIER<LIST>" )
  (let* ((pd (eaf-deferred-earlier-main (eaf-deferred-parallel-array-to-alist lst)))
         (rd (eaf-deferred-nextc pd (lambda (x) (cdr x)))))
    (setf (deferred-cancel rd)
          (lambda (x) (eaf-deferred-default-cancel x)
            (eaf-deferred-cancel pd)))
    rd))


(defun eaf-deferred-earlier (&rest args)
  "Return a deferred object that calls given deferred objects or
functions in parallel and wait for the first callback. The
following deferred task will be called with the first return
value. ARGS can be a list or an alist of deferred objects or
functions."
  (eaf-deferred-message "EARLIER : %s" args)
  (eaf-deferred-trans-multi-args args
    'eaf-deferred-earlier 'eaf-deferred-earlier-list 'eaf-deferred-earlier-main))

(defmacro eaf-deferred-timeout (timeout-msec timeout-form d)
  "Time out macro on a deferred task D.  If the deferred task D
does not complete within TIMEOUT-MSEC, this macro cancels the
deferred task and return the TIMEOUT-FORM."
  `(eaf-deferred-earlier
     (eaf-deferred-nextc (eaf-deferred-wait ,timeout-msec)
       (lambda (x) ,timeout-form))
     ,d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application functions

(defvar eaf-deferred-uid 0 "[internal] Sequence number for some utilities. See the function `eaf-deferred-uid'.")

(defun eaf-deferred-uid ()
  "[internal] Generate a sequence number."
  (cl-incf eaf-deferred-uid))

(defun eaf-deferred-buffer-string (strformat buf)
  "[internal] Return a string in the buffer with the given format."
  (format strformat
          (with-current-buffer buf (buffer-string))))

(defun eaf-deferred-process (command &rest args)
  "A deferred wrapper of `start-process'. Return a deferred
object. The process name and buffer name of the argument of the
`start-process' are generated by this function automatically.
The next deferred object receives stdout and stderr string from
the command process."
  (eaf-deferred-process-gen 'start-process command args))

(defun eaf-deferred-process-shell (command &rest args)
  "A deferred wrapper of `start-process-shell-command'. Return a deferred
object. The process name and buffer name of the argument of the
`start-process-shell-command' are generated by this function automatically.
The next deferred object receives stdout and stderr string from
the command process."
  (eaf-deferred-process-gen 'start-process-shell-command command args))

(defun eaf-deferred-process-buffer (command &rest args)
  "A deferred wrapper of `start-process'. Return a deferred
object. The process name and buffer name of the argument of the
`start-process' are generated by this function automatically.
The next deferred object receives stdout and stderr buffer from
the command process."
  (eaf-deferred-process-buffer-gen 'start-process command args))

(defun eaf-deferred-process-shell-buffer (command &rest args)
  "A deferred wrapper of `start-process-shell-command'. Return a deferred
object. The process name and buffer name of the argument of the
`start-process-shell-command' are generated by this function automatically.
The next deferred object receives stdout and stderr buffer from
the command process."
  (eaf-deferred-process-buffer-gen 'start-process-shell-command command args))

(defun eaf-deferred-process-gen (f command args)
  "[internal]"
  (let ((pd (eaf-deferred-process-buffer-gen f command args)) d)
    (setq d (eaf-deferred-nextc pd
              (lambda (buf)
                (prog1
                    (with-current-buffer buf (buffer-string))
                  (kill-buffer buf)))))
    (setf (deferred-cancel d)
          (lambda (_x)
            (eaf-deferred-default-cancel d)
            (eaf-deferred-default-cancel pd)))
    d))

(defun eaf-deferred-process-buffer-gen (f command args)
  "[internal]"
  (let ((d (eaf-deferred-next)) (uid (eaf-deferred-uid)))
    (let ((proc-name (format "*eaf-deferred-*%s*:%s" command uid))
          (buf-name (format " *eaf-deferred-*%s*:%s" command uid))
          (pwd default-directory)
          (env process-environment)
          (con-type process-connection-type)
          (nd (eaf-deferred-new)) proc-buf proc)
      (eaf-deferred-nextc d
        (lambda (_x)
          (setq proc-buf (get-buffer-create buf-name))
          (condition-case err
              (let ((default-directory pwd)
                    (process-environment env)
                    (process-connection-type con-type))
                (setq proc
                      (if (null (car args))
                          (apply f proc-name buf-name command nil)
                        (apply f proc-name buf-name command args)))
                (set-process-sentinel
                 proc
                 (lambda (proc event)
		   (unless (process-live-p proc)
		     (if (zerop (process-exit-status proc))
			 (eaf-deferred-post-task nd 'ok proc-buf)
		       (let ((msg (format "Deferred process exited abnormally:\n  command: %s\n  exit status: %s %s\n  event: %s\n  buffer contents: %S"
					  command
					  (process-status proc)
					  (process-exit-status proc)
					  (string-trim-right event)
					  (if (buffer-live-p proc-buf)
					      (with-current-buffer proc-buf
						(buffer-string))
					    "(unavailable)"))))
			 (kill-buffer proc-buf)
			 (eaf-deferred-post-task nd 'ng msg))))))
		(setf (deferred-cancel nd)
		      (lambda (x) (eaf-deferred-default-cancel x)
			(when proc
			  (kill-process proc)
			  (kill-buffer proc-buf)))))
	    (error (eaf-deferred-post-task nd 'ng err)))
	  nil))
      nd)))

(defmacro eaf-deferred-processc (d command &rest args)
  "Process chain of `eaf-deferred-process'."
  `(eaf-deferred-nextc ,d
    (lambda (,(cl-gensym)) (eaf-deferred-process ,command ,@args))))

(defmacro eaf-deferred-process-bufferc (d command &rest args)
  "Process chain of `eaf-deferred-process-buffer'."
  `(eaf-deferred-nextc ,d
     (lambda (,(cl-gensym)) (eaf-deferred-process-buffer ,command ,@args))))

(defmacro eaf-deferred-process-shellc (d command &rest args)
  "Process chain of `eaf-deferred-process'."
  `(eaf-deferred-nextc ,d
    (lambda (,(cl-gensym)) (eaf-deferred-process-shell ,command ,@args))))

(defmacro eaf-deferred-process-shell-bufferc (d command &rest args)
  "Process chain of `eaf-deferred-process-buffer'."
  `(eaf-deferred-nextc ,d
     (lambda (,(cl-gensym)) (eaf-deferred-process-shell-buffer ,command ,@args))))

;; Special variables defined in url-vars.el.
(defvar url-request-data)
(defvar url-request-method)
(defvar url-request-extra-headers)

(declare-function url-http-symbol-value-in-buffer "url-http"
                  (symbol buffer &optional unbound-value))

(declare-function eaf-deferred-url-param-serialize "request" (params))

(declare-function eaf-deferred-url-escape "request" (val))

(eval-after-load "url"
  ;; for url package
  ;; TODO: proxy, charaset
  ;; List of gloabl variables to preserve and restore before url-retrieve call
  '(let ((url-global-variables '(url-request-data
                                 url-request-method
                                 url-request-extra-headers)))

     (defun eaf-deferred-url-retrieve (url &optional cbargs silent inhibit-cookies)
       "A wrapper function for url-retrieve. The next deferred
object receives the buffer object that URL will load
into. Values of dynamically bound 'url-request-data', 'url-request-method' and
'url-request-extra-headers' are passed to url-retrieve call."
       (let ((nd (eaf-deferred-new))
             buf
             (local-values (mapcar (lambda (symbol) (symbol-value symbol)) url-global-variables)))
         (eaf-deferred-next
           (lambda (_x)
             (cl-progv url-global-variables local-values
               (condition-case err
                   (setq buf
                         (url-retrieve
                          url (lambda (_xx) (eaf-deferred-post-task nd 'ok buf))
                          cbargs silent inhibit-cookies))
                 (error (eaf-deferred-post-task nd 'ng err)))
             nil)))
         (setf (deferred-cancel nd)
               (lambda (_x)
                 (when (buffer-live-p buf)
                   (kill-buffer buf))))
         nd))

     (defun eaf-deferred-url-delete-header (buf)
       (with-current-buffer buf
         (let ((pos (url-http-symbol-value-in-buffer
                     'url-http-end-of-headers buf)))
           (when pos
             (delete-region (point-min) (1+ pos)))))
       buf)

     (defun eaf-deferred-url-delete-buffer (buf)
       (when (and buf (buffer-live-p buf))
         (kill-buffer buf))
       nil)

     (defun eaf-deferred-url-get (url &optional params &rest args)
       "Perform a HTTP GET method with `url-retrieve'. PARAMS is
a parameter list of (key . value) or key. ARGS will be appended
to eaf-deferred-url-retrieve args list. The next deferred
object receives the buffer object that URL will load into."
       (when params
         (setq url
               (concat url "?" (eaf-deferred-url-param-serialize params))))
       (let ((d (eaf-deferred-$
                  (apply 'eaf-deferred-url-retrieve url args)
                  (eaf-deferred-nextc it 'eaf-deferred-url-delete-header))))
         (eaf-deferred-set-next
           d (eaf-deferred-new 'eaf-deferred-url-delete-buffer))
         d))

     (defun eaf-deferred-url-post (url &optional params &rest args)
       "Perform a HTTP POST method with `url-retrieve'. PARAMS is
a parameter list of (key . value) or key. ARGS will be appended
to eaf-deferred-url-retrieve args list. The next deferred
object receives the buffer object that URL will load into."
       (let ((url-request-method "POST")
             (url-request-extra-headers
              (append url-request-extra-headers
                      '(("Content-Type" . "application/x-www-form-urlencoded"))))
             (url-request-data (eaf-deferred-url-param-serialize params)))
         (let ((d (eaf-deferred-$
                    (apply 'eaf-deferred-url-retrieve url args)
                    (eaf-deferred-nextc it 'eaf-deferred-url-delete-header))))
           (eaf-deferred-set-next
             d (eaf-deferred-new 'eaf-deferred-url-delete-buffer))
           d)))

     (defun eaf-deferred-url-escape (val)
       "[internal] Return a new string that is VAL URI-encoded."
       (unless (stringp val)
         (setq val (format "%s" val)))
       (url-hexify-string
        (encode-coding-string val 'utf-8)))

     (defun eaf-deferred-url-param-serialize (params)
       "[internal] Serialize a list of (key . value) cons cells
into a query string."
       (when params
         (mapconcat
          'identity
          (cl-loop for p in params
                   collect
                   (cond
                    ((consp p)
                     (concat
                      (eaf-deferred-url-escape (car p)) "="
                      (eaf-deferred-url-escape (cdr p))))
                    (t
                     (eaf-deferred-url-escape p))))
          "&")))
     ))

;;==================================================
;; Utility

(defvar eaf-epc-debug-out nil)
(defvar eaf-epc-debug-buffer "*epc log*")

(defvar eaf-epc-mngr)

;;(setq eaf-epc-debug-out t)
;;(setq eaf-epc-debug-out nil)

(defun eaf-epc-log-init ()
  (when (get-buffer eaf-epc-debug-buffer)
    (kill-buffer eaf-epc-debug-buffer)))

(defun eaf-epc-log (&rest args)
  (when eaf-epc-debug-out
    (with-current-buffer
        (get-buffer-create eaf-epc-debug-buffer)
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert (apply 'format args) "\n"))))

(defun eaf-epc-make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))

(defun eaf-epc-document-function (function docstring)
  "Document FUNCTION with DOCSTRING.  Use this for `defstruct' accessor etc."
  (put function 'function-documentation docstring))
(put 'eaf-epc-document-function 'lisp-indent-function 'defun)
(put 'eaf-epc-document-function 'doc-string-elt 2)

;;==================================================
;; Low Level Interface

(defvar eaf-epc-uid 1)

(defun eaf-epc-uid ()
  (cl-incf eaf-epc-uid))

(defvar eaf-epc-accept-process-timeout 150  "Asynchronous timeout time. (msec)")
(defvar eaf-epc-accept-process-timeout-count 100 " Startup function waits (`eaf-epc-accept-process-timeout' * `eaf-epc-accept-process-timeout-count') msec for the external process getting ready.")

(put 'epc-error 'error-conditions '(error epc-error))
(put 'epc-error 'error-message "EPC Error")

(cl-defstruct eaf-epc-connection
  "Set of information for network connection and event handling.

name    : Connection name. This name is used for process and buffer names.
process : Connection process object.
buffer  : Working buffer for the incoming data.
channel : Event channels for incoming messages."
  name process buffer channel)

(eaf-epc-document-function 'eaf-epc-connection-name
  "[internal] Connection name. This name is used for process and buffer names.

\(fn EAF-EPC-CONNECTION)")

(eaf-epc-document-function 'eaf-epc-connection-process
  "[internal] Connection process object.

\(fn EAF-EPC-CONNECTION)")

(eaf-epc-document-function 'eaf-epc-connection-buffer
  "[internal] Working buffer for the incoming data.

\(fn EAF-EPC-CONNECTION)")

(eaf-epc-document-function 'eaf-epc-connection-channel
  "[internal] Event channels for incoming messages.

\(fn EAF-EPC-CONNECTION)")

(defun eaf-epc-connect (host port)
  "[internal] Connect the server, initialize the process and
return eaf-epc-connection object."
  (eaf-epc-log ">> Connection start: %s:%s" host port)
  (let* ((connection-id (eaf-epc-uid))
                 (connection-name (format "epc con %s" connection-id))
                 (connection-buf (eaf-epc-make-procbuf (format "*%s*" connection-name)))
                 (connection-process
                  (open-network-stream connection-name connection-buf host port))
                 (channel (eaf-concurrent-signal-channel connection-name))
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
         (string (concat (eaf-epc-net-encode-length (length msg)) msg))
         (proc (eaf-epc-connection-process connection)))
    (eaf-epc-log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun eaf-epc-disconnect (connection)
  (let
      ((process (eaf-epc-connection-process connection))
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
                  (apply 'eaf-concurrent-signal-send
                         (cons (eaf-epc-connection-channel connection) event))
                  (setq ok t))
              ('error (eaf-epc-log "MsgError: %S / <= %S" err event)))
          (unless ok
            (eaf-epc-run-when-idle 'eaf-epc-process-available-input connection process)))))))

(defun eaf-epc-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (eaf-epc-net-decode-length))))

(defun eaf-epc-run-when-idle (function &rest args)
  "Call FUNCTION as soon as Emacs is idle."
  (apply #'run-at-time
         (if (featurep 'xemacs) itimer-short-interval 0)
         nil function args))

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
         (end (+ start length)) _content)
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun eaf-epc-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun eaf-epc-net-encode-length (n)
  "Encode an integer into a 24-bit hex string."
  (format "%06x" n))

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

;;==================================================
;; High Level Interface

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

(eaf-epc-document-function 'eaf-epc-manager-title
  "Instance name (string) for displaying on the `eaf-epc-controller' UI

You can modify this slot using `setf' to change the title column
in the `eaf-epc-controller' table UI.

\(fn EAF-EPC-MANAGER)")

(eaf-epc-document-function 'eaf-epc-manager-server-process
  "Process object for the peer.

This is *not* network process but the external program started by
`eaf-epc-start-epc'.  For network process, see `eaf-epc-connection-process'.

\(fn EAF-EPC-MANAGER)")

(eaf-epc-document-function 'eaf-epc-manager-commands
  "[internal] a list of (prog . args)

\(fn EAF-EPC-MANAGER)")

(eaf-epc-document-function 'eaf-epc-manager-port
  "Port number (integer).

\(fn EAF-EPC-MANAGER)")

(eaf-epc-document-function 'eaf-epc-manager-connection
  "[internal] eaf-epc-connection instance

\(fn EAF-EPC-MANAGER)")

(eaf-epc-document-function 'eaf-epc-manager-methods
  "[internal] alist of method (name . function)

\(fn EAF-EPC-MANAGER)")

(eaf-epc-document-function 'eaf-epc-manager-sessions
  "[internal] alist of session (id . deferred)

\(fn EAF-EPC-MANAGER)")

(cl-defstruct eaf-epc-method
  "Object to hold serving method information.

name       : method name (symbol)   ex: 'test
task       : method function (function with one argument)
arg-specs  : arg-specs (one string) ex: \"(A B C D)\"
docstring  : docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"
"
  name task docstring arg-specs)

(eaf-epc-document-function 'eaf-epc-method-name
  "[internal] method name (symbol)   ex: 'test

\(fn EAF-EPC-METHOD)")

(eaf-epc-document-function 'eaf-epc-method-task
  "[internal] method function (function with one argument)

\(fn EAF-EPC-METHOD)")

(eaf-epc-document-function 'eaf-epc-method-arg-specs
  "[internal] arg-specs (one string) ex: \"(A B C D)\"

\(fn EAF-EPC-METHOD)")

(eaf-epc-document-function 'eaf-epc-method-docstring
  "[internal] docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"

\(fn EAF-EPC-METHOD)")

(defvar eaf-epc-live-connections nil
  "[internal] A list of `eaf-epc-manager' objects those currently connect to the epc peer.
This variable is for debug purpose.")

(defun eaf-epc-live-connections-add (mngr)
  "[internal] Add the EPC manager object."
  (push mngr eaf-epc-live-connections))

(defun eaf-epc-live-connections-delete (mngr)
  "[internal] Remove the EPC manager object."
  (setq eaf-epc-live-connections (delete mngr eaf-epc-live-connections)))

(defun eaf-epc-start-epc (server-prog server-args)
  "Start the epc server program and return an eaf-epc-manager object.

Start server program SERVER-PROG with command line arguments
SERVER-ARGS.  The server program must print out the port it is
using at the first line of its stdout.  If the server prints out
non-numeric value in the first line or does not print out the
port number in three seconds, it is regarded as start-up
failure."
  (let ((mngr (eaf-epc-start-server server-prog server-args)))
    (eaf-epc-init-epc-layer mngr)
    mngr))

(defun eaf-epc-start-epc-deferred (server-prog server-args)
  "Deferred version of `eaf-epc-start-epc'"
  (eaf-deferred-nextc (eaf-epc-start-server-deferred server-prog server-args)
    #'eaf-epc-init-epc-layer))

(defun eaf-epc-server-process-name (uid)
  (format "eaf-epc-server:%s" uid))

(defun eaf-epc-server-buffer-name (uid)
  (format " *%s*" (eaf-epc-server-process-name uid)))

(defun eaf-epc-start-server (server-prog server-args)
  "[internal] Start a peer server and return an eaf-epc-manager instance which is set up partially."
  (let* ((uid (eaf-epc-uid))
         (process-name (eaf-epc-server-process-name uid))
         (process-buffer (get-buffer-create (eaf-epc-server-buffer-name uid)))
         (process (apply 'start-process
                         process-name process-buffer
                         server-prog server-args))
         (cont 1) port)
    (while cont
      (accept-process-output process 0 eaf-epc-accept-process-timeout t)
      (let ((port-str (with-current-buffer process-buffer
                          (buffer-string))))
        (cond
         ((string-match "^[ \n\r]*[0-9]+[ \n\r]*$" port-str)
          (setq port (string-to-number port-str)
                cont nil))
         ((< 0 (length port-str))
          (error "Server may raise an error. \
Use \"M-x eaf-epc-pop-to-last-server-process-buffer RET\" \
to see full traceback:\n%s" port-str))
         ((not (eq 'run (process-status process)))
          (setq cont nil))
         (t
          (cl-incf cont)
          (when (< eaf-epc-accept-process-timeout-count cont) ; timeout 15 seconds
            (error "Timeout server response."))))))
    (set-process-query-on-exit-flag process nil)
    (make-eaf-epc-manager :server-process process
                      :commands (cons server-prog server-args)
                      :title (mapconcat 'identity (cons server-prog server-args) " ")
                      :port port
                      :connection (eaf-epc-connect "localhost" port))))

(defun eaf-epc-start-server-deferred (server-prog server-args)
  "[internal] Same as `eaf-epc-start-server' but start the server asynchronously."
  (let*
      ((uid (eaf-epc-uid))
       (process-name (eaf-epc-server-process-name uid))
       (process-buffer (get-buffer-create (eaf-epc-server-buffer-name uid)))
       (process (apply 'start-process
                       process-name process-buffer
                       server-prog server-args))
       (mngr (make-eaf-epc-manager
              :server-process process
              :commands (cons server-prog server-args)
              :title (mapconcat 'identity (cons server-prog server-args) " ")))
       (cont 1) port)
    (set-process-query-on-exit-flag process nil)
    (eaf-deferred-$
      (eaf-deferred-next
        (eaf-deferred-lambda (_)
          (accept-process-output process 0 nil t)
          (let ((port-str (with-current-buffer process-buffer
                            (buffer-string))))
            (cond
             ((string-match "^[0-9]+$" port-str)
              (setq port (string-to-number port-str)
                    cont nil))
             ((< 0 (length port-str))
              (error "Server may raise an error. \
Use \"M-x eaf-epc-pop-to-last-server-process-buffer RET\" \
to see full traceback:\n%s" port-str))
             ((not (eq 'run (process-status process)))
              (setq cont nil))
             (t
              (cl-incf cont)
              (when (< eaf-epc-accept-process-timeout-count cont)
                ;; timeout 15 seconds
                (error "Timeout server response."))
              (eaf-deferred-nextc (eaf-deferred-wait eaf-epc-accept-process-timeout)
                self))))))
      (eaf-deferred-nextc it
        (lambda (_)
          (setf (eaf-epc-manager-port mngr) port)
          (setf (eaf-epc-manager-connection mngr) (eaf-epc-connect "localhost" port))
          mngr)))))

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
    (eaf-epc-live-connections-delete mngr)))

(defun eaf-epc-args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond
   ((atom args) args)
   (t (cadr args))))

(defun eaf-epc-init-epc-layer (mngr)
  "[internal] Connect to the server program and return an eaf-epc-connection instance."
  (let*
      ((mngr mngr)
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
              (eaf-concurrent-signal-connect channel method body))
    (eaf-epc-live-connections-add mngr)
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
               ((deferred-p ret)
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
    (eaf-deferred-$ d
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

;;==================================================
;; Troubleshooting / Debugging support

(defun eaf-epc-pop-to-last-server-process-buffer ()
  "Open the buffer for most recently started server program process.
This is useful when you want to check why the server program
failed to start (e.g., to see its traceback / error message)."
  (interactive)
  (let ((buffer (get-buffer (eaf-epc-server-buffer-name eaf-epc-uid))))
    (if buffer
        (pop-to-buffer buffer)
      (error "No buffer for the last server process.  \
Probably the EPC connection exits correctly or you didn't start it yet."))))


;; Concurrent
(defvar eaf-concurrent-version nil "version number")
(setq eaf-concurrent-version "0.3")

;;; Code:


(defmacro eaf-concurrent-aif (test-form then-form &rest else-forms)
  (declare (debug (form form &rest form)))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'eaf-concurrent-aif 'lisp-indent-function 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generator

(defun eaf-concurrent-generator-replace-yield (tree)
  "[internal] Replace `yield' symbols to calling a function in TREE."
  (let (ret)
    (cl-loop for i in tree
             do (cond
                 ((eq i 'yield)
                  (push 'funcall ret)
                  (push i ret))
                 ((listp i)
                  (push (eaf-concurrent-generator-replace-yield i) ret))
                 (t
                  (push i ret))))
    (nreverse ret)))

(defun eaf-concurrent-generator-line (chain line)
  "[internal] Return a macro expansion to execute the sexp LINE
asynchronously."
  (cond
   ;; function object
   ((functionp line)
    `(setq ,chain (eaf-deferred-nextc ,chain ,line)))
   ;; while loop form
   ((eq 'while (car line))
    (let ((condition (cadr line))
          (body (cddr line)))
    `(setq ,chain
      (eaf-deferred-nextc ,chain
        (eaf-deferred-lambda (x)
         (if ,condition
             (eaf-deferred-nextc
               (progn
                 ,@(eaf-concurrent-generator-replace-yield body)) self)))))))
   ;; statement
   (t
    `(setq ,chain
           (eaf-deferred-nextc ,chain
             (eaf-deferred-lambda (x) ,(eaf-concurrent-generator-replace-yield line)))))))

(defmacro eaf-concurrent-generator (callback &rest body)
  "Create a generator object. If BODY has `yield' symbols, it
means calling callback function CALLBACK."
  (let ((chain (cl-gensym))
        (cc (cl-gensym))
        (waiter (cl-gensym)))
    `(let* (,chain
            (,cc ,callback)
            (,waiter (eaf-deferred-new))
            (yield (lambda (x) (funcall ,cc x) ,waiter)))
       (setq ,chain ,waiter)
       ,@(cl-loop for i in body
                  collect
                  (eaf-concurrent-generator-line chain i))
       (lambda () (eaf-deferred-callback ,waiter)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thread

(defun eaf-concurrent-thread-line (wait-time chain line)
  "[internal] Return a macro expansion to execute the sexp LINE asynchronously.
WAIT-TIME is an interval time between tasks.
CHAIN is the previous deferred task."
  (cond
   ;; function object
   ((functionp line)
    `(setq ,chain (eaf-deferred-nextc ,chain ,line)))
   ;; while loop form
   ((eq 'while (car line))
    (let ((condition (cadr line))
          (body (cddr line))
          (retsym (cl-gensym)))
    `(setq ,chain
      (eaf-deferred-nextc ,chain
        (eaf-deferred-lambda (x)
         (if ,condition
             (eaf-deferred-nextc
               (let ((,retsym (progn ,@body)))
                 (if (deferred-p ,retsym) ,retsym
                   (eaf-deferred-wait ,wait-time)))
               self)))))))
   ;; statement
   (t
    `(setq ,chain
           (eaf-deferred-nextc ,chain
             (lambda (x) ,line))))))

(defmacro eaf-concurrent-thread (wait-time-msec &rest body)
  "Return a thread object."
  (let ((chain (cl-gensym))
        (dstart (cl-gensym)))
    `(let* (,chain
            (,dstart (eaf-deferred-new)))
       (setq ,chain ,dstart)
       ,@(cl-loop for i in body
                  collect
                  (eaf-concurrent-thread-line wait-time-msec chain i))
       (eaf-deferred-callback ,dstart))))
(put 'eaf-concurrent-thread 'lisp-indent-function 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semaphore

(cl-defstruct eaf-concurrent-semaphore max-permits permits waiting-deferreds)

(defun eaf-concurrent-semaphore-create(permits-num)
  "Return a semaphore object with PERMITS-NUM permissions."
  (make-eaf-concurrent-semaphore :max-permits permits-num :permits permits-num))

(defun eaf-concurrent-semaphore-acquire(semaphore)
  "Acquire an execution permission and return deferred object to chain.
If this semaphore object has permissions, the subsequent deferred
task is executed immediately.  If this semaphore object has no
permissions, the subsequent deferred task is blocked. After the
permission is returned, the task is executed."
  (cond
   ((< 0 (eaf-concurrent-semaphore-permits semaphore))
    (cl-decf (eaf-concurrent-semaphore-permits semaphore))
    (eaf-deferred-succeed))
   (t
    (let ((d (eaf-deferred-new)))
      (push d (eaf-concurrent-semaphore-waiting-deferreds semaphore))
      d))))

(defun eaf-concurrent-semaphore-release(semaphore)
  "Release an execution permission. The programmer is responsible to return the permissions."
  (when (<= (eaf-concurrent-semaphore-max-permits semaphore)
            (eaf-concurrent-semaphore-permits semaphore))
    (error "Too many calling semaphore-release. [max:%s <= permits:%s]"
           (eaf-concurrent-semaphore-max-permits semaphore)
           (eaf-concurrent-semaphore-permits semaphore)))
  (let ((waiting-deferreds
         (eaf-concurrent-semaphore-waiting-deferreds semaphore)))
    (cond
     (waiting-deferreds
      (let* ((d (car (last waiting-deferreds))))
        (setf (eaf-concurrent-semaphore-waiting-deferreds semaphore)
              (nbutlast waiting-deferreds))
        (eaf-deferred-callback-post d)))
     (t
      (cl-incf (eaf-concurrent-semaphore-permits semaphore)))))
  semaphore)

(defun eaf-concurrent-semaphore-with (semaphore body-func &optional error-func)
  "Execute the task BODY-FUNC asynchronously with the semaphore block."
  (eaf-deferred-try
    (eaf-deferred-nextc (eaf-concurrent-semaphore-acquire semaphore) body-func)
    :catch
    error-func
    :finally
    (lambda (_x) (eaf-concurrent-semaphore-release semaphore))))
(put 'eaf-concurrent-semaphore-with 'lisp-indent-function 1)

(defun eaf-concurrent-semaphore-release-all (semaphore)
  "Release all permissions for resetting the semaphore object.
If the semaphore object has some blocked tasks, this function
return a list of the tasks and clear the list of the blocked
tasks in the semaphore object."
  (setf (eaf-concurrent-semaphore-permits semaphore)
        (eaf-concurrent-semaphore-max-permits semaphore))
  (let ((ds (eaf-concurrent-semaphore-waiting-deferreds semaphore)))
    (when ds
      (setf (eaf-concurrent-semaphore-waiting-deferreds semaphore) nil))
    ds))

(defun eaf-concurrent-semaphore-interrupt-all (semaphore)
  "Clear the list of the blocked tasks in the semaphore and return a deferred object to chain.
This function is used for the interruption cases."
  (when (eaf-concurrent-semaphore-waiting-deferreds semaphore)
    (setf (eaf-concurrent-semaphore-waiting-deferreds semaphore) nil)
    (setf (eaf-concurrent-semaphore-permits semaphore) 0))
  (eaf-concurrent-semaphore-acquire semaphore))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signal / Channel

(defun eaf-concurrent-signal-channel (&optional name parent-channel)
  "Create a channel.
NAME is a channel name for debug.
PARENT-CHANNEL is an upstream channel. The observers of this channel can receive the upstream signals.
In the case of using the function `eaf-concurrent-signal-send', the observers of the upstream channel can not receive the signals of this channel. The function `eaf-concurrent-signal-send-global' can send a signal to the upstream channels from the downstream channels."
  (let ((ch (cons
             (or name (format "signal%s" (eaf-deferred-uid))) ; name for debug
             (cons
              parent-channel ; parent-channel
              nil)))) ; observers
    (when parent-channel
      (eaf-concurrent-signal-connect
       parent-channel
       t (lambda (event)
           (cl-destructuring-bind
               (event-name event-args) event
             (apply 'eaf-concurrent-signal-send
                    ch event-name event-args)))))
    ch))

(defmacro eaf-concurrent-signal-name (ch)
  "[internal] Return signal name."
  `(car ,ch))

(defmacro eaf-concurrent-signal-parent-channel (ch)
  "[internal] Return parent channel object."
  `(cadr ,ch))

(defmacro eaf-concurrent-signal-observers (ch)
  "[internal] Return observers."
  `(cddr ,ch))

(defun eaf-concurrent-signal-connect (channel event-sym &optional callback)
  "Append an observer for EVENT-SYM of CHANNEL and return a deferred object.
If EVENT-SYM is `t', the observer receives all signals of the channel.
If CALLBACK function is given, the deferred object executes the
CALLBACK function asynchronously. One can connect subsequent
tasks to the returned deferred object."
  (let ((d (if callback
               (eaf-deferred-new callback)
             (eaf-deferred-new))))
    (push (cons event-sym d)
          (eaf-concurrent-signal-observers channel))
    d))

(defun eaf-concurrent-signal-send (channel event-sym &rest args)
  "Send a signal to CHANNEL. If ARGS values are given, observers can get the values by following code: (lambda (event) (destructuring-bind (event-sym (args)) event ... )). "
  (let ((observers (eaf-concurrent-signal-observers channel))
        (event (list event-sym args)))
    (cl-loop for i in observers
             for name = (car i)
             for d = (cdr i)
             if (or (eq event-sym name) (eq t name))
             do (eaf-deferred-callback-post d event))))

(defun eaf-concurrent-signal-send-global (channel event-sym &rest args)
  "Send a signal to the most upstream channel. "
  (eaf-concurrent-aif (eaf-concurrent-signal-parent-channel channel)
      (apply 'eaf-concurrent-signal-send-global it event-sym args)
    (apply 'eaf-concurrent-signal-send channel event-sym args)))


(defun eaf-concurrent-signal-disconnect (channel deferred)
  "Remove the observer object DEFERRED from CHANNEL and return
the removed deferred object. "
  (let ((observers (eaf-concurrent-signal-observers channel)) deleted)
    (setf
     (eaf-concurrent-signal-observers channel) ; place
     (cl-loop for i in observers
              for d = (cdr i)
              unless (eq d deferred)
              collect i
              else
              do (push i deleted)))
    deleted))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dataflow

;; Dataflow variable entry
(cl-defstruct eaf-concurrent-dataflow key (value 'eaf-concurrent-dataflow-undefine) deferred-list)

(defun eaf-concurrent-dataflow-undefine-p (obj)
  "[internal] If the variable entry is not bound, return `t'."
  (eq 'eaf-concurrent-dataflow-undefine (eaf-concurrent-dataflow-value obj)))

(defmacro eaf-concurrent-dataflow-parent-environment (df)
  "[internal] Return the parent environment."
  `(car ,df))

(defmacro eaf-concurrent-dataflow-test (df)
  "[internal] Return the test function."
  `(cadr ,df))

(defmacro eaf-concurrent-dataflow-channel (df)
  "[internal] Return the channel object."
  `(cl-caddr ,df))

(defmacro eaf-concurrent-dataflow-list (df)
  "[internal] Return the list of deferred object which are waiting for value binding."
  `(cl-cdddr ,df))

(defun eaf-concurrent-dataflow-environment (&optional parent-env test-func channel)
  "Create a dataflow environment.
PARENT-ENV is the default environment. If this environment doesn't have the entry A and the parent one has the entry A, this environment can return the entry A. One can override the entry, setting another entry A to this environment.
TEST-FUNC is a test function that compares the entry keys. The default function is `equal'.
CHANNEL is a channel object that sends signals of variable events. Observers can receive following signals:
-get-first : the fist referrer is waiting for binding,
-get-waiting : another referrer is waiting for binding,
-set : a value is bound,
-get : returned a bound value,
-clear : cleared one entry,
-clear-all : cleared all entries.
"
  (let ((this (list parent-env
                    (or test-func 'equal)
                    (or channel
                        (eaf-concurrent-signal-channel
                         'dataflow
                         (and parent-env
                              (eaf-concurrent-dataflow-channel parent-env)))))))
    (eaf-concurrent-dataflow-init-connect this)
    this))

(defun eaf-concurrent-dataflow-init-connect (df)
  "[internal] Initialize the channel object."
  (eaf-concurrent-dataflow-connect
   df 'set
   (lambda (args)
     (cl-destructuring-bind (_event (key)) args
       (let* ((obj (eaf-concurrent-dataflow-get-object-for-value df key))
              (value (and obj (eaf-concurrent-dataflow-value obj))))
         (when obj
           (cl-loop for i in (eaf-concurrent-aif (eaf-concurrent-dataflow-get-object-for-deferreds df key)
                                     (eaf-concurrent-dataflow-deferred-list it) nil)
                    do (eaf-deferred-callback-post i value))
           (setf (eaf-concurrent-dataflow-deferred-list obj) nil)))))))

(defun eaf-concurrent-dataflow-get-object-for-value (df key)
  "[internal] Return an entry object that is indicated by KEY.
If the environment DF doesn't have the entry and the parent one has the entry, this function returns the entry of the parent environment. This function doesn't affect the waiting list."
  (or
   (cl-loop for i in (eaf-concurrent-dataflow-list df)
            with test = (eaf-concurrent-dataflow-test df)
            if (and (funcall test key (eaf-concurrent-dataflow-key i))
                    (not (eaf-concurrent-dataflow-undefine-p i)))
            return i)
   (eaf-deferred-aand
     (eaf-concurrent-dataflow-parent-environment df)
     (eaf-concurrent-dataflow-get-object-for-value it key))))

(defun eaf-concurrent-dataflow-get-object-for-deferreds (df key)
  "[internal] Return a list of the deferred objects those are waiting for value binding.
This function doesn't affect the waiting list and doesn't refer the parent environment."
  (cl-loop for i in (eaf-concurrent-dataflow-list df)
           with test = (eaf-concurrent-dataflow-test df)
           if (funcall test key (eaf-concurrent-dataflow-key i))
           return i))

(defun eaf-concurrent-dataflow-connect (df event-sym &optional callback)
  "Append an observer for EVENT-SYM of the channel of DF and return a deferred object.
See the docstring of `eaf-concurrent-dataflow-environment' for details."
  (eaf-concurrent-signal-connect (eaf-concurrent-dataflow-channel df) event-sym callback))

(defun eaf-concurrent-dataflow-signal (df event &optional arg)
  "[internal] Send a signal to the channel of DF."
  (eaf-concurrent-signal-send (eaf-concurrent-dataflow-channel df) event arg))

(defun eaf-concurrent-dataflow-get (df key)
  "Return a deferred object that can refer the value which is indicated by KEY.
If DF has the entry that bound value, the subsequent deferred task is executed immediately.
If not, the task is deferred till a value is bound."
  (let ((obj (eaf-concurrent-dataflow-get-object-for-value df key)))
    (cond
     ((and obj (eaf-concurrent-dataflow-value obj))
      (eaf-concurrent-dataflow-signal df 'get key)
      (eaf-deferred-succeed (eaf-concurrent-dataflow-value obj)))
     (t
      (setq obj (eaf-concurrent-dataflow-get-object-for-deferreds df key))
      (unless obj
        (setq obj (make-eaf-concurrent-dataflow :key key))
        (push obj (eaf-concurrent-dataflow-list df))
        (eaf-concurrent-dataflow-signal df 'get-first key))
      (let ((d (eaf-deferred-new)))
        (push d (eaf-concurrent-dataflow-deferred-list obj))
        (eaf-concurrent-dataflow-signal df 'get-waiting key)
        d)))))

(defun eaf-concurrent-dataflow-get-sync (df key)
  "Return the value which is indicated by KEY synchronously.
If the environment DF doesn't have an entry of KEY, this function returns nil."
  (let ((obj (eaf-concurrent-dataflow-get-object-for-value df key)))
    (and obj (eaf-concurrent-dataflow-value obj))))

(defun eaf-concurrent-dataflow-set (df key value)
  "Bind the VALUE to KEY in the environment DF.
If DF already has the bound entry of KEY, this function throws an error signal.
VALUE can be nil as a value."
  (let ((obj (eaf-concurrent-dataflow-get-object-for-deferreds df key)))
    (cond
     ((and obj (not (eaf-concurrent-dataflow-undefine-p obj)))
      ;; overwrite!
      (error "Can not set a dataflow value. The key [%s] has already had a value. NEW:[%s] OLD:[%s]" key value (eaf-concurrent-dataflow-value obj)))
     (obj
      (setf (eaf-concurrent-dataflow-value obj) value))
     (t
      ;; just value arrived
      (push (make-eaf-concurrent-dataflow :key key :value value)
            (eaf-concurrent-dataflow-list df))))
    ;; value arrived and start deferred objects
    (eaf-concurrent-dataflow-signal df 'set key)
    value))

(defun eaf-concurrent-dataflow-clear (df key)
  "Clear the entry which is indicated by KEY.
This function does nothing for the waiting deferred objects."
  (eaf-concurrent-dataflow-signal df 'clear key)
  (setf (eaf-concurrent-dataflow-list df)
        (cl-loop for i in (eaf-concurrent-dataflow-list df)
                 with test = (eaf-concurrent-dataflow-test df)
                 unless (funcall test key (eaf-concurrent-dataflow-key i))
                 collect i)))

(defun eaf-concurrent-dataflow-get-avalable-pairs (df)
  "Return an available key-value alist in the environment DF and the parent ones."
  (append
   (cl-loop for i in (eaf-concurrent-dataflow-list df)
            for key = (eaf-concurrent-dataflow-key i)
            for val = (eaf-concurrent-dataflow-value i)
            unless (eaf-concurrent-dataflow-undefine-p i) collect (cons key val))
   (eaf-deferred-aand
     (eaf-concurrent-dataflow-parent-environment df)
     (eaf-concurrent-dataflow-get-avalable-pairs it))))

(defun eaf-concurrent-dataflow-get-waiting-keys (df)
  "Return a list of keys which have waiting deferred objects in the environment DF and the parent ones."
  (append
   (cl-loop for i in (eaf-concurrent-dataflow-list df)
            for key = (eaf-concurrent-dataflow-key i)
            if (eaf-concurrent-dataflow-undefine-p i) collect key)
   (eaf-deferred-aand
     (eaf-concurrent-dataflow-parent-environment df)
     (eaf-concurrent-dataflow-get-waiting-keys it))))

(defun eaf-concurrent-dataflow-clear-all (df)
  "Clear all entries in the environment DF.
This function does nothing for the waiting deferred objects."
  (eaf-concurrent-dataflow-signal df 'clear-all)
  (setf (eaf-concurrent-dataflow-list df) nil))


;; epcs
(defvar eaf-epcs-client-processes nil
  "[internal] A list of ([process object] . [`eaf-epc-manager' instance]).
When the server process accepts the client connection, the
`eaf-epc-manager' instance is created and stored in this variable
`eaf-epcs-client-processes'. This variable is used for the management
purpose.")

;; eaf-epcs-server
;;   name    : process name (string)   ex: "EPC Server 1"
;;   process : server process object
;;   port    : port number
;;   connect-function : initialize function for `eaf-epc-manager' instances
(cl-defstruct eaf-epcs-server name process port connect-function)

(defvar eaf-epcs-server-processes nil
  "[internal] A list of ([process object] . [`eaf-epcs-server' instance]).
This variable is used for the management purpose.")

(defun eaf-epcs-server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (let*
      ((connect-function connect-function)
       (name (format "EPC Server %s" (eaf-epc-uid)))
       (buf (eaf-epc-make-procbuf (format "*%s*" name)))
       (main-process
        (make-network-process
         :name name
         :buffer buf
         :family 'ipv4 :server t :nowait t
         :host "127.0.0.1" :service (or port t)
         :sentinel
         (lambda (process message)
           (eaf-epcs-sentinel process message connect-function)))))
    (unless port
      ;; notify port number to the parent process via STDOUT.
      (message "%s\n" (process-contact main-process :service)))
    (push (cons main-process
                (make-eaf-epcs-server
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          eaf-epcs-server-processes)
    main-process))

(defun eaf-epcs-server-stop (process)
  "Stop the TCP server process."
  (cond
   ((and process
         (assq process eaf-epcs-server-processes))
    (eaf-epc-log "EAF-EPCS- Shutdown Server: %S" process)
    (let ((buf (process-buffer process)))
      (delete-process process)
      (kill-buffer buf))
    (setq eaf-epcs-server-processes
          (assq-delete-all process eaf-epcs-server-processes)))
   (t (error "Not found in the server process list. [%S]" process))))

(defun eaf-epcs-get-manager-by-process (proc)
  "[internal] Return the eaf-epc-manager instance for the PROC."
  (cl-loop for (pp . mngr) in eaf-epcs-client-processes
        if (eql pp proc)
        do (cl-return mngr)
        finally return nil))

(defun eaf-epcs-kill-all-processes ()
  "Kill all child processes for debug purpose."
  (interactive)
  (cl-loop for (proc . mngr) in eaf-epcs-client-processes
        do (ignore-errors
             (delete-process proc)
             (kill-buffer (process-buffer proc)))))

(defun eaf-epcs-accept (process)
  "[internal] Initialize the process and return eaf-epc-manager object."
  (eaf-epc-log "EAF-EPCS- >> Connection accept: %S" process)
  (let* ((connection-id (eaf-epc-uid))
                 (connection-name (format "epc con %s" connection-id))
                 (channel (eaf-concurrent-signal-channel connection-name))
                 (connection (make-eaf-epc-connection
                              :name connection-name
                              :process process
                              :buffer (process-buffer process)
                              :channel channel)))
    (eaf-epc-log "EAF-EPCS- >> Connection establish")
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process
                        (lambda (p m)
                          (eaf-epc-process-filter connection p m)))
    (set-process-sentinel process
                          (lambda (p e)
                            (eaf-epc-process-sentinel connection p e)))
    (make-eaf-epc-manager :server-process process :port t
                      :connection connection)))

(defun eaf-epcs-sentinel (process message connect-function)
  "[internal] Process sentinel handler for the server process."
  (eaf-epc-log "EAF-EPCS- SENTINEL: %S %S" process message)
  (let ((mngr (eaf-epcs-get-manager-by-process process)))
    (cond
     ;; new connection
     ((and (string-match "open" message) (null mngr))
      (condition-case err
          (let ((mngr (eaf-epcs-accept process)))
            (push (cons process mngr) eaf-epcs-client-processes)
            (eaf-epc-init-epc-layer mngr)
            (when connect-function (funcall connect-function mngr))
            mngr)
        ('error
         (eaf-epc-log "EAF-EPCS- Protocol error: %S" err)
         (eaf-epc-log "EAF-EPCS- ABORT %S" process)
         (delete-process process))))
     ;; ignore
     ((null mngr) nil )
     ;; disconnect
     (t
      (let ((pair (assq process eaf-epcs-client-processes)) _d)
        (when pair
          (eaf-epc-log "EAF-EPCS- DISCONNECT %S" process)
          (eaf-epc-stop-epc (cdr pair))
          (setq eaf-epcs-client-processes
                (assq-delete-all process eaf-epcs-client-processes))
          ))
      nil))))

(provide 'eaf-epc)
;;; eaf-epc.el ends here
