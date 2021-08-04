;;; concurrent.el --- Concurrent utility functions for emacs lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2016  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai at kiwanami.net>
;; Version: 0.5.0
;; Keywords: deferred, async, concurrent
;; Package-Requires: ((emacs "24.3") (deferred "0.5.0"))
;; URL: https://github.com/kiwanami/emacs-deferred/blob/master/README-concurrent.markdown

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

;; 'concurrent.el' is a higher level library for concurrent tasks
;; based on 'deferred.el'. This library has following features:
;;
;; - Generator
;; - Green thread
;; - Semaphore
;; - Dataflow
;; - Signal/Channel

(require 'cl-lib)

(require 'deferred)

(defvar cc:version nil "version number")
(setq cc:version "0.3")

;;; Code:



(defmacro cc:aif (test-form then-form &rest else-forms)
  (declare (debug (form form &rest form)))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'cc:aif 'lisp-indent-function 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generator

(defun cc:generator-replace-yield (tree)
  "[internal] Replace `yield' symbols to calling a function in TREE."
  (let (ret)
    (cl-loop for i in tree
             do (cond
                 ((eq i 'yield)
                  (push 'funcall ret)
                  (push i ret))
                 ((listp i)
                  (push (cc:generator-replace-yield i) ret))
                 (t
                  (push i ret))))
    (nreverse ret)))

(defun cc:generator-line (chain line)
  "[internal] Return a macro expansion to execute the sexp LINE
asynchronously."
  (cond
   ;; function object
   ((functionp line)
    `(setq ,chain (deferred:nextc ,chain ,line)))
   ;; while loop form
   ((eq 'while (car line))
    (let ((condition (cadr line))
          (body (cddr line)))
    `(setq ,chain
      (deferred:nextc ,chain
        (deferred:lambda (x)
         (if ,condition
             (deferred:nextc
               (progn
                 ,@(cc:generator-replace-yield body)) self)))))))
   ;; statement
   (t
    `(setq ,chain
           (deferred:nextc ,chain
             (deferred:lambda (x) ,(cc:generator-replace-yield line)))))))

(defmacro cc:generator (callback &rest body)
  "Create a generator object. If BODY has `yield' symbols, it
means calling callback function CALLBACK."
  (let ((chain (cl-gensym))
        (cc (cl-gensym))
        (waiter (cl-gensym)))
    `(let* (,chain
            (,cc ,callback)
            (,waiter (deferred:new))
            (yield (lambda (x) (funcall ,cc x) ,waiter)))
       (setq ,chain ,waiter)
       ,@(cl-loop for i in body
                  collect
                  (cc:generator-line chain i))
       (lambda () (deferred:callback ,waiter)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thread

(defun cc:thread-line (wait-time chain line)
  "[internal] Return a macro expansion to execute the sexp LINE asynchronously.
WAIT-TIME is an interval time between tasks.
CHAIN is the previous deferred task."
  (cond
   ;; function object
   ((functionp line)
    `(setq ,chain (deferred:nextc ,chain ,line)))
   ;; while loop form
   ((eq 'while (car line))
    (let ((condition (cadr line))
          (body (cddr line))
          (retsym (cl-gensym)))
    `(setq ,chain
      (deferred:nextc ,chain
        (deferred:lambda (x)
         (if ,condition
             (deferred:nextc
               (let ((,retsym (progn ,@body)))
                 (if (deferred-p ,retsym) ,retsym
                   (deferred:wait ,wait-time)))
               self)))))))
   ;; statement
   (t
    `(setq ,chain
           (deferred:nextc ,chain
             (lambda (x) ,line))))))

(defmacro cc:thread (wait-time-msec &rest body)
  "Return a thread object."
  (let ((chain (cl-gensym))
        (dstart (cl-gensym)))
    `(let* (,chain
            (,dstart (deferred:new)))
       (setq ,chain ,dstart)
       ,@(cl-loop for i in body
                  collect
                  (cc:thread-line wait-time-msec chain i))
       (deferred:callback ,dstart))))
(put 'cc:thread 'lisp-indent-function 1)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semaphore

(cl-defstruct cc:semaphore max-permits permits waiting-deferreds)

(defun cc:semaphore-create(permits-num)
  "Return a semaphore object with PERMITS-NUM permissions."
  (make-cc:semaphore :max-permits permits-num :permits permits-num))

(defun cc:semaphore-acquire(semaphore)
  "Acquire an execution permission and return deferred object to chain.
If this semaphore object has permissions, the subsequent deferred
task is executed immediately.  If this semaphore object has no
permissions, the subsequent deferred task is blocked. After the
permission is returned, the task is executed."
  (cond
   ((< 0 (cc:semaphore-permits semaphore))
    (cl-decf (cc:semaphore-permits semaphore))
    (deferred:succeed))
   (t
    (let ((d (deferred:new)))
      (push d (cc:semaphore-waiting-deferreds semaphore))
      d))))

(defun cc:semaphore-release(semaphore)
  "Release an execution permission. The programmer is responsible to return the permissions."
  (when (<= (cc:semaphore-max-permits semaphore)
            (cc:semaphore-permits semaphore))
    (error "Too many calling semaphore-release. [max:%s <= permits:%s]"
           (cc:semaphore-max-permits semaphore)
           (cc:semaphore-permits semaphore)))
  (let ((waiting-deferreds
         (cc:semaphore-waiting-deferreds semaphore)))
    (cond
     (waiting-deferreds
      (let* ((d (car (last waiting-deferreds))))
        (setf (cc:semaphore-waiting-deferreds semaphore)
              (nbutlast waiting-deferreds))
        (deferred:callback-post d)))
     (t
      (cl-incf (cc:semaphore-permits semaphore)))))
  semaphore)

(defun cc:semaphore-with (semaphore body-func &optional error-func)
  "Execute the task BODY-FUNC asynchronously with the semaphore block."
  (deferred:try
    (deferred:nextc (cc:semaphore-acquire semaphore) body-func)
    :catch
    error-func
    :finally
    (lambda (_x) (cc:semaphore-release semaphore))))
(put 'cc:semaphore-with 'lisp-indent-function 1)

(defun cc:semaphore-release-all (semaphore)
  "Release all permissions for resetting the semaphore object.
If the semaphore object has some blocked tasks, this function
return a list of the tasks and clear the list of the blocked
tasks in the semaphore object."
  (setf (cc:semaphore-permits semaphore)
        (cc:semaphore-max-permits semaphore))
  (let ((ds (cc:semaphore-waiting-deferreds semaphore)))
    (when ds
      (setf (cc:semaphore-waiting-deferreds semaphore) nil))
    ds))

(defun cc:semaphore-interrupt-all (semaphore)
  "Clear the list of the blocked tasks in the semaphore and return a deferred object to chain.
This function is used for the interruption cases."
  (when (cc:semaphore-waiting-deferreds semaphore)
    (setf (cc:semaphore-waiting-deferreds semaphore) nil)
    (setf (cc:semaphore-permits semaphore) 0))
  (cc:semaphore-acquire semaphore))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signal / Channel

(defun cc:signal-channel (&optional name parent-channel)
  "Create a channel.
NAME is a channel name for debug.
PARENT-CHANNEL is an upstream channel. The observers of this channel can receive the upstream signals.
In the case of using the function `cc:signal-send', the observers of the upstream channel can not receive the signals of this channel. The function `cc:signal-send-global' can send a signal to the upstream channels from the downstream channels."
  (let ((ch (cons
             (or name (format "signal%s" (deferred:uid))) ; name for debug
             (cons
              parent-channel ; parent-channel
              nil)))) ; observers
    (when parent-channel
      (cc:signal-connect
       parent-channel
       t (lambda (event)
           (cl-destructuring-bind
               (event-name event-args) event
             (apply 'cc:signal-send
                    ch event-name event-args)))))
    ch))

(defmacro cc:signal-name (ch)
  "[internal] Return signal name."
  `(car ,ch))

(defmacro cc:signal-parent-channel (ch)
  "[internal] Return parent channel object."
  `(cadr ,ch))

(defmacro cc:signal-observers (ch)
  "[internal] Return observers."
  `(cddr ,ch))

(defun cc:signal-connect (channel event-sym &optional callback)
  "Append an observer for EVENT-SYM of CHANNEL and return a deferred object.
If EVENT-SYM is `t', the observer receives all signals of the channel.
If CALLBACK function is given, the deferred object executes the
CALLBACK function asynchronously. One can connect subsequent
tasks to the returned deferred object."
  (let ((d (if callback
               (deferred:new callback)
             (deferred:new))))
    (push (cons event-sym d)
          (cc:signal-observers channel))
    d))

(defun cc:signal-send (channel event-sym &rest args)
  "Send a signal to CHANNEL. If ARGS values are given, observers can get the values by following code: (lambda (event) (destructuring-bind (event-sym (args)) event ... )). "
  (let ((observers (cc:signal-observers channel))
        (event (list event-sym args)))
    (cl-loop for i in observers
             for name = (car i)
             for d = (cdr i)
             if (or (eq event-sym name) (eq t name))
             do (deferred:callback-post d event))))

(defun cc:signal-send-global (channel event-sym &rest args)
  "Send a signal to the most upstream channel. "
  (cc:aif (cc:signal-parent-channel channel)
      (apply 'cc:signal-send-global it event-sym args)
    (apply 'cc:signal-send channel event-sym args)))


(defun cc:signal-disconnect (channel deferred)
  "Remove the observer object DEFERRED from CHANNEL and return
the removed deferred object. "
  (let ((observers (cc:signal-observers channel)) deleted)
    (setf
     (cc:signal-observers channel) ; place
     (cl-loop for i in observers
              for d = (cdr i)
              unless (eq d deferred)
              collect i
              else
              do (push i deleted)))
    deleted))

(defun cc:signal-disconnect-all (channel)
  "Remove all observers."
  (setf
   (cc:signal-observers channel) ; place
   nil))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dataflow

;; Dataflow variable entry
(cl-defstruct cc:dataflow key (value 'cc:dataflow-undefine) deferred-list)

(defun cc:dataflow-undefine-p (obj)
  "[internal] If the variable entry is not bound, return `t'."
  (eq 'cc:dataflow-undefine (cc:dataflow-value obj)))

(defmacro cc:dataflow-parent-environment (df)
  "[internal] Return the parent environment."
  `(car ,df))

(defmacro cc:dataflow-test (df)
  "[internal] Return the test function."
  `(cadr ,df))

(defmacro cc:dataflow-channel (df)
  "[internal] Return the channel object."
  `(cl-caddr ,df))

(defmacro cc:dataflow-list (df)
  "[internal] Return the list of deferred object which are waiting for value binding."
  `(cl-cdddr ,df))

(defun cc:dataflow-environment (&optional parent-env test-func channel)
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
                        (cc:signal-channel
                         'dataflow
                         (and parent-env
                              (cc:dataflow-channel parent-env)))))))
    (cc:dataflow-init-connect this)
    this))

(defun cc:dataflow-init-connect (df)
  "[internal] Initialize the channel object."
  (cc:dataflow-connect
   df 'set
   (lambda (args)
     (cl-destructuring-bind (_event (key)) args
       (let* ((obj (cc:dataflow-get-object-for-value df key))
              (value (and obj (cc:dataflow-value obj))))
         (when obj
           (cl-loop for i in (cc:aif (cc:dataflow-get-object-for-deferreds df key)
                                     (cc:dataflow-deferred-list it) nil)
                    do (deferred:callback-post i value))
           (setf (cc:dataflow-deferred-list obj) nil)))))))

(defun cc:dataflow-get-object-for-value (df key)
  "[internal] Return an entry object that is indicated by KEY.
If the environment DF doesn't have the entry and the parent one has the entry, this function returns the entry of the parent environment. This function doesn't affect the waiting list."
  (or
   (cl-loop for i in (cc:dataflow-list df)
            with test = (cc:dataflow-test df)
            if (and (funcall test key (cc:dataflow-key i))
                    (not (cc:dataflow-undefine-p i)))
            return i)
   (deferred:aand
     (cc:dataflow-parent-environment df)
     (cc:dataflow-get-object-for-value it key))))

(defun cc:dataflow-get-object-for-deferreds (df key)
  "[internal] Return a list of the deferred objects those are waiting for value binding.
This function doesn't affect the waiting list and doesn't refer the parent environment."
  (cl-loop for i in (cc:dataflow-list df)
           with test = (cc:dataflow-test df)
           if (funcall test key (cc:dataflow-key i))
           return i))

(defun cc:dataflow-connect (df event-sym &optional callback)
  "Append an observer for EVENT-SYM of the channel of DF and return a deferred object.
See the docstring of `cc:dataflow-environment' for details."
  (cc:signal-connect (cc:dataflow-channel df) event-sym callback))

(defun cc:dataflow-signal (df event &optional arg)
  "[internal] Send a signal to the channel of DF."
  (cc:signal-send (cc:dataflow-channel df) event arg))

(defun cc:dataflow-get (df key)
  "Return a deferred object that can refer the value which is indicated by KEY.
If DF has the entry that bound value, the subsequent deferred task is executed immediately.
If not, the task is deferred till a value is bound."
  (let ((obj (cc:dataflow-get-object-for-value df key)))
    (cond
     ((and obj (cc:dataflow-value obj))
      (cc:dataflow-signal df 'get key)
      (deferred:succeed (cc:dataflow-value obj)))
     (t
      (setq obj (cc:dataflow-get-object-for-deferreds df key))
      (unless obj
        (setq obj (make-cc:dataflow :key key))
        (push obj (cc:dataflow-list df))
        (cc:dataflow-signal df 'get-first key))
      (let ((d (deferred:new)))
        (push d (cc:dataflow-deferred-list obj))
        (cc:dataflow-signal df 'get-waiting key)
        d)))))

(defun cc:dataflow-get-sync (df key)
  "Return the value which is indicated by KEY synchronously.
If the environment DF doesn't have an entry of KEY, this function returns nil."
  (let ((obj (cc:dataflow-get-object-for-value df key)))
    (and obj (cc:dataflow-value obj))))

(defun cc:dataflow-set (df key value)
  "Bind the VALUE to KEY in the environment DF.
If DF already has the bound entry of KEY, this function throws an error signal.
VALUE can be nil as a value."
  (let ((obj (cc:dataflow-get-object-for-deferreds df key)))
    (cond
     ((and obj (not (cc:dataflow-undefine-p obj)))
      ;; overwrite!
      (error "Can not set a dataflow value. The key [%s] has already had a value. NEW:[%s] OLD:[%s]" key value (cc:dataflow-value obj)))
     (obj
      (setf (cc:dataflow-value obj) value))
     (t
      ;; just value arrived
      (push (make-cc:dataflow :key key :value value)
            (cc:dataflow-list df))))
    ;; value arrived and start deferred objects
    (cc:dataflow-signal df 'set key)
    value))

(defun cc:dataflow-clear (df key)
  "Clear the entry which is indicated by KEY.
This function does nothing for the waiting deferred objects."
  (cc:dataflow-signal df 'clear key)
  (setf (cc:dataflow-list df)
        (cl-loop for i in (cc:dataflow-list df)
                 with test = (cc:dataflow-test df)
                 unless (funcall test key (cc:dataflow-key i))
                 collect i)))

(defun cc:dataflow-get-avalable-pairs (df)
  "Return an available key-value alist in the environment DF and the parent ones."
  (append
   (cl-loop for i in (cc:dataflow-list df)
            for key = (cc:dataflow-key i)
            for val = (cc:dataflow-value i)
            unless (cc:dataflow-undefine-p i) collect (cons key val))
   (deferred:aand
     (cc:dataflow-parent-environment df)
     (cc:dataflow-get-avalable-pairs it))))

(defun cc:dataflow-get-waiting-keys (df)
  "Return a list of keys which have waiting deferred objects in the environment DF and the parent ones."
  (append
   (cl-loop for i in (cc:dataflow-list df)
            for key = (cc:dataflow-key i)
            if (cc:dataflow-undefine-p i) collect key)
   (deferred:aand
     (cc:dataflow-parent-environment df)
     (cc:dataflow-get-waiting-keys it))))

(defun cc:dataflow-clear-all (df)
  "Clear all entries in the environment DF.
This function does nothing for the waiting deferred objects."
  (cc:dataflow-signal df 'clear-all)
  (setf (cc:dataflow-list df) nil))


(provide 'concurrent)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; concurrent.el ends here
