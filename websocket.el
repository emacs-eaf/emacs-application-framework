;;; websocket.el --- Emacs WebSocket client and server  -*- lexical-binding:t -*-

;; Copyright (c) 2013, 2016-2017  Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/emacs-websocket
;; Keywords: Communication, Websocket, Server
;; Version: 1.12
;; Package-Requires: ((cl-lib "0.5"))
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This implements RFC 6455, which can be found at
;; http://tools.ietf.org/html/rfc6455.
;;
;; This library contains code to connect Emacs as a client to a
;; websocket server, and for Emacs to act as a server for websocket
;; connections.
;;
;; Websockets clients are created by calling `websocket-open', which
;; returns a `websocket' struct.  Users of this library use the
;; websocket struct, and can call methods `websocket-send-text', which
;; sends text over the websocket, or `websocket-send', which sends a
;; `websocket-frame' struct, enabling finer control of what is sent.
;; A callback is passed to `websocket-open' that will retrieve
;; websocket frames called from the websocket.  Websockets are
;; eventually closed with `websocket-close'.
;;
;; Server functionality is similar.  A server is started with
;; `websocket-server' called with a port and the callbacks to use,
;; which returns a process.  The process can later be closed with
;; `websocket-server-close'.  A `websocket' struct is also created
;; for every connection, and is exposed through the callbacks.

(require 'bindat)
(require 'url-parse)
(require 'url-cookie)
(require 'seq)
(eval-when-compile (require 'cl-lib))

;;; Code:

(cl-defstruct (websocket
            (:constructor nil)
            (:constructor websocket-inner-create))
  "A websocket structure.
This follows the W3C Websocket API, except translated to elisp
idioms.  The API is implemented in both the websocket struct and
additional methods.  Due to how defstruct slots are accessed, all
API methods are prefixed with \"websocket-\" and take a websocket
as an argument, so the distrinction between the struct API and
the additional helper APIs are not visible to the caller.

A websocket struct is created with `websocket-open'.

`ready-state' contains one of `connecting', `open', or
`closed', depending on the state of the websocket.

The W3C API \"bufferedAmount\" call is not currently implemented,
since there is no elisp API to get the buffered amount from the
subprocess.  There may, in fact, be output data buffered,
however, when the `on-message' or `on-close' callbacks are
called.

`on-open', `on-message', `on-close', and `on-error' are described
in `websocket-open'.

The `negotiated-extensions' slot lists the extensions accepted by
both the client and server, and `negotiated-protocols' does the
same for the protocols."
  ;; API
  (ready-state 'connecting)
  client-data
  on-open
  on-message
  on-close
  on-error
  negotiated-protocols
  negotiated-extensions
  (server-p nil :read-only t)

  ;; Other data - clients should not have to access this.
  (url (cl-assert nil) :read-only t)
  (protocols nil :read-only t)
  (extensions nil :read-only t)
  (conn (cl-assert nil) :read-only t)
  ;; Only populated for servers, this is the server connection.
  server-conn
  accept-string
  (inflight-input nil))

(defvar websocket-version "1.12"
  "Version numbers of this version of websocket.el.")

(defvar websocket-debug nil
  "Set to true to output debugging info to a per-websocket buffer.
The buffer is ` *websocket URL debug*' where URL is the
URL of the connection.")

(defconst websocket-guid "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  "The websocket GUID as defined in RFC 6455.
Do not change unless the RFC changes.")

(defvar websocket-callback-debug-on-error nil
  "If true, when an error happens in a client callback, invoke the debugger.
Having this on can cause issues with missing frames if the debugger is
exited by quitting instead of continuing, so it's best to have this set
to nil unless it is especially needed.")

(defmacro websocket-document-function (function docstring)
  "Document FUNCTION with DOCSTRING.  Use this for defstruct accessor etc."
  (declare (indent defun)
           (doc-string 2))
  `(put ',function 'function-documentation ,docstring))

(websocket-document-function websocket-on-open
  "Accessor for websocket on-open callback.
See `websocket-open' for details.

\(fn WEBSOCKET)")

(websocket-document-function websocket-on-message
  "Accessor for websocket on-message callback.
See `websocket-open' for details.

\(fn WEBSOCKET)")

(websocket-document-function websocket-on-close
  "Accessor for websocket on-close callback.
See `websocket-open' for details.

\(fn WEBSOCKET)")

(websocket-document-function websocket-on-error
  "Accessor for websocket on-error callback.
See `websocket-open' for details.

\(fn WEBSOCKET)")

(defun websocket-genbytes (nbytes)
  "Generate NBYTES random bytes."
  (let ((s (make-string nbytes ?\s)))
    (dotimes (i nbytes)
      (aset s i (random 256)))
    s))

(defun websocket-try-callback (websocket-callback callback-type websocket
                                                  &rest rest)
  "Invoke function WEBSOCKET-CALLBACK with WEBSOCKET and REST args.
If an error happens, it is handled according to
`websocket-callback-debug-on-error'."
  ;; This looks like it should be able to done more efficiently, but
  ;; I'm not sure that's the case.  We can't do it as a macro, since
  ;; we want it to change whenever websocket-callback-debug-on-error
  ;; changes.
  (let ((args rest)
        (debug-on-error websocket-callback-debug-on-error))
    (push websocket args)
    (if websocket-callback-debug-on-error
        (condition-case err
            (apply (funcall websocket-callback websocket) args)
          ((debug error) (funcall (websocket-on-error websocket)
                                  websocket callback-type err)))
      (condition-case err
          (apply (funcall websocket-callback websocket) args)
        (error (funcall (websocket-on-error websocket) websocket
                        callback-type err))))))

(defun websocket-genkey ()
  "Generate a key suitable for the websocket handshake."
  (base64-encode-string (websocket-genbytes 16)))

(defun websocket-calculate-accept (key)
  "Calculate the expect value of the accept header.
This is based on the KEY from the Sec-WebSocket-Key header."
  (base64-encode-string
   (sha1 (concat key websocket-guid) nil nil t)))

(defun websocket-get-bytes (s n)
  "From string S, retrieve the value of N bytes.
Return the value as an unsigned integer.  The value N must be a
power of 2, up to 8.

We support getting frames up to 536870911 bytes (2^29 - 1),
approximately 537M long."
  (if (= n 8)
      (let* ((32-bit-parts
              (bindat-get-field (bindat-unpack '((:val vec 2 u32)) s) :val))
             (cval
              (logior (lsh (aref 32-bit-parts 0) 32) (aref 32-bit-parts 1))))
        (if (and (= (aref 32-bit-parts 0) 0)
                 (= (lsh (aref 32-bit-parts 1) -29) 0))
            cval
          (signal 'websocket-unparseable-frame
                  (list "Frame value found too large to parse!"))))
    ;; n is not 8
    (bindat-get-field
     (condition-case _
         (bindat-unpack
          `((:val
             ,(cond ((= n 1) 'u8)
                    ((= n 2) 'u16)
                    ((= n 4) 'u32)
                    ;; This is an error with the library,
                    ;; not a user-facing, meaningful error.
                    (t (error
                        "websocket-get-bytes: Unknown N: %S" n)))))
          s)
       (args-out-of-range (signal 'websocket-unparseable-frame
                                  (list (format "Frame unexpectedly short: %s" s)))))
     :val)))

(defun websocket-to-bytes (val nbytes)
  "Encode the integer VAL in NBYTES of data.
NBYTES much be a power of 2, up to 8.

This supports encoding values up to 536870911 bytes (2^29 - 1),
approximately 537M long."
  (when (and (< nbytes 8)
             (> val (expt 2 (* 8 nbytes))))
    ;; not a user-facing error, this must be caused from an error in
    ;; this library
    (error "websocket-to-bytes: Value %d could not be expressed in %d bytes"
           val nbytes))
  (if (= nbytes 8)
      (progn
        (let* ((hi-32bits (lsh val -32))
               ;; This is just VAL on systems that don't have >= 32 bits.
               (low-32bits (- val (lsh hi-32bits 32))))
          (when (or (> hi-32bits 0) (> (lsh low-32bits -29) 0))
            (signal 'websocket-frame-too-large (list val)))
          (bindat-pack `((:val vec 2 u32))
                       `((:val . [,hi-32bits ,low-32bits])))))
    (bindat-pack
     `((:val ,(cond ((= nbytes 1) 'u8)
                    ((= nbytes 2) 'u16)
                    ((= nbytes 4) 'u32)
                    ;; Library error, not system error
                    (t (error "websocket-to-bytes: Unknown NBYTES: %S" nbytes)))))
     `((:val . ,val)))))

(defun websocket-get-opcode (s)
  "Retrieve the opcode from first byte of string S."
  (websocket-ensure-length s 1)
  (let ((opcode (logand #xf (aref s 0))))
    (cond ((= opcode 0) 'continuation)
          ((= opcode 1) 'text)
          ((= opcode 2) 'binary)
          ((= opcode 8) 'close)
          ((= opcode 9) 'ping)
          ((= opcode 10) 'pong))))

(defun websocket-get-payload-len (s)
  "Parse out the payload length from the string S.
We start at position 0, and return a cons of the payload length and how
many bytes were consumed from the string."
  (websocket-ensure-length s 1)
  (let* ((initial-val (logand 127 (aref s 0))))
    (cond ((= initial-val 127)
           (websocket-ensure-length s 9)
           (cons (websocket-get-bytes (substring s 1) 8) 9))
          ((= initial-val 126)
           (websocket-ensure-length s 3)
           (cons (websocket-get-bytes (substring s 1) 2) 3))
          (t (cons initial-val 1)))))

(cl-defstruct websocket-frame opcode payload length completep)

(defun websocket-frame-text (frame)
  "Given FRAME, return the payload as a utf-8 encoded string."
  (cl-assert (websocket-frame-p frame))
  (decode-coding-string (websocket-frame-payload frame) 'utf-8))

(defun websocket-mask (key data)
  "Using string KEY, mask string DATA according to the RFC.
This is used to both mask and unmask data."
  ;; Returning the string as unibyte is important here. Because we set the
  ;; string byte by byte, this results in a unibyte string.
  (cl-loop
   with result = (make-string (length data) ?x)
   for i from 0 below (length data)
   do (setf (seq-elt result i) (logxor (aref key (mod i 4)) (seq-elt data i)))
   finally return result))

(defun websocket-ensure-length (s n)
  "Ensure the string S has at most N bytes.
Otherwise we throw the error `websocket-incomplete-frame'."
  (when (< (length s) n)
    (throw 'websocket-incomplete-frame nil)))

(defun websocket-encode-frame (frame should-mask)
  "Encode the FRAME struct to the binary representation.
We mask the frame or not, depending on SHOULD-MASK."
  (let* ((opcode (websocket-frame-opcode frame))
         (payload (websocket-frame-payload frame))
         (fin (websocket-frame-completep frame))
         (payloadp (and payload
                        (memq opcode '(continuation ping pong text binary))))
         (mask-key (when should-mask (websocket-genbytes 4))))
    (apply #'unibyte-string
           (let ((val (append (list
                               (logior (pcase opcode
                                         (`continuation 0)
                                         (`text         1)
                                         (`binary       2)
                                         (`close        8)
                                         (`ping         9)
                                         (`pong         10))
                                       (if fin 128 0)))
                           (when payloadp
                             (list
                              (logior
                               (if should-mask 128 0)
                               (cond ((< (length payload) 126) (length payload))
                                     ((< (length payload) 65536) 126)
                                     (t 127)))))
                           (when (and payloadp (>= (length payload) 126))
                             (append (websocket-to-bytes
                                      (length payload)
                                      (cond ((< (length payload) 126) 1)
                                            ((< (length payload) 65536) 2)
                                            (t 8))) nil))
                           (when (and payloadp should-mask)
                             (append mask-key nil))
                           (when payloadp
                             (append (if should-mask (websocket-mask mask-key payload)
                                       payload)
                                     nil)))))
             ;; We have to make sure the non-payload data is a full 32-bit frame
             (if (= 1 (length val))
                 (append val '(0)) val)))))

(defun websocket-read-frame (s)
  "Read from string S a `websocket-frame' struct with the contents.
This only gets complete frames.  Partial frames need to wait until
the frame finishes.  If the frame is not completed, return NIL."
  (catch 'websocket-incomplete-frame
    (websocket-ensure-length s 1)
    (let* ((opcode (websocket-get-opcode s))
           (fin (logand 128 (aref s 0)))
           (payloadp (memq opcode '(continuation text binary ping pong)))
           (payload-len (when payloadp
                          (websocket-get-payload-len (substring s 1))))
           (maskp (and
                   payloadp
                   (= 128 (logand 128 (aref s 1)))))
           (payload-start (when payloadp (+ (if maskp 5 1) (cdr payload-len))))
           (payload-end (when payloadp (+ payload-start (car payload-len))))
           (unmasked-payload (when payloadp
                               (websocket-ensure-length s payload-end)
                               (substring s payload-start payload-end))))
      (make-websocket-frame
       :opcode opcode
       :payload
       (if maskp
           (let ((masking-key (substring s (+ 1 (cdr payload-len))
                                         (+ 5 (cdr payload-len)))))
             (websocket-mask masking-key unmasked-payload))
         unmasked-payload)
       :length (if payloadp payload-end 1)
       :completep (> fin 0)))))

(defun websocket-format-error (err)
  "Format an error message like command level does.
ERR should be a cons of error symbol and error data."

  ;; Formatting code adapted from `edebug-report-error'
  (concat (or (get (car err) 'error-message)
              (format "peculiar error (%s)" (car err)))
          (when (cdr err)
            (format ": %s"
                    (mapconcat #'prin1-to-string
                               (cdr err) ", ")))))

(defun websocket-default-error-handler (_websocket type err)
  "The default error handler used to handle errors in callbacks."
  (display-warning 'websocket
                   (format "in callback `%S': %s"
                           type
                           (websocket-format-error err))
                   :error))

;; Error symbols in use by the library
(put 'websocket-unsupported-protocol 'error-conditions
     '(error websocket-error websocket-unsupported-protocol))
(put 'websocket-unsupported-protocol 'error-message "Unsupported websocket protocol")
(put 'websocket-wss-needs-emacs-24 'error-conditions
     '(error websocket-error websocket-unsupported-protocol
             websocket-wss-needs-emacs-24))
(put 'websocket-wss-needs-emacs-24 'error-message
     "wss protocol is not supported for Emacs before version 24.")
(put 'websocket-received-error-http-response 'error-conditions
     '(error websocket-error websocket-received-error-http-response))
(put 'websocket-received-error-http-response 'error-message
     "Error response received from websocket server")
(put 'websocket-invalid-header 'error-conditions
     '(error websocket-error websocket-invalid-header))
(put 'websocket-invalid-header 'error-message
     "Invalid HTTP header sent")
(put 'websocket-illegal-frame 'error-conditions
     '(error websocket-error websocket-illegal-frame))
(put 'websocket-illegal-frame 'error-message
     "Cannot send illegal frame to websocket")
(put 'websocket-closed 'error-conditions
     '(error websocket-error websocket-closed))
(put 'websocket-closed 'error-message
     "Cannot send message to a closed websocket")
(put 'websocket-unparseable-frame 'error-conditions
     '(error websocket-error websocket-unparseable-frame))
(put 'websocket-unparseable-frame 'error-message
     "Received an unparseable frame")
(put 'websocket-frame-too-large 'error-conditions
     '(error websocket-error websocket-frame-too-large))
(put 'websocket-frame-too-large 'error-message
     "The frame being sent is too large for this emacs to handle")

(defun websocket-intersect (a b)
  "Simple list intersection, should function like Common Lisp's `intersection'."
  (let ((result))
    (dolist (elem a (nreverse result))
      (when (member elem b)
        (push elem result)))))

(defun websocket-get-debug-buffer-create (websocket)
  "Get or create the buffer corresponding to WEBSOCKET."
  (let ((buf (get-buffer-create (format "*websocket %s debug*"
                                    (websocket-url websocket)))))
    (when (= 0 (buffer-size buf))
      (buffer-disable-undo buf))
    buf))

(defun websocket-debug (websocket msg &rest args)
  "In the WEBSOCKET's debug buffer, send MSG, with format ARGS."
  (when websocket-debug
    (let ((buf (websocket-get-debug-buffer-create websocket)))
      (save-excursion
        (with-current-buffer buf
          (goto-char (point-max))
          (insert "[WS] ")
          (insert (apply #'format (append (list msg) args)))
          (insert "\n"))))))

(defun websocket-verify-response-code (output)
  "Verify that OUTPUT contains a valid HTTP response code.
The only acceptable one to websocket is responce code 101.
A t value will be returned on success, and an error thrown
if not."
  (unless (string-match "^HTTP/1.1 \\([[:digit:]]+\\)" output)
    (signal 'websocket-invalid-header (list "Invalid HTTP status line")))
  (unless (equal "101" (match-string 1 output))
    (signal 'websocket-received-error-http-response
	        (list (string-to-number (match-string 1 output)))))
  t)

(defun websocket-parse-repeated-field (output field)
  "From header-containing OUTPUT, parse out the list from a
possibly repeated field."
  (let ((pos 0)
        (extensions))
    (while (and pos
                (string-match (format "\r\n%s: \\(.*\\)\r\n" field)
                              output pos))
      (when (setq pos (match-end 1))
        (setq extensions (append extensions (split-string
                                             (match-string 1 output) ", ?")))))
    extensions))

(defun websocket-process-frame (websocket frame)
  "Using the WEBSOCKET's filter and connection, process the FRAME.
This returns a lambda that should be executed when all frames have
been processed.  If the frame has a payload, the lambda has the frame
passed to the filter slot of WEBSOCKET.  If the frame is a ping,
the lambda has a reply with a pong.  If the frame is a close, the lambda
has connection termination."
  (let ((opcode (websocket-frame-opcode frame)))
    (cond ((memq opcode '(continuation text binary))
           (lambda () (websocket-try-callback 'websocket-on-message 'on-message
                                         websocket frame)))
          ((eq opcode 'ping)
           (lambda () (websocket-send websocket
                                 (make-websocket-frame
                                  :opcode 'pong
                                  :payload (websocket-frame-payload frame)
                                  :completep t))))
          ((eq opcode 'close)
           (lambda () (delete-process (websocket-conn websocket))))
          (t (lambda ())))))

(defun websocket-process-input-on-open-ws (websocket text)
  "This handles input processing for both the client and server filters."
  (let ((current-frame)
        (processing-queue)
        (start-point 0))
    (while (setq current-frame (websocket-read-frame
                                (substring text start-point)))
      (push (websocket-process-frame websocket current-frame) processing-queue)
      (cl-incf start-point (websocket-frame-length current-frame)))
    (when (> (length text) start-point)
      (setf (websocket-inflight-input websocket)
            (substring text start-point)))
    (dolist (to-process (nreverse processing-queue))
      (funcall to-process))))

(defun websocket-send-text (websocket text)
  "To the WEBSOCKET, send TEXT as a complete frame."
  (websocket-send
   websocket
   (make-websocket-frame :opcode 'text
                         :payload (encode-coding-string
                                   text 'raw-text)
                         :completep t)))

(defun websocket-check (frame)
  "Check FRAME for correctness, returning true if correct."
  (or
   ;; Text, binary, and continuation frames need payloads
   (and (memq (websocket-frame-opcode frame) '(text binary continuation))
        (websocket-frame-payload frame))
   ;; Pings and pongs may optionally have them
   (memq (websocket-frame-opcode frame) '(ping pong))
   ;; And close shouldn't have any payload, and should always be complete.
   (and (eq (websocket-frame-opcode frame) 'close)
        (not (websocket-frame-payload frame))
        (websocket-frame-completep frame))))

(defun websocket-send (websocket frame)
  "To the WEBSOCKET server, send the FRAME.
This will raise an error if the frame is illegal.

The error signaled may be of type `websocket-illegal-frame' if
the frame is malformed in some way, also having the condition
type of `websocket-error'.  The data associated with the signal
is the frame being sent.

If the websocket is closed a signal `websocket-closed' is sent,
also with `websocket-error' condition.  The data in the signal is
also the frame.

The frame may be too large for this buid of Emacs, in which case
`websocket-frame-too-large' is returned, with the data of the
size of the frame which was too large to process.  This also has
the `websocket-error' condition."
  (unless (websocket-check frame)
    (signal 'websocket-illegal-frame (list frame)))
  (websocket-debug websocket "Sending frame, opcode: %s payload: %s"
                   (websocket-frame-opcode frame)
                   (websocket-frame-payload frame))
  (websocket-ensure-connected websocket)
  (unless (websocket-openp websocket)
    (signal 'websocket-closed (list frame)))
  (process-send-string (websocket-conn websocket)
                       ;; We mask only when we're a client, following the spec.
                       (websocket-encode-frame frame (not (websocket-server-p websocket)))))

(defun websocket-openp (websocket)
  "Check WEBSOCKET and return non-nil if the connection is open."
  (and websocket
       (not (eq 'close (websocket-ready-state websocket)))
       (member (process-status (websocket-conn websocket)) '(open run))))

(defun websocket-close (websocket)
  "Close WEBSOCKET and erase all the old websocket data."
  (websocket-debug websocket "Closing websocket")
  (websocket-try-callback 'websocket-on-close 'on-close websocket)
  (when (websocket-openp websocket)
    (websocket-send websocket
                    (make-websocket-frame :opcode 'close
                                          :completep t))
    (setf (websocket-ready-state websocket) 'closed))
  (delete-process (websocket-conn websocket)))

(defun websocket-ensure-connected (websocket)
  "If the WEBSOCKET connection is closed, open it."
  (unless (and (websocket-conn websocket)
               (cl-ecase (process-status (websocket-conn websocket))
                 ((run open listen) t)
                 ((stop exit signal closed connect failed nil) nil)))
    (websocket-close websocket)
    (websocket-open (websocket-url websocket)
                    :protocols (websocket-protocols websocket)
                    :extensions (websocket-extensions websocket)
                    :on-open (websocket-on-open websocket)
                    :on-message (websocket-on-message websocket)
                    :on-close (websocket-on-close websocket)
                    :on-error (websocket-on-error websocket))))

;;;;;;;;;;;;;;;;;;;;;;
;; Websocket client ;;
;;;;;;;;;;;;;;;;;;;;;;

(cl-defun websocket-open (url &key protocols extensions (on-open 'identity)
                              (on-message (lambda (_w _f))) (on-close 'identity)
                              (on-error 'websocket-default-error-handler)
                              (nowait nil) (custom-header-alist nil))
  "Open a websocket connection to URL, returning the `websocket' struct.
The PROTOCOL argument is optional, and setting it will declare to
the server that this client supports the protocols in the list
given.  We will require that the server also has to support that
protocols.

Similar logic applies to EXTENSIONS, which is a list of conses,
the car of which is a string naming the extension, and the cdr of
which is the list of parameter strings to use for that extension.
The parameter strings are of the form \"key=value\" or \"value\".
EXTENSIONS can be NIL if none are in use.  An example value would
be (\"deflate-stream\" . (\"mux\" \"max-channels=4\")).

Cookies that are set via `url-cookie-store' will be used during
communication with the server, and cookies received from the
server will be stored in the same cookie storage that the
`url-cookie' package uses.

Optionally you can specify
ON-OPEN, ON-MESSAGE and ON-CLOSE callbacks as well.

The ON-OPEN callback is called after the connection is
established with the websocket as the only argument.  The return
value is unused.

The ON-MESSAGE callback is called after receiving a frame, and is
called with the websocket as the first argument and
`websocket-frame' struct as the second.  The return value is
unused.

The ON-CLOSE callback is called after the connection is closed, or
failed to open.  It is called with the websocket as the only
argument, and the return value is unused.

The ON-ERROR callback is called when any of the other callbacks
have an error.  It takes the websocket as the first argument, and
a symbol as the second argument either `on-open', `on-message',
or `on-close', and the error as the third argument. Do NOT
rethrow the error, or else you may miss some websocket messages.
You similarly must not generate any other errors in this method.
If you want to debug errors, set
`websocket-callback-debug-on-error' to t, but this also can be
dangerous is the debugger is quit out of.  If not specified,
`websocket-default-error-handler' is used.

For each of these event handlers, the client code can store
arbitrary data in the `client-data' slot in the returned
websocket.

The following errors might be thrown in this method or in
websocket processing, all of them having the error-condition
`websocket-error' in addition to their own symbol:

`websocket-unsupported-protocol': Data in the error signal is the
protocol that is unsupported.  For example, giving a URL starting
with http by mistake raises this error.

`websocket-wss-needs-emacs-24': Trying to connect wss protocol
using Emacs < 24 raises this error.  You can catch this error
also by `websocket-unsupported-protocol'.

`websocket-received-error-http-response': Data in the error
signal is the integer error number.

`websocket-invalid-header': Data in the error is a string
describing the invalid header received from the server.

`websocket-unparseable-frame': Data in the error is a string
describing the problem with the frame.

`nowait': If NOWAIT is true, return without waiting for the
connection to complete.

`custom-headers-alist': An alist of custom headers to pass to the
server. The car is the header name, the cdr is the header value.
These are different from the extensions because it is not related
to the websocket protocol.
"
  (let* ((name (format "websocket to %s" url))
         (url-struct (url-generic-parse-url url))
         (key (websocket-genkey))
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (conn (if (member (url-type url-struct) '("ws" "wss"))
                   (let* ((type (if (equal (url-type url-struct) "ws")
                                    'plain 'tls))
                          (port (if (= 0 (url-port url-struct))
                                    (if (eq type 'tls) 443 80)
                                  (url-port url-struct)))
                          (host (url-host url-struct)))
                     (if (eq type 'plain)
                         (make-network-process :name name :buffer nil :host host
                                               :service port :nowait nowait)
                       (condition-case-unless-debug nil
                           (open-network-stream name nil host port :type type :nowait nowait)
                         (wrong-number-of-arguments
                          (signal 'websocket-wss-needs-emacs-24 (list "wss"))))))
                 (signal 'websocket-unsupported-protocol (list (url-type url-struct)))))
         (websocket (websocket-inner-create
                     :conn conn
                     :url url
                     :on-open on-open
                     :on-message on-message
                     :on-close on-close
                     :on-error on-error
                     :protocols protocols
                     :extensions (mapcar 'car extensions)
                     :accept-string
                     (websocket-calculate-accept key))))
    (unless conn (error "Could not establish the websocket connection to %s" url))
    (process-put conn :websocket websocket)
    (set-process-filter conn
                        (lambda (process output)
                          (let ((websocket (process-get process :websocket)))
                            (websocket-outer-filter websocket output))))
    (set-process-sentinel
     conn
     (websocket-sentinel url conn key protocols extensions custom-header-alist nowait))
    (set-process-query-on-exit-flag conn nil)
    (websocket-ensure-handshake url conn key protocols extensions custom-header-alist nowait)
    websocket))

(defun websocket-sentinel (url conn key protocols extensions custom-header-alist nowait)
  #'(lambda (process change)
      (let ((websocket (process-get process :websocket)))
        (websocket-debug websocket "State change to %s" change)
        (let ((status (process-status process)))
          (when (and nowait (eq status 'open))
            (websocket-ensure-handshake url conn key protocols extensions custom-header-alist nowait))

          (when (and (member status '(closed failed exit signal))
                     (not (eq 'closed (websocket-ready-state websocket))))
            (websocket-try-callback 'websocket-on-close 'on-close websocket))))))

(defun websocket-ensure-handshake (url conn key protocols extensions custom-header-alist nowait)
  (let ((url-struct (url-generic-parse-url url))
        (websocket (process-get conn :websocket)))
    (when (and (eq 'connecting (websocket-ready-state websocket))
               (memq (process-status conn)
                     (list 'run (if nowait 'connect 'open))))
      (websocket-debug websocket "Sending handshake, key: %s, acceptance: %s"
                       key (websocket-accept-string websocket))
      (process-send-string conn
                           (format "GET %s HTTP/1.1\r\n%s"
                                   (let ((path (url-filename url-struct)))
                                     (if (> (length path) 0) path "/"))
                                   (websocket-create-headers
                                    url key protocols extensions custom-header-alist))))))

(defun websocket-process-headers (url headers)
  "On opening URL, process the HEADERS sent from the server."
  (when (string-match "Set-Cookie: \(.*\)\r\n" headers)
    ;; The url-current-object is assumed to be set by
    ;; url-cookie-handle-set-cookie.
    (let ((url-current-object (url-generic-parse-url url)))
      (url-cookie-handle-set-cookie (match-string 1 headers)))))

(defun websocket-outer-filter (websocket output)
  "Filter the WEBSOCKET server's OUTPUT.
This will parse headers and process frames repeatedly until there
is no more output or the connection closes.  If the websocket
connection is invalid, the connection will be closed."
  (websocket-debug websocket "Received: %s" output)
  (let ((start-point)
        (text (concat (websocket-inflight-input websocket) output))
        (header-end-pos))
    (setf (websocket-inflight-input websocket) nil)
    ;; If we've received the complete header, check to see if we've
    ;; received the desired handshake.
    (when (and (eq 'connecting (websocket-ready-state websocket)))
      (if (and (setq header-end-pos (string-match "\r\n\r\n" text))
               (setq start-point (+ 4 header-end-pos)))
          (progn
            (condition-case err
                (progn
                  (websocket-verify-response-code text)
                  (websocket-verify-headers websocket text)
                  (websocket-process-headers (websocket-url websocket) text))
              (error
               (websocket-close websocket)
               (funcall (websocket-on-error websocket)
                        websocket 'on-open err)))
            (setf (websocket-ready-state websocket) 'open)
            (websocket-try-callback 'websocket-on-open 'on-open websocket))
        (setf (websocket-inflight-input websocket) text)))
    (when (eq 'open (websocket-ready-state websocket))
      (websocket-process-input-on-open-ws
       websocket (substring text (or start-point 0))))))

(defun websocket-verify-headers (websocket output)
  "Based on WEBSOCKET's data, ensure the headers in OUTPUT are valid.
The output is assumed to have complete headers.  This function
will either return t or call `error'.  This has the side-effect
of populating the list of server extensions to WEBSOCKET."
  (let ((accept-regexp
         (concat "Sec-Web[Ss]ocket-Accept: " (regexp-quote (websocket-accept-string websocket)))))
    (websocket-debug websocket "Checking for accept header regexp: %s" accept-regexp)
    (unless (string-match accept-regexp output)
      (signal 'websocket-invalid-header
              (list "Incorrect handshake from websocket: is this really a websocket connection?"))))
  (let ((case-fold-search t))
    (websocket-debug websocket "Checking for upgrade header")
    (unless (string-match "\r\nUpgrade: websocket\r\n" output)
      (signal 'websocket-invalid-header
              (list "No 'Upgrade: websocket' header found")))
    (websocket-debug websocket "Checking for connection header")
    (unless (string-match "\r\nConnection: upgrade\r\n" output)
      (signal 'websocket-invalid-header
              (list "No 'Connection: upgrade' header found")))
    (when (websocket-protocols websocket)
      (dolist (protocol (websocket-protocols websocket))
        (websocket-debug websocket "Checking for protocol match: %s"
                         protocol)
        (let ((protocols
               (if (string-match (format "\r\nSec-Websocket-Protocol: %s\r\n"
                                         protocol)
                                 output)
                   (list protocol)
                 (signal 'websocket-invalid-header
                         (list "Incorrect or missing protocol returned by the server.")))))
          (setf (websocket-negotiated-protocols websocket) protocols))))
    (let* ((extensions (websocket-parse-repeated-field
                        output
                        "Sec-WebSocket-Extensions"))
           (extra-extensions))
      (dolist (ext extensions)
        (let ((x (cl-first (split-string ext "; ?"))))
          (unless (or (member x (websocket-extensions websocket))
                      (member x extra-extensions))
            (push x extra-extensions))))
      (when extra-extensions
        (signal 'websocket-invalid-header
                (list (format "Non-requested extensions returned by server: %S"
                              extra-extensions))))
      (setf (websocket-negotiated-extensions websocket) extensions)))
  t)

;;;;;;;;;;;;;;;;;;;;;;
;; Websocket server ;;
;;;;;;;;;;;;;;;;;;;;;;

(defvar websocket-server-websockets nil
  "A list of current websockets live on any server.")

(cl-defun websocket-server (port &rest plist)
  "Open a websocket server on PORT.
If the plist contains a `:host' HOST pair, this value will be
used to configure the addresses the socket listens on. The symbol
`local' specifies the local host. If unspecified or nil, the
socket will listen on all addresses.

This also takes a plist of callbacks: `:on-open', `:on-message',
`:on-close' and `:on-error', which operate exactly as documented
in the websocket client function `websocket-open'.  Returns the
connection, which should be kept in order to pass to
`websocket-server-close'."
  (let* ((conn (make-network-process
                :name (format "websocket server on port %s" port)
                :server t
                :family 'ipv4
                :noquery t
                :filter 'websocket-server-filter
                :log 'websocket-server-accept
                :filter-multibyte nil
                :plist plist
                :host (plist-get plist :host)
                :service port)))
    conn))

(defun websocket-server-close (conn)
  "Closes the websocket, as well as all open websockets for this server."
  (let ((to-delete))
    (dolist (ws websocket-server-websockets)
      (when (eq (websocket-server-conn ws) conn)
        (if (eq (websocket-ready-state ws) 'closed)
            (unless (member ws to-delete)
              (push ws to-delete))
          (websocket-close ws))))
    (dolist (ws to-delete)
      (setq websocket-server-websockets (remove ws websocket-server-websockets))))
  (delete-process conn))

(defun websocket-server-accept (server client _message)
  "Accept a new websocket connection from a client."
  (let ((ws (websocket-inner-create
             :server-conn server
             :conn client
             :url client
             :server-p t
             :on-open (or (process-get server :on-open) 'identity)
             :on-message (or (process-get server :on-message) (lambda (_ws _frame)))
             :on-close (let ((user-method
                              (or (process-get server :on-close) 'identity)))
                         (lambda (ws)
                           (setq websocket-server-websockets
                                 (remove ws websocket-server-websockets))
                           (funcall user-method ws)))
             :on-error (or (process-get server :on-error)
                           'websocket-default-error-handler)
             :protocols (process-get server :protocol)
             :extensions (mapcar 'car (process-get server :extensions)))))
    (unless (member ws websocket-server-websockets)
      (push ws websocket-server-websockets))
    (process-put client :websocket ws)
    (set-process-coding-system client 'binary 'binary)
    (set-process-sentinel client
                          (lambda (process change)
                            (let ((websocket (process-get process :websocket)))
                              (websocket-debug websocket "State change to %s" change)
                              (when (and
                                     (member (process-status process) '(closed failed exit signal))
                                     (not (eq 'closed (websocket-ready-state websocket))))
                                (websocket-try-callback 'websocket-on-close 'on-close websocket)))))))

(defun websocket-create-headers (url key protocol extensions custom-headers-alist)
  "Create connections headers for the given URL, KEY, PROTOCOL, and EXTENSIONS.
Additionally, the CUSTOM-HEADERS-ALIST is passed from the client.
All these parameters are defined as in `websocket-open'."
  (let* ((parsed-url (url-generic-parse-url url))
         (host-port (if (url-port-if-non-default parsed-url)
                        (format "%s:%s" (url-host parsed-url) (url-port parsed-url))
                      (url-host parsed-url)))
         (cookie-header (url-cookie-generate-header-lines
                         host-port (car (url-path-and-query parsed-url))
                         (equal (url-type parsed-url) "wss"))))
    (format (concat "Host: %s\r\n"
                    "Upgrade: websocket\r\n"
                    "Connection: Upgrade\r\n"
                    "Sec-WebSocket-Key: %s\r\n"
                    "Sec-WebSocket-Version: 13\r\n"
                    (when protocol
                      (concat
                       (mapconcat
                        (lambda (protocol)
                          (format "Sec-WebSocket-Protocol: %s" protocol))
                        protocol "\r\n")
                       "\r\n"))
                    (when extensions
                      (format "Sec-WebSocket-Extensions: %s\r\n"
                              (mapconcat
                               (lambda (ext)
                                 (concat
                                  (car ext)
                                  (when (cdr ext) "; ")
                                  (when (cdr ext)
                                    (mapconcat 'identity (cdr ext) "; "))))
                               extensions ", ")))
                    (when cookie-header cookie-header)
                    (concat (mapconcat (lambda (cons) (format "%s: %s" (car cons) (cdr cons)))
                                       custom-headers-alist "\r\n")
                            (when custom-headers-alist "\r\n"))
                    "\r\n")
            host-port
            key
            protocol)))

(defun websocket-get-server-response (websocket client-protocols client-extensions)
  "Get the websocket response from client WEBSOCKET."
  (let ((separator "\r\n"))
      (concat "HTTP/1.1 101 Switching Protocols" separator
              "Upgrade: websocket" separator
              "Connection: Upgrade" separator
              "Sec-WebSocket-Accept: "
              (websocket-accept-string websocket) separator
              (let ((protocols
                         (websocket-intersect client-protocols
                                              (websocket-protocols websocket))))
                    (when protocols
                      (concat
                       (mapconcat
                        (lambda (protocol) (format "Sec-WebSocket-Protocol: %s"
                                              protocol)) protocols separator)
                       separator)))
              (let ((extensions (websocket-intersect
                                   client-extensions
                                   (websocket-extensions websocket))))
                  (when extensions
                    (concat
                     (mapconcat
                      (lambda (extension) (format "Sec-Websocket-Extensions: %s"
                                             extension)) extensions separator)
                     separator)))
              separator)))

(defun websocket-server-filter (process output)
  "This acts on all OUTPUT from websocket clients PROCESS."
  (let* ((ws (process-get process :websocket))
         (text (concat (websocket-inflight-input ws) output)))
    (setf (websocket-inflight-input ws) nil)
    (cond ((eq (websocket-ready-state ws) 'connecting)
           ;; check for connection string
           (let ((end-of-header-pos
                  (let ((pos (string-match "\r\n\r\n" text)))
                    (when pos (+ 4 pos)))))
               (if end-of-header-pos
                   (progn
                     (let ((header-info (websocket-verify-client-headers text)))
                       (if header-info
                           (progn (setf (websocket-accept-string ws)
                                        (websocket-calculate-accept
                                         (plist-get header-info :key)))
                                  (process-send-string
                                   process
                                   (websocket-get-server-response
                                    ws (plist-get header-info :protocols)
                                    (plist-get header-info :extensions)))
                                  (setf (websocket-ready-state ws) 'open)
                                  (websocket-try-callback 'websocket-on-open
                                                          'on-open ws))
                         (message "Invalid client headers found in: %s" output)
                         (process-send-string process "HTTP/1.1 400 Bad Request\r\n\r\n")
                         (websocket-close ws)))
                     (when (> (length text) (+ 1 end-of-header-pos))
                       (websocket-server-filter process (substring
                                                           text
                                                           end-of-header-pos))))
                 (setf (websocket-inflight-input ws) text))))
          ((eq (websocket-ready-state ws) 'open)
           (websocket-process-input-on-open-ws ws text))
          ((eq (websocket-ready-state ws) 'closed)
           (message "WARNING: Should not have received further input on closed websocket")))))

(defun websocket-verify-client-headers (output)
  "Verify the headers from the WEBSOCKET client connection in OUTPUT.
Unlike `websocket-verify-headers', this is a quieter routine.  We
don't want to error due to a bad client, so we just print out
messages and a plist containing `:key', the websocket key,
`:protocols' and `:extensions'."
  (cl-block nil
    (let ((case-fold-search t)
          (plist))
      (unless (string-match "HTTP/1.1" output)
        (message "Websocket client connection: HTTP/1.1 not found")
        (cl-return nil))
      (unless (string-match "^Host: " output)
        (message "Websocket client connection: Host header not found")
        (cl-return nil))
      (unless (string-match "^Upgrade: websocket\r\n" output)
        (message "Websocket client connection: Upgrade: websocket not found")
        (cl-return nil))
      (if (string-match "^Sec-WebSocket-Key: \\([[:graph:]]+\\)\r\n" output)
          (setq plist (plist-put plist :key (match-string 1 output)))
        (message "Websocket client connect: No key sent")
        (cl-return nil))
      (unless (string-match "^Sec-WebSocket-Version: 13" output)
        (message "Websocket client connect: Websocket version 13 not found")
        (cl-return nil))
      (when (string-match "^Sec-WebSocket-Protocol:" output)
        (setq plist (plist-put plist :protocols (websocket-parse-repeated-field
                                                 output
                                                 "Sec-Websocket-Protocol"))))
      (when (string-match "^Sec-WebSocket-Extensions:" output)
        (setq plist (plist-put plist :extensions (websocket-parse-repeated-field
                                                  output
                                                  "Sec-Websocket-Extensions"))))
      plist)))

(provide 'websocket)

;;; websocket.el ends here
