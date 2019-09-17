;;; eaf.el --- Emacs application framework

;; Filename: eaf.el
;; Description: Emacs application framework
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.2
;; Last-Updated: Tue Sep 17 09:30:54 2019 (-0400)
;;           By: Mingde (Matthew) Zeng
;; URL: http://www.emacswiki.org/emacs/download/eaf.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;;
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

;;; Commentary:
;;
;; Emacs application framework
;;

;;; Installation:
;;
;; Put eaf.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'eaf)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET eaf RET
;;

;;; Change log:
;;
;; 2018/06/15
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'dbus)
(require 'subr-x)

;;; Code:
(defcustom eaf-mode-hook '()
  "Eaf mode hook."
  :type 'hook
  :group 'eaf-mode)

(defvar eaf-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used by `eaf-mode'.")

(define-derived-mode eaf-mode text-mode "EAF"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'eaf-mode)
  (setq mode-name "EAF")
  ;; Split window combinations proportionally.
  (setq window-combination-resize t)    ;
  (set (make-local-variable 'buffer-id) (eaf-generate-id))
  (use-local-map eaf-mode-map)
  (run-hooks 'eaf-mode-hook))

(defvar eaf-python-file (expand-file-name "eaf.py" (file-name-directory load-file-name)))

(defvar eaf-process nil)

(defvar eaf-first-start-url nil)

(defvar eaf-first-start-app-name nil)

(defvar eaf-first-start-arguments nil)

(defvar eaf-title-length 30)

(defvar eaf-org-file-list '())

(defvar eaf-org-killed-file-list '())

(defvar eaf-last-frame-width 0)

(defvar eaf-last-frame-height 0)

(defvar eaf-grip-token nil)

(defvar eaf-http-proxy-host "")

(defvar eaf-http-proxy-port "")

(defcustom eaf-name "*eaf*"
  "Name of eaf buffer."
  :type 'string
  :group 'eaf)

(defcustom eaf-python-command "python3"
  "The Python interpreter used to run eaf.py."
  :type 'string
  :group 'eaf)

(defun eaf-call (method &rest args)
  (apply 'dbus-call-method
         :session                   ; use the session (not system) bus
         "com.lazycat.eaf"          ; service name
         "/com/lazycat/eaf"         ; path name
         "com.lazycat.eaf"          ; interface name
         method args))

(defun eaf-get-emacs-xid (frame)
  (frame-parameter frame 'window-id))

(defun eaf-start-process ()
  (interactive)
  (if (process-live-p eaf-process)
      (message "EAF process has started.")
    (setq eaf-process
          (apply 'start-process
                 eaf-name
                 eaf-name
                 eaf-python-command (append (list eaf-python-file) (eaf-get-render-size) (list eaf-http-proxy-host eaf-http-proxy-port))
                 ))
    (set-process-query-on-exit-flag eaf-process nil)
    (set-process-sentinel
     eaf-process
     #'(lambda (process event)
         (message (format "%s %s" process event))
         ))
    (message "EAF process starting...")))

(defun eaf-stop-process ()
  (interactive)
  ;; Kill eaf buffers.
  (let ((current-buf (current-buffer))
        (count 0))
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when (equal major-mode 'eaf-mode)
        (incf count)
        (kill-buffer buffer)))
    ;; Just report to me when eaf buffer exists.
    (if (> count 1)
        (message "Killed EAF %s buffer%s" count (if (> count 1) "s" ""))))

  ;; Clean cache url and app name, avoid next start process to open buffer.
  (setq eaf-first-start-url nil)
  (setq eaf-first-start-app-name nil)
  (setq eaf-first-start-arguments nil)

  ;; Clean `eaf-org-file-list' and `eaf-org-killed-file-list'.
  (dolist (org-file-name eaf-org-file-list)
    (eaf-delete-org-preview-file org-file-name))
  (setq eaf-org-file-list nil)
  (setq eaf-org-killed-file-list nil)

  ;; Kill process after kill buffer, make application can save session data.
  (eaf-kill-python-process))

(defun eaf-kill-python-process ()
  "Kill eaf background python process for debug.
NOTE: this function just use for developer debug.
Don't call this function if you not eaf developer."
  (interactive)
  (if (process-live-p eaf-process)
      ;; Delete eaf server process.
      (delete-process eaf-process)
    (message "EAF process has dead.")))

(defun eaf-restart-process ()
  (interactive)
  (eaf-stop-process)
  (eaf-start-process))

(defun eaf-get-render-size ()
  "Get allocation for render application in backend.
We need calcuate render allocation to make sure no black border around render content."
  (let* (;; We use `window-inside-pixel-edges' and `window-absolute-pixel-edges' calcuate height of window header, such as tabbar.
         (window-header-height (- (nth 1 (window-inside-pixel-edges)) (nth 1 (window-absolute-pixel-edges))))
         (width (frame-pixel-width))
         ;; Render height should minus mode-line height, minibuffer height, header height.
         (height (- (frame-pixel-height) (window-mode-line-height) (window-pixel-height (minibuffer-window)) window-header-height)))
    (mapcar (lambda (x) (format "%s" x)) (list width height))))

(defun eaf-get-window-allocation (&optional window)
  (let* ((window-edges (window-inside-pixel-edges window))
         (x (nth 0 window-edges))
         (y (nth 1 window-edges))
         (w (- (nth 2 window-edges) x))
         (h (- (nth 3 window-edges) y))
         )
    (list x y w h)))

(defun eaf-generate-id ()
  (format "%04x-%04x-%04x-%04x-%04x-%04x-%04x"
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))))

(defun eaf-create-buffer (input-content)
  (let ((eaf-buffer (generate-new-buffer (truncate-string-to-width input-content eaf-title-length))))
    (with-current-buffer eaf-buffer
      (eaf-mode)
      (read-only-mode))
    eaf-buffer))

(defun eaf-is-support (url)
  (dbus-call-method
   :session "com.lazycat.eaf"
   "/com/lazycat/eaf"
   "com.lazycat.eaf"
   "is_support"
   url))

(defun eaf-monitor-window-size-change (frame)
  (setq eaf-last-frame-width (frame-pixel-width frame))
  (setq eaf-last-frame-height (frame-pixel-height frame))
  (run-with-timer 1 nil (lambda () (eaf-try-adjust-view-with-frame-size))))

(defun eaf-try-adjust-view-with-frame-size ()
  (when (and (equal (frame-pixel-width) eaf-last-frame-width)
             (equal (frame-pixel-height) eaf-last-frame-height))
    (eaf-monitor-configuration-change)))

(defun eaf-monitor-configuration-change (&rest _)
  (ignore-errors
    (let (view-infos)
      (dolist (frame (frame-list))
        (dolist (window (window-list frame))
          (let ((buffer (window-buffer window)))
            (with-current-buffer buffer
              (if (eq major-mode 'eaf-mode)
                  (let* ((window-allocation (eaf-get-window-allocation window))
                         (x (nth 0 window-allocation))
                         (y (nth 1 window-allocation))
                         (w (nth 2 window-allocation))
                         (h (nth 3 window-allocation))
                         )
                    (add-to-list 'view-infos (format "%s:%s:%s:%s:%s:%s" buffer-id (eaf-get-emacs-xid frame) x y w h))
                    ))))))
      ;; I don't know how to make emacs send dbus-message with two-dimensional list.
      ;; So i package two-dimensional list in string, then unpack on server side. ;)
      (eaf-call "update_views" (mapconcat 'identity view-infos ","))
      )))

(defun eaf-delete-org-preview-file (org-file)
  (setq org-html-file (concat (file-name-sans-extension org-file) ".html"))
  (when (file-exists-p org-html-file)
    (delete-file org-html-file)
    (message (format "Clean org preview file %s (%s)" org-html-file org-file))
    ))

(defun eaf-org-killed-buffer-clean ()
  (dolist (org-killed-buffer eaf-org-killed-file-list)
    (unless (get-file-buffer org-killed-buffer)
      (setq eaf-org-file-list (remove org-killed-buffer eaf-org-file-list))
      (eaf-delete-org-preview-file org-killed-buffer)
      ))
  (setq eaf-org-killed-file-list nil))

(defun eaf-monitor-buffer-kill ()
  (ignore-errors
    (with-current-buffer (buffer-name)
      (cond ((eq major-mode 'org-mode)
             ;; NOTE:
             ;; Because save org buffer will trigger `kill-buffer' action,
             ;; but org buffer still live after do `kill-buffer' action.
             ;; So i run a timer to check org buffer is live after `kill-buffer' aciton.
             (when (member (buffer-file-name) eaf-org-file-list)
               (unless (member (buffer-file-name) eaf-org-killed-file-list)
                 (push (buffer-file-name) eaf-org-killed-file-list))
               (run-with-timer 1 nil (lambda () (eaf-org-killed-buffer-clean)))
               ))
            ((eq major-mode 'eaf-mode)
             (eaf-call "kill_buffer" buffer-id)
             (message (format "Kill %s" buffer-id)))
            ))))

(defun eaf-monitor-buffer-save ()
  (ignore-errors
    (with-current-buffer (buffer-name)
      (cond ((and
              (eq major-mode 'org-mode)
              (member (buffer-file-name) eaf-org-file-list))
             (org-html-export-to-html)
             (eaf-call "update_buffer_with_url" "app.orgpreviewer.buffer" (buffer-file-name) "")
             (message (format "export %s to html" (buffer-file-name))))))))

(defun eaf-monitor-key-event ()
  (unless
      (ignore-errors
        (with-current-buffer (buffer-name)
          (when (eq major-mode 'eaf-mode)
            (let* ((event last-command-event)
                   (key (make-vector 1 event))
                   (key-command (format "%s" (key-binding key)))
                   (key-desc (key-description key))
                   )

              ;; Uncomment for debug.
              ;; (message (format "!!!!! %s %s %s %s" event key key-command key-desc))

              (cond
               ;; Just send event when user insert single character.
               ;; Don't send event 'M' if user press Ctrl + M.
               ((and
                 (or
                  (equal key-command "self-insert-command")
                  (equal key-command "completion-select-if-within-overlay"))
                 (equal 1 (string-width (this-command-keys))))
                (eaf-call "send_key" buffer-id key-desc))
               ((string-match "^[CMSs]-.*" key-desc)
                (eaf-call "send_keystroke" buffer-id key-desc))
               ((or
                 (equal key-command "nil")
                 (equal key-desc "RET")
                 (equal key-desc "DEL")
                 (equal key-desc "TAB")
                 (equal key-desc "SPC")
                 (equal key-desc "<backtab>")
                 (equal key-desc "<home>")
                 (equal key-desc "<end>")
                 (equal key-desc "<left>")
                 (equal key-desc "<right>")
                 (equal key-desc "<up>")
                 (equal key-desc "<down>")
                 (equal key-desc "<prior>")
                 (equal key-desc "<next>")
                 )
                (eaf-call "send_key" buffer-id key-desc)
                )
               (t
                (unless (or
                         (equal key-command "keyboard-quit")
                         (equal key-command "kill-this-buffer")
                         (equal key-command "eaf-open"))
                  (ignore-errors (call-interactively (key-binding key))))
                )))
            ;; Set `last-command-event' with nil, emacs won't notify me buffer is ready-only,
            ;; because i insert nothing in buffer.
            (setq last-command-event nil))
          ))
    ;; If something wrong in `eaf-monitor-key-event', emacs will remove `eaf-monitor-key-event' from `pre-command-hook' hook list.
    ;; Then we add `eaf-monitor-key-event' in `pre-command-hook' list again, hahahaha.
    (run-with-timer
     0.1
     nil
     (lambda ()
       (progn
         (add-hook 'pre-command-hook #'eaf-monitor-key-event))))))

(defun eaf-focus-buffer (msg)
  (let* ((coordinate-list (split-string msg ","))
         (mouse-press-x (string-to-number (nth 0 coordinate-list)))
         (mouse-press-y (string-to-number (nth 1 coordinate-list))))
    (catch 'find-window
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (with-current-buffer buffer
            (if (eq major-mode 'eaf-mode)
                (let* ((window-allocation (eaf-get-window-allocation window))
                       (x (nth 0 window-allocation))
                       (y (nth 1 window-allocation))
                       (w (nth 2 window-allocation))
                       (h (nth 3 window-allocation))
                       )
                  (when (and
                         (> mouse-press-x x)
                         (< mouse-press-x (+ x w))
                         (> mouse-press-y y)
                         (< mouse-press-y (+ y h)))
                    (select-window window)
                    (throw 'find-window t)
                    )
                  ))))))))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "message_to_emacs"
 'message)

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "create_new_browser_buffer"
 'eaf-create-new-browser-buffer)

(defun eaf-create-new-browser-buffer (new-window-buffer-id)
  (let ((eaf-buffer (generate-new-buffer (concat "Browser Popup Window " new-window-buffer-id))))
    (with-current-buffer eaf-buffer
      (eaf-mode)
      (read-only-mode)
      (set (make-local-variable 'buffer-id) new-window-buffer-id)
      (set (make-local-variable 'buffer-url) "")
      (set (make-local-variable 'buffer-app-name) "browser")
      )
    (switch-to-buffer eaf-buffer)))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "request_kill_buffer"
 'eaf-request-kill-buffer)

(defun eaf-request-kill-buffer (kill-buffer-id)
  (catch 'found-match-buffer
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when (equal major-mode 'eaf-mode)
        (when (string= buffer-id kill-buffer-id)
          (kill-buffer buffer)
          (message (format "Request kill buffer %s" kill-buffer-id))
          (throw 'found-match-buffer t))))))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "focus_emacs_buffer"
 'eaf-focus-buffer)

(defun eaf-start-finish ()
  "Call `eaf-open-internal' after receive `start_finish' signal from server process."
  (eaf-open-internal eaf-first-start-url eaf-first-start-app-name eaf-first-start-arguments))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "start_finish"
 'eaf-start-finish)

(defun eaf-update-buffer-title (bid title)
  (when (> (length title) 0)
    (catch 'find-buffer
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (with-current-buffer buffer
            (when (and
                   (eq major-mode 'eaf-mode)
                   (equal buffer-id bid))
              (rename-buffer (truncate-string-to-width title eaf-title-length))
              (throw 'find-buffer t)
              )))))))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "update_buffer_title"
 'eaf-update-buffer-title)

(defun eaf-open-buffer-url (url)
  (eaf-open url))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "open_buffer_url"
 'eaf-open-buffer-url)

(defun eaf-read-string (interactive-string)
  "Like `read-string', but return nil if user execute `keyboard-quit' when input."
  (condition-case nil (read-string interactive-string) (quit nil)))

(defun eaf-input-message (buffer-id interactive-string callback-type)
  (let* ((input-message (eaf-read-string interactive-string)))
    (when input-message
      (eaf-call "handle_input_message" buffer-id callback-type input-message)
      )))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "input_message"
 'eaf-input-message)

(add-hook 'window-size-change-functions 'eaf-monitor-window-size-change)
(add-hook 'window-configuration-change-hook #'eaf-monitor-configuration-change)
(add-hook 'pre-command-hook #'eaf-monitor-key-event)
(add-hook 'kill-buffer-hook #'eaf-monitor-buffer-kill)
(add-hook 'after-save-hook #'eaf-monitor-buffer-save)

(defun eaf-open-internal (url app-name arguments)
  (let* ((buffer (eaf-create-buffer url))
         buffer-result)
    (with-current-buffer buffer
      (setq buffer-result (eaf-call "new_buffer" buffer-id url app-name arguments)))
    (if (equal buffer-result "")
        (progn
          ;; Switch to new buffer if buffer create successful.
          (switch-to-buffer buffer)
          (set (make-local-variable 'buffer-url) url)
          (set (make-local-variable 'buffer-app-name) app-name)
          ;; Focus to file window if is previewer application.
          (when (or (string= app-name "markdownpreviewer")
                    (string= app-name "orgpreviewer"))
            (other-window +1)))
      ;; Kill buffer and show error message from python server.
      (kill-buffer buffer)
      (switch-to-buffer eaf-name)
      (message buffer-result))
    ))

(defun eaf-open-browser (url &optional arguments)
  "Open EAF browser application given a URL and ARGUMENTS."
  (interactive "MEAF Browser - Enter URL: ")
  ;; Validate URL legitimacy
  (if (and (not (string-prefix-p "/" url))
           (not (string-prefix-p "~" url))
           (string-match "^\\(https?:\/\/\\)?[a-z0-9]+\\([\-\.]\\{1\\}[a-z0-9]+\\)*\.[a-z]\\{2,5\\}\\(:[0-9]{1,5}\\)?\\(\/.*\\)?$" url))
      (progn (setq app-name "browser")
             (unless (or (string-prefix-p "http://" url) (string-prefix-p "https://" url))
               (setq url (concat "http://" url)))
             (eaf-open url "browser" arguments))
    (when (string= app-name "browser")
      (message (format "EAF: %s is an invalid URL." url)))))

(defun eaf-open-demo ()
  "Open EAF demo screen to verify that EAF is working properly."
  (interactive)
  (eaf-open "eaf-demo" "demo"))

(defun eaf-open-camera ()
  "Open EAF camera application."
  (interactive)
  (eaf-open "eaf-camera" "camera"))

(defun eaf-open-qutebrowser ()
  "Open EAF Qutebrowser application."
  (interactive)
  (eaf-open "eaf-qutebrowser" "qutebrowser"))

(defun eaf-open (url &optional app-name arguments)
  "Open an EAF application with URL, optional APP-NAME and ARGUMENTS.

When called interactively, URL accepts a file that can be opened by EAF."
  (interactive "FOpen with EAF: ")
  ;; Try to set app-name along with url if app-name is unset.
  (when (and (not app-name) (file-exists-p url))
    (setq url (expand-file-name url))
    (setq extension-name (file-name-extension url))
    (cond ((member extension-name '("pdf" "xps" "oxps" "cbz" "epub" "fb2" "fbz"))
           (setq app-name "pdfviewer"))
          ((member extension-name '("md"))
           ;; Try get user's github token if `eaf-grip-token' is nil.
           (if eaf-grip-token
               (setq arguments eaf-grip-token)
             (setq arguments (read-string "Fill your own github token (or set `eaf-grip-token' with token string): ")))
           ;; Split window to show file and previewer.
           (eaf-split-preview-windows)
           (setq app-name "markdownpreviewer"))
          ((member extension-name '("jpg" "png" "bmp"))
           (setq app-name "imageviewer"))
          ((member extension-name '("avi" "rmvb" "ogg" "mp4"))
           (setq app-name "videoplayer"))
          ((member extension-name '("html"))
           (setq url (concat "file://" url))
           (setq app-name "browser"))
          ((member extension-name '("org"))
           ;; Find file first, because `find-file' will trigger `kill-buffer' operation.
           (save-excursion
             (find-file url)
             (with-current-buffer (buffer-name)
               (org-html-export-to-html)))
           ;; Add file name to `eaf-org-file-list' after command `find-file'.

           (unless (member url eaf-org-file-list)
             (push url eaf-org-file-list))
           ;; Split window to show file and previewer.
           (eaf-split-preview-windows)
           (setq app-name "orgpreviewer"))))

  (unless arguments
    (setq arguments ""))
  (if app-name
      ;; Open url with eaf application if app-name is not empty.
      (if (process-live-p eaf-process)
          (let (exists-eaf-buffer)
            ;; Try to open buffer
            (catch 'found-match-buffer
              (dolist (buffer (buffer-list))
                (set-buffer buffer)
                (when (equal major-mode 'eaf-mode)
                  (when (and (string= buffer-url url)
                             (string= buffer-app-name app-name))
                    (setq exists-eaf-buffer buffer)
                    (throw 'found-match-buffer t)))))
            ;; Switch to exists buffer,
            ;; if no match buffer found, call `eaf-open-internal'.
            (if exists-eaf-buffer
                (switch-to-buffer exists-eaf-buffer)
              (eaf-open-internal url app-name arguments)))
        ;; Record user input, and call `eaf-open-internal' after receive `start_finish' signal from server process.
        (setq eaf-first-start-url url)
        (setq eaf-first-start-app-name app-name)
        (setq eaf-first-start-arguments arguments)
        (eaf-start-process)
        (message (format "Opening %s with EAF-%s..." url app-name)))
    ;; Output something to user if app-name is empty string.
    (if (or (string-prefix-p "/" url)
            (string-prefix-p "~" url))
        (if (not (file-exists-p url))
            (message (format "EAF: %s does not exist." url))
          (message (format "EAF doesn't know how to open %s." url)))
      (message (format "EAF doesn't know how to open %s." url)))))

(defun eaf-split-preview-windows ()
  (delete-other-windows)
  (find-file url)
  (split-window-horizontally)
  (other-window +1))

(defun eaf-show-file-qrcode (url)
  (interactive "FShow file QR code: ")
  (eaf-open url "filetransfer"))

(defun dired-show-file-qrcode ()
  (interactive)
  (eaf-show-file-qrcode (dired-get-filename)))

(defun eaf-air-share ()
  (interactive)
  (let* ((current-symbol (if (use-region-p)
                             (buffer-substring-no-properties (region-beginning) (region-end))
                           (thing-at-point 'symbol)))
         (input-string (string-trim (read-string (format "Info (%s): " current-symbol)))))
    (when (string-empty-p input-string)
      (setq input-string current-symbol))
    (eaf-open input-string "airshare")
    ))

(defun eaf-upload-file (dir)
  (interactive "DDirectory to save uploade file: ")
  (eaf-open dir "fileuploader"))

(defun eaf-dired-open-file ()
  "Open html/pdf/image/video file with eaf, other file use `find-file'"
  (interactive)
  (dolist (file (dired-get-marked-files))
    (setq extension-name (file-name-extension file))
    (cond ((member extension-name '("html"))
           (eaf-open (concat "file://" file) "browser"))
          ((member extension-name '("pdf" "xps" "oxps" "cbz" "epub" "fb2" "fbz"))
           (eaf-open file "pdfviewer"))
          ((member extension-name '("jpg" "png" "bmp"))
           (eaf-open file "imageviewer"))
          ((member extension-name '("avi" "rmvb" "ogg" "mp4"))
           (eaf-open file "videoplayer"))
          (t
           (find-file file)))))

;;;;;;;;;;;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eaf-get-view-info ()
  (let* ((window-allocation (eaf-get-window-allocation (selected-window)))
         (x (nth 0 window-allocation))
         (y (nth 1 window-allocation))
         (w (nth 2 window-allocation))
         (h (nth 3 window-allocation)))
    (format "%s:%s:%s:%s:%s" buffer-id x y w h)))

;;;;;;;;;;;;;;;;;;;; Advice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice scroll-other-window (around eaf-scroll-up-or-next-page activate)
  "When next buffer is `eaf-mode', do `eaf-scroll-up-or-next-page'."
  (other-window +1)
  (if (eq major-mode 'eaf-mode)
      (let ((arg (ad-get-arg 0)))
        (if (null arg)
            (eaf-call "scroll_buffer" (eaf-get-view-info) "up" "page")
          (eaf-call "scroll_buffer" (eaf-get-view-info) "up" "line"))
        (other-window -1))
    (other-window -1)
    ad-do-it))

(defadvice scroll-other-window-down (around eaf-scroll-down-or-previous-page activate)
  "When next buffer is `eaf-mode', do `eaf-scroll-down-or-previous-page'."
  (other-window +1)
  (if (eq major-mode 'eaf-mode)
      (let ((arg (ad-get-arg 0)))
        (if (null arg)
            (eaf-call "scroll_buffer" (eaf-get-view-info) "down" "page")
          (eaf-call "scroll_buffer" (eaf-get-view-info) "down" "line"))
        (other-window -1))
    (other-window -1)
    ad-do-it))

(provide 'eaf)

;;; eaf.el ends here
