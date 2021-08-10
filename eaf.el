;;; eaf.el --- Emacs application framework  -*- lexical-binding: t; -*-

;; Filename: eaf.el
;; Description: Emacs application framework
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: Sat Jul 31 11:35:34 2021 (-0400)
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

;;; Commentary:
;;
;; Emacs Application Framework
;;

;;; Installation:
;;
;; Please check README
;;

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

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'map)
(require 's)
(require 'seq)
(require 'subr-x)
(require 'bookmark)

(define-obsolete-function-alias 'eaf-setq 'setq "Version 0.5, Commit d8abd23"
  "See https://github.com/manateelazycat/emacs-application-framework/issues/734.
for more information.

In summary, the EAF's python applications can now access users' customized
variables with the get_emacs_var module, eliminating the need to store
them in `eaf-var-list' and thus, `eaf-setq' is no longer necessary.")

(defvar eaf-app-binding-alist '()
  "Mapping app names to keybinding variables.

Any new app should add the its name and the corresponding
keybinding variable to this list.")

(defvar eaf-app-module-path-alist '())

(defun eaf-add-subdirs-to-load-path ()
  "Recursively add all subdirectories of `default-directory' to `load-path'.
More precisely, this uses only the subdirectories whose names
start with letters or digits; it excludes any subdirectory named `RCS'
or `CVS', and any subdirectory that contains a file named `.nosearch'."
  (let (dirs
        attrs
        (pending (list default-directory)))
    ;; This loop does a breadth-first tree walk on DIR's subtree,
    ;; putting each subdir into DIRS as its contents are examined.
    (while pending
      (push (pop pending) dirs)
      (let* ((this-dir (car dirs))
             (contents (directory-files this-dir))
             (default-directory this-dir)
             (canonicalized (if (fboundp 'w32-untranslated-canonical-name)
                                (w32-untranslated-canonical-name this-dir))))
        ;; The Windows version doesn't report meaningful inode numbers, so
        ;; use the canonicalized absolute file name of the directory instead.
        (setq attrs (or canonicalized
                        (nthcdr 10 (file-attributes this-dir))))
        (unless (member attrs normal-top-level-add-subdirs-inode-list)
          (push attrs normal-top-level-add-subdirs-inode-list)
          (dolist (file contents)
            (and
             ;; NOTE:
             ;; Don't scan node_modules directories, such as EAF npm subdirectories.
             (not (string-match-p "/node_modules" this-dir))

             (string-match "\\`[[:alnum:]]" file)
             ;; The lower-case variants of RCS and CVS are for DOS/Windows.
             (not (member file '("RCS" "CVS" "rcs" "cvs")))
             (file-directory-p file)
             (let ((expanded (expand-file-name file)))
               (or (file-exists-p (expand-file-name ".nosearch" expanded))
                   (setq pending (nconc pending (list expanded))))))))))
    (normal-top-level-add-to-load-path (cdr (nreverse dirs)))))

(defun eaf-add-app-dirs-to-load-path ()
  "Add EAF app directories where .el exists to `load-path'."
  (let ((default-directory (file-name-directory (locate-library "eaf"))))
    (add-to-list 'load-path default-directory)
    (eaf-add-subdirs-to-load-path)))

(eaf-add-app-dirs-to-load-path)

(require 'eaf-epc)
(require 'eaf-browser)
(require 'eaf-pdf-viewer)
(require 'eaf-markdown-previewer)
(require 'eaf-js-video-player)
(require 'eaf-video-player)
(require 'eaf-image-viewer)
(require 'eaf-org-previewer)
(require 'eaf-mindmap)
(require 'eaf-mail)
(require 'eaf-terminal)
(require 'eaf-camera)
(require 'eaf-jupyter)
(require 'eaf-netease-cloud-music)
(require 'eaf-music-player)
(require 'eaf-system-monitor)
(require 'eaf-file-manager)
(require 'eaf-file-browser)
(require 'eaf-demo)
(require 'eaf-vue-demo)

;;;###autoload
(defun eaf-install-dependencies ()
  "An interactive function that run install-eaf.sh or install-eaf-win32.js or install-eaf-mac.sh for Linux or Windows or macOS respectively."
  (interactive)
  (let* ((eaf-dir (file-name-directory (locate-library "eaf")))
         (default-directory eaf-dir))
    (cond ((eq system-type 'gnu/linux)
           (shell-command (concat "./install-eaf.sh" "&")))
          ((memq system-type '(cygwin windows-nt ms-dos))
           (shell-command (format "node %s" (concat "install-eaf-win32.js" "&"))))
          ((eq system-type 'darwin)
           (shell-command (concat "./install-eaf-mac.sh" "&"))))))


(defgroup eaf nil
  "Emacs Application Framework."
  :group 'applications)

(defcustom eaf-mode-hook '()
  "EAF mode hook."
  :type 'hook)

(defvar eaf-mode-map*
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-h m") #'eaf-describe-bindings)
    (define-key map (kbd "C-o") #'eaf-duplicate-current-buffer)
    (define-key map [remap describe-bindings] #'eaf-describe-bindings)
    (define-key map (kbd "C-c b") #'eaf-open-bookmark)
    (define-key map (kbd "C-c i") #'eaf-import-chrome-bookmarks)
    (define-key map (kbd "C-c e") #'eaf-open-external)
    (define-key map (kbd "M-'") #'eaf-toggle-fullscreen)
    (define-key map (kbd "M-/") #'eaf-get-path-or-url)
    (define-key map (kbd "M-[") #'eaf-share-path-or-url)
    (define-key map (vector 'remap #'keyboard-quit) #'eaf-keyboard-quit)
    (define-key map (vector 'remap #'self-insert-command) #'eaf-send-key)
    (dolist (single-key '("RET" "DEL" "TAB" "SPC" "<backtab>" "<home>" "<end>" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>" "<backspace>" "<return>"))
      (define-key map (kbd single-key) #'eaf-send-key))
    map)
  "Keymap for default bindings available in all apps.")

(defvar eaf-mode-map nil
  "Keymap used by `eaf-mode'.

Don't modify this map directly.  To bind keys for all apps use
`eaf-mode-map*' and to bind keys for individual apps use
`eaf-bind-key'.")

(defvar eaf-server nil
  "The EAF Server.")

(defvar eaf-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") #'eaf-edit-buffer-switch-to-org-mode)
    (define-key map (kbd "C-c C-k") #'eaf-edit-buffer-cancel)
    (define-key map (kbd "C-c C-c") #'eaf-edit-buffer-confirm)
    map))

(define-derived-mode eaf-edit-mode text-mode "EAF/edit"
  "The major mode to edit focus text input.")

(defun eaf-describe-bindings ()
  "Like `describe-bindings' for EAF buffers."
  (interactive)
  (let ((emulation-mode-map-alists nil)
        (eaf-mode-map (current-local-map)))
    (call-interactively 'describe-mode)))

(defvar-local eaf--buffer-id nil
  "Internal id used by EAF app.")

(defvar-local eaf--buffer-url nil
  "EAF buffer-local URL.")

(defvar-local eaf--buffer-app-name nil
  "EAF buffer-local app-name.")

(defvar-local eaf--buffer-args nil
  "EAF buffer-local args.")

(defvar-local eaf--buffer-map-alist nil
  "EAF buffer-local map alist.")

(defvar-local eaf--buffer-map-alist-order 1
  "Order of EAF buffer-local map alist in `emulation-mode-map-alists'.")

(define-derived-mode eaf-mode fundamental-mode "EAF"
  "Major mode for Emacs Application Framework buffers.

This mode is used by all apps. The mode map `eaf-mode-map' is
created dynamically for each app and should not be changed
manually. See `eaf-bind-key' for customization of app bindings.

Within EAF buffers the variable `eaf--buffer-app-name' holds the
name of the current app. Each app can setup app hooks by using
`eaf-<app-name>-hook'. This hook runs after the app buffer has
been initialized."
  ;; Split window combinations proportionally.
  (setq-local window-combination-resize t)
  ;; Disable cursor in eaf buffer.
  (setq-local cursor-type nil)

  (set (make-local-variable 'eaf--buffer-id) (eaf--generate-id))
  (setq-local bookmark-make-record-function #'eaf--bookmark-make-record)

  ;; Copy default value in case user already has bindings there
  (setq-local emulation-mode-map-alists
              (copy-alist (default-value 'emulation-mode-map-alists)))
  ;; Construct map alist
  (setq-local eaf--buffer-map-alist (list (cons t eaf-mode-map)))
  ;; Eanble mode map and make it the first priority
  (add-to-ordered-list
   'emulation-mode-map-alists
   'eaf--buffer-map-alist
   'eaf--buffer-map-alist-order)

  (add-hook 'kill-buffer-hook #'eaf--monitor-buffer-kill nil t)
  (add-hook 'kill-emacs-hook #'eaf--monitor-emacs-kill))

(defvar eaf-python-file (expand-file-name "eaf.py" (file-name-directory load-file-name)))

(defvar eaf-server-port nil)

(defun eaf-epcs-server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (let*
      ((connect-function connect-function)
       (name (format "EPC Server %s" (eaf-epc-uid)))
       (buf (eaf-epc-make-procbuf (format " *%s*" name)))
       (main-process
        (make-network-process
         :name name
         :buffer buf
         :family 'ipv4
         :server t
         :host "127.0.0.1"
         :service (or port t)
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

(defun eaf--start-epc-server ()
  "Function to start the EPC server."
  (unless eaf-server
    (setq eaf-server
          (eaf-epcs-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (eaf-epc-define-method mngr 'eval-in-emacs 'eval-in-emacs-func)
               (eaf-epc-define-method mngr 'get-emacs-var 'get-emacs-var-func)
               ))))
    (if eaf-server
        (setq eaf-server-port (process-contact eaf-server :service))
      (error "[EAF] eaf-server failed to start")))
  eaf-server)

(when noninteractive
  ;; Start "event loop".
  (cl-loop repeat 600
           do (sleep-for 0.1)))

(defun eval-in-emacs-func (&rest args)
  (apply (read (car args))
         (mapcar
          (lambda (arg)
            (let ((arg (eaf--decode-string arg)))
              (cond ((string-prefix-p "'" arg) ;; single quote
                     (read (substring arg 1)))
                    ((string= arg "TRUE") 't)
                    ((string= arg "FALSE") 'nil)
                    ((and (string-prefix-p "(" arg)
                          (string-suffix-p ")" arg)) ;; list
                     (split-string (substring arg 1 -1) " "))
                    (t arg))))
          (cdr args))))

(defun get-emacs-var-func (var-name)
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defvar eaf-epc-process nil)

(defvar eaf-internal-process nil)
(defvar eaf-internal-process-prog nil)
(defvar eaf-internal-process-args nil)

(defvar eaf--active-buffers nil
  "Contains a list of '(buffer-url buffer-app-name buffer-args).")

(defvar eaf--webengine-include-private-codec nil)

(defvar eaf-org-file-list '())

(defvar eaf-org-killed-file-list '())

(defvar eaf-last-frame-width 0)

(defvar eaf-last-frame-height 0)

(when (eq system-type 'darwin)
  (defvar eaf--mac-switch-to-python nil
    "Record if Emacs switchs to Python process")

  (defvar eaf--mac-has-focus t
    "Record if Emacs has focus"))

(defcustom eaf-name "*eaf*"
  "Name of EAF buffer."
  :type 'string)

(defcustom eaf-python-command (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python interpreter used to run eaf.py."
  :type 'string)

(defcustom eaf-config-location (expand-file-name (locate-user-emacs-file "eaf/"))
  "Directory where eaf will store configuration files."
  :type 'directory)

(defcustom eaf-marker-letters "ASDFHJKLWEOPCNM"
  ""
  :type 'string)

(defcustom eaf-buffer-background-color "#000000"
  ""
  :type 'string)

(defcustom eaf-markdown-extension-list
  '("md")
  "The extension list of markdown previewer application."
  :type 'cons)

(defcustom eaf-image-extension-list
  '("jpg" "jpeg" "png" "bmp" "gif" "svg" "webp")
  "The extension list of image viewer application."
  :type 'cons)

(defcustom eaf-video-extension-list
  '("avi" "webm" "rmvb" "ogg" "mp4" "mkv" "m4v")
  "The extension list of video player application."
  :type 'cons)

(defcustom eaf-org-extension-list
  '("org")
  "The extension list of org previewer application."
  :type 'cons)

(defcustom eaf-mindmap-extension-list
  '("emm" "mm")
  "The extension list of mindmap application."
  :type 'cons)

(defcustom eaf-office-extension-list
  '("docx" "doc" "ppt" "pptx" "xlsx" "xls")
  "The extension list of office application."
  :type 'cons)

(defcustom eaf-find-file-ext-blacklist '()
  "A blacklist of extensions to avoid when opening `find-file' file using EAF."
  :type 'cons)

(defcustom eaf-proxy-host ""
  "Proxy Host used by EAF Browser."
  :type 'string)

(defcustom eaf-proxy-port ""
  "Proxy Port used by EAF Browser."
  :type 'string)

(defcustom eaf-proxy-type ""
  "Proxy Type used by EAF Browser.  The value is either \"http\" or \"socks5\"."
  :type 'string)

(defcustom eaf-enable-debug nil
  "If you got segfault error, please turn this option.
Then EAF will start by gdb, please send new issue with `*eaf*' buffer content when next crash."
  :type 'boolean)

(defcustom eaf-kill-process-after-last-buffer-closed t
  "Kill eaf process when last eaf buffer closed, default is non-nil.

Improve EAF new page creation speed if this option is nil."
  :type 'boolean)

(defcustom eaf-wm-name ""
  "The desktop name, set by function `eaf--get-current-desktop-name'."
  :type 'string)

(defcustom eaf-wm-focus-fix-wms
  `(
    "i3"                                ;i3
    "LG3D"                              ;QTile
    "Xpra"                              ;Windows WSL
    "EXWM"                              ;EXWM
    "Xfwm4"                             ;Xfce4
    )
  "Set mouse cursor to frame bottom in these wms, to make EAF receive input event.

EAF confirms that the desktop environment or window manager you can work includes:
KDE, Gnome2, Gnome3, Mate, Xfce, LXDE, Sway, i3, QTile, Xpra, EXWM.

If your window manager can't receive input event, you can try add `NAME' of command `wmctrl -m' to this list.

Please send PR if it works.
Please fill an issue if it still doesn't work."
  :type 'list)

(defvar eaf-app-display-function-alist
  '(("markdown-previewer" . eaf--markdown-preview-display)
    ("org-previewer" . eaf--org-preview-display))
  "Mapping app names to display functions.

Display functions are called to initilize the initial view when
starting an app.

A display function receives the initialized app buffer as
argument and defaults to `switch-to-buffer'.")


(defvar eaf-app-bookmark-handlers-alist
  '(("browser" . eaf--browser-bookmark)
    ("pdf-viewer" . eaf--pdf-viewer-bookmark))
  "Mapping app names to bookmark handler functions.

A bookmark handler function is used as
`bookmark-make-record-function' and should follow its spec.")

(defvar eaf-app-extensions-alist
  '(("pdf-viewer" . eaf-pdf-extension-list)
    ("markdown-previewer" . eaf-markdown-extension-list)
    ("image-viewer" . eaf-image-extension-list)
    ("video-player" . eaf-video-extension-list)
    ("browser" . eaf-browser-extension-list)
    ("org-previewer" . eaf-org-extension-list)
    ("mindmap" . eaf-mindmap-extension-list)
    ("office" . eaf-office-extension-list))
  "Mapping app names to extension list variables.

A new app can use this to configure extensions which should
handled by it.")

(defvar eaf--monitor-configuration-p t
  "When this variable is non-nil, `eaf-monitor-configuration-change' executes.
This variable is used to open buffer in backend and avoid graphics blink.

EAF call python method `new_buffer' to create EAF application buffer.
EAF call python method `update_views' to create EAF application view.

Python process only create application view when Emacs window or buffer state change.")

(defvar eaf-fullscreen-p nil
  "When non-nil, EAF will intelligently hide modeline as necessray.")

(defvar eaf-buffer-title-format "%s")

(defvar-local eaf--bookmark-title nil)

(defvar-local eaf-mindmap--current-add-mode nil)

(defmacro eaf-for-each-eaf-buffer (&rest body)
  "A syntactic sugar to loop through each EAF buffer and evaluat BODY.

Within BODY, `buffer' can be used to"
  `(dolist (buffer (eaf--get-eaf-buffers))
     (with-current-buffer buffer
       ,@body)))

(defun eaf--bookmark-make-record ()
  "Create a EAF bookmark.

The bookmark will try to recreate EAF buffer session.
For now only EAF browser app is supported."
  (let ((handler (cdr
                  (assoc eaf--buffer-app-name
                         eaf-app-bookmark-handlers-alist))))
    (when handler
      (funcall handler))))

(defun eaf--bookmark-restore (bookmark)
  "Restore EAF buffer according to BOOKMARK."
  (let ((app (cdr (assq 'eaf-app bookmark))))
    (cond ((equal app "browser")
           (eaf-open-browser (cdr (assq 'filename bookmark))))
          ((equal app "pdf-viewer")
           (eaf-open (cdr (assq 'filename bookmark)))))))

;;;###autoload
(defun eaf-open-bookmark ()
  "Command to open or create EAF bookmarks with completion."
  (interactive)
  (bookmark-maybe-load-default-file)
  (let* ((bookmarks (cl-remove-if-not
                     (lambda (entry)
                       (bookmark-prop-get entry 'eaf-app))
                     bookmark-alist))
         (names (mapcar #'car bookmarks))
         (cand (completing-read "EAF Bookmarks: " bookmarks)))
    (cond ((member cand names)
           (bookmark-jump cand))
          (t
           (unless (derived-mode-p 'eaf-mode)
             (message "This command can only be called in an EAF buffer!"))
           ;; create new one for current buffer with provided name
           (bookmark-set cand)))))

(defvar eaf--existing-bookmarks nil
  "Existing bookmarks in Emacs.
A hashtable, key is url and value is title.")

(defun eaf--load-existing-bookmarks()
  "Load existing bookmarks."
  (let ((bookmarks (make-hash-table :test 'equal)))
    (dolist (bm bookmark-alist)
      (let* ((name (car bm))
             (file (bookmark-get-filename name)))
        (puthash file name bookmarks)))
    bookmarks))

(defun eaf-open-external ()
  "Command to open current path or url with external application."
  (interactive)
  (let ((path-or-url (eaf-get-path-or-url)))
    (cond ((memq system-type '(cygwin windows-nt ms-dos))
           (w32-shell-execute "open" path-or-url))
          ((eq system-type 'darwin)
           (shell-command (concat "open " (shell-quote-argument path-or-url))))
          ((eq system-type 'gnu/linux)
           (let ((process-connection-type nil))
             (start-process "" nil "xdg-open" path-or-url))))))

(defun eaf-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (eaf-deferred-chain
   (eaf-epc-call-deferred eaf-epc-process (read method) args)))

(defun eaf-call-sync (method &rest args)
  "Call Python EPC function METHOD and ARGS synchronously."
  (eaf-epc-call-sync eaf-epc-process (read method) args))

(defun eaf-get-emacs-xid (frame)
  "Get Emacs FRAME xid."
  (if (eaf--called-from-wsl-on-windows-p)
      (eaf-call-sync "get_emacs_wsl_window_id")
    (frame-parameter frame 'window-id)))

(defun eaf-start-process ()
  "Start EAF process if it isn't started."
  (cond
   ((not eaf--active-buffers)
    (user-error "[EAF] Please initiate EAF with eaf-open-... functions only"))
   ((eaf-epc-live-p eaf-epc-process)
    (user-error "[EAF] Process is already running")))
  ;; start epc server and set `eaf-server-port'
  (eaf--start-epc-server)
  (let* ((eaf-args (append
                    (list eaf-python-file)
                    (eaf-get-render-size)
                    (list (number-to-string eaf-server-port))
                    ))
         (gdb-args (list "-batch" "-ex" "run" "-ex" "bt" "--args" eaf-python-command)))
    (if (and (getenv "WAYLAND_DISPLAY") (not (string= (getenv "WAYLAND_DISPLAY") "")))
        (progn
          ;; We need manually set scale factor when at Gnome/Wayland environment.
          ;; It is important to set QT_AUTO_SCREEN_SCALE_FACTOR=0
          ;; otherwise Qt which explicitly force high DPI enabling get scaled TWICE.
          (setenv "QT_AUTO_SCREEN_SCALE_FACTOR" "0")
          ;; Set EAF application scale factor.
          (setenv "QT_SCALE_FACTOR" "1")
          ;; Force xwayland to ensure SWay works.
          (setenv "QT_QPA_PLATFORM" "xcb"))
      (setq process-environment
            (seq-filter
             (lambda (var)
               (and (not (string-match-p "QT_SCALE_FACTOR" var))
                    (not (string-match-p "QT_SCREEN_SCALE_FACTOR" var))))
             process-environment)))
    ;; Start python process.
    (if eaf-enable-debug
        (progn
          (setq eaf-internal-process-prog "gdb")
          (setq eaf-internal-process-args (append gdb-args eaf-args)))
      (setq eaf-internal-process-prog eaf-python-command)
      (setq eaf-internal-process-args eaf-args))
    (let ((process-connection-type (not (eaf--called-from-wsl-on-windows-p))))
      (setq eaf-internal-process
            (apply 'start-process
                   eaf-name eaf-name
                   eaf-internal-process-prog eaf-internal-process-args)))
    (set-process-query-on-exit-flag eaf-internal-process nil))
  (message "[EAF] Process starting..."))

(defun eaf-stop-process (&optional restart)
  "Stop EAF process and kill all EAF buffers.

If RESTART is non-nil, cached URL and app-name will not be cleared."
  (interactive)
  (unless restart
    ;; Clear active buffers
    (setq eaf--active-buffers nil)
    ;; Remove all EAF related hooks since the EAF process is stopped.
    (remove-hook 'kill-buffer-hook #'eaf--monitor-buffer-kill)
    (remove-hook 'kill-emacs-hook #'eaf--monitor-emacs-kill)
    (remove-hook 'after-save-hook #'eaf--org-preview-monitor-buffer-save)
    (remove-hook 'kill-buffer-hook #'eaf--org-preview-monitor-kill)
    (remove-hook 'window-size-change-functions #'eaf-monitor-window-size-change)
    (remove-hook 'window-configuration-change-hook #'eaf-monitor-configuration-change))

  ;; Clean `eaf-org-file-list' and `eaf-org-killed-file-list'.
  (dolist (org-file-name eaf-org-file-list)
    (eaf--org-delete-preview-file org-file-name))
  (setq eaf-org-file-list nil)
  (setq eaf-org-killed-file-list nil)
  (setq-local eaf-fullscreen-p nil)

  ;; Kill EAF-mode buffers.
  (let* ((eaf-buffers (eaf--get-eaf-buffers))
         (count (length eaf-buffers)))
    (dolist (buffer eaf-buffers)
      (kill-buffer buffer))
    ;; Just report to me when EAF buffer exists.
    (message "[EAF] Killed %s EAF buffer%s" count (if (> count 1) "s!" "!")))

  ;; Kill process after kill buffer, make application can save session data.
  (eaf--kill-python-process))

(defalias 'eaf-kill-process #'eaf-stop-process)

(defun eaf--kill-python-process ()
  "Kill EAF background python process."
  (interactive)
  (when (eaf-epc-live-p eaf-epc-process)
    ;; Cleanup before exit EAF server process.
    (eaf-call-async "cleanup")
    ;; Delete EAF server process.
    (eaf-epc-stop-epc eaf-epc-process)
    ;; Kill *eaf* buffer.
    (when (get-buffer eaf-name)
      (kill-buffer eaf-name))
    (message "[EAF] Process terminated.")))

(defun eaf-restart-process ()
  "Stop and restart EAF process."
  (interactive)
  (when (get-buffer "DevTools - file:///")
    (kill-buffer "DevTools - file:///"))
  (setq eaf--active-buffers nil)
  (eaf-for-each-eaf-buffer
   (push `(,eaf--buffer-url ,eaf--buffer-app-name ,eaf--buffer-args) eaf--active-buffers))
  (eaf-stop-process t)
  (eaf-start-process))

(defun eaf--decode-string (str)
  "Decode string STR with UTF-8 coding using Base64."
  (decode-coding-string (base64-decode-string str) 'utf-8))

(defun eaf--encode-string (str)
  "Encode string STR with UTF-8 coding using Base64."
  (base64-encode-string (encode-coding-string str 'utf-8)))

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
  "Get WINDOW allocation."
  (let* ((window-edges (window-pixel-edges window))
         (x (nth 0 window-edges))
         (y (+ (nth 1 window-edges)
               (if (version< emacs-version "27.0")
                   (window-header-line-height window)
                 (window-tab-line-height window))))
         (w (- (nth 2 window-edges) x))
         (h (- (nth 3 window-edges) (window-mode-line-height window) y)))
    (list x y w h)))

(defun eaf--generate-id ()
  "Randomly generate a seven digit id used for EAF buffers."
  (format "%04x-%04x-%04x-%04x-%04x-%04x-%04x"
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))))

(defun eaf-execute-app-cmd (cmd &optional buf)
  "Execute app CMD.

If BUF is given it should be the EAF buffer for the command
otherwise it is assumed that the current buffer is the EAF
buffer."
  (with-current-buffer (or buf (current-buffer))
    (let ((this-command cmd))
      (call-interactively cmd))))

(defun eaf-get-path-or-url ()
  "Get the current file path or web URL.

When called interactively, copy to ‘kill-ring’."
  (interactive)
  (if (derived-mode-p 'eaf-mode)
      (if (called-interactively-p 'any)
          (message "%s" (kill-new (eaf-call-sync "call_function" eaf--buffer-id "get_url")))
        (eaf-call-sync "call_function" eaf--buffer-id "get_url"))
    (user-error "This command can only be called in an EAF buffer!")))

(defun eaf-toggle-fullscreen ()
  "Toggle fullscreen."
  (interactive)
  (eaf-call-async "execute_function" eaf--buffer-id "toggle_fullscreen" (key-description (this-command-keys-vector))))

(defun eaf--make-proxy-function (fun)
  "Define elisp command which can call python function string FUN."
  (let ((sym (intern (format "eaf-proxy-%s" fun))))
    (unless (fboundp sym)
      (defalias sym
        (lambda nil
          (interactive)
          ;; Ensure this is only called from EAF buffer
          (if (derived-mode-p 'eaf-mode)
              (eaf-call-async "execute_function" eaf--buffer-id fun (key-description (this-command-keys-vector)))
            (message "%s command can only be called in an EAF buffer!" sym)))
        (format
         "Proxy function to call \"%s\" on the Python side.

Use `eaf-execute-app-cmd' if you want to execute this command programmatically.
Please ONLY use `eaf-bind-key' and use the unprefixed command name (\"%s\")
to edit EAF keybindings!" fun fun)))
    sym))

(defun eaf--call-js-function (fun &optional args)
  (lambda nil
    (interactive)
    (unless args
      (setq args ""))
    ;; Ensure this is only called from EAF buffer
    (when (derived-mode-p 'eaf-mode)
      (eaf-call-async "execute_js_function" eaf--buffer-id (string-trim-left fun "js_") args)
      )))

(defun eaf--gen-keybinding-map (keybinding &optional no-inherit-eaf-mode-map*)
  "Configure the `eaf-mode-map' from KEYBINDING, one of the eaf-.*-keybinding variables."
  (setq eaf-mode-map
        (let ((map (make-sparse-keymap)))
          (unless no-inherit-eaf-mode-map*
            (set-keymap-parent map eaf-mode-map*))
          (cl-loop for (key . fun) in keybinding
                   do (define-key map (kbd key)
                        (cond
                         ;; If command is normal symbol, just call it directly.
                         ((symbolp fun)
                          fun)
                         ;; If command is string and include - , it's elisp function, use `intern' build elisp function from function name.
                         ((string-match "-" fun)
                          (intern fun))
                         ;; If command prefix with js_, call JavaScript function directly.
                         ((string-prefix-p "js_" fun)
                          (eaf--call-js-function fun))
                         ;; If command is not built-in function and not include char '-'
                         ;; it's command in python side, build elisp proxy function to call it.
                         (t
                          (eaf--make-proxy-function fun))
                         ))
                   finally return map))))

(defun eaf--get-app-bindings (app-name)
  "Get the specified APP-NAME keybinding.

Every app has its name and the corresponding
keybinding variable to eaf-app-binding-alist."
  (symbol-value
   (cdr (assoc app-name eaf-app-binding-alist))))

(defun eaf--get-app-module-path (app-name)
  (symbol-value
   (cdr (assoc app-name eaf-app-module-path-alist))))

(defun eaf--create-buffer (url app-name args)
  "Create an EAF buffer given URL, APP-NAME, and ARGS."
  (eaf--gen-keybinding-map (eaf--get-app-bindings app-name))
  (let* ((eaf-buffer-name (if (equal (file-name-nondirectory url) "")
                              url
                            (file-name-nondirectory url)))
         (eaf-buffer (generate-new-buffer eaf-buffer-name))
         (url-directory (or (file-name-directory url) url)))
    (with-current-buffer eaf-buffer
      (eaf-mode)
      (when (file-accessible-directory-p url-directory)
        (setq-local default-directory url-directory)
        (when (file-exists-p url)
          (setq-local buffer-file-name url)))
      ;; `eaf-buffer-url' should record full path of url, otherwise `eaf-open' will open duplicate PDF tab for same url.
      (set (make-local-variable 'eaf--buffer-url) url)
      (set (make-local-variable 'eaf--buffer-app-name) app-name)
      (set (make-local-variable 'eaf--buffer-args) args)
      (run-hooks (intern (format "eaf-%s-hook" app-name)))
      (setq mode-name (concat "EAF/" app-name)))
    eaf-buffer))

(defun eaf-monitor-window-size-change (frame)
  "Delay some time and run `eaf-try-adjust-view-with-frame-size' to compare with Emacs FRAME size."
  (when (eaf-epc-live-p eaf-epc-process)
    (setq eaf-last-frame-width (frame-pixel-width frame))
    (setq eaf-last-frame-height (frame-pixel-height frame))
    (run-with-timer 1 nil (lambda () (eaf-try-adjust-view-with-frame-size frame)))))

(defun eaf-try-adjust-view-with-frame-size (frame)
  "Update EAF view once Emacs window size of the FRAME is changed."
  (unless (and (equal (frame-pixel-width frame) eaf-last-frame-width)
               (equal (frame-pixel-height frame) eaf-last-frame-height))
    (eaf-monitor-configuration-change)))

(defun eaf--frame-left (frame)
  "Return outer left position"
  (let ((left (frame-parameter frame 'left)))
    (if (listp left) (nth 1 left) left)))

(defun eaf--frame-top (frame)
  "Return outer top position."
  (let ((top (frame-parameter frame 'top)))
    (if (listp top) (nth 1 top) top)))

(defun eaf--frame-internal-height (frame)
  "Height of internal objects.
Including title-bar, menu-bar, offset depends on window system, and border."
  (let ((geometry (frame-geometry frame)))
    (+ (cdr (alist-get 'title-bar-size geometry))
       (cdr (alist-get 'tool-bar-size geometry)))))

(defun eaf--buffer-x-position-adjust (frame)
  "Adjust the x position of EAF buffers for macOS"
  (if (eq system-type 'darwin)
      (eaf--frame-left frame)
    0))

(defun eaf--buffer-y-postion-adjust (frame)
  "Adjust the y position of EAF buffers for macOS"
  (if (eq system-type 'darwin)
      (+ (eaf--frame-top frame) (eaf--frame-internal-height frame))
    0))

(eval-when-compile
  (when (eq system-type 'darwin)
    (defun eaf--mac-focus-change ()
      "Manage Emacs's focus change"
      (cond
       ((string= "Python\n" (shell-command-to-string "app-frontmost --name"))
        (setq eaf--mac-switch-to-python t))

       ((string= "Emacs\n" (shell-command-to-string "app-frontmost --name"))
        (cond
         (eaf--mac-switch-to-python
          (setq eaf--mac-switch-to-python nil))
         ((not eaf--mac-has-focus)
          (run-with-timer 0.1 nil #'eaf--mac-focus-in)
          )
         (eaf--mac-has-focus
          (eaf--mac-focus-out))))
       (t (eaf--mac-focus-out))))

    (defun eaf--mac-replace-eaf-buffers ()
      (dolist (window (window-list))
        (select-window window)
        (when (eq major-mode 'eaf-mode)
          (get-buffer-create "*eaf temp*")
          (switch-to-buffer "*eaf temp*" t))))

    (defun eaf--mac-focus-in ()
      (setq eaf--mac-has-focus t)
      (ignore-errors
        (set-window-configuration (frame-parameter (selected-frame) 'eaf--mac-frame))
        (bury-buffer "*eaf temp*")))

    (defun eaf--mac-focus-out (&optional frame)
      (when eaf--mac-has-focus
        (setq eaf--mac-has-focus nil)
        (set-frame-parameter (or frame (selected-frame)) 'eaf--mac-frame (current-window-configuration))
        (eaf--mac-replace-eaf-buffers)))

    (add-function :after after-focus-change-function #'eaf--mac-focus-change)
    (add-to-list 'delete-frame-functions #'eaf--mac-focus-out)
    ))

(defun eaf-monitor-configuration-change (&rest _)
  "EAF function to respond when detecting a window configuration change."
  (when (and eaf--monitor-configuration-p
             (eaf-epc-live-p eaf-epc-process))
    (ignore-errors
      (let (view-infos)
        (dolist (frame (frame-list))
          (dolist (window (window-list frame))
            (with-current-buffer (window-buffer window)
              (when (derived-mode-p 'eaf-mode)
                ;; When `eaf-fullscreen-p' is non-nil, and only the EAF window is present, use frame size
                (if (and eaf-fullscreen-p (equal (length (window-list frame)) 1))
                    (push (format "%s:%s:%s:%s:%s:%s"
                                  eaf--buffer-id
                                  (eaf-get-emacs-xid frame)
                                  0 0 (frame-pixel-width frame) (frame-pixel-height frame))
                          view-infos)
                  (let* ((window-allocation (eaf-get-window-allocation window))
                         (window-divider-right-padding (if window-divider-mode window-divider-default-right-width 0))
                         (window-divider-bottom-padding (if window-divider-mode window-divider-default-bottom-width 0))
                         (x (+ (eaf--buffer-x-position-adjust frame) (nth 0 window-allocation)))
                         (y (+ (eaf--buffer-y-postion-adjust frame) (nth 1 window-allocation)))
                         (w (nth 2 window-allocation))
                         (h (nth 3 window-allocation)))
                    (push (format "%s:%s:%s:%s:%s:%s"
                                  eaf--buffer-id
                                  (eaf-get-emacs-xid frame)
                                  x
                                  y
                                  (- w window-divider-right-padding)
                                  (- h window-divider-bottom-padding))
                          view-infos)))))))
        (eaf-call-async "update_views" (mapconcat #'identity view-infos ","))))))

(defun eaf--get-eaf-buffers ()
  "A function that return a list of EAF buffers."
  (cl-remove-if-not
   (lambda (buffer)
     (with-current-buffer buffer
       (derived-mode-p 'eaf-mode)))
   (buffer-list)))

(defun eaf--monitor-buffer-kill ()
  "A function monitoring when an EAF buffer is killed."
  (ignore-errors
    (eaf-call-async "kill_buffer" eaf--buffer-id))

  ;; Kill eaf process when last eaf buffer closed.
  ;; We need add timer to avoid the last web page kill when terminal is exited.
  (when eaf-kill-process-after-last-buffer-closed
    (run-at-time
     5 nil
     (lambda ()
       (when (equal (length (eaf--get-eaf-buffers)) 0)
         (eaf--kill-python-process))
       ))))

(defun eaf--monitor-emacs-kill ()
  "Function monitoring when Emacs is killed."
  (ignore-errors
    (when eaf-browser-continue-where-left-off
      (let* ((browser-restore-file-path
              (concat eaf-config-location
                      (file-name-as-directory "browser")
                      (file-name-as-directory "history")
                      "restore.txt"))
             (browser-urls ""))
        (write-region
         (dolist (buffer (eaf--get-eaf-buffers) browser-urls)
           (with-current-buffer buffer
             (when (equal eaf--buffer-app-name "browser")
               (setq browser-urls (concat eaf--buffer-url "\n" browser-urls)))))
         nil browser-restore-file-path)))
    (eaf-call-async "kill_emacs")))

(defun eaf-keyboard-quit ()
  "Wrap around `keyboard-quit' and signals a ‘quit’ condition to EAF applications."
  (interactive)
  (eaf-call-async "action_quit" eaf--buffer-id)
  (call-interactively 'keyboard-quit))

(defun eaf-send-key ()
  "Directly send key to EAF Python side."
  (interactive)
  (eaf-call-async "send_key" eaf--buffer-id (key-description (this-command-keys-vector))))

(defun eaf-send-key-sequence ()
  "Directly send key sequence to EAF Python side."
  (interactive)
  (eaf-call-async "send_key_sequence" eaf--buffer-id (key-description (this-command-keys-vector))))

(defmacro eaf-create-send-key-function (key &optional value)
  (let ((send-key-function (intern (format "eaf-send-%s-key" key))))
    `(defun ,send-key-function()
       (interactive)
       (eaf-call-async "send_key" eaf--buffer-id (or ,value (format "<%s>" ,key))))))

(defmacro eaf-create-send-sequence-function (key value)
  (let ((send-key-sequence-function (intern (format "eaf-send-%s-sequence" key))))
    `(defun ,send-key-sequence-function()
       (interactive)
       (eaf-call-async "send_key_sequence" eaf--buffer-id ,value))))

(eaf-create-send-key-function "left")
(eaf-create-send-key-function "right")
(eaf-create-send-key-function "down")
(eaf-create-send-key-function "up")
(eaf-create-send-key-function "escape")
(eaf-create-send-key-function "return" "RET")

(eaf-create-send-sequence-function "ctrl-return" "C-RET")
(eaf-create-send-sequence-function "alt-backspace" "M-<backspace>")
(eaf-create-send-sequence-function "shift-return" "S-RET")

(defmacro eaf-bind-key (command key eaf-app-keybinding)
  "This function binds COMMAND to KEY in EAF-APP-KEYBINDING list.

Use this to bind keys for EAF applications.

COMMAND is a symbol of a regular Emacs command or a python app
command.  You can see a list of available commands by calling
`eaf-describe-bindings' in an EAF buffer.  The `eaf-proxy-' prefix
should be dropped for the COMMAND symbol.

KEY is a string representing a sequence of keystrokes and events.

EAF-APP-KEYBINDING is one of the `eaf-<app-name>-keybinding'
variables, where <app-name> can be obtained by checking the value
of `eaf--buffer-app-name' inside the EAF buffer."
  `(setf (map-elt ,eaf-app-keybinding ,key)
         ,(if (string-match "_" (symbol-name command))
              (symbol-name command)
            `(quote ,command))))

(defun eaf-focus-buffer (focus-buffer-id)
  "Focus the buffer given the FOCUS-BUFFER-ID."
  (catch 'found-eaf
    (eaf-for-each-eaf-buffer
     (when (string= eaf--buffer-id focus-buffer-id)
       (let ((buffer-window (get-buffer-window buffer)))
         (when buffer-window
           (select-window buffer-window)))
       (throw 'found-eaf t)))))

(defun eaf--show-message (format-string)
  "A wrapper around `message' that prepend [EAF/app-name] before FORMAT-STRING."
  (message "[EAF/%s] %s" eaf--buffer-app-name format-string))

(defun eaf--set-emacs-var (name value)
  "Set Lisp variable NAME with VALUE on the Emacs side."
  (set (intern name) value))

(defun eaf-request-kill-buffer (kill-buffer-id)
  "Function for requesting to kill the given buffer with KILL-BUFFER-ID."
  (catch 'found-eaf
    (eaf-for-each-eaf-buffer
     (when (string= eaf--buffer-id kill-buffer-id)
       (kill-buffer buffer)
       (throw 'found-eaf t)))))

(defun eaf--first-start (eaf-epc-port webengine-include-private-codec)
  "Call `eaf--open-internal' upon receiving `start_finish' signal from server.

WEBENGINE-INCLUDE-PRIVATE-CODEC is only useful when app-name is video-player."
  ;; Make EPC process.
  (setq eaf-epc-process (make-eaf-epc-manager
                         :server-process eaf-internal-process
                         :commands (cons eaf-internal-process-prog eaf-internal-process-args)
                         :title (mapconcat 'identity (cons eaf-internal-process-prog eaf-internal-process-args) " ")
                         :port eaf-epc-port
                         :connection (eaf-epc-connect "localhost" eaf-epc-port)
                         ))
  (eaf-epc-init-epc-layer eaf-epc-process)

  ;; If webengine-include-private-codec and app name is "video-player", replace by "js-video-player".
  (setq eaf--webengine-include-private-codec webengine-include-private-codec)
  (let* ((first-buffer-info (pop eaf--active-buffers))
         (first-start-url (nth 0 first-buffer-info))
         (first-start-app-name (nth 1 first-buffer-info))
         (first-start-args (nth 2 first-buffer-info)))
    (when (and (string-equal first-start-app-name "video-player")
               eaf--webengine-include-private-codec)
      (setq first-start-app-name "js-video-player"))
    ;; Start first app.
    (eaf--open-internal first-start-url first-start-app-name first-start-args))

  (dolist (buffer-info eaf--active-buffers)
    (eaf--open-internal (nth 0 buffer-info) (nth 1 buffer-info) (nth 2 buffer-info)))
  (setq eaf--active-buffers nil))

(defun eaf--update-buffer-details (buffer-id title url)
  "Function for updating buffer details with its BUFFER-ID, TITLE and URL."
  (when (eaf--called-from-wsl-on-windows-p)
    (eaf-monitor-configuration-change))
  (when (> (length title) 0)
    (catch 'found-eaf
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (with-current-buffer buffer
            (when (and
                   (derived-mode-p 'eaf-mode)
                   (equal eaf--buffer-id buffer-id))
              (setq mode-name (concat "EAF/" eaf--buffer-app-name))
              (setq-local eaf--bookmark-title title)
              (setq-local eaf--buffer-url url)
              (rename-buffer (format eaf-buffer-title-format title) t)
              (eaf--update-modeline-icon)
              (throw 'found-eaf t))))))))

(defun eaf-translate-text (text)
  "Use sdcv to translate selected TEXT."
  (when (featurep 'sdcv)
    (sdcv-search-input+ text)))

(defun eaf--input-message (input-buffer-id interactive-string callback-tag interactive-type initial-content)
  "Handles input message INTERACTIVE-STRING on the Python side given INPUT-BUFFER-ID and CALLBACK-TYPE."
  (let* ((input-message (eaf-read-input (concat "[EAF/" eaf--buffer-app-name "] " interactive-string) interactive-type initial-content)))
    (if input-message
        (eaf-call-async "handle_input_response" input-buffer-id callback-tag input-message)
      (eaf-call-async "cancel_input_response" input-buffer-id callback-tag))))

(defun eaf-read-input (interactive-string interactive-type initial-content)
  "EAF's multi-purpose read-input function which read an INTERACTIVE-STRING with INITIAL-CONTENT, determines the function base on INTERACTIVE-TYPE."
  (condition-case nil
      (cond ((string-equal interactive-type "string")
             (read-string interactive-string initial-content))
            ((string-equal interactive-type "file")
             (expand-file-name (read-file-name interactive-string)))
            ((string-equal interactive-type "yes-or-no")
             (yes-or-no-p interactive-string)))
    (quit nil)))

(defun eaf--open-internal (url app-name args)
  "Open an EAF application internally with URL, APP-NAME and ARGS."
  (let* ((buffer (eaf--create-buffer url app-name args)))
    (with-current-buffer buffer
      (eaf-call-async "new_buffer" eaf--buffer-id
                      (if (eaf--called-from-wsl-on-windows-p)
                          (eaf--translate-wsl-url-to-windows url)
                        url)
                      (eaf--get-app-module-path app-name)
                      args)
      (eaf--update-modeline-icon))
    (eaf--display-app-buffer app-name buffer))
  (eaf--post-open-actions url app-name args))

(defun eaf--post-open-actions (url app-name args)
  "The function to run after `eaf--open-internal', taking the same URL, APP-NAME and ARGS."
  (cond ((and args (equal app-name "pdf-viewer"))
         (let ((office-pdf (string-match "office-pdf" args)))
           (when office-pdf
             (with-current-buffer (file-name-nondirectory url)
               (rename-buffer (concat "[Converted] " (substring args 0 (- office-pdf 1))) t)))))))

(defun eaf--update-modeline-icon ()
  "Update modeline icon if used"
  (when (and (ignore-errors (require 'all-the-icons) (featurep 'eaf-all-the-icons)))
    (declare-function eaf-all-the-icons-update-icon "eaf-all-the-icons.el")
    (eaf-all-the-icons-update-icon)))

(defun eaf-goto-left-tab ()
  "Go to left tab when awesome-tab exists."
  (interactive)
  (when (ignore-errors (require 'awesome-tab))
    (awesome-tab-backward-tab)))

(defun eaf-goto-right-tab ()
  "Go to right tab when awesome-tab exists."
  (interactive)
  (when (ignore-errors (require 'awesome-tab))
    (awesome-tab-forward-tab)))

(defun eaf--non-remote-default-directory ()
  "Return `default-directory' itself if is not part of remote, otherwise return $HOME."
  (if (or (file-remote-p default-directory)
          (not (file-accessible-directory-p default-directory)))
      (getenv "HOME")
    default-directory))

(defun eaf--get-app-for-extension (extension-name)
  "Given the EXTENSION-NAME, loops through `eaf-app-extensions-alist', set and return `app-name'."
  (let ((app-name
         (cl-loop for (app . ext) in eaf-app-extensions-alist
                  if (member extension-name (symbol-value ext))
                  return app)))
    (if (string-equal app-name "video-player")
        ;; Use Browser play video if QWebEngine include private codec.
        (if eaf--webengine-include-private-codec
            "js-video-player"
          "video-player")
      app-name)))

;;;###autoload
(defun eaf-get-file-name-extension (file)
  "A wrapper around `file-name-extension' that downcases the extension of the FILE."
  (downcase (file-name-extension file)))

(defun eaf--called-from-wsl-on-windows-p ()
  "Check whether eaf is called by Emacs on WSL and is running on Windows."
  (and (eq system-type 'gnu/linux)
       (string-match-p ".exe" eaf-python-command)))

(defun eaf--translate-wsl-url-to-windows (path)
  "Translate from a WSL PATH to a Windows path."
  (replace-regexp-in-string "/mnt/\\([a-zA-Z]\\)" "\\1:" path))

;;;###autoload
(defun eaf-open (url &optional app-name args always-new)
  "Open an EAF application with URL, optional APP-NAME and ARGS.

Interactively, a prefix arg replaces ALWAYS-NEW, which means to open a new
 buffer regardless of whether a buffer with existing URL and APP-NAME exists.

By default, `eaf-open' will switch to buffer if corresponding url exists.
`eaf-open' always open new buffer if option OPEN-ALWAYS is non-nil.

When called interactively, URL accepts a file that can be opened by EAF."
  (interactive "G[EAF] EAF Open: ")
  ;; Try to set app-name along with url when calling INTERACTIVELY
  (when (and (not app-name) (file-exists-p url))
    (when (and eaf-pdf-store-history (string-match "^\\(.+\\)\\.pdf$" url))
      (eaf-store-pdf-history url))
    (setq url (expand-file-name url))
    (when (featurep 'recentf)
      (recentf-add-file url))
    (if (file-directory-p url)
        (setq app-name "file-manager")
      (let* ((extension-name (eaf-get-file-name-extension url)))
        ;; Initialize url, app-name and args
        (setq app-name (eaf--get-app-for-extension extension-name))
        (cond
         ((equal app-name "browser")
          (setq url (concat "file://" url)))
         ((equal app-name "office")
          (user-error "Please use `eaf-open-office' instead!"))
         ((equal app-name "markdown-previewer")
          ;; Warning user install java if found PlantUML syntax in markdown file.
          (with-temp-buffer
            (insert-file-contents url)
            (goto-char (point-min))
            (when (search-forward "```puml" nil t)
              (unless (executable-find "java")
                (user-error (format "Have PlantUML code in file '%s', you need to install Java to preview normally." url))
                ))))))))
  ;; Now that app-name should hopefully be set
  (unless app-name
    ;; Output error to user if app-name is empty string.
    (user-error (concat (if app-name (concat "[EAF/" app-name "] ") "[EAF] ")
                        (cond
                         ((not (or (string-prefix-p "/" url)
                                   (string-prefix-p "~" url))) "File %s cannot be opened.")
                         ((file-exists-p url) "File %s cannot be opened.")
                         (t "File %s does not exist.")))
                url))
  (unless args (setq args ""))
  (setq always-new (or always-new current-prefix-arg))
  ;; Hooks are only added if not present already...
  (add-hook 'window-size-change-functions #'eaf-monitor-window-size-change)
  (add-hook 'window-configuration-change-hook #'eaf-monitor-configuration-change)
  ;; Open URL with EAF application
  (if (eaf-epc-live-p eaf-epc-process)
      (let (exists-eaf-buffer)
        ;; Try to open buffer
        (catch 'found-eaf
          (eaf-for-each-eaf-buffer
           (when (and (string= eaf--buffer-url url)
                      (string= eaf--buffer-app-name app-name))
             (setq exists-eaf-buffer buffer)
             (throw 'found-eaf t))))
        ;; Switch to existing buffer,
        ;; if no match buffer found, call `eaf--open-internal'.
        (if (and exists-eaf-buffer
                 (not always-new))
            (progn
              (eaf--display-app-buffer app-name exists-eaf-buffer)
              (message (concat "[EAF/" app-name "] " "Switch to %s") url))
          (eaf--open-internal url app-name args)
          (message (concat "[EAF/" app-name "] " "Opening %s") url)))
    ;; Record user input, and call `eaf--open-internal' after receive `start_finish' signal from server process.
    (unless eaf--active-buffers
      (push `(,url ,app-name ,args) eaf--active-buffers))
    (eaf-start-process)
    (message (concat "[EAF/" app-name "] " "Opening %s") url)))

(defun eaf-duplicate-current-buffer ()
  "Duplicate the current EAF buffer in a new buffer.

So multiple EAF buffers visiting the same file do not sync with each other."
  (interactive)
  (when (derived-mode-p 'eaf-mode)
    (delete-other-windows)
    (split-window-horizontally)
    (other-window +1)
    (eaf-open eaf--buffer-url eaf--buffer-app-name eaf--buffer-args t)))

(defun eaf--display-app-buffer (app-name buffer)
  "Display specified APP-NAME's app buffer in BUFFER."
  (let ((display-fun (or (cdr (assoc app-name
                                     eaf-app-display-function-alist))
                         #'switch-to-buffer)))
    (funcall display-fun buffer)))

(defun eaf-split-preview-windows (url)
  "Function for spliting preview windows with specified URL."
  (delete-other-windows)
  (find-file url)
  (split-window-horizontally)
  (other-window +1))

(defun eaf-edit-buffer-confirm ()
  "Confirm input text and send the text to corresponding EAF app."
  (interactive)
  ;; Note: pickup buffer-id from buffer name and not restore buffer-id from buffer local variable.
  ;; Then we can switch edit buffer to any other mode, such as org-mode, to confirm buffer string.
  (cond ((equal eaf-mindmap--current-add-mode "sub")
         (eaf-call-async "update_multiple_sub_nodes"
                         eaf--buffer-id
                         (buffer-string)))
        ((equal eaf-mindmap--current-add-mode "brother")
         (eaf-call-async "update_multiple_brother_nodes"
                         eaf--buffer-id
                         (buffer-string)))
        ((equal eaf-mindmap--current-add-mode "middle")
         (eaf-call-async "update_multiple_middle_nodes"
                         eaf--buffer-id
                         (buffer-string)))
        (t
         (eaf-call-async "update_focus_text"
                         eaf--buffer-id
                         (eaf--encode-string (kill-new (buffer-string))))))
  (kill-buffer)
  (delete-window))

(defun eaf--enter-fullscreen-request ()
  "Entering EAF browser fullscreen use Emacs frame's size."
  (setq-local eaf-fullscreen-p t)
  (eaf-monitor-configuration-change)
  (when (and eaf-browser-fullscreen-move-cursor-corner
             (or (string= eaf--buffer-app-name "browser")
                 (string= eaf--buffer-app-name "js-video-player")))
    (eaf-call-async "execute_function" eaf--buffer-id "move_cursor_to_corner" (key-description (this-command-keys-vector)))))

;; Update and load the theme
(defun eaf-get-theme-mode ()
  (format "%s" (frame-parameter nil 'background-mode)))

(defun eaf-get-theme-background-color ()
  (format "%s" (frame-parameter nil 'background-color)))

(defun eaf-get-theme-foreground-color ()
  (format "%s" (frame-parameter nil 'foreground-color)))

(defcustom eaf-emacs-theme-mode (eaf-get-theme-mode)
  ""
  :type 'string)

(defcustom eaf-emacs-theme-background-color (eaf-get-theme-background-color)
  ""
  :type 'string)

(defcustom eaf-emacs-theme-foreground-color (eaf-get-theme-foreground-color)
  ""
  :type 'string)

(advice-add 'load-theme :around #'eaf-monitor-load-theme)
(defun eaf-monitor-load-theme (orig-fun &optional arg &rest args)
  "Update `eaf-emacs-theme-mode' after execute `load-theme'."
  (apply orig-fun arg args)
  (setq eaf-emacs-theme-mode (eaf-get-theme-mode))
  (setq eaf-emacs-theme-background-color (eaf-get-theme-background-color))
  (setq eaf-emacs-theme-foreground-color (eaf-get-theme-foreground-color)))

(defun eaf--get-current-desktop-name ()
  "Get current desktop name by `wmctrl'."
  (if (string-empty-p eaf-wm-name)
      (if (executable-find "wmctrl")
          ;; Get desktop name by command `wmctrl -m'.
          (cl-second (split-string (cl-first (split-string (shell-command-to-string "wmctrl -m") "\n")) ": "))
        ;; Otherwise notify user and return emptry string.
        (message "You need install wmctrl to get the name of desktop.")
        "")
    eaf-wm-name))

(defun eaf--activate-emacs-win32-window()
  "Use vbs activate Emacs win32 window."
  (let* ((activate-window-file-path
          (concat eaf-config-location "activate-window.vbs"))
         (activate-window-file-exists (file-exists-p activate-window-file-path)))
    (unless activate-window-file-exists
      (with-temp-file activate-window-file-path
        (insert "set WshShell = CreateObject(\"WScript.Shell\")\nWshShell.AppActivate Wscript.Arguments(0)")))
    (shell-command-to-string (format "cscript %s %s" activate-window-file-path (emacs-pid)))))

(defun eaf--activate-emacs-wsl-window()
  "Activate Emacs window running on Wsl."
  (eaf-call-async "activate_emacs_wsl_window" (frame-parameter nil 'name)))

(defun eaf--activate-emacs-linux-window (&optional buffer_id)
  "Activate Emacs window by `wmctrl'."
  (if (member (eaf--get-current-desktop-name) eaf-wm-focus-fix-wms)
      ;; When switch app focus in WM, such as, i3 or qtile.
      ;; Emacs window cannot get the focus normally if mouse in EAF buffer area.
      ;;
      ;; So we move mouse to frame bottom of Emacs, to make EAF receive input event.
      (eaf-call-async "execute_function" (or eaf--buffer-id buffer_id) "move_cursor_to_corner" (key-description (this-command-keys-vector)))

    ;; When press Alt + Tab in DE, such as KDE.
    ;; Emacs window cannot get the focus normally if mouse in EAF buffer area.
    ;;
    ;; So we use wmctrl activate on Emacs window after Alt + Tab operation.
    (if (executable-find "wmctrl")
        (shell-command-to-string (format "wmctrl -i -a $(wmctrl -lp | awk -vpid=$PID '$3==%s {print $1; exit}')" (emacs-pid)))
      (message "Please install wmctrl to active Emacs window."))))

(defun eaf--activate-emacs-mac-window()
  "Activate Emacs macOS window."
  (shell-command-to-string "open -a emacs"))

(defun eaf-activate-emacs-window(&optional buffer_id)
  "Activate Emacs window."
  (cond
   ((eaf--called-from-wsl-on-windows-p)
    (eaf--activate-emacs-wsl-window))
   ((memq system-type '(cygwin windows-nt ms-dos))
    (eaf--activate-emacs-win32-window))
   ((eq system-type 'darwin)
    (eaf--activate-emacs-mac-window))
   ((or (eq system-type 'gnu/linux)
        (eq system-type 'berkeley-unix))
    (eaf--activate-emacs-linux-window buffer_id))))

(defun eaf--change-default-directory (directory)
  "Change default directory to DIRECTORY."
  (when (file-accessible-directory-p (or (file-name-directory directory) directory))
    (setq-local default-directory directory)))

;;;;;;;;;;;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eaf-get-view-info ()
  (let* ((window-allocation (eaf-get-window-allocation (selected-window)))
         (x (nth 0 window-allocation))
         (y (nth 1 window-allocation))
         (w (nth 2 window-allocation))
         (h (nth 3 window-allocation)))
    (format "%s:%s:%s:%s:%s" eaf--buffer-id x y w h)))

(defun eaf-generate-keymap-doc ()
  "This command use for generate keybindings document Wiki."
  (interactive)
  (let ((vars (mapcar 'cdr eaf-app-binding-alist)))
    (erase-buffer)
    (insert "**** Entire document automatically generated by command =eaf-generate-keymap-doc=.\n\n")
    (insert "* Overview
  Each EAF App has its own set of keybindings. Their default bindings are listed below. You can also see this list by executing =(describe-mode)= or =C-h m= within an EAF buffer.

  You can customize them very easily with the =eaf-bind-key= function: find the corresponding *Keybinding Variable*, and add the something similar to the following to =.emacs=
  #+BEGIN_SRC emacs-lisp
    (eaf-bind-key scroll_up \"C-n\" eaf-pdf-viewer-keybinding)
  #+END_SRC
  To *unbind* an existing keybinding, use the following:
  #+begin_src emacs-lisp
    (eaf-bind-key nil \"C-n\" eaf-pdf-viewer-keybinding)
  #+end_src

* Global keybindings
  | Key   | Event                       |
  |-------+-----------------------------|
  | C-h m | eaf-describe-bindings       |
  | C-c b | eaf-open-bookmark           |
  | C-c e | eaf-open-external           |
  | C-c i | eaf-import-chrome-bookmarks |
  | M-/   | eaf-get-path-or-url         |
  | M-'   | eaf-toggle-fullscreen       |
  | M-[   | eaf-share-path-or-url       |

* Browser Edit Mode
  | Key     | Event                              |
  |---------+------------------------------------|
  | C-c C-c | eaf-edit-buffer-confirm            |
  | C-c C-k | eaf-edit-buffer-cancel             |
  | C-c C-t | eaf-edit-buffer-switch-to-org-mode |

")
    (dolist (var vars)
      (insert (format "* %s\n" (get var 'variable-documentation)))
      (insert (format "  *Keybinding Variable*: =%s=\n" (symbol-name var)))
      (insert "| Key | Event |\n")
      (insert "|-----+------|\n")
      ;; NOTE: `standard-value' use for fetch origin value of keybinding variable.
      ;; Otherwise, developer's personal config will dirty document.
      (dolist (element (eval (car (get var 'standard-value))))
        (insert (format "| %s | %s |\n" (car element) (cdr element))))
      (insert "\n"))))

;;;;;;;;;;;;;;;;;;;; Advice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: In the code below we should use `save-selected-window' (or even
;; better `with-selected-window') rather than (other-window +1) followed by
;; (other-window -1) since this is not always a no-op.

(advice-add 'scroll-other-window :around #'eaf--scroll-other-window)
(defun eaf--scroll-other-window (orig-fun &optional arg &rest args)
  "When next buffer is `eaf-mode', do `eaf-scroll-up-or-next-page'."
  (other-window +1)
  (if (derived-mode-p 'eaf-mode)
      (progn
        (eaf-call-async "scroll_other_buffer" (eaf-get-view-info) "up"
                        (if arg "line" "page"))
        (other-window -1))
    (other-window -1)
    (apply orig-fun arg args)))

(advice-add 'scroll-other-window-down :around #'eaf--scroll-other-window-down)
(defun eaf--scroll-other-window-down (orig-fun &optional arg &rest args)
  "When next buffer is `eaf-mode', do `eaf-scroll-down-or-previous-page'."
  (other-window +1)
  (if (derived-mode-p 'eaf-mode)
      (progn
        (eaf-call-async "scroll_other_buffer" (eaf-get-view-info) "down"
                        (if arg "line" "page"))
        (other-window -1))
    (other-window -1)
    (apply orig-fun arg args)))

(advice-add 'watch-other-window-internal :around
            #'eaf--watch-other-window-internal)
(defun eaf--watch-other-window-internal (orig-fun &optional direction line
                                                  &rest args)
  "When next buffer is `eaf-mode', do `eaf-watch-other-window'."
  (other-window +1)
  (if (derived-mode-p 'eaf-mode)
      (progn
        (eaf-call-async "scroll_other_buffer" (eaf-get-view-info)
                        (if (string-equal direction "up") "up" "down")
                        (if line "line" "page"))
        (other-window -1))
    (other-window -1)
    (apply orig-fun direction line args)))

(defun eaf--buffer-file-p ()
  "Determine if the file opened at the current buffer be opened by EAF."
  (let ((ext (when (and buffer-file-name
                        (file-exists-p buffer-file-name))
               (file-name-extension buffer-file-name))))
    (and ext
         (member (downcase ext) (append
                                 eaf-pdf-extension-list
                                 eaf-markdown-extension-list
                                 eaf-image-extension-list
                                 eaf-video-extension-list
                                 eaf-org-extension-list
                                 eaf-mindmap-extension-list
                                 eaf-office-extension-list)))))

(defun eaf-open-this-buffer ()
  "Try to open the current buffer using EAF, if possible."
  (interactive)
  (if (eaf--buffer-file-p)
      (eaf-open buffer-file-name)
    (user-error "[EAF] Current buffer is not supported by EAF!")))

(defun eaf--find-file-ext-p (ext)
  "Determine if file extension EXT can be opened by EAF directly by `find-file'.

You can configure a blacklist using `eaf-find-file-ext-blacklist'"
  (and ext
       (member (downcase ext) (append
                               eaf-pdf-extension-list
                               eaf-video-extension-list
                               eaf-image-extension-list
                               eaf-mindmap-extension-list))
       (not (member ext eaf-find-file-ext-blacklist))))

;; Make EAF as default app for supported extensions.
;; Use `eaf-open' in `find-file'
(defun eaf--find-file-advisor (orig-fn file &rest args)
  "Advisor of `find-file' that opens EAF supported file using EAF.

It currently identifies PDF, videos, images, and mindmap file extensions."
  (let ((fn (if (commandp 'eaf-open)
                #'(lambda (file)
                    (eaf-open file))
              orig-fn))
        (ext (file-name-extension file)))
    (if (eaf--find-file-ext-p ext)
        (apply fn file nil)
      (apply orig-fn file args))))
(advice-add #'find-file :around #'eaf--find-file-advisor)

;; Use `eaf-open' in `dired-find-file' and `dired-find-alternate-file'
(defun eaf--dired-find-file-advisor (orig-fn)
  "Advisor of `dired-find-file' and `dired-find-alternate-file' that opens EAF supported file using EAF.

It currently identifies PDF, videos, images, and mindmap file extensions."
  (dolist (file (dired-get-marked-files))
    (let ((fn (if (commandp 'eaf-open)
                  #'(lambda (file)
                      (eaf-open file))
                orig-fn))
          (ext (file-name-extension file)))
      (if (eaf--find-file-ext-p ext)
          (apply fn file nil)
        (funcall-interactively orig-fn)))))
(advice-add #'dired-find-file :around #'eaf--dired-find-file-advisor)
(advice-add #'dired-find-alternate-file :around #'eaf--dired-find-file-advisor)

(provide 'eaf)

;;; eaf.el ends here
