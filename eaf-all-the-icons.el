;;; eaf-all-the-icons.el --- Emacs application framework -*- lexical-binding: t -*-

;; Filename: eaf-all-the-icons.el
;; Description: Emacs application framework
;; Author: lhpfvs <lhpfvs@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; URL: https://github.com/manateelazycat/emacs-application-framework
;; Keywords:
;; Compatibility: emacs-version >= 27
;;
;; Features that might be required by this library:
;;
;; Please check README
;;

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'all-the-icons)

(defvar eaf-all-the-icons-alist
  '(
    ("EAF/browser" all-the-icons-faicon "firefox" :v-adjust -0.1 :face all-the-icons-red)
    ("EAF/pdf-viewer" all-the-icons-octicon "file-pdf" :v-adjust 0.0 :face all-the-icons-dred)
    ("EAF/image-viewer" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-orange)
    ("EAF/markdown-previewer" all-the-icons-octicon "markdown" :v-adjust 0.0 :face all-the-icons-lblue)
    ("EAF/js-video-player" all-the-icons-faicon "film" :face all-the-icons-blue)
    ("EAF/camera" all-the-icons-faicon "camera-retro" :height 1.0 :v-adjust -0.1)
    ("EAF/music" all-the-icons-faicon "music" :height 1.0 :v-adjust -0.1)
    ("EAF/terminal" all-the-icons-faicon "terminal" :v-adjust 0.2)
    ("EAF/org-previewer" all-the-icons-fileicon "org" :face all-the-icons-lgreen)
    ("EAF/mindmap" all-the-icons-alltheicon "html5" :face all-the-icons-orange)
    ("EAF/demo" all-the-icons-alltheicon "html5" :face all-the-icons-orange)
    ("EAF/vue-demo" all-the-icons-alltheicon "html5" :face all-the-icons-orange)
    ("EAF/file-sender" all-the-icons-octicon "file-directory"   :v-adjust 0.0)
    ("EAF/file-receiver" all-the-icons-octicon "file-directory" :v-adjust 0.0)
    ("EAF/airshare" all-the-icons-octicon "file-directory" :v-adjust 0.0)
    ("EAF/jupyter" all-the-icons-fileicon "jupyter" :height 1.0 :face all-the-icons-dorange)))

(defun eaf-all-the-icons-icon (mode-name &rest arg-overrides)
  (let* ((icon (all-the-icons-match-to-alist mode-name eaf-all-the-icons-alist))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (apply (car icon) args)))

(when all-the-icons-ibuffer-mode
  (define-ibuffer-column icon
  (:name "  " :inline t)
  (let ((icon (cond ((and (buffer-file-name) (all-the-icons-auto-mode-match?))
                     (all-the-icons-icon-for-file (file-name-nondirectory (buffer-file-name))
                                               :height all-the-icons-ibuffer-icon-size
                                               :v-adjust all-the-icons-ibuffer-icon-v-adjust))
                   ((eq major-mode 'eaf-mode)
                     (eaf-all-the-icons-icon mode-name
                                                 :height all-the-icons-ibuffer-icon-size
                                                 :v-adjust all-the-icons-ibuffer-icon-v-adjust))
                    (t
                     (all-the-icons-icon-for-mode major-mode
                                             :height all-the-icons-ibuffer-icon-size
                                             :v-adjust all-the-icons-ibuffer-icon-v-adjust)))))
    (if (or (null icon) (symbolp icon))
        (setq icon (all-the-icons-faicon "file-o"
                                         :face (if all-the-icons-ibuffer-color-icon
                                                   'all-the-icons-dsilver
                                                 'all-the-icons-ibuffer-icon-face)
                                         :height (* 0.9 all-the-icons-ibuffer-icon-size)
                                         :v-adjust all-the-icons-ibuffer-icon-v-adjust))
      (let* ((props (get-text-property 0 'face icon))
             (family (plist-get props :family))
             (face (if all-the-icons-ibuffer-color-icon
                       (or (plist-get props :inherit) props)
                     'all-the-icons-ibuffer-icon-face))
             (new-face `(:inherit ,face
                         :family ,family
                         :height ,all-the-icons-ibuffer-icon-size)))
        (propertize icon 'face new-face))))))

(defun eaf-all-the-icons-update-icon ()
  (when (and doom-modeline-mode doom-modeline-icon doom-modeline-major-mode-icon)
    (setq-local doom-modeline--buffer-file-icon (eaf-all-the-icons-icon mode-name))))

(when all-the-icons-ivy-rich-mode
  (defun eaf-all-the-icons-ivy-rich (candidate)
    "Add EAF buffer icon for `ivy-rich'."
    (let* ((buffer (get-buffer candidate))
           (buffer-file-name (buffer-file-name buffer))
           (major-mode (buffer-local-value 'major-mode buffer))
           (icon (with-current-buffer buffer (if (eq major-mode 'eaf-mode)
                                                 (eaf-all-the-icons-icon mode-name)
                                               (all-the-icons-icon-for-buffer)))))
      (all-the-icons-ivy-rich--format-icon
       (if (or (null icon) (symbolp icon))
           (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.9 :v-adjust 0.0)
         (propertize icon 'display '(raise 0.0))))))
  (advice-add #'all-the-icons-ivy-rich-buffer-icon :override #'eaf-all-the-icons-ivy-rich))

(provide 'eaf-all-the-icons)

;;; eaf-all-the-icons.el ends here
