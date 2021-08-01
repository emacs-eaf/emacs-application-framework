;;; eaf-pdf-viewer.el --- PDF Viewer

;; Filename: eaf-pdf-viewer.el
;; Description: PDF Viewer
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-07-20 22:16:40
;; Version: 0.1
;; Last-Updated: Fri Jul 30 00:48:38 2021 (-0400)
;;           By: Mingde (Matthew) Zeng
;; URL: http://www.emacswiki.org/emacs/download/eaf-pdf-viewer.el
;; Keywords:
;; Compatibility: GNU Emacs 28.0.50
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
;; PDF Viewer
;;

;;; Installation:
;;
;; Put eaf-pdf-viewer.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'eaf-pdf-viewer)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET eaf-pdf-viewer RET
;;

;;; Change log:
;;
;; 2021/07/20
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


;;; Code:

(defvar eaf-pdf-outline-buffer-name "*eaf pdf outline*"
  "The name of pdf-outline-buffer.")

(defvar eaf-pdf-outline-window-configuration nil
  "Save window configure before popup outline buffer.")

(defcustom eaf-pdf-extension-list
  '("pdf" "xps" "oxps" "cbz" "epub" "fb2" "fbz")
  "The extension list of pdf application."
  :type 'cons)

(defcustom eaf-pdf-store-history t
  "If it is t, the pdf file path will be stored in eaf-config-location/pdf/history/log.txt for eaf-open-pdf-from-history to use"
  :type 'boolean)

(defcustom eaf-pdf-dark-mode "follow"
  ""
  :type 'string)

(defcustom eaf-pdf-default-zoom 1.0
  ""
  :type 'float)

(defcustom eaf-pdf-scroll-ratio 0.05
  ""
  :type 'float)

(defcustom eaf-pdf-dark-exclude-image t
  ""
  :type 'boolean)

(defcustom eaf-pdf-viewer-keybinding
  '(("j" . "scroll_up")
    ("<down>" . "scroll_up")
    ("C-n" . "scroll_up")
    ("k" . "scroll_down")
    ("<up>" . "scroll_down")
    ("C-p" . "scroll_down")
    ("h" . "scroll_left")
    ("<left>" . "scroll_left")
    ("C-b" . "scroll_left")
    ("l" . "scroll_right")
    ("<right>" . "scroll_right")
    ("C-f" . "scroll_right")
    ("SPC" . "scroll_up_page")
    ("b" . "scroll_down_page")
    ("C-v" . "scroll_up_page")
    ("M-v" . "scroll_down_page")
    ("t" . "toggle_read_mode")
    ("0" . "zoom_reset")
    ("=" . "zoom_in")
    ("-" . "zoom_out")
    ("g" . "scroll_to_begin")
    ("G" . "scroll_to_end")
    ("p" . "jump_to_page")
    ("P" . "jump_to_percent")
    ("[" . "save_current_pos")
    ("]" . "jump_to_saved_pos")
    ("i" . "toggle_inverted_mode")
    ("m" . "toggle_mark_link")
    ("f" . "jump_to_link")
    ("M-w" . "copy_select")
    ("C-s" . "search_text_forward")
    ("C-r" . "search_text_backward")
    ("x" . "close_buffer")
    ("C-<right>" . "rotate_clockwise")
    ("C-<left>" . "rotate_counterclockwise")
    ("M-h" . "add_annot_highlight")
    ("M-u" . "add_annot_underline")
    ("M-s" . "add_annot_squiggly")
    ("M-d" . "add_annot_strikeout_or_delete_annot")
    ("M-e" . "add_annot_text_or_edit_annot")
    ("M-p" . "toggle_presentation_mode")
    ("J" . "select_left_tab")
    ("K" . "select_right_tab")
    ("o" . "eaf-pdf-outline")
    ("T" . "toggle_trim_white_margin"))
  "The keybinding of EAF PDF Viewer."
  :type 'cons)

(add-to-list 'eaf-app-binding-alist '("pdf-viewer" . eaf-pdf-viewer-keybinding))

(define-minor-mode eaf-pdf-outline-mode
  "EAF pdf outline mode."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "RET") 'eaf-pdf-outline-jump)
            (define-key map (kbd "q") 'quit-window)
            map))

(defun eaf-pdf-outline ()
  "Create PDF outline."
  (interactive)
  (let ((buffer-name (buffer-name (current-buffer)))
        (toc (eaf-call-sync "call_function" eaf--buffer-id "get_toc"))
        (page-number (string-to-number (eaf-call-sync "call_function" eaf--buffer-id "current_page"))))
    ;; Save window configuration before outline.
    (setq eaf-pdf-outline-window-configuration (current-window-configuration))

    ;; Insert outline content.
    (with-current-buffer (get-buffer-create  eaf-pdf-outline-buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert toc)
      (setq toc (mapcar (lambda (line)
                          (string-to-number (car (last (split-string line " ")))))
                        (butlast (split-string (buffer-string) "\n"))))
      (goto-line (seq-count (apply-partially #'>= page-number) toc))
      (set (make-local-variable 'eaf-pdf-outline-original-buffer-name) buffer-name)
      (let ((view-read-only nil))
        (read-only-mode 1))
      (eaf-pdf-outline-mode 1))

    ;; Popup ouline buffer.
    (pop-to-buffer eaf-pdf-outline-buffer-name)))

(defun eaf-pdf-outline-jump ()
  "Jump into specific page."
  (interactive)
  (let* ((line (thing-at-point 'line))
         (page-num (replace-regexp-in-string "\n" "" (car (last (s-split " " line))))))
    ;; Jump to page.
    (switch-to-buffer-other-window eaf-pdf-outline-original-buffer-name)
    (eaf-call-sync "call_function_with_args" eaf--buffer-id "jump_to_page_with_num" (format "%s" page-num))

    ;; Restore window configuration before outline operation.
    (when eaf-pdf-outline-window-configuration
      (set-window-configuration eaf-pdf-outline-window-configuration)
      (setq eaf-pdf-outline-window-configuration nil))))

(defun eaf-pdf-imenu-create-index-from-toc ()
  "Create an alist based on the table of contents of this buffer.

It call the Python's function \"get_toc\" then from the output, make an alist
with each element that looks like
(\"CHAPTER_NAME\" PAGE_NUMBER 'eaf-pdf-imenu-go-to-index nil).

(See why the element has to be that way in `imenu--index-alist'
 Hint: Look for \"Special elements\" in the documentation.)

the \"CHAPTER_NAME\" part will be replace with \"Page PAGE_NUMBER\"
when there is no table of contents for the buffer."
  (interactive)
  (or imenu--index-alist
      (setq imenu--index-alist
            (let ((toc (eaf-call-sync "call_function" eaf--buffer-id "get_toc")))
              (cond ((string= toc "")
                     (mapcar #'(lambda (page-num)
                                 (list (concat "Page " (number-to-string page-num)) page-num
                                       #'eaf-pdf-imenu-go-to-index
                                       nil))
                             (number-sequence 1
                                              (string-to-number
                                               (eaf-call-sync "call_function"
                                                              eaf--buffer-id
                                                              "page_total_number")))))
                    (t
                     (mapcar #'(lambda (line)
                                 (let ((line-split (split-string line " ")))
                                   (list (string-trim (string-join (butlast line-split) " "))
                                         (string-to-number (car (last line-split)))
                                         #'eaf-pdf-imenu-go-to-index
                                         nil)))
                             (split-string toc "\n"))))))))

(defun eaf-pdf-imenu-go-to-index (_chapter-name page-num _arg)
  "Ignore _CHAPTER-NAME and _ARG, call Python's \"jump_page\" function with PAGE-NUM as its argument.

The _CHAPTER-NAME is from the car of a element in `eaf-pdf-imenu-create-index-from-toc'
The _ARG is hardcoded to be nil from `eaf-pdf-imenu-create-index-from-toc'
Just ignore them and call \"jump_page\" to PAGE-NUM."
  (eaf-call-async "handle_input_response" eaf--buffer-id "jump_page" page-num))

(defun eaf-pdf-imenu-setup ()
  (setq imenu-create-index-function 'eaf-pdf-imenu-create-index-from-toc))

(add-hook 'eaf-pdf-viewer-hook 'eaf-pdf-imenu-setup)

(defun eaf-pdf-get-annots (page)
  "Return a map of annotations on PAGE.

The key is the annot id on PAGE."
  (eaf-call-sync "call_function_with_args" eaf--buffer-id "get_annots" (format "%s" page)))

(defun eaf-pdf-jump-to-annot (annot)
  "Jump to specifical pdf annot."
  (let ((rect (gethash "rect" annot))
        (page (gethash "page" annot)))
    (eaf-call-sync "call_function_with_args" eaf--buffer-id "jump_to_rect" (format "%s" page) rect)))

(defun eaf--pdf-viewer-bookmark ()
  "Restore EAF buffer according to pdf bookmark from the current file path or web URL."
  `((handler . eaf--bookmark-restore)
    (eaf-app . "pdf-viewer")
    (defaults . ,(list eaf--bookmark-title))
    (filename . ,(eaf-get-path-or-url))))

(defun eaf--pdf-update-position (page-index page-total-number)
  "Format mode line position indicator to show the current page and the total pages."
  (setq-local mode-line-position
              `(" P" ,page-index
                "/" ,page-total-number))
  (force-mode-line-update))

(defun eaf-store-pdf-history (url)
  "A wrapper around `eaf-open' that store pdf history candidates."
  (let* (found-history-result (pdf-history-file-path
                               (concat eaf-config-location
                                       (file-name-as-directory "pdf")
                                       (file-name-as-directory "history")
                                       "log.txt")))
    (if (not (file-exists-p pdf-history-file-path))
        (progn
          ;; If it does not exist, create a folder to store the log and create a log file
          (make-directory (file-name-directory pdf-history-file-path) t)
          (with-temp-file pdf-history-file-path "")))
    (find-file pdf-history-file-path)
    (goto-char (point-min))
    (if (search-forward url nil t) ;; search with no error
        (kill-whole-line))         ;; Delete this record
    (goto-char (point-min))
    (insert (concat url "\n"))
    (basic-save-buffer)
    (kill-current-buffer)))

(defun eaf-open-pdf-from-history ()
  "A wrapper around `eaf-open' that provides pdf history candidates.
This function works best if paired with a fuzzy search package."
  (interactive)
  (let* ((pdf-history-file-path
          (concat eaf-config-location
                  (file-name-as-directory "pdf")
                  (file-name-as-directory "history")
                  "log.txt"))
         (history-pattern "^\\(.+\\)\\.pdf$")
         (history-file-exists (file-exists-p pdf-history-file-path))
         (history-pdf (completing-read
                       "[EAF/pdf] Search || History: "
                       (if history-file-exists
                           (mapcar
                            (lambda (h) (when (string-match history-pattern h)
                                      (if (file-exists-p h)
                                          (format "%s" h))))
                            (with-temp-buffer (insert-file-contents pdf-history-file-path)
                                              (split-string (buffer-string) "\n" t)))
                         (make-directory (file-name-directory pdf-history-file-path) t)
                         (with-temp-file pdf-history-file-path "")))))
    (if history-pdf (eaf-open history-pdf))))

;;;###autoload
(defun eaf-open-office (file)
  "View Microsoft Office FILE as READ-ONLY PDF."
  (interactive "f[EAF/office] Open Office file as PDF: ")
  (if (executable-find "libreoffice")
      (let* ((file-md5 (eaf-get-file-md5 file))
             (file-name-base (file-name-base file))
             (convert-file (format "/tmp/%s.pdf" file-name-base))
             (pdf-file (format "/tmp/%s.pdf" file-md5)))
        (if (file-exists-p pdf-file)
            (eaf-open pdf-file "pdf-viewer" (concat file-name-base "_office-pdf"))
          (message "Converting %s to PDF, EAF will start shortly..." file)
          (make-process
           :name ""
           :buffer " *eaf-open-office*"
           :command (list "libreoffice" "--headless" "--convert-to" "pdf" (file-truename file) "--outdir" "/tmp")
           :sentinel (lambda (_ event)
                       (when (string= (substring event 0 -1) "finished")
                         (rename-file convert-file pdf-file)
                         (eaf-open pdf-file "pdf-viewer" (concat file-name-base "_office-pdf")))))))
    (error "[EAF/office] libreoffice is required convert Office file to PDF!")))

(defun eaf-get-file-md5 (file)
  "Get the MD5 value of a specified FILE."
  (car (split-string (shell-command-to-string (format "md5sum '%s'" (file-truename file))) " ")))

(provide 'eaf-pdf-viewer)
;;; eaf-pdf-viewer.el ends here
