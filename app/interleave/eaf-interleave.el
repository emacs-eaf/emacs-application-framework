;;; eaf-interleave.el --- Interleaving text books on EAF -*- lexical-binding: t -*-

;; Author: Sebastian Christ <rudolfo.christ@gmail.com>
;; URL: https://github.com/rudolfochrist/interleave
;; Version: 1.4.20161123-610
;; Fork: luhuaei <luhuaei@gmail.com>

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

;;; Commentary:

;; In the past, textbooks were sometimes published as 'interleaved'
;; editions.  That meant, each page was followed by a blank page and
;; ambitious students/scholars had the ability to take their notes
;; directly in their copy of the textbook.  Newton and Kant were
;; prominent representatives of this technique.

;; Nowadays textbooks (or lecture material) come in PDF format.  Although almost
;; every PDF Reader has the ability to add some notes to the PDF itself, it is
;; not as powerful as it could be.

;; This is what this minor mode tries to accomplish.  It presents your PDF side by
;; side to an [[http://orgmode.org][Org Mode]] buffer with your notes, narrowing
;; down to just those passages that are relevant to the particular page in the
;; document viewer.

;;; Usage:

;;; Code:

(require 'org)
(require 'org-element)

(defcustom eaf-interleave-org-notes-dir-list '("~/org/interleave_notes" ".")
  "List of directories to look into when opening notes org from a pdf file.

The notes file is assumed to have the exact
same base name as the pdf file (just that the file extension is
.org instead of .pdf).

If the notes org file is not found, it is created in the
directory returned on doing `car' of this list (first element of
the list).

The notes file is searched in order from the first list element
till the last; the search is aborted once the file is found.

If a list element is \".\" or begins with \"./\", that portion is
replaced with the pdf directory name.  e.g. \".\" is interpreted
as \"/pdf/file/dir/\", \"./notes\" is interpreted as
\"/pdf/file/dir/notes/\"."
  :type '(repeat directory))

(defcustom eaf-interleave-split-direction 'vertical
  "Specify how to split the notes buffer."
  :type '(choice (const vertical)
                 (const horizontal)))

(defcustom eaf-interleave-split-lines nil
  "Specify the number of lines the PDF buffer should be increased or decreased.

If nil both buffers are split equally.  If the number is positive,
the window is enlarged.  If the number is negative, the window is
shrunken.

If `eaf-interleave-split-direction' is 'vertical then the number is
taken as columns."
  :type '(choice integer
                 (const nil)))

(defcustom eaf-interleave-disable-narrowing nil
  "Disable narrowing in notes/org buffer."
  :type 'boolean)

;;; Interleave
;; Minor mode for the org file buffer containing notes
(defvar eaf-interleave-mode-map (make-sparse-keymap)
  "Keymap while command `eaf-interleave-mode' is active in the org file buffer.")

;;;###autoload
(define-minor-mode eaf-interleave-mode
  "Interleaving your text books since 2015.

In the past, textbooks were sometimes published as 'interleaved' editions.
That meant, each page was followed by a blank page and the ambitious student/
scholar had the ability to take their notes directly in their copy of the
textbook. Newton and Kant were prominent representatives of this technique.

Nowadays textbooks (or lecture material) come in PDF format. Although almost
every PDF Reader has the ability to add some notes to the PDF itself, it is
not as powerful as it could be.

This is what this minor mode tries to accomplish. It presents your PDF side by
pppside to an [[http://orgmode.org][Org Mode]] buffer with your notes, narrowing
down to just those passages that are relevant to the particular page in the
document viewer.

The split direction is determined by the customizable variable
`eaf-interleave-split-direction'. When `eaf-interleave-mode' is invoked
with a prefix argument the inverse split direction is used
e.g. if `eaf-interleave-split-direction' is 'vertical the buffer is
split horizontally."
  :keymap eaf-interleave-mode-map
  (if eaf-interleave-mode
      (progn
        (setq eaf-interleave-org-buffer (current-buffer))
        (setq eaf-interleave--window-configuration (current-window-configuration))
        (eaf-interleave--open-file)
        ;; expand/show all headlines if narrowing is disabled
        (when eaf-interleave-disable-narrowing
          (with-current-buffer eaf-interleave-org-buffer
            (goto-char (point-min))
            (org-cycle-hide-drawers 'all)))
        (eaf-interleave--go-to-page-note 1)
        (message "EAF Interleave enabled"))
    ;; Disable the corresponding minor mode in the PDF file too.
    (set-window-configuration eaf-interleave--window-configuration)
    (setq eaf-interleave--window-configuration nil)
    (setq eaf-interleave-org-buffer nil)
    ))

(defun eaf-interleave--close-all ())

;;; Interleave PDF Mode
;; Minor mode for the pdf file buffer associated with the notes
(defvar eaf-interleave-pdf-mode-map (make-sparse-keymap)
  "Keymap while command `eaf-interleave-pdf-mode' is active in the pdf file buffer."
  )

;;;###autoload
(define-minor-mode eaf-interleave-pdf-mode
  "Interleave view for the pdf."
  :keymap eaf-interleave-pdf-mode-map)

;; variables
(defvar eaf-interleave-org-buffer nil
  "Org notes buffer name.")

(defvar eaf-interleave--window-configuration nil
  "Variable to store the window configuration before interleave mode was enabled.")

(defconst eaf-interleave--page-note-prop "interleave_page_note"
  "The page note property string.")

(defconst eaf-interleave--url-prop "interleave_url"
  "The pdf property string.")

;; functions
(defun eaf-interleave--open-file ()
  "Opens the pdf file in besides the notes buffer.

SPLIT-WINDOW is a function that actually splits the window, so it must be either
`split-window-right' or `split-window-below'."
  (let ((pdf-file-name
         (or (org-entry-get-with-inheritance eaf-interleave--url-prop)
             (eaf-interleave--headline-pdf-path eaf-interleave-org-buffer)
             (eaf-interleave--find-pdf-path eaf-interleave-org-buffer)
             (eaf-interleave--handle-parse-pdf-file-name))))
    (eaf-interleave--select-split-function)
    (eaf-interleave--eaf-open-pdf pdf-file-name)
    pdf-file-name))

(defun eaf-interleave--handle-parse-pdf-file-name ()
  "When don't parse responsive pdf file on current org file."
  (let ((pdf-file-name (read-file-name "No INTERLEAVE_PDF property found. Please specify path: " nil nil t)))
    ;; Check whether we have any entry at point with `org-entry-properties' before
    ;; prompting if the user wants multi-pdf.
    (if (and (org-entry-properties) (y-or-n-p "Is this multi-pdf? "))
        (org-entry-put (point) "INTERLEAVE_PDF" pdf-file-name)
      (save-excursion
        (goto-char (point-min))
        (insert "#+INTERLEAVE_PDF: " pdf-file-name)))
    pdf-file-name))

(defun eaf-interleave--headline-pdf-path (buffer)
  "Return the INTERLEAVE_PDF property of the current headline in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (let ((headline (org-element-at-point)))
        (when (and (equal (org-element-type headline) 'headline)
                   (org-entry-get nil eaf-interleave--url-prop))
          (org-entry-get nil eaf-interleave--url-prop))))))

(defun eaf-interleave--find-pdf-path (buffer)
  "Search the `interleave_pdf' property in BUFFER and extracts it when found."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^#\\+interleave_pdf: \\(.*\\)" nil :noerror)
          (match-string 1))))))

(defun eaf-interleave--select-split-function ()
  "Determine which split function to use.

This returns either `split-window-below' or `split-window-right'
based on a combination of `current-prefix-arg' and
`eaf-interleave-split-direction'."
  (let ()
    (delete-other-windows)
    (if (string= eaf-interleave-split-direction "vertical")
        (split-window-right)
      (split-window-below))
    (when (integerp eaf-interleave-split-lines)
      (if (eql eaf-interleave-split-direction 'horizontal)
          (enlarge-window eaf-interleave-split-lines)
        (enlarge-window-horizontally eaf-interleave-split-lines)))
    ))

(defun eaf-interleave--go-to-page-note (page)
  "Look up the notes for the current pdf PAGE.

Effectively resolves the headline with the interleave_page_note
property set to PAGE and returns the point.

If `eaf-interleave-disable-narrowing' is non-nil then the buffer gets
re-centered to the page heading.

It (possibly) narrows the subtree when found."
  (with-current-buffer eaf-interleave-org-buffer
    (let (point (window (get-buffer-window (current-buffer) 'visible)))
      (save-excursion
        (widen)
        (goto-char (point-min))
        (when (re-search-forward (format "^\[ \t\r\]*\:interleave_page_note\: %d$" page) nil t)
          ;; widen the buffer again for the case it is narrowed from
          ;; multi-pdf notes search. Kinda ugly I know. Maybe a macro helps?
          (widen)
          (org-back-to-heading t)
          (eaf-interleave--narrow-to-subtree)
          (org-show-subtree)
          (org-cycle-hide-drawers t)
          (setq point (point))))
      ;; When narrowing is disabled, and the notes/org buffer is
      ;; visible recenter to the current headline. So even if not
      ;; narrowed the notes buffer scrolls allong with the PDF.
      (when (and eaf-interleave-disable-narrowing point window)
        (with-selected-window window
          (goto-char point)
          (recenter)))
      point)))

(defun eaf-interleave-sync-previous-note ()
  "Move to the previous set of notes.
This show the previous notes and synchronizes the PDF to the right page number."
  (interactive)
  (eaf-interleave--switch-to-org-buffer)
  (widen)
  (eaf-interleave--goto-parent-headline eaf-interleave--page-note-prop)
  (org-backward-heading-same-level 1)
  (eaf-interleave--narrow-to-subtree)
  (org-show-subtree)
  (org-cycle-hide-drawers t)
  (eaf-interleave-sync-current-note))

(defun eaf-interleave-sync-next-note ()
  "Move to the next set of notes.
This shows the next notes and synchronizes the PDF to the right page number."
  (interactive)
  (eaf-interleave--switch-to-org-buffer)
  (widen)
  ;; go to the first notes heading if we're not at an headline or if
  ;; we're on multi-pdf heading. This is useful to quickly jump to the
  ;; notes if they start at page 96 or so. Image you need to skip page
  ;; for page.
  (if (eaf-interleave--goto-parent-headline eaf-interleave--page-note-prop)
      (org-forward-heading-same-level 1)
    (outline-next-visible-heading 1))
  (eaf-interleave--narrow-to-subtree)
  (org-show-subtree)
  (org-cycle-hide-drawers t)
  (eaf-interleave-sync-current-note))

(defun eaf-interleave--goto-parent-headline (property)
  "Traverse the tree until the parent headline.

Consider a headline with property PROPERTY as parent headline."
  (catch 'done
    (if (and (eql (org-element-type (org-element-at-point)) 'headline)
             (org-entry-get (point) property))
        (org-element-at-point)
      (condition-case nil
          (org-up-element)
        ('error
         (throw 'done nil)))
      (eaf-interleave--goto-parent-headline property))))

(defun eaf-interleave--narrow-to-subtree (&optional force)
  "Narrow buffer to the current subtree.

If `eaf-interleave-disable-narrowing' is non-nil this
function does nothing.

When FORCE is non-nil `eaf-interleave-disable-narrowing' is
ignored."
  (when (and (not (org-before-first-heading-p))
             (or (not eaf-interleave-disable-narrowing)
                 force))
    (org-narrow-to-subtree)))

(defun eaf-interleave--switch-to-org-buffer (&optional insert-newline-maybe position)
  "Switch to the notes buffer.

Inserts a newline into the notes buffer if INSERT-NEWLINE-MAYBE
is non-nil.
If POSITION is non-nil move point to it."
  (if (derived-mode-p 'eaf-mode)
      (switch-to-buffer-other-window eaf-interleave-org-buffer)
    (switch-to-buffer eaf-interleave-org-buffer))
  (when (integerp position)
    (goto-char position))
  (when insert-newline-maybe
    (save-restriction
      (when eaf-interleave-disable-narrowing
        (eaf-interleave--narrow-to-subtree t))
      (goto-char (point-max)))
    ;; Expand again. Sometimes the new content is outside the narrowed
    ;; region.
    (org-show-subtree)
    (redisplay)
    ;; Insert a new line if not already on a new line
    (when (not (looking-back "^ *" (line-beginning-position)))
      (org-return))))

(defun eaf-interleave--insert-heading-respect-content (parent-headline)
  "Create a new heading in the notes buffer.

Adjust the level of the new headline according to the
PARENT-HEADLINE.

Return the position of the newly inserted heading."
  (org-insert-heading-respect-content)
  (let* ((parent-level 0 )
         (change-level (if (> (org-element-property :level (org-element-at-point))
                              (1+ parent-level))
                           #'org-promote
                         #'org-demote)))
    (while (/= (org-element-property :level (org-element-at-point))
               (1+ parent-level))
      (funcall change-level)))
  (point))

(defun eaf-interleave--create-new-note (url page)
  "Create a new headline for the page PAGE."
  (let (new-note-position)
    (with-current-buffer eaf-interleave-org-buffer
      (save-excursion
        (widen)
        (let ((position (goto-char (point-max))))
          (setq new-note-position (eaf-interleave--insert-heading-respect-content position)))
        (insert (format "Notes for page %d" page))
        (org-set-property eaf-interleave--url-prop url)
        (org-set-property eaf-interleave--page-note-prop (number-to-string page))
        (eaf-interleave--narrow-to-subtree)
        (org-cycle-hide-drawers t)))
    (eaf-interleave--switch-to-org-buffer t new-note-position)))

(defun eaf-interleave-add-note ()
  "Add note for the EAF buffer.

If there are already notes for this url, jump to the notes
buffer."
  (interactive)
  (cond ((and (derived-mode-p 'eaf-mode)
              (equal eaf--buffer-app-name "pdf-viewer"))
         (eaf-interleave-pdf-add-note))))

(defun eaf-interleave-pdf-add-note ()
  "EAF pdf-viewer-mode add note"
  (let* ((page (eaf-interleave--pdf-viewer-current-page eaf--buffer-url))
         (position (eaf-interleave--go-to-page-note page)))
    (if position
        (eaf-interleave--switch-to-org-buffer t position)
      (eaf-interleave--create-new-note eaf--buffer-url page)))
  )

(defun eaf-interleave-sync-current-note ()
  "Sync EAF buffer on current note"
  (let ((url (org-entry-get-with-inheritance eaf-interleave--url-prop)))
    (cond ((and (string-prefix-p "/" url) (string-suffix-p "pdf" url t))
           (eaf-interleave-sync-pdf-page-current))))
  )

(defun eaf-interleave-sync-pdf-page-current ()
  "Open PDF page for currently visible notes."
  (interactive)
  (let* ((pdf-page (string-to-number (org-entry-get-with-inheritance eaf-interleave--page-note-prop)))
         (pdf-url (org-entry-get-with-inheritance eaf-interleave--url-prop))
         (buffer (eaf-interleave--find-buffer pdf-url)))
    (if buffer
        (when (and (integerp pdf-page) (> pdf-page 0)) ; The page number needs to be a positive integer
          (eaf-interleave--narrow-to-subtree)
          (display-buffer-reuse-mode-window buffer '(("mode" . "eaf-interleave-pdf-mode")))
          (with-current-buffer buffer
            (eaf-interleave--pdf-viewer-goto-page pdf-url pdf-page)))
      (eaf-interleave--open-file)
      )))

;;;###autoload
(defun eaf-interleave-open-notes-file-for-pdf ()
  "Open the notes org file for the current pdf file if it exists.
Else create it.

It is assumed that the notes org file will have the exact same base name
as the pdf file (just that the notes file will have a .org extension instead
of .pdf)."
  (interactive)
  (when (derived-mode-p 'eaf-mode)
    (let* ((pdf-file-name (eaf-get-path-or-url))
           (org-file-name-sans-directory (concat (file-name-base pdf-file-name) ".org"))
           org-file-create-dir
           (cnt 0)
           try-org-file-name
           (org-file-name (catch 'break
                            (dolist (dir eaf-interleave-org-notes-dir-list)
                              ;; If dir is "." or begins with "./", replace
                              ;; the "." or "./" with the pdf dir name
                              (setq dir (replace-regexp-in-string
                                         "^\\(\\.$\\|\\./\\).*"
                                         (file-name-directory pdf-file-name)
                                         dir nil nil 1))
                              (when (= cnt 0)
                                ;; In the event the org file is needed to be
                                ;; created, it will be created in the directory
                                ;; listed as the first element in
                                ;; `eaf-interleave-org-notes-dir-list'
                                (setq org-file-create-dir dir))
                              (setq cnt (1+ cnt))
                              (setq try-org-file-name (locate-file
                                                       org-file-name-sans-directory
                                                       (list dir)))
                              (when try-org-file-name
                                ;; return the first match
                                (throw 'break try-org-file-name))))))
      ;; Create the notes org file if it does not exist
      (when (null org-file-name)
        (setq org-file-name (if (null eaf-interleave-org-notes-dir-list)
                                (read-file-name "Path: " "~/")
                              (progn
                                (when (null (file-exists-p org-file-create-dir))
                                  (make-directory org-file-create-dir))
                                (expand-file-name org-file-name-sans-directory
                                                  org-file-create-dir))))
        (with-temp-file org-file-name
          (insert "#+INTERLEAVE_PDF: " pdf-file-name)))
      ;; Open the notes org file and enable `eaf-interleave-mode'
      (find-file org-file-name)
      (eaf-interleave-mode))))

(defun eaf-interleave-quit ()
  "Quit interleave mode."
  (interactive)
  (with-current-buffer eaf-interleave-org-buffer
    (widen)
    (goto-char (point-min))
    (when (eaf-interleave--headlines-available-p)
      (org-overview))
    (eaf-interleave-mode 0)))

(defun eaf-interleave--headlines-available-p ()
  "True if there are headings in the notes buffer."
  (save-excursion
    (re-search-forward "^\* .*" nil t)))

;; utils
(defun eaf-interleave--eaf-open-pdf (pdf-file-name)
  "Use EAF PdfViewer open this pdf-file-name document."
  (eaf-open pdf-file-name)
  (add-hook 'eaf-pdf-viewer-hook 'eaf-interleave-pdf-mode))

(defun eaf-interleave--find-buffer (url)
  "find EAF buffer base url"
  (let (current-buffer)
    (catch 'find-buffer
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
          (when (and
                 (derived-mode-p 'eaf-mode)
                 (equal eaf--buffer-url url))
            (setq current-buffer buffer)
            (throw 'find-buffer t)))))
    current-buffer))

(defun eaf-interleave--kill-buffer (url)
  "Kill the current converter process and buffer."
  (interactive)
  (let ((buffer (eaf-interleave--find-buffer url)))
    (kill-buffer buffer)))

(defun eaf-interleave--pdf-viewer-current-page (url)
  "get current page index."
  (let ((id (buffer-local-value 'eaf--buffer-id (eaf-interleave--find-buffer url))))
    (string-to-number (eaf-call "call_function" id "current_page"))))

(defun eaf-interleave--pdf-viewer-goto-page (url page)
  "goto page"
  (let ((id (buffer-local-value 'eaf--buffer-id (eaf-interleave--find-buffer url))))
    (eaf-call "handle_input_message" id "jump_page" page)))

(provide 'eaf-interleave)
;;; interleave.el ends here
