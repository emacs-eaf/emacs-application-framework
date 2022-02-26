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
  :type '(repeat directory)
  :group 'eaf)

(defcustom eaf-interleave-split-direction 'vertical
  "Specify how to split the notes buffer."
  :type '(choice (const vertical)
                 (const horizontal))
  :group 'eaf)

(defcustom eaf-interleave-split-lines nil
  "Specify the number of lines the PDF buffer should be increased or decreased.

If nil both buffers are split equally.  If the number is positive,
the window is enlarged.  If the number is negative, the window is
shrunken.

If `eaf-interleave-split-direction' is 'vertical then the number is
taken as columns."
  :type '(choice integer
                 (const nil))
  :group 'eaf)

(defcustom eaf-interleave-disable-narrowing nil
  "Disable narrowing in notes/org buffer."
  :type 'boolean
  :group 'eaf)

;; variables
(defvar eaf-interleave-org-buffer nil
  "Org notes buffer name.")

(defvar eaf-interleave--window-configuration nil
  "Variable to store the window configuration before interleave mode was enabled.")

(defconst eaf-interleave--page-note-prop "interleave_page_note"
  "The page note property string.")

(defconst eaf-interleave--url-prop "interleave_url"
  "The pdf property string.")

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
      (setq eaf-interleave-org-buffer (current-buffer))
    ;; Disable the corresponding minor mode in the PDF file too.
    (setq eaf-interleave-org-buffer nil)))

(defvar eaf-interleave-app-mode-map (make-sparse-keymap)
  "Keymap while command `eaf-interleave-app-mode' is active.")

;;;###autoload
(define-minor-mode eaf-interleave-app-mode
  "Interleave view for the EAF app."
  :keymap eaf-interleave-app-mode-map)

;;; functions
;; interactive
(defun eaf-interleave-sync-current-note ()
  "Sync EAF buffer on current note"
  (interactive)
  (let ((url (org-entry-get-with-inheritance eaf-interleave--url-prop)))
    (cond ((and (string-prefix-p "/" url) (string-suffix-p "pdf" url t))
           (eaf-interleave-sync-pdf-page-current))
          ((string-prefix-p "http" url)
           (eaf-interleave-sync-browser-url-current))))
  )

(defun eaf-interleave-sync-pdf-page-current ()
  "Open PDF page for currently visible notes."
  (interactive)
  (let* ((pdf-page (org-entry-get-with-inheritance eaf-interleave--page-note-prop))
         (pdf-url (org-entry-get-with-inheritance eaf-interleave--url-prop))
         (buffer (eaf-interleave--find-buffer pdf-url)))
    (if buffer
        (progn
          (eaf-interleave--display-buffer buffer)
          (when pdf-page
            (with-current-buffer buffer
              (eaf-interleave--pdf-viewer-goto-page pdf-url pdf-page))))
      (eaf-interleave--select-split-function)
      (eaf-interleave--open-pdf pdf-url)
      )))

(defun eaf-interleave-sync-next-note ()
  "Move to the next set of notes.
This shows the next notes and synchronizes the PDF to the right page number."
  (interactive)
  (eaf-interleave--switch-to-org-buffer)
  (widen)
  (org-forward-heading-same-level 1)
  (eaf-interleave--narrow-to-subtree)
  (org-show-subtree)
  (org-cycle-hide-drawers t)
  (eaf-interleave-sync-current-note))

(defun eaf-interleave-add-note ()
  "Add note for the EAF buffer.

If there are already notes for this url, jump to the notes
buffer."
  (interactive)
  (if (derived-mode-p 'eaf-mode)
      (cond ((equal eaf--buffer-app-name "pdf-viewer")
             (eaf-interleave--pdf-add-note))
            ((equal eaf--buffer-app-name "browser")
             (eaf-interleave--browser-add-note)))
    ))

(defun eaf-interleave-add-file-url ()
  "Add a new url on note if the property is none, else modify current url."
  (interactive)
  (let ((url (read-file-name "Please specify path: " nil nil t)))
    (org-entry-put (point) eaf-interleave--url-prop url)))

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

(defun eaf-interleave-open-notes-file ()
  "Find current EAF url corresponding note files if it exists."
  (interactive)
  (if (derived-mode-p 'eaf-mode)
      (cond ((equal eaf--buffer-app-name "pdf-viewer")
             (eaf-interleave--open-notes-file-for-pdf))
            ((equal eaf--buffer-app-name "browser")
             (eaf-interleave--open-notes-file-for-browser))))
  )

(defun eaf-interleave-quit ()
  "Quit interleave mode."
  (interactive)
  (with-current-buffer eaf-interleave-org-buffer
    (widen)
    (goto-char (point-min))
    (when (eaf-interleave--headlines-available-p)
      (org-overview))
    (eaf-interleave-mode 0)))

;;;###autoload
(defun eaf-interleave--open-notes-file-for-pdf ()
  "Open the notes org file for the current pdf file if it exists.
Else create it. It is assumed that the notes org file will have
the exact same base name as the pdf file (just that the notes
file will have a .org extension instead of .pdf)."
  (let ((org-file (concat (file-name-base eaf--buffer-url) ".org")))
    (eaf-interleave--open-notes-file-for-app org-file)))

(defun eaf-interleave--open-notes-file-for-browser ()
  "Find current open interleave-mode org file, if exists, else
will create new org file with URL. It is assumed that the notes
org file will have the exact sam base name as the url domain."
  (unless (buffer-live-p eaf-interleave-org-buffer)
    (let* ((domain (url-domain (url-generic-parse-url eaf--buffer-url)))
           (org-file (concat domain ".org")))
      (eaf-interleave--open-notes-file-for-app org-file))))

(defun eaf-interleave--open-notes-file-for-app (org-file)
  "Open the notes org file for the current url if it exists.
Else create it."
  (let ((org-file-path (eaf-interleave--find-match-org eaf-interleave-org-notes-dir-list eaf--buffer-url))
        (buffer (eaf-interleave--find-buffer eaf--buffer-url)))
    ;; Create the notes org file if it does not exist
    (unless org-file-path
      (setq org-file-path (eaf-interleave--ensure-org-file-exist eaf-interleave-org-notes-dir-list org-file)))
    ;; Open the notes org file and enable `eaf-interleave-mode'
    (find-file org-file-path)
    (eaf-interleave-mode)
    (eaf-interleave--select-split-function)
    (switch-to-buffer buffer)
    ))

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

(defun eaf-interleave--go-to-page-note (url page)
  "Look up the notes for the current pdf PAGE.

Effectively resolves the headline with the interleave_page_note
property set to PAGE and returns the point.

If `eaf-interleave-disable-narrowing' is non-nil then the buffer gets
re-centered to the page heading.

It (possibly) narrows the subtree when found."
  (with-current-buffer eaf-interleave-org-buffer
    (let ((property-list (org-map-entries (lambda ()
                                        (let ((url (org-entry-get-with-inheritance eaf-interleave--url-prop))
                                              (page (org-entry-get-with-inheritance eaf-interleave--page-note-prop)))
                                          (cons url page)))))
          point)
      (catch 'find-property
        (dolist (property property-list)
          (when (and (string= (car property) url)
                     (string= (cdr property) (number-to-string page)))
            (widen)
            (org-back-to-heading t)
            (eaf-interleave--narrow-to-subtree)
            (org-show-subtree)
            (org-cycle-hide-drawers t)
            (setq point (point))
            (throw 'find-property nil))))
      point)))

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

(defun eaf-interleave--insert-heading-respect-content ()
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

(defun eaf-interleave--create-new-note (url &optional title page)
  "Create a new headline for current EAF url."
  (let (new-note-position)
    (with-current-buffer eaf-interleave-org-buffer
      (save-excursion
        (widen)
        (setq new-note-position (eaf-interleave--insert-heading-respect-content))
        (org-set-property eaf-interleave--url-prop url)
        (when title
          (insert (format "Notes for %s" title)))
        (when page
          (org-set-property eaf-interleave--page-note-prop (number-to-string page)))
        (eaf-interleave--narrow-to-subtree)
        (org-cycle-hide-drawers t)))
    (eaf-interleave--switch-to-org-buffer t new-note-position)))

(defun eaf-interleave-sync-browser-url-current ()
  "Sync current note url for browser"
  (let* ((web-url (org-entry-get-with-inheritance eaf-interleave--url-prop))
        (buffer (eaf-interleave--find-buffer web-url)))
    (if buffer
        (eaf-interleave--display-buffer buffer)
      (eaf-interleave--select-split-function)
      (eaf-interleave--open-web-url web-url))))

(defun eaf-interleave--display-buffer (buffer)
  "Use already used window display buffer"
  (eaf-interleave--narrow-to-subtree)
  (display-buffer-reuse-mode-window buffer '(("mode" . "eaf-interleave-app-mode")))
  (eaf-interleave--ensure-buffer-window buffer))

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

(defun eaf-interleave--pdf-add-note ()
  "EAF pdf-viewer-mode add note"
  (let* ((page (eaf-interleave--pdf-viewer-current-page eaf--buffer-url))
         (position (eaf-interleave--go-to-page-note eaf--buffer-url page)))
    (if position
        (eaf-interleave--switch-to-org-buffer t position)
      (eaf-interleave--create-new-note eaf--buffer-url eaf--buffer-app-name page)))
  )

(defun eaf-interleave--browser-add-note ()
  "EAF browser add note"
  (eaf-interleave--create-new-note eaf--buffer-url eaf--buffer-app-name))

(defun eaf-interleave--headlines-available-p ()
  "True if there are headings in the notes buffer."
  (save-excursion
    (re-search-forward "^\* .*" nil t)))

;; utils
(defun eaf-interleave--open-pdf (pdf-file-name)
  "Use EAF PdfViewer open this pdf-file-name document."
  (eaf-open pdf-file-name)
  (add-hook 'eaf-pdf-viewer-hook 'eaf-interleave-app-mode))

(defun eaf-interleave--open-web-url (url)
  "Use EAF Browser open current note web address"
  (eaf-open-browser url)
  (add-hook 'eaf-browser-hook 'eaf-interleave-app-mode))

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
  (let ((buffer (eaf-interleave--find-buffer url)))
    (kill-buffer buffer)))

(defun eaf-interleave--pdf-viewer-current-page (url)
  "get current page index."
  (let ((id (buffer-local-value 'eaf--buffer-id (eaf-interleave--find-buffer url))))
    (string-to-number (eaf-call-sync "execute_function" id "current_page"))))

(defun eaf-interleave--pdf-viewer-goto-page (url page)
  "goto page"
  (let ((id (buffer-local-value 'eaf--buffer-id (eaf-interleave--find-buffer url))))
    (eaf-call-async "handle_input_response" id "jump_page" page)))

(defun eaf-interleave--ensure-buffer-window (buffer)
  "If BUFFER don't display, will use other window display"
  (if (get-buffer-window buffer)
      nil
    (eaf-interleave--select-split-function)
    (switch-to-buffer buffer)))

(defun eaf-interleave--parse-current-dir (dir url)
  "If dir is '.' or begins with './', replace the '.' or './' with the current url name"
  (replace-regexp-in-string
   "^\\(\\.$\\|\\./\\).*"
   (file-name-directory url)
   dir nil nil 1))

(defun eaf-interleave--find-match-org (dir-list url)
  "Find corresponding org file base url on dir list"
  (let ((org-file (concat (file-name-base url) ".org"))
        path)
    (catch 'break
      (dolist (dir dir-list)
        (setq dir (eaf-interleave--parse-current-dir dir url))
        (setq path (locate-file org-file (list dir)))
        (when path
          ;; return the first match
          (throw 'break path))))
    path))

(defun eaf-interleave--ensure-org-file-exist (dir-list file-name)
  "If the org file directory exist return this path, else created directory."
  (let ((default-dir (nth 0 dir-list)))
    (if dir-list
        (progn
          (unless (file-exists-p default-dir)
            (make-directory default-dir))
          (expand-file-name file-name default-dir))
      (read-file-name "Path for org file: " "~/"))))

(provide 'eaf-interleave)
;;; interleave.el ends here
