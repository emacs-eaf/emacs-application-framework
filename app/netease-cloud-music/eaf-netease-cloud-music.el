;;; eaf-netease-cloud-music.el --- EAF Netease Cloud Music App -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan


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

;;; Code:

(defcustom eaf-netease-cloud-music-repeat-mode ""
  ""
  :type 'string)

(defcustom eaf-netease-cloud-music-playlist-id 0
  ""
  :type 'integer)

(defcustom eaf-netease-cloud-music-play-status ""
  ""
  :type 'string)

(defcustom eaf-netease-cloud-music-local-playlist+list '()
  ""
  :type 'cons)

(defcustom eaf-netease-cloud-music-user-playlists+list '()
  ""
  :type 'cons)

(defcustom eaf-netease-cloud-music-playlists-songs+list '()
  ""
  :type 'cons)

(defcustom eaf-netease-cloud-music-current-song+list '("" "")
  ""
  :type 'cons)

(defcustom eaf-netease-cloud-music-user+list '()
  ""
  :type 'cons)

(defcustom eaf-netease-cloud-music-keybinding
  '(("<f12>" . "open_devtools")
    ("<up>" . "eaf--netease-cloud-music-move-song-up")
    ("<down>" . "eaf--netease-cloud-music-move-song-down")
    ("SPC" . "play_or_pause")
    ("<return>" . "eaf--netease-cloud-music-switch-enter")
    ("C-n" . "scroll_up")
    ("C-p" . "scroll_down")
    ("M-v" . "scroll_up_page")
    ("M-V" . "scroll_down_page")
    ("M-<" . "scroll_to_begin")
    ("M->" . "scroll_to_bottom")
    ("M-f" . "netease-cloud-music-switch-next-page")
    ("M-b" . "netease-cloud-music-switch-prev-page")
    ("M-n" . "scroll_playlist_up")
    ("M-p" . "scroll_playlist_down")
    ("q" . "netease-cloud-music-back")
    ("Q" . "netease-cloud-music-quit")
    ("r" . "change_repeat_mode")
    ("x" . "netease-cloud-music-kill-current-song")
    ("/" . "eaf--netease-cloud-music-play-with-index")
    ("n" . "play_next")
    ("N" . "netease-cloud-music-random-play")
    ("p" . "play_prev")
    ("P" . "netease-cloud-music-playlist-play")
    ("c" . "netease-cloud-music-change-lyric-type")
    ("d" . "eaf--netease-cloud-music-delete-song-from-playlist")
    ("D" . "netease-cloud-music-delete-playing-song")
    ("<" . "netease-cloud-music-seek-backward")
    (">" . "netease-cloud-music-seek-forward")
    ("k" . "netease-cloud-music-clear-playlist")
    ("w" . "eaf--netease-cloud-music-write-mode-enter")
    ("s" . "eaf--netease-cloud-music-switch-playlist")
    ("f" . "search_song")
    ("F" . "search_playlist")
    ("a" . "eaf--netease-cloud-music-add-to-playlist")
    ("A" . "netease-cloud-music-switch-add-page")
    ("C" . "netease-cloud-music-create-playlist")
    ("m" . "netease-cloud-music-change-playlist-name")
    ("M" . "netease-cloud-music-delete-playlist")
    ("g" . "cancel_search")
    ("l" . "netease-cloud-music-login")
    ("e" . "netease-cloud-music-get-recommend-songs")
    ("E" . "netease-cloud-music-get-recommend-playlists")
    ("j" . "netease-cloud-music-storage-song")
    ("J" . "netease-cloud-music-add-storage-to-current-playlist")
    ("o" . "netease-cloud-music-show-storage")
    ("O" . "netease-cloud-music-delete-song-from-storage")
    ("K" . "netease-cloud-music-clear-storage")
    )
  "The keybinding of EAF Netease Cloud Music."
  :type 'cons)

(defun eaf--netease-cloud-music-change-playlist (pid)
  "Change the current playlist to PID."
  (when (featurep 'netease-cloud-music)
    (when (stringp pid)
      (setq pid (string-to-number pid)))
    (let (playlist)
      (cond ((= pid 0)
             (setq netease-cloud-music-use-local-playlist t
                   netease-cloud-music-playlists-songs nil
                   netease-cloud-music-playlist-id nil)
             (when netease-cloud-music-playlist-refresh-timer
               (cancel-timer netease-cloud-music-playlist-refresh-timer)
               (setq netease-cloud-music-playlist-refresh-timer nil))
             (setq eaf-netease-cloud-music-local-playlist+list netease-cloud-music-playlist)
             (setq eaf-netease-cloud-music-playlist-id 0)
             (setq playlist netease-cloud-music-playlist))

            ((and netease-cloud-music-playlists
                  (netease-cloud-music-alist-cdr
                   pid netease-cloud-music-playlists))
             (setq netease-cloud-music-use-local-playlist nil
                   netease-cloud-music-playlist-id pid
                   netease-cloud-music-playlists-songs
                   (netease-cloud-music-get-playlist-songs
                    netease-cloud-music-playlist-id))
             (setq eaf-netease-cloud-music-playlist-id
                   (setq pid
                         (1+
                          (cl-position
                           (netease-cloud-music-alist-cdr pid netease-cloud-music-playlists)
                           netease-cloud-music-playlists))))
             (setq eaf-netease-cloud-music-playlists-songs+list
                   netease-cloud-music-playlists-songs)
             (setq playlist netease-cloud-music-playlists-songs))
            (t (na-error "The pid cannot be found!")))

      (eaf-call-async "call_function_with_args" eaf--buffer-id
                     "update_playlist_style" nil pid)
      (eaf-call-async "call_function_with_args" eaf--buffer-id
                     "set_playlist" playlist))
    (when netease-cloud-music-process
      (netease-cloud-music-kill-current-song))))

(defun eaf--netease-cloud-music-init ()
  "Init the netease-cloud-music."
  (netease-cloud-music-get-playlist)
  (unless (file-exists-p netease-cloud-music-cache-directory)
    (make-directory netease-cloud-music-cache-directory))
  (when (and (netease-cloud-music--api-downloaded)
             (not (netease-cloud-music-api-process-live-p)))
    (netease-cloud-music-start-api))

  (when (file-exists-p netease-cloud-music-user-loginfo-file)
    (let ((loginfo (netease-cloud-music-get-loginfo)))
      (when loginfo
        (setq netease-cloud-music-phone (car loginfo)
              netease-cloud-music-user-password (cdr loginfo)
              netease-cloud-music-login-timer
              (run-with-timer
               1 2
               (lambda ()
                 (if (and netease-cloud-music-user-id
                          netease-cloud-music-username)
                     (progn
                       (cancel-timer netease-cloud-music-login-timer)
                       (setq netease-cloud-music-login-timer nil))
                   (netease-cloud-music--get-user-info)
                   (ignore-errors
                     (setq netease-cloud-music-playlists
                           (netease-cloud-music-get-user-playlist
                            netease-cloud-music-user-id))
                     (setq eaf-netease-cloud-music-user-playlists+list
                           netease-cloud-music-playlists)
                     (with-current-buffer "eaf-netease-cloud-music"
                       (eaf-call-async "call_function_with_args" eaf--buffer-id
                                       "refresh_user_playlist"
                                       netease-cloud-music-playlists)))))))))))

(defun eaf--netease-cloud-music--update-song-style ()
  "Update song style."
  (when (string= "playing" netease-cloud-music-process-status)
    (eaf-call-async "call_function_with_args" eaf--buffer-id
                    "change_song_style" netease-cloud-music-playlist-song-index)))

(defun eaf--netease-cloud-music-play-with-index ()
  "Read index from Minibuffer."
  (interactive)
  (let* ((index (read-number "Enter the song's index: "))
         (song (nth (1- index)
                    (if netease-cloud-music-use-local-playlist
                        netease-cloud-music-playlist
                      netease-cloud-music-playlists-songs))))
    (if (null song)
        (user-error "[EAF/Netease-Cloud-Music]: The index is error!")
      (netease-cloud-music-play
       (car song) (nth 1 song) (nth 3 song)))))

(defun eaf--netease-cloud-music-delete-song-from-playlist ()
  "Delete song from playlist read from Minibuffer."
  (interactive)
  (let ((index (read-number "Enter the song's index: ")))
    (netease-cloud-music-delete-song-from-playlist (1- index))))

(defun eaf--netease-cloud-music-move-song (up)
  "Move the song read from Minibuffer up or down.
If Up is not non-nil, move the song up.Otherwise move it down."
  (let ((index (read-number "Enter the song's index: ")))
    (if (string= up "True")
        (netease-cloud-music-move-up (1- index))
      (netease-cloud-music-move-down (1- index)))))

(defun eaf--netease-cloud-music-move-song-up ()
  "Move song up."
  (interactive)
  (eaf--netease-cloud-music-move-song t))

(defun eaf--netease-cloud-music-move-song-down ()
  "Move song down."
  (interactive)
  (eaf--netease-cloud-music-move-song nil))

(defun eaf--netease-cloud-music-write-mode-enter ()
  "Enter the write mode."
  (interactive)
  (switch-to-buffer (get-buffer-create "eaf-netease-cloud-music-write"))
  (netease-cloud-music-write-mode))

(defun eaf--netease-cloud-music-switch-enter (&optional index)
  "Add current song or playlist into current playlist."
  (interactive)
  (cond ((null index)
         (setq index (1- (read-number "Enter the item's index: "))))
        ((stringp index)
         (setq index (string-to-number index))))
  (if (eq netease-cloud-music-search-type 'song)
      (netease-cloud-music-switch-enter index)
    (netease-cloud-music-playlist-enter index)))

(defun eaf--netease-cloud-music-switch-playlist ()
  "Switch playlist by getting index."
  (interactive)
  (with-current-buffer "eaf-netease-cloud-music"
    (eaf-call-sync "call_function_with_args" eaf--buffer-id
                   "set_index_style" t)
    (let ((index (1- (read-number "Enter the playlist's index: "))))
      (eaf--netease-cloud-music-change-playlist
       (if (= index 0)
           index
         (cdr (nth (1- index) netease-cloud-music-playlists)))))
    (eaf-call-sync "call_function_with_args" eaf--buffer-id
                   "set_index_style" "False")))

(defun eaf--netease-cloud-music-add-to-playlist ()
  "Add the search songs or playlists to current playlist."
  (interactive)
  (if (eq netease-cloud-music-search-type 'song)
      (netease-cloud-music-switch-add-to-playlist)
    (netease-cloud-music-playlist-add-all)))

;;;###autoload
(defun eaf-open-netease-cloud-music ()
  "Open EAF netease cloud music."
  (interactive)
  (if (ignore-errors (or (featurep 'netease-cloud-music)
                         (load-library "netease-cloud-music")))
      (progn
        (setq netease-cloud-music-last-buffer (current-buffer))
        (if (get-buffer "eaf-netease-cloud-music")
            (switch-to-buffer "eaf-netease-cloud-music")
          (eaf-open "eaf-netease-cloud-music" "netease-cloud-music")))
    (user-error "[EAF/Netease-Cloud-Music]: You haven't install the package netease-cloud-music.")))

(provide 'eaf-netease-cloud-music)

;;; eaf-netease-cloud-music.el ends here
