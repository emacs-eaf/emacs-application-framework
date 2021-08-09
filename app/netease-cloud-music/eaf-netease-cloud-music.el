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

(defcustom eaf-netease-cloud-music-user+list '()
  ""
  :type 'cons)

(defcustom eaf-netease-cloud-music-keybinding
  '(("<f12>" . "open_devtools")
    ("<up>" . "eaf--netease-cloud-music-move-song-up")
    ("<down>" . "eaf--netease-cloud-music-move-song-down")
    ("SPC" . "netease-cloud-music-pause-or-continue")
    ("<return>" . "eaf--netease-cloud-music-switch-enter")
    ("C-n" . "js_scroll_up")
    ("C-p" . "js_scroll_down")
    ("M-v" . "js_scroll_up_page")
    ("M-V" . "js_scroll_down_page")
    ("M-<" . "js_scroll_to_begin")
    ("M->" . "js_scroll_to_bottom")
    ("M-f" . "netease-cloud-music-switch-next-page")
    ("M-b" . "netease-cloud-music-switch-prev-page")
    ("M-n" . "js_scroll_playlist_up")
    ("M-p" . "js_scroll_playlist_down")
    ("q" . "netease-cloud-music-back")
    ("Q" . "netease-cloud-music-quit")
    ("r" . "netease-cloud-music-change-repeat-mode")
    ("x" . "netease-cloud-music-kill-current-song")
    ("/" . "eaf--netease-cloud-music-play-with-index")
    ("n" . "js_play_next")
    ("N" . "netease-cloud-music-random-play")
    ("p" . "js_play_prev")
    ("P" . "netease-cloud-music-playlist-play")
    ("c" . "netease-cloud-music-change-lyric-type")
    ("d" . "eaf--netease-cloud-music-delete-song-from-playlist")
    ("D" . "netease-cloud-music-delete-playing-song")
    ("<" . "netease-cloud-music-seek-backward")
    (">" . "netease-cloud-music-seek-forward")
    ("k" . "netease-cloud-music-clear-playlist")
    ("w" . "eaf--netease-cloud-music-write-mode-enter")
    ("s" . "eaf--netease-cloud-music-switch-playlist")
    ("f" . "netease-cloud-music-search-song")
    ("F" . "netease-cloud-music-search-playlist")
    ("a" . "eaf--netease-cloud-music-add-to-playlist")
    ("A" . "netease-cloud-music-switch-add-page")
    ("C" . "netease-cloud-music-create-playlist")
    ("m" . "netease-cloud-music-change-playlist-name")
    ("M" . "netease-cloud-music-delete-playlist")
    ("g" . "eaf--netease-cloud-music-cancel-search")
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

(add-to-list 'eaf-app-binding-alist '("netease-cloud-music" . eaf-netease-cloud-music-keybinding))

(setq eaf-netease-cloud-music-module-path (concat (file-name-directory load-file-name) "buffer.py"))
(add-to-list 'eaf-app-module-path-alist '("netease-cloud-music" . eaf-netease-cloud-music-module-path))

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
             (setq playlist netease-cloud-music-playlist))

            ((and netease-cloud-music-playlists
                  (netease-cloud-music-alist-cdr
                   pid netease-cloud-music-playlists))
             (setq netease-cloud-music-use-local-playlist nil
                   netease-cloud-music-playlist-id pid
                   netease-cloud-music-playlists-songs
                   (netease-cloud-music-get-playlist-songs
                    netease-cloud-music-playlist-id))
             (setq playlist netease-cloud-music-playlists-songs))
            (t (na-error "The pid cannot be found!")))

      (eaf-call-async "execute_js_function" eaf--buffer-id
                      (string-trim-left "change_playlist_style" "js_")
                      (json-encode
                       (if netease-cloud-music-use-local-playlist
                           0
                         (1+ (cl-position
                              (netease-cloud-music-alist-cdr
                               netease-cloud-music-playlist-id
                               netease-cloud-music-playlists)
                              netease-cloud-music-playlists)))))
      (eaf-call-async "execute_js_function" eaf--buffer-id
                      (string-trim-left "set_playlist" "js_") (json-encode-array playlist)))
    (when netease-cloud-music-process
      (netease-cloud-music-kill-current-song))))

(defun eaf--netease-cloud-music-change-play-status ()
  "Change play status."
  (let ((icon (pcase netease-cloud-music-process-status
                ("playing" "pause-circle")
                ("paused" "play-circle")
                (_ "play-circle"))))
    (eaf-call-async "execute_js_function" eaf--buffer-id
                    (string-trim-left "set_play_icon_status" "js_")
                    (json-encode icon))))

(defun eaf--netease-cloud-music-init ()
  "Init the netease-cloud-music."
  (when eaf-netease-cloud-music-user+list
    (eaf-call-async "execute_js_function" eaf--buffer-id
                    (string-trim-left "update_user_info" "js_") (json-encode eaf-netease-cloud-music-user+list)))
  (when netease-cloud-music-playlists
    (eaf-call-async "execute_js_function" eaf--buffer-id
                    (string-trim-left "set_user_playlists" "js_")
                    (json-encode-array
                     (netease-cloud-music--cons-to-list
                      netease-cloud-music-playlists))))
  (eaf-call-async "execute_js_function" eaf--buffer-id
                  (string-trim-left "change_playlist_style" "js_")
                  (json-encode
                   (list (if netease-cloud-music-use-local-playlist
                             0
                           (1+ (cl-position
                                (netease-cloud-music-alist-cdr
                                 netease-cloud-music-playlist-id
                                 netease-cloud-music-playlists)
                                netease-cloud-music-playlists)))
                         t)))
  (eaf-call-async "execute_js_function" eaf--buffer-id
                  (string-trim-left "set_repeat_mode" "js_")
                  (json-encode netease-cloud-music-repeat-mode))
  (eaf-call-async "execute_js_function" eaf--buffer-id
                  (string-trim-left "set_panel_song_info" "js_")
                  (json-encode
                   (if netease-cloud-music-process
                       (list (car netease-cloud-music-current-song)
                             (nth 1 netease-cloud-music-current-song))
                     (list "" ""))))
  (eaf--netease-cloud-music-change-play-status)

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
                     (with-current-buffer "eaf-netease-cloud-music"
                       (eaf-call-async "execute_js_function" eaf--buffer-id
                                       (string-trim-left "set_user_playlists" "js_")
                                       (json-encode-array
                                        (netease-cloud-music--cons-to-list
                                         netease-cloud-music-playlists))))))))))))
  
  (eaf-call-async "execute_js_function" eaf--buffer-id
                  (string-trim-left "set_playlist" "js_")
                  (json-encode-array
                   (if netease-cloud-music-use-local-playlist
                       netease-cloud-music-playlist
                     netease-cloud-music-playlists-songs)))
  (eaf--netease-cloud-music--update-song-style))

(defun eaf--netease-cloud-music--update-song-style ()
  "Update song style."
  (when (string= "playing" netease-cloud-music-process-status)
    (eaf-call-async "execute_js_function" eaf--buffer-id
                    (string-trim-left "change_song_style" "js_") netease-cloud-music-playlist-song-index)))

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
    (eaf-call-async "execute_js_function" eaf--buffer-id
                    (string-trim-left "set_index_style" "js_") (json-encode t))
    (let ((index (1- (read-number "Enter the playlist's index: "))))
      (eaf--netease-cloud-music-change-playlist
       (if (= index 0)
           index
         (cdr (nth (1- index) netease-cloud-music-playlists)))))
    (eaf-call-async "execute_js_function" eaf--buffer-id
                    (string-trim-left "set_index_style" "js_") (json-encode nil))))

(defun eaf--netease-cloud-music-add-to-playlist ()
  "Add the search songs or playlists to current playlist."
  (interactive)
  (if (eq netease-cloud-music-search-type 'song)
      (netease-cloud-music-switch-add-to-playlist)
    (netease-cloud-music-playlist-add-all)))

(defun eaf--netease-cloud-music-cancel-search ()
  "Cancel search."
  (interactive)
  (eaf-call-async "execute_js_function" eaf--buffer-id
                  (string-trim-left "change_playlist_mode" "js_")
                  (json-encode nil))
  (eaf-call-async "execute_js_function" eaf--buffer-id
                  (string-trim-left "set_playlist" "js_")
                  (json-encode-array
                   (if netease-cloud-music-use-local-playlist
                       netease-cloud-music-playlist
                     netease-cloud-music-playlists-songs)))
  (netease-cloud-music-adjust-song-index))

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
