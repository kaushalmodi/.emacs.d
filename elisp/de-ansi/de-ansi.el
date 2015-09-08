;; Time-stamp: <2015-09-04 15:31:28 kmodi>

;; De-ansi

;; Display a temporary buffer showing content without ANSI color codes
;; from a still-updating file with ANSI color codes.

;; Usage:
;; - Call `M-x de-ansify' to view the de-ansified version of the current file
;; - Hit "XX" to delete the de-ansified file
;; - "AA" with toggle the auto-revert-mode in the de-ansified buffer

(require 'filenotify)
(require 'key-chord)
(require 'temp-mode)

(defvar de-ansi/temp-file-path-prefix (concat "/tmp/" (getenv "USER") "_deansi_"))

(defvar de-ansi/file-notify-names-list (quote nil))
(defvar de-ansi/file-notify-alist (quote nil))

(defun de-ansify ()
  "Open the de-ansified version of the current file and add `file-notify' watch for it."
  (interactive)
  (let* ((file-name-full (buffer-file-name))
         (file-name (file-name-nondirectory file-name-full))
         (temp-file-name-full (concat de-ansi/temp-file-path-prefix
                                      file-name)))
    (write-file temp-file-name-full nil) ; Save the current file as a temp file
    (auto-revert-mode 1) ; Enable auto-revert-mode for the temp file
    (temp-mode 1) ; Enable temp-mode
    ;; (key-chord-define-local "XX" 'de-ansi/delete-de-ansified)
    ;; (key-chord-define-local "AA" 'auto-revert-mode)
    (key-chord-define temp-mode-map "XX" 'de-ansi/delete-de-ansified)
    (key-chord-define temp-mode-map "AA" 'auto-revert-mode)
    (setq-local kill-buffer-query-functions
                (remq 'server-kill-buffer-query-function
                      kill-buffer-query-functions))
    (shell-command (concat "perl -pi -e 's/\\e\\[\\d+(?>(;\\d+)*)m//g' "
                           temp-file-name-full))
    ;; Add a watch for this file IF one is not already added
    (when (not (member file-name 'de-ansi/file-notify-names-list))
      (file-notify-add-watch file-name-full
                             '(change attribute-change)
                             'de-ansi/notify-callback)
      (add-to-list 'de-ansi/file-notify-names-list file-name))))

(defun de-ansi/notify-callback (event)
  "On getting triggered,
- Run perl script to save the de-ansified file to the temp file"
  (let* ((file-descriptor (nth 0 event))
         (file-name-full (nth 2 event))
         (file-name (file-name-nondirectory file-name-full))
         (temp-file-name-full (concat de-ansi/temp-file-path-prefix
                                      file-name)))
    (shell-command (concat "\\cp -f "
                           file-name-full " "
                           temp-file-name-full))
    (shell-command (concat "perl -pi -e 's/\\e\\[\\d+(?>(;\\d+)*)m//g' "
                           temp-file-name-full))
    (message (format "De-ansified file updated with changes from %s"
                     file-name))
    (add-to-list 'de-ansi/file-notify-alist (cons file-name file-descriptor))))

(defun de-ansi/delete-de-ansified (dont-delete-file)
  "When deleting the temp file, also remove its watch from `file-notify',
while updating `de-ansi/file-notify-alist'."
  (interactive "p")
  (let* ((temp-file-name-full (buffer-file-name))
         (temp-file-name (file-name-nondirectory temp-file-name-full))
         (buffer (current-buffer))
         original-file-name)
    (setq original-file-name temp-file-name)
    (replace-regexp-in-string de-ansi/temp-file-path-prefix "" original-file-name)
    (when (not (= dont-delete-file 4))
      (when (yes-or-no-p "[de-ansi] Are you sure you want to delete this file? ")
        (delete-file temp-file-name-full)
        (kill-buffer buffer)))
    (file-notify-rm-watch (cdr (assoc original-file-name de-ansi/file-notify-alist)))))

(defun de-ansi/flush-watch-list ()
  "Flush the watch list specific to de-ansi."
  (interactive)
  (setq de-ansi/file-notify-names-list (quote nil))
  (dotimes (index (safe-length de-ansi/file-notify-alist))
    (file-notify-rm-watch (pop de-ansi/file-notify-alist))))


(provide 'de-ansi)
