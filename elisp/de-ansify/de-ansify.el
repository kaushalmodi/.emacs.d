;; Time-stamp: <2016-02-18 02:01:44 kmodi>

;; De-ansify

;; Display a buffer showing the original file content without ANSI color codes
;; from that live-updating file with ANSI color codes.

;; Usage:
;; - Call `M-x de-ansify' to view the de-ansified version of the current file.
;; - Hit "C-c C-k" to delete the de-ansified file.
;; - "C-c C-a" will toggle the auto-revert-mode in the de-ansified buffer.
;; - "C-c C-x" will flush the filenotify watch list associated with de-ansify files.

(require 'filenotify)

(defvar de-ansify-no-ansi-file-path-prefix (concat "/tmp/" (getenv "USER") "_deansify_"))

(defvar de-ansify--file-notify-alist '())

(defun de-ansify-async (file)
  (start-process "perl-deansify" nil
                 "perl" "-pi" "-e" "'s/\\e\\[\\d+(?>(;\\d+)*)m//g'"
                 file))

(defun de-ansify ()
  "Open the de-ansified version of the current file and add `file-notify'
watch for it."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (file-name-no-ansi file-name)
         (file-name-no-ansi (concat de-ansify-no-ansi-file-path-prefix
                                    (replace-regexp-in-string
                                     "/" "!" file-name-no-ansi)))
         file-descriptor)
    ;; Add a watch for this file IF one is not already added
    (if (not (assoc file-name de-ansify--file-notify-alist))
        (progn
          (setq file-descriptor (file-notify-add-watch file-name
                                                       '(change attribute-change)
                                                       #'de-ansify-notify-callback))
          (add-to-list 'de-ansify--file-notify-alist
                       (cons file-name file-descriptor))

          (write-file file-name-no-ansi nil) ; Save the current file as the "no-ansi" file
          (auto-revert-mode 1) ; Enable auto-revert-mode in this new file
          (setq-local kill-buffer-query-functions
                      (remq 'server-kill-buffer-query-function
                            kill-buffer-query-functions))
          (de-ansify-async file-name-no-ansi)
          (define-key (current-local-map) (kbd "C-c C-k") #'de-ansify-remove-watch)
          (define-key (current-local-map) (kbd "C-c C-x") #'de-ansify-flush-watch-list)
          (define-key (current-local-map) (kbd "C-c C-a") #'auto-revert-mode))
      (message "This file is already being watched."))))

(defun de-ansify-notify-callback (event)
  "On getting triggered, copy the original file to a separate file and de-ansify
that file asynchronously using `de-ansify-async' function."
  ;; EVENT is of the form (DESCRIPTOR ACTION FILE [FILE1])
  (let* ((file-name (nth 2 event))
         (file-name-no-ansi (concat de-ansify-no-ansi-file-path-prefix
                                    (replace-regexp-in-string
                                     "/" "!" file-name))))
    ;; (message "Event: %S" event)
    (call-process "cp" nil nil nil "-f" file-name file-name-no-ansi)
    (de-ansify-async file-name-no-ansi)
    (message "De-ansified file updated with changes from %s" file-name)))

(defun de-ansify-remove-watch (no-delete)
  "Remove the current file from the filenotify watch list and also delete the file.
But if NO-DELETE is non-nil, do not delete this file."
  (interactive "P")
  (let* ((file-name-no-ansi (buffer-file-name))
         (buffer (current-buffer))
         (file-name (replace-regexp-in-string
                     "!" "/"
                     (replace-regexp-in-string
                      de-ansify-no-ansi-file-path-prefix "" file-name-no-ansi)))
         (alist-element (assoc file-name de-ansify--file-notify-alist)))
    (when (not no-delete)
      (when (yes-or-no-p "[de-ansify] Are you sure you want to delete this file? ")
        (delete-file file-name-no-ansi)
        (kill-buffer buffer)))
    ;; Remove the `alist-element' from the filenotify watch list and
    ;; `de-ansify--file-notify-alist'
    (file-notify-rm-watch (cdr alist-element))
    (setq de-ansify--file-notify-alist
          (delete alist-element de-ansify--file-notify-alist))
    (find-file file-name)))

(defun de-ansify-flush-watch-list ()
  "Flush the watch list specific to `de-ansify'."
  (interactive)
  (message "Flushing the `de-ansify' filenotify watch list ..")
  (dotimes (index (safe-length de-ansify--file-notify-alist))
    (file-notify-rm-watch (pop de-ansify--file-notify-alist))))


(provide 'de-ansify)
