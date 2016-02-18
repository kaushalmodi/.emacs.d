;; Time-stamp: <2016-02-18 11:07:05 kmodi>

;; De-ansify

;; Display a buffer showing the original file content without ANSI color codes
;; from that live-updating file with ANSI color codes.

;; Usage: Call `M-x de-ansify' to view the de-ansified version of the current file.

(require 'filenotify)

(defvar de-ansify-no-ansi-file-path-prefix (concat temporary-file-directory
                                                   (getenv "USER") "_de-ansify"))

(defvar de-ansify--file-notify-alist '())

(defvar de-ansify-mode-map (make-sparse-keymap)
  "Keymap to use in a de-ansified file.")

(define-minor-mode de-ansify-mode
  "A temporary minor mode used to set buffer-specific key bindings in a
de-ansified file buffer."
  :init-value nil
  :lighter ""
  :keymap de-ansify-mode-map)

(define-key de-ansify-mode-map (kbd "C-c C-a") #'auto-revert-mode)
(define-key de-ansify-mode-map (kbd "C-c C-k") #'de-ansify-remove-watch)
(define-key de-ansify-mode-map (kbd "C-c C-x") #'de-ansify-flush-watch-list)

(defun de-ansify--copy-and-de-ansify (orig-file no-ansi-file)
  "Internal function that copies ORIG-FILE to NO-ANSI-FILE, which is
then de-ansified."
  (copy-file orig-file no-ansi-file :ok-to-overwrite)
  (start-process "perl-de-ansify" nil ; async process
                 "perl" "-pi" "-e" "'s/\\e\\[\\d+(?>(;\\d+)*)m//g'" no-ansi-file))

;;;###autoload
(defun de-ansify ()
  "Open the de-ansified version of the current file and add `file-notify'
watch for it."
  (interactive)
  (let* ((orig-file (buffer-file-name))
         (no-ansi-file (concat de-ansify-no-ansi-file-path-prefix
                               (replace-regexp-in-string "/" "!" orig-file)))
         descriptor)
    ;; Add a watch for this file IF one is not already added
    (when (not (assoc orig-file de-ansify--file-notify-alist))
      (setq descriptor (file-notify-add-watch
                        orig-file
                        '(change attribute-change)
                        #'de-ansify-notify-callback))
      (add-to-list 'de-ansify--file-notify-alist (cons orig-file descriptor)))
    (de-ansify--copy-and-de-ansify orig-file no-ansi-file)
    (find-file no-ansi-file)
    (setq-local kill-buffer-query-functions
                (remq 'server-kill-buffer-query-function
                      kill-buffer-query-functions))
    (de-ansify-mode 1) ; Set the `de-ansify-mode-map' bindings
    (auto-revert-mode 1)))

(defun de-ansify-notify-callback (event)
  "On getting triggered, copy the original file to a separate file and de-ansify
that file asynchronously using `de-ansify-async' function."
  ;; EVENT is of the form (DESCRIPTOR ACTION FILE [FILE1])
  (let* ((orig-file (nth 2 event))
         (no-ansi-file (concat de-ansify-no-ansi-file-path-prefix
                               (replace-regexp-in-string "/" "!" orig-file))))
    ;; (message "Event: %S" event)
    (de-ansify--copy-and-de-ansify orig-file no-ansi-file)
    (message "De-ansified file updated with changes in %s" orig-file)))

(defun de-ansify-remove-watch (no-delete)
  "Remove the current file from the filenotify watch list and also delete the
file. But if NO-DELETE is non-nil, do not delete this file."
  (interactive "P")
  (let* ((no-ansi-file (buffer-file-name))
         (orig-file (replace-regexp-in-string
                     "!" "/"
                     (replace-regexp-in-string
                      de-ansify-no-ansi-file-path-prefix "" no-ansi-file)))
         (elt (assoc orig-file de-ansify--file-notify-alist))
         (descriptor (cdr elt)))
    (when (not no-delete)
      (when (yes-or-no-p "[de-ansify] Are you sure you want to delete this file? ")
        (delete-file no-ansi-file)
        (kill-buffer (current-buffer))))
    ;; Remove the `elt' from the filenotify watch list and
    ;; `de-ansify--file-notify-alist'
    (file-notify-rm-watch descriptor)
    (setq de-ansify--file-notify-alist (delete elt de-ansify--file-notify-alist))
    (find-file orig-file)))

(defun de-ansify-flush-watch-list ()
  "Flush the watch list specific to `de-ansify'."
  (interactive)
  (message "Flushing the `de-ansify' filenotify watch list ..")
  (dotimes (index (safe-length de-ansify--file-notify-alist))
    (file-notify-rm-watch (pop de-ansify--file-notify-alist))))


(provide 'de-ansify)
