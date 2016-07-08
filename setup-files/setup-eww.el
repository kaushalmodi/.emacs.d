;; Time-stamp: <2016-07-08 18:22:46 kmodi>

;; Eww - Emacs browser (needs emacs 24.4 or higher)

(use-package eww
  :bind (:map modi-mode-map
         ("M-s M-w" . eww-search-words)
         ("M-s M-l" . modi/eww-get-link))
  :chords (("-=" . eww))
  :commands (modi/eww-im-feeling-lucky
             modi/eww-browse-url-of-file)
  :init
  (progn
    (bind-to-modi-map "e" #'eww-open-file))
  :config
  (progn
    ;; (setq eww-search-prefix                 "https://duckduckgo.com/html/?q=")
    (setq eww-search-prefix                 "https://www.google.com/search?q=")
    (setq eww-download-directory            "~/downloads")
    (setq eww-form-checkbox-symbol          "[ ]")
    ;; (setq eww-form-checkbox-symbol          "☐") ; Unicode hex 2610
    (setq eww-form-checkbox-selected-symbol "[X]")
    ;; (setq eww-form-checkbox-selected-symbol "☑") ; Unicode hex 2611
    ;; Improve the contract of pages like Google results
    ;; http://emacs.stackexchange.com/q/2955/115
    (setq shr-color-visible-luminance-min 80) ; default = 40

    ;; Auto-rename new eww buffers
    ;; http://ergoemacs.org/emacs/emacs_eww_web_browser.html
    (defun xah-rename-eww-hook ()
      "Rename eww browser's buffer so sites open in new page."
      (rename-buffer "eww" t))
    (add-hook 'eww-mode-hook #'xah-rename-eww-hook)

    ;; If the current buffer is an eww buffer, "M-x eww" will always reuse the
    ;; current buffer to load the new page. Below advice will make "C-u M-x eww"
    ;; force a new eww buffer even when the current buffer is an eww buffer.
    ;; The above `xah-rename-eww-hook' fix is still needed in order to create
    ;; uniquely named eww buffers.
    ;; http://emacs.stackexchange.com/a/24477/115
    (defun modi/force-new-eww-buffer (orig-fun &rest args)
      "When prefix argument is used, a new eww buffer will be created.
This is regardless of whether the current buffer is an eww buffer. "
      (if current-prefix-arg
          (with-temp-buffer
            (apply orig-fun args))
        (apply orig-fun args)))
    (advice-add 'eww :around #'modi/force-new-eww-buffer)

    ;; Override the default definition of `eww-search-words'
    (defun eww-search-words (&optional beg end)
      "Search the web for the text between the point and marker.
See the `eww-search-prefix' variable for the search engine used."
      (interactive "r")
      (if (use-region-p)
          (eww (buffer-substring beg end))
        (eww (modi/get-symbol-at-point))))

    (defun modi/eww--go-to-first-search-result (search-term)
      "Navigate to the first search result in the *eww* buffer."
      ;; Keep on burying the current buffer if it turns out to be an eww buffer.
      (while (string-match "^\\*?eww" (buffer-name))
        (bury-buffer))
      ;; Start a new eww search
      (eww search-term)
      (let* ((max-wait 5) ; seconds
             (search-repeat-interval 0.1) ; seconds
             (max-trials (floor max-wait search-repeat-interval))
             (start-time (current-time))
             (n 1))
        ;; The while loop will keep on repeating every `search-repeat-interval'
        ;; seconds till the return value of `eww-links-at-point' is non-nil.
        (catch 'break
          (while (<= n max-trials)
            (goto-char (point-min)) ; go to the top of the buffer
            (re-search-forward "[0-9]+\\s-+results\\s-*$" nil :noerror) ; go to the start of results
            (shr-next-link) ; go to the first search result
            (when (eww-links-at-point)
              (throw 'break nil))
            ;; Wait for a while before trying link check again
            (sleep-for search-repeat-interval)
            ;; (message "eww search result trial # %d" n)
            (setq n (1+ n))))
        (message "Search for `%s' finished in %0.2f seconds."
                 search-term (float-time (time-since start-time)))))

    (defun modi/eww-get-link (search-term)
      "Copy the link to the first search result."
      (interactive "sSearch term: ")
      (let ((eww-buffer-name))
        (modi/eww--go-to-first-search-result search-term)
        (setq eww-buffer-name (rename-buffer "*eww-temp*" t))
        ;; Copy the actual link instead of the redirection link by calling
        ;; `shr-copy-url' twice
        (dotimes (i 2)
          (shr-copy-url))
        (kill-buffer eww-buffer-name)))

    (defun modi/eww-im-feeling-lucky (search-term)
      "Navigate to the first search result directly."
      (interactive "sSearch term (I'm Feeling Lucky!): ")
      (modi/eww--go-to-first-search-result search-term)
      (eww-follow-link))

    (defun modi/eww-copy-url-dwim(&optional option)
      "Copy the URL under point to the kill ring.

If OPTION is other than 16 or nil (`C-u'), or there is no link under
point, but there is an image under point then copy the URL of the
image under point instead.

If called twice, then try to fetch the URL and see whether it
redirects somewhere else.

If both link and image url recovery fails, copy the page url.

If OPTION is 16 (`C-u C-u'), copy the page url."
      (interactive "P")
      (cl-case (car option)
        (16 (message "Copied page url: %s" (eww-copy-page-url))) ; C-u C-u
        (t  (when (string= (shr-copy-url option) ; no prefix or C-u
                           "No URL under point")
              ;; Copy page url if COMMAND or C-u COMMAND returns
              ;; "No URL under point"
              (message "Copied page url: %s" (eww-copy-page-url))))))

    (defun modi/eww-keep-lines (regexp)
      "Show only the lines matching regexp in the web page.
Call `eww-reload' to undo the filtering."
      (interactive (list (read-from-minibuffer
                          "Keep only lines matching regexp: ")))
      (let ((inhibit-read-only t)) ; ignore read-only status of eww buffers
        (save-excursion
          (goto-char (point-min))
          (keep-lines regexp))))

    (defun modi/eww-browse-url-of-file ()
      "Browse the current file using `eww'."
      (interactive)
      (let ((browse-url-browser-function 'eww-browse-url))
        (call-interactively #'browse-url-of-file)))

    ;; eww-lnum
    ;; https://github.com/m00natic/eww-lnum
    (use-package eww-lnum
      :bind (:map eww-mode-map
             ("f" . eww-lnum-follow)
             ("U" . eww-lnum-universal)))

    ;; org-eww
    ;; Copy text from html page for pasting in org mode file/buffer
    ;; e.g. Copied HTML hyperlinks get converted to [[link][desc]] for org mode.
    ;; http://emacs.stackexchange.com/a/8191/115
    (use-package org-eww
      :bind (:map eww-mode-map
             ("o" . org-eww-copy-for-org-mode)))

    ;; Auto-refreshing eww buffer whenever the html file it's showing changes
    ;; http://emacs.stackexchange.com/a/2566/115
    (defvar modi/eww--file-notify-descriptors-list ()
      "List to store file-notify descriptor for all files that have an
associated auto-reloading eww buffer.")

    (defun modi/advice-eww-open-file-to-auto-reload (orig-fun &rest args)
      "When `eww-open-file' is called with \\[universal-argument], open
the file in eww and also add `file-notify' watch for it so that the eww
buffer auto-reloads when the HTML file changes."
      (prog1
          (apply orig-fun args)
        (when current-prefix-arg ; C-u M-x eww-open-file
          (require 'filenotify)
          (let ((file-name (car args)))
            (file-notify-add-watch file-name
                                   '(change attribute-change)
                                   #'modi/file-notify-callback-eww-reload)
            ;; Show the HTML file and its rendered form in eww side-by-side
            (find-file-other-window file-name))
          ;; Redefine the `q' binding in `eww-mode-map'
          (bind-key "q" #'modi/eww-quit-and-update-fn-descriptors eww-mode-map))))
    (advice-add 'eww-open-file :around #'modi/advice-eww-open-file-to-auto-reload)

    (defun modi/file-notify-callback-eww-reload (event)
      "On getting triggered, switch to the eww buffer, reload and switch
back to the working buffer. Also save the `file-notify-descriptor' of the
triggering event."
      (let* ((working-buffer (buffer-name)))
        (switch-to-buffer-other-window "eww")
        (eww-reload)
        (switch-to-buffer-other-window working-buffer))
      ;; `(car event)' will return the event descriptor
      (add-to-list 'modi/eww--file-notify-descriptors-list (car event)))

    (defun modi/eww-quit-and-update-fn-descriptors ()
      "When quitting `eww', first remove any saved file-notify descriptors
specific to eww, while also updating `modi/eww--file-notify-descriptors-list'."
      (interactive)
      (dotimes (index (safe-length modi/eww--file-notify-descriptors-list))
        (file-notify-rm-watch (pop modi/eww--file-notify-descriptors-list)))
      (quit-window :kill))

    (bind-keys
     :map eww-mode-map
      (":" . eww) ; Go to URL
      ("h" . eww-list-histories) ; View history
      ("w" . modi/eww-copy-url-dwim)
      ("/" . highlight-regexp)
      ("k" . modi/eww-keep-lines))
    ;; Make the binding for `revert-buffer' do `eww-reload' in eww-mode
    (define-key eww-mode-map [remap revert-buffer] #'eww-reload)
    (bind-keys
     :map eww-text-map ; For single line text fields
      ("<backtab>"  . shr-previous-link) ; S-TAB Jump to previous link on the page
      ("<C-return>" . eww-submit)) ; S-TAB Jump to previous link on the page
    (bind-keys
     :map eww-textarea-map ; For multi-line text boxes
      ("<backtab>"  . shr-previous-link) ; S-TAB Jump to previous link on the page
      ("<C-return>" . eww-submit)) ; S-TAB Jump to previous link on the page
    (bind-keys
     :map eww-checkbox-map
      ("<down-mouse-1>" . eww-toggle-checkbox))
    (bind-keys
     :map shr-map
      ("w" . modi/eww-copy-url-dwim))
    (bind-keys
     :map eww-link-keymap
      ("w" . modi/eww-copy-url-dwim))))


(provide 'setup-eww)

;; Default eww key bindings
;; |-----------+---------------------------------------------------------------------|
;; | Key       | Function                                                            |
;; |-----------+---------------------------------------------------------------------|
;; | &         | Browse the current URL with an external browser.                    |
;; | -         | Begin a negative numeric argument for the next command.             |
;; | 0 .. 9    | Part of the numeric argument for the next command.                  |
;; | C         | Display a buffer listing the current URL cookies, if there are any. |
;; | H         | List the eww-histories.                                             |
;; | F         | Toggle font between variable-width and fixed-width.                 |
;; | G         | Go to a URL                                                         |
;; | R         | Readable mode                                                       |
;; | S         | List eww buffers                                                    |
;; | d         | Download URL under point to `eww-download-directory'.               |
;; | g         | Reload the current page.                                            |
;; | q         | Quit WINDOW and bury its buffer.                                    |
;; | v         | `eww-view-source'                                                   |
;; | w         | `eww-copy-page-url'                                                 |
;; |-----------+---------------------------------------------------------------------|
;; | b         | Add the current page to the bookmarks.                              |
;; | B         | Display the bookmark list.                                          |
;; | M-n       | Visit the next bookmark                                             |
;; | M-p       | Visit the previous bookmark                                         |
;; |-----------+---------------------------------------------------------------------|
;; | t         | Go to the page marked `top'.                                        |
;; | u         | Go to the page marked `up'.                                         |
;; |-----------+---------------------------------------------------------------------|
;; | n         | Go to the page marked `next'.                                       |
;; | p         | Go to the page marked `previous'.                                   |
;; |-----------+---------------------------------------------------------------------|
;; | l         | Go to the previously displayed page.                                |
;; | r         | Go to the next displayed page.                                      |
;; |-----------+---------------------------------------------------------------------|
;; | TAB       | Move point to next link on the page.                                |
;; | S-TAB     | Move point to previous link on the page.                            |
;; |-----------+---------------------------------------------------------------------|
;; | SPC       | Scroll up                                                           |
;; | DEL/Bkspc | Scroll down                                                         |
;; | S-SPC     | Scroll down                                                         |
;; |-----------+---------------------------------------------------------------------|
