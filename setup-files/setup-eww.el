;; Time-stamp: <2015-04-15 14:44:04 kmodi>

;; Eww - Emacs browser (needs emacs 24.4 or higher)

(use-package eww
  :commands (eww eww-search-words
                 modi/eww-copy-link-first-search-result
                 modi/eww-im-feeling-lucky)
  :init
  (progn
    (bind-keys
     :map modi-mode-map
      ("M-s M-w" . eww-search-words)
      ("M-s M-l" . modi/eww-copy-link-first-search-result))
    (key-chord-define-global "-=" #'eww))
  :config
  (progn
    (setq modi/eww-file-notify-descriptors-list (quote nil))
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

    ;; Override the default definition of `eww-search-words'
    (defun eww-search-words (&optional beg end)
      "Search the web for the text between the point and marker.
See the `eww-search-prefix' variable for the search engine used."
      (interactive "r")
      (if (use-region-p)
          (eww (buffer-substring beg end))
        (eww (modi/get-symbol-at-point))))

    (defun modi/eww-go-to-first-search-result (search-term)
      "Navigate to the first search result in the *eww* buffer.

This function is not for interactive use."
      (while (string-match "eww" (buffer-name))
        (bury-buffer))
      (eww search-term)
      ;; The while loop will keep on repeating every 0.1 seconds till the
      ;; result of `(re-search-forward " +1 +" nil :noerror)' is non-nil
      (catch 'break
        (while t
          (goto-char (point-min)) ; go to the top of the buffer
          (re-search-forward "[0-9]+\\s-+results" nil :noerror) ; go to the start of results
          (when (re-search-forward "\\s-+1\\s-+" nil :noerror) ; go to the first result
            (throw 'break nil))
          (sleep-for 0.1))) ; 0.1 second wait
      (forward-char 5)) ; locate the point safely on the first result link

    (defun modi/eww-copy-link-first-search-result (search-term)
      "Copy the link to the first search result."
      (interactive "sSearch term: ")
      (let ((eww-buffer-name))
        (modi/eww-go-to-first-search-result search-term)
        (setq eww-buffer-name (rename-buffer "*eww-temp*" t))
        ;; Copy the actual link instead of redirection link by calling
        ;; `shr-copy-url' twice
        (dotimes (i 2) (shr-copy-url))
        (kill-buffer eww-buffer-name))) ; kill the eww buffer

    (defun modi/eww-im-feeling-lucky (search-term)
      "Navigate to the first search result directly."
      (interactive "sSearch term (I'm Feeling Lucky!): ")
      (modi/eww-go-to-first-search-result search-term)
      (eww-follow-link))

    ;; Auto-refreshing *eww* buffer
    ;; http://emacs.stackexchange.com/a/2566/115
    (with-eval-after-load 'filenotify
      (defun modi/eww-open-file-with-notify (file)
        "Open a file in eww and add `file-notify' watch for it."
        (interactive "fFile: ")
        (eww-open-file file)
        (file-notify-add-watch file
                               '(change attribute-change)
                               #'modi/eww-notify-callback))

      (defun modi/eww-notify-callback (event)
        "On getting triggered, switch to the *eww* buffer,
reload and switch back to the working buffer. Also save
the `file-notify-descriptor' of the triggering event."
        (let* ((working-buffer (buffer-name)))
          (switch-to-buffer-other-window "*eww*")
          (eww-reload)
          (switch-to-buffer-other-window working-buffer))
        ;; `(car event)' will return the event descriptor
        (add-to-list 'modi/eww-file-notify-descriptors-list (car event)))

      (defun modi/eww-quit ()
        "When quitting `eww', also remove any saved file-notify descriptors
specific to eww, while updating `modi/eww-file-notify-descriptors-list'."
        (interactive)
        (quit-window :kill)
        (dotimes (index (safe-length modi/eww-file-notify-descriptors-list))
          (file-notify-rm-watch (pop modi/eww-file-notify-descriptors-list))))
      (key-chord-define eww-mode-map "XX" #'modi/eww-quit))
    ;;

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

    ;; eww-lnum
    (use-package eww-lnum
      :commands (eww-lnum-follow eww-lnum-universal)
      :init
      (bind-keys
       :map eww-mode-map
        ("f" . eww-lnum-follow)
        ("F" . eww-lnum-universal)))

    ;; org-eww
    ;; Copy text from html page for pasting in org mode file/buffer
    ;; e.g. Copied HTML hyperlinks get converted to [[link][desc]] for org mode.
    ;; http://emacs.stackexchange.com/a/8191/115
    (use-package org-eww
      :load-path "elisp/org-mode/contrib/lisp"
      :commands (org-eww-copy-for-org-mode)
      :init
      (bind-keys
       :map eww-mode-map
        ("o" . org-eww-copy-for-org-mode)))

    (bind-keys
     :map eww-mode-map
      ("G"           . eww) ; Go to URL
      ("g"           . eww-reload) ; Reload
      ("h"           . eww-list-histories) ; View history
      ("q"           . modi/eww-quit)
      ("r"           . eww-reload) ; Reload
      ("p"           . shr-previous-link)
      ("<backtab>"   . shr-previous-link) ; S-TAB Jump to previous link on the page
      ("n"           . shr-next-link)
      ("<tab>"       . shr-next-link)
      ("N"           . eww-next-url)
      ("P"           . eww-previous-url)
      ("<backspace>" . eww-back-url)
      ("C-d"         . eww-add-bookmark) ; Add bookmark
      ("w"           . modi/eww-copy-url-dwim)
      ("C-w"         . modi/eww-copy-url-dwim)
      ("\<"          . eww-back-url)
      ("\>"          . eww-forward-url)
      ("/"           . highlight-regexp))
    (>=e "25.0"
         (bind-key "R" #'eww-readable eww-mode-map)
         ;; Refresh the page with `eww-reload' or `g' to switch back to
         ;; the default view
         )
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
;; |----------+---------------------------------------------------------------------------------|
;; | Key      | Function                                                                        |
;; |----------+---------------------------------------------------------------------------------|
;; | TAB      | Skip to the next link.                                                          |
;; | SPC      | Scroll text of selected window upward ARG lines; or near full screen if no ARG. |
;; | &        | Browse the current URL with an external browser.                                |
;; | -        | Begin a negative numeric argument for the next command.                         |
;; | 0 .. 9   | Part of the numeric argument for the next command.                              |
;; | B        | Display the bookmarks.                                                          |
;; | C        | Display a buffer listing the current URL cookies, if there are any.             |
;; | H        | List the eww-histories.                                                         |
;; | b        | Add the current page to the bookmarks.                                          |
;; | d        | Download URL under point to `eww-download-directory'.                           |
;; | g        | Reload the current page.                                                        |
;; | l        | Go to the previously displayed page.                                            |
;; | n        | Go to the page marked `next'.                                                   |
;; | p        | Go to the page marked `previous'.                                               |
;; | q        | Quit WINDOW and bury its buffer.                                                |
;; | r        | Go to the next displayed page.                                                  |
;; | t        | Go to the page marked `top'.                                                    |
;; | u        | Go to the page marked `up'.                                                     |
;; | v        | `eww-view-source' (not documented)                                              |
;; | w        | `eww-copy-page-url' (not documented)                                            |
;; | DEL      | Scroll text of selected window down ARG lines; or near full screen if no ARG.   |
;; | S-SPC    | Scroll text of selected window down ARG lines; or near full screen if no ARG.   |
;; | <delete> | Scroll text of selected window down ARG lines; or near full screen if no ARG.   |
;; | C-M-i    | Skip to the previous link.                                                      |
;; |----------+---------------------------------------------------------------------------------|
