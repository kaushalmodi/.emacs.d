;; Time-stamp: <2015-01-27 15:26:27 kmodi>

;; Eww
;; Emacs browser (needs emacs 24.4 or higher)

(>=e "24.4"
     (req-package eww
       :require (eww-lnum key-chord filenotify)
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

         (defun modi/eww-open-file-with-notify (file)
           "Open a file in eww and add `file-notify' watch for it."
           (interactive "fFile: ")
           (eww-open-file file)
           (file-notify-add-watch file
                                  '(change attribute-change)
                                  'modi/eww-notify-callback))

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

         (bind-keys
          :map eww-mode-map
          ("g"           . eww) ; Go to URL
          ("h"           . eww-list-histories) ; View history
          ("q"           . modi/eww-quit)
          ("r"           . eww-reload) ; Reload
          ("R"           . eww-reload) ; Reload
          ("<backtab>"   . shr-previous-link) ; S-TAB Jump to previous link on the page
          ("<backspace>" . eww-back-url)
          ("C-d"         . eww-add-bookmark) ; Add bookmark
          ("C-w"         . eww-copy-page-url)
          ("\<"          . eww-back-url)
          ("\>"          . eww-forward-url)
          ("/"           . highlight-regexp))
         (bind-keys
          :map eww-text-map ; For single line text fields
          ("<backtab>"  . shr-previous-link) ; S-TAB Jump to previous link on the page
          ("<C-return>" . eww-submit) ; S-TAB Jump to previous link on the page
          )
         (bind-keys
          :map eww-textarea-map ; For multi-line text boxes
          ("<backtab>"  . shr-previous-link) ; S-TAB Jump to previous link on the page
          ("<C-return>" . eww-submit) ; S-TAB Jump to previous link on the page
          )
         (bind-keys
          :map eww-checkbox-map
          ("<down-mouse-1>" . eww-toggle-checkbox)
          )
         (key-chord-define-global       "-=" 'eww)
         (key-chord-define eww-mode-map "XX" 'modi/eww-quit)

         ;; eww-lnum
         (eval-after-load "eww"
           '(progn
              (bind-keys
               :map eww-mode-map
               ("f" . eww-lnum-follow)
               ("F" . eww-lnum-universal)))))))


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
