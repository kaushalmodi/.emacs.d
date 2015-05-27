;; Time-stamp: <2015-05-27 14:29:06 kmodi>

;; Popwin
;; https://github.com/m2ym/popwin-el

;; popwin is a popup window manager for Emacs which makes you free from the hell
;; of annoying buffers such like *Help*, *Completions*, *compilation*, etc.
;; Windows of such temporary buffers will be shown as a popup window, and you
;; can close them smoothly by typing `C-g' in anytime.

(use-package popwin
  :config
  (progn
    ;; Special Display Config
    ;; M-!
    (push "*Shell Command Output*"
          popwin:special-display-config)
    ;; M-x compile
    (push '(compilation-mode
            :noselect t)
          popwin:special-display-config)
    ;; undo-tree
    (push '(" *undo-tree*"
            :width 0.3 :position right)
          popwin:special-display-config)

    (popwin-mode 1)))


(provide 'setup-popwin)

;; Special Display Config
;;
;; popwin:special-display-config is a list of CONFIG.
;;
;; CONFIG may be a form of (PATTERN . KEYWORDS), where PATTERN is a
;; pattern of specifying a buffer, and KEYWORDS is a list of a pair of
;; key and value. PATTERN is a buffer name, a symbol specifying
;; major-mode, or a predicate function which takes the buffer. If CONFIG
;; is a string or a symbol, PATTERN will be CONFIG and KEYWORDS will be
;; empty. Available keywords are following:
;;
;; |-----------------+--------------------------------------------------------------|
;; | Keyword         | Description                                                  |
;; |-----------------+--------------------------------------------------------------|
;; | :regexp         | If the value is non-nil, PATTERN will be used as regexp      |
;; |                 | to matching buffer.                                          |
;; | :width, :height | Specify width or height of the popup window. If no size      |
;; |                 | specified, `popwin:popup-window-width' or                    |
;; |                 | `popwin:popup-window-height' will be used.                   |
;; |                 | See also :position keyword.                                  |
;; | :position       | The value must be one of (left top right bottom). The        |
;; |                 | popup window will shown at the position of the frame. If     |
;; |                 | no position specified, `popwin:popup-window-position' will   |
;; |                 | be used.                                                     |
;; | :noselect       | If the value is non-nil, the popup window will not be        |
;; |                 | selected when it is shown.                                   |
;; | :dedicated      | If the value is non-nil, the popup window will be dedicated  |
;; |                 | to the original popup buffer. In this case, when another     |
;; |                 | buffer is selected in the popup window, the popup window     |
;; |                 | will be closed immedicately and the selected buffer will be  |
;; |                 | shown on the previously selected window.                     |
;; | :stick          | If the value is non-nil, the popup window will be stuck when |
;; |                 | it is shown.                                                 |
;; | :tail           | If the value is non-nil, the popup window will show the      |
;; |                 | last contents.                                               |
;; |-----------------+--------------------------------------------------------------|
