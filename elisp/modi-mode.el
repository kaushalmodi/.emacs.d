;; Time-stamp: <2015-09-17 11:50:47 kmodi>

;; My minor mode
;; Main use is to have my key bindings have the highest priority

(defvar modi-special-keymap-prefix (kbd "C-x m")
  "`modi-mode' keymap prefix.
Overrides the default binding for `compose-mail'.")

(defvar modi-mode-special-map (make-sparse-keymap)
  "Special keymap for `modi-mode' whose bindings begin with
`modi-special-keymap-prefix'.")
(fset 'modi-mode-special-map modi-mode-special-map)

(defvar modi-mode-map (let ((map (make-sparse-keymap)))
                        (define-key map modi-special-keymap-prefix 'modi-mode-special-map)
                        map)
  "Keymap for `modi-mode'.")

;;;###autoload
(define-minor-mode modi-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-modi-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter    " μ"
  :keymap     modi-mode-map)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((modi-mode . ,modi-mode-map)))

;;;###autoload
(defun turn-on-modi-mode ()
  "Turn on modi-mode."
  (interactive)
  (modi-mode 1))

(defun turn-off-modi-mode ()
  "Turn off modi-mode."
  (interactive)
  (modi-mode -1))

;;;###autoload
(define-globalized-minor-mode global-modi-mode modi-mode turn-on-modi-mode)

;; Turn off the minor mode in the minibuffer
(add-hook 'minibuffer-setup-hook #'turn-off-modi-mode)

(defmacro bind-to-modi-map (key fn)
  "Bind a function to the `modi-mode-special-map'.
USAGE: (bind-to-modi-map \"f\" #'full-screen-center)."
  `(define-key modi-mode-special-map (kbd ,key) ,fn))

;; http://emacs.stackexchange.com/a/12906/115
(defun unbind-from-modi-map (key)
  "Unbind a function from the `modi-mode-map'
USAGE: (unbind-from-modi-map \"C-x m f\")
"
  (interactive "kUnset key from modi-mode-map: ")
  (define-key modi-mode-map (kbd (key-description key)) nil)
  (message (concat "Unbound "
                   (propertize (key-description key)
                               'face 'font-lock-function-name-face)
                   " key from the "
                   (propertize "modi-mode-map"
                               'face 'font-lock-function-name-face)
                   ".")))


(provide 'modi-mode)

;; Minor mode tutorial: http://nullprogram.com/blog/2013/02/06/
