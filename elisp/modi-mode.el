;; Time-stamp: <2015-08-27 12:37:35 kmodi>

;; My minor mode
;; Main use is to have my key bindings have the highest priority

;; Sources:
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;; http://nullprogram.com/blog/2013/02/06/

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
  :init-value nil
  :lighter    " μ"
  :keymap     modi-mode-map)

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'modi-mode))
      (let ((mykeys (assq 'modi-mode minor-mode-map-alist)))
        (assq-delete-all 'modi-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

;;;###autoload
(defun turn-on-modi-mode ()
  "Turns on modi-mode."
  (interactive)
  (modi-mode 1))

;;;###autoload
(defun turn-off-modi-mode ()
  "Turns off modi-mode."
  (interactive)
  (modi-mode -1))

;;;###autoload
(define-globalized-minor-mode global-modi-mode modi-mode turn-on-modi-mode)

;; Turn off the minor mode in the minibuffer
(add-hook 'minibuffer-setup-hook #'turn-off-modi-mode)

;; ###autoload
;; (add-hook 'text-mode-hook 'modi-mode)

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
