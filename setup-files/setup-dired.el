;; Time-stamp: <2013-12-11 11:20:26 kmodi>

;; dired, dired-x, dired+, dired-single

(require 'dired-x)
(setq-default dired-omit-files-p t) ;; hide backup, autosave, *.*~ files
;; omit mode can be toggled using `M-o' in dired buffer

(require 'dired+)

;; Rename the dired buffer name to distinguish it from file
;; buffers; it added extra strings at the front and back of the default
;; dired buffer name (directory name)
(add-hook 'dired-mode-hook 'rename-dired-buffer-name)
(defun rename-dired-buffer-name ()
  "change buffer name to end with slash"
  (let ((name (buffer-name)))
    (if (not (string-match "/$" name))
        (rename-buffer (concat "*Dired* " name "/") t))))

;; Source: http://www.emacswiki.org/emacs-en/dired-single.el
(require 'dired-single)

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
        loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer "..")))))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))


(setq setup-dired-loaded t)
(provide 'setup-dired)

;; TIPS

;; (1) Jump to the dired of the current file
;; `C-x C-j` - Calls `dired-jump' function.
;; Jump to dired buffer corresponding to current buffer.
;; If in a file, dired the current directory and move to file's line.
;; If in Dired already, pop up a level and goto old directory's line.
;; In case the proper dired file line cannot be found, refresh the dired
;; buffer and try again.
