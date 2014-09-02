;; Time-stamp: <2014-08-13 15:15:43 kmodi>

;; dired, dired-x, dired+, dired-single
;; Source: http://www.emacswiki.org/emacs-en/dired-single.el

(req-package dired
  :defer t
  :config
  (progn
    (req-package dired-single)
    (req-package dired+)
    (req-package dired-x
      :config
      (progn
        (setq dired-omit-verbose nil)
        ;; ;; hide backup, autosave, *.*~ files
        ;; ;; omit mode can be toggled using `M-o' in dired buffer
        ;; (setq-default dired-omit-files-p t)))
        (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))))

    (defun rename-dired-buffer-name ()
      "Rename the dired buffer name to distinguish it from file buffers.
It added extra strings at the front and back of the default dired buffer name."
      (let ((name (buffer-name)))
        (if (not (string-match "/$" name))
            (rename-buffer (concat "*Dired* " name "/") t))))

    (defun dired-single-magic-buffer-current-dir ()
      "Open a single magic dired buffer for the current buffer directory."
      (interactive)
      (dired-single-magic-buffer default-directory))

    (defun dired-single-up-directory ()
      (interactive)
      (dired-single-buffer ".."))

    (add-hook 'dired-mode-hook 'rename-dired-buffer-name)

    (bind-keys
     :map dired-mode-map
     ("<return>"  . dired-single-buffer)
     ("<mouse-1>" . dired-single-buffer-mouse)
     ("^"         . dired-single-up-directory))

    (bind-keys
     :map modi-mode-map
     ;; Change the default `C-x C-d` key binding from `ido-list-directory'
     ("C-x C-d" . dired-single-magic-buffer-current-dir)
     ;; Change the default `C-x C-j` key binding from `dired-jump'
     ;; Opens dired-single-magic-buffer but asks which directory to open that
     ;; dired buffer for.
     ("C-x C-j" . dired-single-magic-buffer))))


(provide 'setup-dired)

;; TIPS

;; (1) Jump to the dired of the current file
;; `C-x C-j` - Calls `dired-jump' function.
;; Jump to dired buffer corresponding to current buffer.
;; If in a file, dired the current directory and move to file's line.
;; If in Dired already, pop up a level and goto old directory's line.
;; In case the proper dired file line cannot be found, refresh the dired
;; buffer and try again.
