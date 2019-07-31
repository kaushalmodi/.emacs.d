;; Time-stamp: <2019-03-22 16:42:04 kmodi>

;; Dired

;; https://www.emacswiki.org/emacs/dired-single.el
(use-package dired-single
  :bind (:map modi-mode-map
         ;; Change the default `C-x C-d` key binding from `ido-list-directory'
         ("C-x C-d" . dired-single-magic-buffer-current-dir)
         ;; Change the default `C-x C-j` key binding from `dired-jump'
         ;; Opens dired-single-magic-buffer but asks which directory to open that
         ;; dired buffer for.
         ("C-x C-j" . dired-single-magic-buffer))
  :config
  (progn
    (defun dired-single-magic-buffer-current-dir ()
      "Open a single magic dired buffer for the current buffer directory."
      (interactive)
      (dired-single-magic-buffer default-directory))

    (defun dired-single-up-directory ()
      (interactive)
      (dired-single-buffer ".."))

    (with-eval-after-load 'dired
      (bind-keys
       :map dired-mode-map
       ("<return>"         . dired-single-buffer)
       ("<double-mouse-1>" . dired-single-buffer-mouse)
       ("^"                . dired-single-up-directory)))))

(use-package dired
  :commands (dired-toggle-read-only ; to toggle read-only state of any buffer
             dired-get-filename) ; called by `dired-single'
  :config
  (progn
    (setq dired-recursive-deletes 'always)
    (setq dired-recursive-copies  'always)
    ;; Set this variable to non-nil, Dired will try to guess a default
    ;; target directory. This means: if there is a dired buffer
    ;; displayed in the next window, use its current subdir, instead
    ;; of the current subdir of this dired buffer. The target is used
    ;; in the prompt for file copy, rename etc.
    (setq dired-dwim-target t)

    ;; Dired listing switches
    ;;  -a : Do not ignore entries starting with .
    ;;  -l : Use long listing format.
    ;;  -G : Do not print group names like 'users'
    ;;  -h : Human-readable sizes like 1K, 234M, ..
    ;;  -v : Do natural sort .. so the file names starting with . will show up first.
    ;;  -F : Classify filenames by appending '*' to executables,
    ;;       '/' to directories, etc.
    (setq dired-listing-switches "-alGhvF --group-directories-first") ; default: "-al"

    (defun modi/dired-rename-buffer-name ()
      "Rename the dired buffer name to distinguish it from file buffers.
It added extra strings at the front and back of the default dired buffer name."
      (let ((name (buffer-name)))
        (if (not (string-match "/$" name))
            (rename-buffer (concat "*Dired* " name "/") t))))

    (defun modi/dired-truncate-lines ()
      (toggle-truncate-lines 1))

    (add-hook 'dired-mode-hook #'modi/dired-rename-buffer-name)
    (add-hook 'dired-mode-hook #'modi/dired-truncate-lines)

    (use-package dired-x
      :config
      (progn
        (setq dired-omit-verbose nil)
        ;; hide backup, autosave, *.*~ files
        ;; omit mode can be toggled using `M-o' in dired buffer
        (add-hook 'dired-mode-hook #'dired-omit-mode)))

    ;; http://www.emacswiki.org/DiredPlus
    (use-package dired+
      :load-path "elisp/manually-synced/dired-plus"
      :init
      (progn
        ;; Details toggling is bound to "(" in `dired-mode' by default
        (setq diredp-hide-details-initially-flag nil))
      :config
      (progn
        ;; Privilege indicator faces
        (defun modi/dired-update-privilege-faces ()
          (set-face-attribute 'diredp-dir-priv nil
                              :foreground "#7474FFFFFFFF"
                              :background (face-background 'default))
          (set-face-attribute 'diredp-exec-priv nil
                              :foreground "dodger blue"
                              :background (face-background 'default))
          (set-face-attribute 'diredp-other-priv nil
                              :background (face-background 'default))
          (set-face-attribute 'diredp-write-priv nil
                              :foreground "#25258F8F2929"
                              :background (face-background 'default))
          (set-face-attribute 'diredp-read-priv nil
                              :foreground "#999932325555"
                              :background (face-background 'default))
          (set-face-attribute 'diredp-no-priv nil
                              :foreground "#2C2C2C2C2C2C"
                              :background (face-background 'default))
          (set-face-attribute 'diredp-rare-priv nil
                              :foreground "Green"
                              :background (face-background 'default))
          (set-face-attribute 'diredp-link-priv nil
                              :foreground "#00007373FFFF"))
        (add-hook 'dired-mode-hook #'modi/dired-update-privilege-faces)))

    ;; https://fuco1.github.io/2017-07-15-Collapse-unique-nested-paths-in-dired-with-dired-collapse-mode.html
    ;; https://github.com/Fuco1/dired-hacks/blob/master/dired-collapse.el
    (use-package dired-collapse
      :config
      (progn
        (add-hook 'dired-mode-hook #'dired-collapse-mode)))))


(provide 'setup-dired)

;; TIPS

;; (1) Jump to the dired of the current file
;;     C-x C-j - Calls `dired-jump' function.
;;     Jump to dired buffer corresponding to current buffer.
;;     If in a file, dired the current directory and move to file's line.
;;     If in Dired already, pop up a level and goto old directory's line.
;;     In case the proper dired file line cannot be found, refresh the dired
;;     buffer and try again.

;; https://peterreavy.wordpress.com/2011/05/04/emacs-dired-tips/
;; (2) To copy the name of the file at point, in order to make use of
;;     it elsewhere, use `dired-copy-filename-as-kill', which is bound to
;;     `w'. To make it copy the absolute path: `0 w'

;; (3) To copy the path to the folder you’re looking at in dired: `M-< w'

;; (4) Enable wdired mode in dired to edit the file names by hitting C-x C-q
;;     which is bound to `dired-toggle-read-only' by default. That's a wrapper
;;     function which calls `wdired-change-to-wdired-mode' in `dired-mode'.

;; http://truongtx.me/2013/04/24/dired-as-default-file-manager-1-introduction
