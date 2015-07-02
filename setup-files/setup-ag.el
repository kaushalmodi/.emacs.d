;; Time-stamp: <2015-06-25 14:24:47 kmodi>

;; Ag
;; https://github.com/Wilfred/ag.el

(use-package ag
  :init
  (progn
    (bind-to-modi-map "a" #'ag-project-regexp)
    (bind-to-modi-map "g" #'ag-project-regexp))
  :config
  (progn
    ;; wgrep-ag : To allow editing in *ag* buffer
    ;; https://github.com/mhayashi1120/Emacs-wgrep
    (use-package wgrep-ag
      :commands (wgrep-ag-setup)
      :config
      (progn
        (add-hook 'ag-mode-hook #'wgrep-ag-setup)
        (bind-keys
         :map wgrep-mode-map
          ("C-x s"   . wgrep-save-all-buffers)
          ("C-c C-c" . wgrep-finish-edit) ;; Apply changes to file buffers
          ("C-c C-e" . wgrep-finish-edit)
          ("C-x C-s" . wgrep-finish-edit)
          ("C-c C-d" . wgrep-mark-deletion)
          ("C-c C-p" . wgrep-toggle-readonly-area)
          ("C-c C-r" . wgrep-remove-change)
          ("C-c C-u" . wgrep-remove-all-change)
          ("C-c C-[" . wgrep-remove-all-change)
          ("C-c C-k" . wgrep-abort-changes)
          ("C-x C-q" . wgrep-exit))))

    ;; Set default ag arguments
    (setq ag-arguments '(
                         ;; Mandatory arguments for ag.el
                         ;; As per https://github.com/Wilfred/ag.el/issues/41
                         "--nogroup"
                         "--column"
                         ;; Other args
                         "--skip-vcs-ignores"
                         "--line-numbers"
                         "--smart-case"
                         "--follow" ; follow symlinks
                         "--stats"
                         ;; It looks like the ~/.agignore is used when
                         ;; launching ag from emacs too. So the ignores from
                         ;; ~/.agignore don't have to be set here again.
                         ))
    (setq ag-highlight-search t)
    ;; By default, ag.el will open results in a different window in the frame, so
    ;; the results buffer is still visible. You can override this so the results
    ;; buffer is hidden and the selected result is shown in its place:
    (setq ag-reuse-window nil)
    ;; reuse the same *ag* buffer for all your searches
    (setq ag-reuse-buffers t)
    ;; ;; To save buffer automatically when `wgrep-finish-edit'
    ;; (setq wgrep-auto-save-buffer t)

    (with-eval-after-load 'projectile
      ;; Override the default function to use the projectile function instead
      (defun ag/project-root (file-path)
        (let ((proj-name (projectile-project-root)))
          (if proj-name
              proj-name ; return `projectile-project-root' if non-nil
            ;; Else condition is same as the `ag/project-root' definition
            ;; from ag.el
            (if ag-project-root-function
                (funcall ag-project-root-function file-path)
              (or (ag/longest-string
                   (vc-git-root file-path)
                   (vc-svn-root file-path)
                   (vc-hg-root file-path))
                  file-path))))))

    ;; Redefine the ag-regexp function where the default search pattern is
    ;; word at point
    (defun ag-regexp (string directory)
      "Search using ag in a given DIRECTORY for a given search REGEXP,
with REGEXP defaulting to the symbol under point.
Search using ag in a given directory for a given regexp.

If called with a prefix, prompts for flags to pass to ag."
      (interactive (list (read-from-minibuffer "Search regexp: " (ag/dwim-at-point))
                         (read-directory-name "Directory: ")))
      (ag/search string directory :regexp t))

    (defun ag-regexp-cwd (string)
      "Search using ag in the CURRENT DIRECTORY for a given search REGEXP,
with REGEXP defaulting to the symbol under point.

If called with a prefix, prompts for flags to pass to ag."
      (interactive
       (list (read-from-minibuffer "Search regexp in current dir: "
                                   (ag/dwim-at-point))))
      (ag/search string (file-name-directory (buffer-file-name)) :regexp t))

    (bind-keys
     :map ag-mode-map
      ("i" . wgrep-change-to-wgrep-mode)
      ("/" . isearch-forward)
      ("n" . next-error-no-select)
      ("p" . previous-error-no-select)
      ("q" . ag-kill-buffers))))


(provide 'setup-ag)

;; NOTES
;; Simply put an empty folder called `.git' at a location that you want to consider
;; as a "project root"; even if that is not revision controlled in git.
;; After that, if you are in any file under that path or under any of that path's
;; sub-directories, `ag-project' or `ag-project-regexp' will work right away!

;; Default key binding when in wgrep mode
