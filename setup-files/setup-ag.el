;; Time-stamp: <2016-10-10 13:19:30 kmodi>

;; Ag
;; https://github.com/Wilfred/ag.el

(use-package ag
  :commands (modi/ag-regexp-cwd
             modi/verilog-find-parent-module)
  :init
  (progn
    (bind-to-modi-map "a" #'ag-project-regexp))
  :config
  (progn
    (defun ag/jump-to-result-if-only-one-match ()
      "Jump to the first ag result if that ag search came up with just one match."
      (let (only-one-match)
        (when (member "--stats" ag-arguments)
          (save-excursion
            (goto-char (point-min))
            (setq only-one-match (re-search-forward "^1 matches\\s-*$" nil :noerror)))
          (when only-one-match
            (next-error)
            (kill-buffer (current-buffer))
            (message (concat "ag: Jumping to the only found match and "
                             "killing the *ag* buffer."))))))
    (add-hook 'ag-search-finished-hook #'ag/jump-to-result-if-only-one-match)

    ;; wgrep-ag
    ;; Allow editing in *ag* buffers
    ;; https://github.com/mhayashi1120/Emacs-wgrep
    (use-package wgrep-ag
      :defer t
      :config
      (progn
        (add-hook 'ag-mode-hook #'wgrep-ag-setup)
        (bind-keys
         :map wgrep-mode-map
         ("C-x s" . wgrep-save-all-buffers))))

    ;; Set default ag arguments
    ;; It looks like the ~/.ignore is used when launching ag from emacs too.
    ;; So the ignores from ~/.ignore don't have to be set here again.
    (setq ag-arguments
          (append modi/ag-arguments
                  '("--stats")))

    (setq ag-highlight-search t)

    (setq ag-reuse-buffers nil) ; Open new buffers for new searches

    ;; Workaround for issue where the edits in `wgrep' mode always resulted in
    ;; (No changes to be performed)
    ;; https://github.com/Wilfred/ag.el/issues/119
    (setq ag-group-matches nil)

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

    (defun modi/ag-regexp-cwd (string)
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
     ("Q" . ag-kill-buffers))))


(provide 'setup-ag)

;; Simply put an empty folder called `.git' at a location that you want to consider
;; as a "project root"; even if that is not revision controlled in git.
;; After that, if you are in any file under that path or under any of that path's
;; sub-directories, `ag-project' or `ag-project-regexp' will work right away!

;; Default key bindings in `wgrep-mode-map'
;; |---------+---------------------------------------------------|
;; | Binding | Command                                           |
;; |---------+---------------------------------------------------|
;; | C-c C-c | wgrep-finish-edit (Apply changes to file buffers) |
;; | C-c C-e | wgrep-finish-edit                                 |
;; | C-x C-s | wgrep-finish-edit                                 |
;; | C-c C-d | wgrep-mark-deletion                               |
;; | C-c C-p | wgrep-toggle-readonly-area                        |
;; | C-c C-r | wgrep-remove-change                               |
;; | C-c C-u | wgrep-remove-all-change                           |
;; | C-c C-[ | wgrep-remove-all-change                           |
;; | C-c C-k | wgrep-abort-changes                               |
;; | C-x C-q | wgrep-exit                                        |
;; |---------+---------------------------------------------------|
