;; Time-stamp: <2018-06-08 13:13:44 kmodi>

;; magit
;; https://github.com/magit/magit

(use-package magit
  :bind (:map modi-mode-map
         ("C-c g". hydra-magit/body))
  :commands (magit-status magit-log-all-branches)
  :config
  (progn
    ;; Magit Submodule support
    ;; https://www.reddit.com/r/emacs/comments/6aiwk5/how_to_manage_multiple_gitrepositories_at_once/dhf47dg/
    (dolist (fn '(;; Below will end up being the last of these newly added fns,
                  ;; and the last element in `magit-status-sections-hook' too.
                  magit-insert-modules-unpulled-from-upstream
                  magit-insert-modules-unpushed-to-pushremote
                  magit-insert-modules-unpushed-to-upstream
                  ;; Below will end up being the first of these newly added fns.
                  magit-insert-modules-unpulled-from-pushremote))
      (magit-add-section-hook 'magit-status-sections-hook `,fn nil :append))

    (defhydra hydra-magit (:color blue
                           :columns 4)
      "Magit"
      ("g" magit-status "status")
      ("s" magit-status "status")
      ("l" magit-log-all-branches "log")
      ("b" magit-branch-popup "branch popup")
      ("r" magit-rebase-popup "rebase popup")
      ("f" magit-fetch-popup "fetch popup")
      ("P" magit-push-popup "push popup")
      ("F" magit-pull-popup "pull popup")
      ("W" magit-format-patch "format patch")
      ("$" magit-process "process"))))

(use-package magit-log
  :init
  (progn
    ;; Set `magit-log-margin' value in :init as many other variables will be
    ;; dynamically set based on its value when `magit-log' is loaded.
    ;; (setq magit-log-margin '(t age magit-log-margin-width t 18)) ;Default value
    ;; Show the commit ages with 1-char time units
    ;;   minute->m, hour->h, day->d, week->w, month->M, year->Y
    ;; Also reduce the author column width to 11 as the author name is being
    ;; abbreviated below.
    (setq magit-log-margin '(t age-abbreviated magit-log-margin-width :author 11)))
  :config
  (progn
    ;; Abbreviate author name. I added this so that I can view Magit log without
    ;; too much commit message truncation even on narrow screens (like on phone).
    (defun modi/magit-log--abbreviate-author (&rest args)
      "The first arg is AUTHOR, abbreviate it.
First Last  -> F Last
First.Last  -> F Last
Last, First -> F Last
First       -> First (no change).

It is assumed that the author has only one or two names."
      ;; ARGS             -> '((AUTHOR DATE))
      ;; (car ARGS)       -> '(AUTHOR DATE)
      ;; (car (car ARGS)) -> AUTHOR
      (let* ((author (car (car args)))
             (author-abbr (if (string-match-p "," author)
                              ;; Last, First -> F Last
                              (replace-regexp-in-string "\\(.*?\\), *\\(.\\).*" "\\2 \\1" author)
                            ;; First Last -> F Last
                            (replace-regexp-in-string "\\(.\\).*?[. ]+\\(.*\\)" "\\1 \\2" author))))
        (setf (car (car args)) author-abbr))
      (car args))                       ;'(AUTHOR-ABBR DATE)
    (advice-add 'magit-log-format-margin :filter-args #'modi/magit-log--abbreviate-author)))


(provide 'setup-magit)

;; |---------+----------------------------------|
;; | Binding | Description                      |
;; |---------+----------------------------------|
;; | j n     | Jump to Untracked section        |
;; | j u     | Jump to Unstaged section         |
;; | j s     | Jump to Staged section           |
;; | j p     | Jump to Unpushed section         |
;; | M-p     | Jump to previous sibling section |
;; | M-n     | Jump to next sibling section     |
;; |---------+----------------------------------|

;; Tip: Adding prefix to above jump commands also expands those sections and
;; brings that section to the top of the buffer.
;;   So `C-u j s' is analogous to doing `j s C-l C-l 4`
