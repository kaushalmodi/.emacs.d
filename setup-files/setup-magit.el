;; Time-stamp: <2022-08-19 14:20:16 kmodi>

;; magit
;; https://github.com/magit/magit

(use-package magit
  :bind (:map modi-mode-map
         ("C-c g". hydra-magit/body))
  :commands (magit-status magit-log-all-branches)
  :config
  (progn
    ;; Speed up Magit a bit
    ;; https://magit.vc/manual/magit/Performance.html
    (setq magit-refresh-status-buffer nil)

    ;; https://github.com/hlissner/doom-emacs/blob/1456108d5bef89195b2b63f962e0ae9d24f4c652/modules/tools/magit/config.el#L23
    (setq magit-diff-refine-hunk t) ;Show granular diffs in selected hunk.
    ;; Don't autosave repo buffers. This is too magical, and saving can
    ;; trigger a bunch of unwanted side-effects, like save hooks and
    ;; formatters. Trust the user to know what they're doing.
    (setq magit-save-repository-buffers nil)
    ;; Don't display parent/related refs in commit buffers; they are rarely
    ;; helpful and only add to runtime costs.
    (setq magit-revision-insert-related-refs nil)

    ;; ;; https://jakemccrary.com/blog/2020/11/14/speeding-up-magit/
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)

    ;; Mon Sep 27 13:39:10 EDT 2021 - kmodi
    ;; Later I realized that the major contributor to the slowness of magit-status was that
    ;; the submodules were being recursed in the `git status' and `git diff` commands (which are
    ;; called by `magit-insert-untracked-files' and `magit-insert-unstaged-changes' functions
    ;; respectively. Doing this once in the git repo with a lot of submodules fixed the magit-status
    ;; sluggishness:
    ;;
    ;;     git config diff.ignoreSubmodules all

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
      ("$" magit-process "process")))

  ;; https://scripter.co/view-github-pull-requests-in-magit/
  ;; https://endlessparentheses.com/automatically-configure-magit-to-access-github-prs.html
  (defun modi/add-PR-fetch-ref (&optional remote-name)
    "If refs/pull is not defined on a GH repo, define it.

If REMOTE-NAME is not specified, it defaults to the `remote' set
for the \"main\" or \"master\" branch."
    (let* ((remote-name (or remote-name
                            (magit-get "branch" "main" "remote")
                            (magit-get "branch" "master" "remote")))
           (remote-url (magit-get "remote" remote-name "url"))
           (fetch-refs (and (stringp remote-url)
                            (string-match "github" remote-url)
                            (magit-get-all "remote" remote-name "fetch")))
           ;; https://oremacs.com/2015/03/11/git-tricks/
           (fetch-address (format "+refs/pull/*/head:refs/pull/%s/*" remote-name)))
      (when fetch-refs
        (unless (member fetch-address fetch-refs)
          (magit-git-string "config"
                            "--add"
                            (format "remote.%s.fetch" remote-name)
                            fetch-address)))))
  (add-hook 'magit-mode-hook #'modi/add-PR-fetch-ref))

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
      ;; ARGS               -> '((REV AUTHOR DATE))
      ;; (car ARGS)         -> '(REV AUTHOR DATE)
      ;; (nth 1 (car ARGS)) -> AUTHOR
      (let* ((author (nth 1 (car args)))
             (author-abbr (if (string-match-p "," author)
                              ;; Last, First -> F Last
                              (replace-regexp-in-string "\\(.*?\\), *\\(.\\).*" "\\2 \\1" author)
                            ;; First Last -> F Last
                            (replace-regexp-in-string "\\(.\\).*?[. ]+\\(.*\\)" "\\1 \\2" author))))
        (setf (nth 1 (car args)) author-abbr))
      (car args))                       ;'(REV AUTHOR-ABBR DATE)
    (advice-add 'magit-log-format-margin :filter-args #'modi/magit-log--abbreviate-author)))

;; https://github.com/dandavison/magit-delta
;; Requires the user to download the `delta' executable and put it somewhere
;; in the PATH before starting emacs.
;;
;; Due to a current issue in delta, you cannot enable its `line-numbers' feature without
;; messing up the diffs in magit. A workaround is that you can enable `side-by-side'
;; mode. That shows line numbers in side-by-side mode in the terminal, but leaves
;; `magit-delta' unaffected!
;;
;; [delta]
;;     # https://github.com/dandavison/magit-delta/issues/13
;;     # line-numbers = true    # Don't do this.. messes up diffs in magit
;;     #
;;     side-by-side = true      # Display a side-by-side diff view instead of the traditional view
(when (executable-find "delta")
  (use-package magit-delta
    :ensure t
    :hook (magit-mode . magit-delta-mode)))


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
