;; Time-stamp: <2018-08-23 17:27:03 kmodi>

;; Deadgrep
;; https://github.com/Wilfred/deadgrep

(use-package deadgrep
  :ensure t
  :defer t
  :init
  (progn
    (bind-to-modi-map "r" #'deadgrep))
  :config
  (progn
    (setq-default deadgrep--search-type 'regexp) ;Default is 'string

    (defconst modi/deadgrep--rg-extra-args
      `("--no-ignore-vcs"       ;Ignore files/dirs ONLY from `.ignore'
        "--follow"              ;Follow symlinks
        "--ignore-file" ,(expand-file-name ".ignore" (getenv "HOME")))
      "Extra rg arguments to be added to `deadgrep--format-command' output.")

    ;; Thu Aug 23 15:51:37 EDT 2018 - kmodi
    ;; Adding extra arguments to the ones
    ;; already in `deadgrep--format-command' --
    ;; https://github.com/Wilfred/deadgrep/issues/24
    (defun modi/deadgrep--format-command-advice (orig-ret-val)
      "Add arguments from `modi/deadgrep--rg-extra-args' to ORIG-RET-VAL."
      (let ((new-ret (replace-regexp-in-string
                      (format "\\`\\(%s \\)\\(.*\\)\\'" (regexp-quote deadgrep-executable))
                      (concat "\\1"
                              (mapconcat #'identity modi/deadgrep--rg-extra-args " ")
                              " \\2")
                      orig-ret-val)))
        (message "[Deadgrep] %s" new-ret)
        new-ret))
    (advice-add 'deadgrep--format-command :filter-return #'modi/deadgrep--format-command-advice)

    (defun modi/deadgrep--jump-to (re)
      "Move point to button in Deadgrep buffer that matches RE.

Hit RET after running this function to change option and rerun rg
using the updated option."
      (goto-char (point-min))
      (re-search-forward re)
      (backward-char 3))

    (defun modi/deadgrep-jump-to-change-search-term ()
      "Move point to the 'change' button for search term."
      (interactive)
      (modi/deadgrep--jump-to "^Search term: .*change$"))

    (defun modi/deadgrep-jump-to-search-type-string ()
      "Move point to the 'string' search type button."
      (interactive)
      (modi/deadgrep--jump-to "^Search type: .*string"))

    (defun modi/deadgrep-jump-to-search-type-words ()
      "Move point to the 'words' search type button."
      (interactive)
      (modi/deadgrep--jump-to "^Search type: .*words"))

    (defun modi/deadgrep-jump-to-search-type-regexp ()
      "Move point to the 'regexp' search type button."
      (interactive)
      (modi/deadgrep--jump-to "^Search type: .*regexp"))

    (defun modi/deadgrep-jump-to-case-smart ()
      "Move point to the 'smart' case button."
      (interactive)
      (modi/deadgrep--jump-to "^Case: .*smart"))

    (defun modi/deadgrep-jump-to-case-sensitive ()
      "Move point to the 'sensitive' case button."
      (interactive)
      (modi/deadgrep--jump-to "^Case: .*sensitive"))

    (defun modi/deadgrep-jump-to-case-ignore ()
      "Move point to the 'ignore' case button."
      (interactive)
      (modi/deadgrep--jump-to "^Case: .*ignore"))

    (defun modi/deadgrep-jump-to-context-none ()
      "Move point to the 'none' context button."
      (interactive)
      (modi/deadgrep--jump-to "^Context: .*none"))

    (defun modi/deadgrep-jump-to-context-before ()
      "Move point to the 'before' context button."
      (interactive)
      (modi/deadgrep--jump-to "^Context: .*before"))

    (defun modi/deadgrep-jump-to-context-after ()
      "Move point to the 'after' context button."
      (interactive)
      (modi/deadgrep--jump-to "^Context: .*after"))

    (defun modi/deadgrep-jump-to-change-directory ()
      "Move point to change the directory."
      (interactive)
      (modi/deadgrep--jump-to "^Directory: .*$"))

    (defun modi/deadgrep-jump-to-files-all ()
      "Move point to change file search to 'all'."
      (interactive)
      (modi/deadgrep--jump-to "^Files: .*all"))

    (defun modi/deadgrep-jump-to-files-type ()
      "Move point to change file search to 'type'."
      (interactive)
      (modi/deadgrep--jump-to "^Files: .*type"))

    (defun modi/deadgrep-jump-to-files-glob ()
      "Move point to change file search to 'glob'."
      (interactive)
      (modi/deadgrep--jump-to "^Files: .*glob"))

    (bind-keys
     :map deadgrep-mode-map
     ;; "s RET" will bring up a prompt to change the search term.
     ("s" . modi/deadgrep-jump-to-change-search-term)
     ("ts" . modi/deadgrep-jump-to-search-type-string)
     ("tw" . modi/deadgrep-jump-to-search-type-words)
     ("tr" . modi/deadgrep-jump-to-search-type-regexp)
     ("cs" . modi/deadgrep-jump-to-case-smart)
     ("cc" . modi/deadgrep-jump-to-case-sensitive)
     ("ci" . modi/deadgrep-jump-to-case-ignore)
     ("xn" . modi/deadgrep-jump-to-context-none)
     ("xb" . modi/deadgrep-jump-to-context-before)
     ("xa" . modi/deadgrep-jump-to-context-after)
     ("d" . modi/deadgrep-jump-to-change-directory)
     ("fa" . modi/deadgrep-jump-to-files-all)
     ("ft" . modi/deadgrep-jump-to-files-type)
     ("fg" . modi/deadgrep-jump-to-files-glob))))


(provide 'setup-rg)
