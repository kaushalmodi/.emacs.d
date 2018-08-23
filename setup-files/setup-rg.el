;; Time-stamp: <2018-08-23 16:56:35 kmodi>

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
    (advice-add 'deadgrep--format-command :filter-return #'modi/deadgrep--format-command-advice)))


(provide 'setup-rg)
