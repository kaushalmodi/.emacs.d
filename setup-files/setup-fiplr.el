;; Time-stamp: <2014-08-12 17:51:24 kmodi>

;; Source: https://github.com/d11wtq/fiplr
;; Fiplr (pronounced FIP-ler, as in Find in Project) is an Emacs package to
;; allow you to locate and open files deep within a complex directory tree,
;; using fuzzy matching.
;; A key design goal is to make Fiplr really easy to use with little-to-no
;; configuration, beyond a single key binding.
;; It is heavily inspired by Vim's ctrlp, TextMate's Command-T, and
;; Sublime's Control+P.
;; Internally it uses Grizzl to do the fuzzy searching.

(req-package fiplr
  :config
  (progn
    (setq fiplr-root-markers  '(".git" ".svn" ".hg" ".bzr"
                                ".SOS"))
    (setq fiplr-ignored-globs '((directories (".git" ".svn" ".hg" ".bzr"
                                              ".SOS" "nobackup" "nobkp"
                                              "INCA_libs.r"))
                                (files (".#*" "*.*#" "*~" "*.so"
                                        "*.jpg" "*.png" "*.gif"
                                        "*.pdf" "*.gz" "*.zip"
                                        "*.elc" "*.pyc"
                                        "*.txt" "*.log" "*.out"
                                        "GTAGS" "GRTAGS" "GPATH" "TAGS"))))
    (bind-keys
     :map modi-mode-map
     ("s-f s-f" . fiplr-find-file) ;; Win-f Win-f
     ("s-f f"   . fiplr-find-file-other-window) ;; Win-f f
     ("s-f s-d" . fiplr-find-directory)))) ;; Win-f Win-d


(provide 'setup-fiplr)

;; M-x fiplr-clear-cache <- Reset fiplr cache
