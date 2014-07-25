;; Time-stamp: <2014-07-23 13:17:05 kmodi>

;; Source: https://github.com/d11wtq/fiplr
;; Fiplr (pronounced FIP-ler, as in Find in Project) is an Emacs package to
;; allow you to locate and open files deep within a complex directory tree,
;; using fuzzy matching.
;; A key design goal is to make Fiplr really easy to use with little-to-no
;; configuration, beyond a single key binding.
;; It is heavily inspired by Vim's ctrlp, TextMate's Command-T, and
;; Sublime's Control+P.
;; Internally it uses Grizzl to do the fuzzy searching.

(require 'fiplr)

(setq fiplr-root-markers  '(".git" ".svn" ".hg" ".bzr"
                            ".SOS"))
(setq fiplr-ignored-globs '((directories (".git" ".svn" ".hg" ".bzr"
                                          ".SOS" "nobackup" "nobkp" "INCA_libs.r"))
                            (files (".#*" "*.*#" "*~" "*.so" "*.jpg" "*.png" "*.gif"
                                    "*.pdf" "*.gz" "*.zip"
                                    "*.elc" "*.pyc"
                                    "*.txt" "*.log" "*.out"
                                    "GTAGS" "GRTAGS" "GPATH" "TAGS"))))


(setq setup-fiplr-loaded t)
(provide 'setup-fiplr)

;; M-x fiplr-clear-cache <- Reset fiplr cache
