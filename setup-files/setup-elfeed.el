;; Time-stamp: <2014-10-22 11:04:10 kmodi>

;; Elfeed
;; Source: https://github.com/skeeto/elfeed

(req-package elfeed
  :init
  (progn
    (setq elfeed-feeds
          '(;; emacs
            ("http://stackexchange.com/feeds/tagsets/152198/emacs?sort=active" emacs)
            ("http://nullprogram.com/feed/" emacs)
            ("http://planet.emacsen.org/atom.xml" emacs)
            ("http://emacsredux.com/atom.xml" emacs)
            ("http://www.masteringemacs.org/feed/" emacs)
            ("http://endlessparentheses.com/atom.xml" emacs)
            ("http://www.lunaryorn.com/feed.atom" emacs)
            ;; verilog
            ("http://stackexchange.com/feeds/tagsets/152227/verilog?sort=active" verilog)
            ))
    ;; Entries older than 4 weeks are marked as read
    (add-hook 'elfeed-new-entry-hook
              (elfeed-make-tagger :before "4 weeks ago"
                                  :remove 'unread))
    ;; Mark all as read
    (defun elfeed-mark-all-as-read ()
      (interactive)
      (mark-whole-buffer)
      (elfeed-search-untag-all-unread))

    (bind-keys
     :map elfeed-search-mode-map
     ("R" . elfeed-mark-all-as-read))

    (bind-to-modi-map ";" elfeed)))


(provide 'setup-elfeed)

;; | RET    | Display the currently selected item in a buffer.            |
;; | +      | Apply TAG to all selected entries.                          |
;; | -      | Remove TAG from all selected entries.                       |
;; | 0 .. 9 | Part of the numeric argument for the next command.          |
;; | G      | Update all the feeds in `elfeed-feeds'.                     |
;; | S      | Set a new search filter for the elfeed-search buffer.       |
;; | b      | Visit the current entry in your browser using `browse-url'. |
;; | g      | `elfeed-search-update--force' (not documented)              |
;; | n      | Move cursor vertically down ARG lines.                      |
;; | p      | Move cursor vertically up ARG lines.                        |
;; | q      | Quit WINDOW and bury its buffer.                            |
;; | r      | `elfeed-search-untag-all-unread' (not documented)           |
;; | s      | Filter the elfeed-search buffer as the filter is written.   |
;; | u      | `elfeed-search-tag-all-unread' (not documented)             |
;; | y      | Copy the selected feed item URL to clipboard                |

;; elfeed Tips and Tricks: http://nullprogram.com/blog/2013/11/26/
