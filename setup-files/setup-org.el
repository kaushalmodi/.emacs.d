;; Time-stamp: <2013-12-02 17:08:12 kmodi>

;; Org Mode

(setq org-directory "~/org")

(setq org-log-done 'timestamp) ;; Insert only timestamp when closing an org TODO item
;; (setq org-log-done 'note) ;; Insert timestamp and note when closing an org TODO item
;; Source:http://orgmode.org/manual/Closing-items.html
;; (setq org-startup-indented t) ;; indented headers
(setq org-hide-leading-stars t) ;; hidden leading stars
(setq org-agenda-files (concat org-directory "/agenda.files"))
(setq org-blank-before-new-entry (quote ((heading) (plain-list-item)))) ;; prevent auto blank lines
(setq org-completion-use-ido t) ;; use ido for auto completion
(setq org-return-follows-link t) ;; Hitting <RET> while on a link follows the link
(setq org-startup-folded (quote showeverything))
;; TODO
(setq org-todo-keywords (quote ((sequence "TODO" "SOMEDAY" "DONE"))))
(setq org-enforce-todo-dependencies t) ;; block entries from changing state to DONE
                          ;; while they have children that are not DONE
                          ;; Source: http://orgmode.org/manual/TODO-dependencies.html
;; Capture
(setq org-capture-templates
      '(
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "\n* %?\n  Entered on %U")
        ("n" "Note" entry (file "~/org/notes.org")
         "\n* %?\n  Context:\n    %i\n  Entered on %U")
        ("u" "UVM/System Verilog Notes" entry (file "~/org/uvm.org")
         "\n* %?\n  Context:\n    %i\n  Entered on %U")
        ))


(setq setup-org-loaded t)
(provide 'setup-org)
