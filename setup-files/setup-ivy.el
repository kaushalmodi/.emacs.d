;; Time-stamp: <2015-06-23 22:25:23 kmodi>

;; Ivy (comes packaged with the `swiper' package)

(use-package ivy
  :if (not (bound-and-true-p disable-pkg-ivy))
  :config
  (progn
    (when (not (bound-and-true-p disable-pkg-ivy))
      ;; Disable ido
      (with-eval-after-load 'ido
        (ido-mode -1))
      ;; Enable ivy
      (ivy-mode 1))

    (setq ivy-use-virtual-buffers t)

    (setq ivy-re-builders-alist '((t . ivy--regex-plus))) ; default
    ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

    ;; Revert the default bindings to C-j and C-m
    (bind-keys
     :map ivy-minibuffer-map
      ("C-m"   . ivy-alt-done) ; RET
      ("C-S-m" . ivy-immediate-done)
      ("C-j"   . ivy-done)
      ("C-t"   . ivy-toggle-fuzzy))
    (bind-keys
     :map modi-mode-map
      ;; Override the default binding for `upcase-word'
      ("M-u"     . ivy-resume)
      ;; Override the default binding for `delete-blank-lines'
      ("C-x C-o" . ivy-recentf))))


(provide 'setup-ivy)

;; Call `ivy-immediate-done' if you want to use whatever you typed in the
;; search field, and ignore the suggestions provided by ivy in the list.
;;
;;  C-u <`ivy-alt-done' binding> <-- `ivy-immediate-done'
;;
;; This is useful especially when renaming files (and the name you want to
;; rename to partially matches one of the existing files).
