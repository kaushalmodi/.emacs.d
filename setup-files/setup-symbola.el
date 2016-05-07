;; Time-stamp: <2016-05-07 00:14:08 kmodi>

;; Symbola font check (required for emoji and other Unicode 6+ display)

(defvar font-symbola-p nil
  "If non-nil, Symbola font is available on the system.")

(defun modi/find-font-symbola (frame)
  "Set `font-symbola-p' to t if Symbola font is available."
  ;; The below `select-frame' form is required for the `find-font'
  ;; to work correctly when using emacs daemon (emacsclient).
  (select-frame frame)
  (when (find-font (font-spec :name "Symbola"))
    ;; Manually choose a fallback font for Unicode
    ;; http://endlessparentheses.com/manually-choose-a-fallback-font-for-unicode.html
    (set-fontset-font "fontset-default" nil (font-spec :size 20 :name "Symbola"))
    (setq font-symbola-p t)))
(add-hook 'after-make-frame-functions #'modi/find-font-symbola)


(provide 'setup-symbola)
