;; Time-stamp: <2015-05-27 23:35:31 kmodi>

;; Symbola font check (required for emoji and other Unicode 6+ display)

(defvar font-symbola-p nil
  "If non-nil, Symbola font is available on the system.")

(when (find-font (font-spec :name "Symbola"))
  ;; Manually choose a fallback font for Unicode
  ;; http://endlessparentheses.com/manually-choose-a-fallback-font-for-unicode.html
  (set-fontset-font "fontset-default" nil (font-spec :size 20 :name "Symbola"))
  (setq font-symbola-p t))


(provide 'setup-symbola)
