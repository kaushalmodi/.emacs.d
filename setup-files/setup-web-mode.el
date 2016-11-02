;; Time-stamp: <2016-11-02 12:17:09 kmodi>

;; Web Mode
;; http://web-mode.org

(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.php\\'"
         "\\.as[cp]x\\'")
  :config
  (progn
    (setq web-mode-engines-alist '(("gtl" . ".*hugo.*html\\'"))) ; Go Template

    ;; https://github.com/fxbois/web-mode/issues/812
    ;; Override the default functions for commenting/uncommenting Go Templates
    (defun web-mode-comment-go-block (pos)
      (let (beg end)
        (setq beg (web-mode-block-beginning-position pos)
              end (web-mode-block-end-position pos))
        (web-mode-insert-text-at-pos "*/" (1- end))
        (web-mode-insert-text-at-pos "/*" (+ beg (if (web-mode-looking-at "{{" beg) 2 0)))))

    (defun web-mode-uncomment-go-block (pos)
      (let (beg end)
        (setq beg (web-mode-block-beginning-position pos)
              end (web-mode-block-end-position pos))
        (web-mode-remove-text-at-pos 2 (+ beg 2))
        (web-mode-remove-text-at-pos 2 (- end 5))))))


(provide 'setup-web-mode)
