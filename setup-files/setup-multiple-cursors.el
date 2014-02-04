;; Time-stamp: <2013-12-13 16:37:41 kmodi>

;; Multiple Cursors
;; Source: https://github.com/magnars/multiple-cursors.el

(require 'multiple-cursors)

;; Key bindings for multiple cursors are saved in setup-key-bindings.el
;; Search for `setup-multiple-cursors-loaded' there.

(require 'expand-region)
(defun mc/edit-all-like-this ()
  "Select the current word and create multiple cursors at all the occurences
of that work in the current buffer."
  (interactive)
  (er/expand-region 1) ;; select the word that the cursor is on
  (mc/mark-all-like-this))


(setq setup-multiple-cursors-loaded t)
(provide 'setup-multiple-cursors)
