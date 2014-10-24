;; Time-stamp: <2014-10-23 14:13:44 kmodi>

;; Collection of general purposes defuns and macros

;; Save typing the lambda mumbo-jumbo
;; Source: https://github.com/waymondo/hemacs/blob/master/defuns.el
(defmacro λ (&rest body)
  (declare (indent 1))
  `(lambda ()
     (interactive)
     ,@body))
(key-chord-define-global "^^" (λ (insert "λ")))

(defmacro ==e243 (&rest body)
  `(when (and (version<= "24.3.0" emacs-version )
              (version<= emacs-version "24.3.99"))
     ,@body))

(defmacro >=e244 (&rest body)
  "The BODY can contain both 'if' (emacs version at least 24.4) and
'else' (emacs version older than 24.4) blocks."
  `(if (version<= "24.4" emacs-version)
       ,@body))

;; Alias ^ as a function to calculate exponents
;; (^ 2 15) `C-x C-e' -> 32768
(defalias '^ 'expt)

;; Source https://github.com/Wilfred/ag.el
(defun modi/get-symbol-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

;; Below is not required any more as per
;; http://emacs.stackexchange.com/questions/2112/why-does-load-theme-reset-the-custom-theme-load-path
;; (defun update-custom-theme-load-path ()
;;   "Ensure that the custom-theme-load-path has all the theme paths added.
;; Source: http://stackoverflow.com/a/15381087/1219634"
;;   (interactive)
;;   (require 'dash)
;;   (require 's)
;;   (-each
;;       (-map
;;        (lambda (item)
;;          (format (concat elpa-dir "/%s") item))
;;        (-filter
;;         (lambda (item)
;;           (or (s-contains? "theme" item)
;;               (s-contains? "smart-mode-line" item)))
;;         (directory-files elpa-dir)))
;;     (lambda (item)
;;       (add-to-list 'custom-theme-load-path item))))


(provide 'defuns)
