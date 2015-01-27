;; Time-stamp: <2015-01-27 14:09:57 kmodi>

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

(defmacro >=e250 (&rest body)
  "The BODY can contain both 'if' (emacs version at least 25.0) and
'else' (emacs version older than 25.0) blocks."
  `(if (version<= "25.0" emacs-version)
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

(defmacro remove-from-alist-matching-car (alist car-of-list-to-delete)
  "For ALIST, remove a list from it whose `car' matches CAR-OF-LIST-TO-DELETE."
  `(let* ((to-delete nil))
     (dolist (item ,alist)
       (when (eq ',car-of-list-to-delete (car item))
         (setq to-delete item)))
     (setq ,alist (delete to-delete ,alist))))

;; Kill emacs when running in daemon mode or not
;; Source: http://lists.gnu.org/archive/html/emacs-devel/2011-11/msg00348.html
(defun tv-stop-emacs ()
  (interactive)
  (if (daemonp)
      (save-buffers-kill-emacs)
    (save-buffers-kill-terminal)))

;; `with-eval-after-load' macro was introduced in emacs 24.4
;; Below code makes this macro compatible with older versions of emacsen
;; Source: http://www.lunaryorn.com/2013/06/25/introducing-with-eval-after-load.html
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))

;; Source: http://oremacs.com/2015/01/14/repeatable-commands/
(defun def-rep-command (alist)
  "Return a lambda that calls the first function of ALIST.
It sets the transient map to all functions of ALIST."
  (lexical-let ((keymap (make-sparse-keymap))
                (func (cdar alist)))
    (mapc (lambda (x)
            (define-key keymap (car x) (cdr x)))
          alist)
    (lambda (arg)
      (interactive "p")
      (funcall func arg)
      (set-transient-map keymap t))))

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
