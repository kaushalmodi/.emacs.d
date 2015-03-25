;; Time-stamp: <2015-03-25 18:28:33 kmodi>

;; Aggressive Indent
;; https://github.com/Malabarba/aggressive-indent-mode

(use-package aggressive-indent
  :if (not (bound-and-true-p disable-pkg-aggressive-indent))
  :config
  (progn
    (defvar modi/aggressive-indent-mode-hooks '(emacs-lisp-mode-hook)
      "List of hooks of major modes in which aggressive-indent-mode should be enabled.")

    (defun modi/turn-on-aggressive-indent-mode ()
      "Turn on aggressive-indent-mode only for specific modes."
      (interactive)
      (dolist (hook modi/aggressive-indent-mode-hooks)
        (add-hook hook #'aggressive-indent-mode)))

    (defun modi/turn-off-aggressive-indent-mode ()
      "Turn off aggressive-indent-mode only for specific modes."
      (interactive)
      (dolist (hook modi/aggressive-indent-mode-hooks)
        (remove-hook hook #'aggressive-indent-mode)))

    (modi/turn-on-aggressive-indent-mode)))


(provide 'setup-aggressive-indent)


;; ;; Aggressive auto indentation
;; (defun endless/indent-defun ()
;;   "Indent current defun.
;; Do nothing if mark is active (to avoid deactivaing it), or if
;; buffer is not modified (to avoid creating accidental
;; modifications)."
;;   (interactive)
;;   (ignore-errors
;;     (unless (or (region-active-p)
;;                 buffer-read-only
;;                 (null (buffer-modified-p)))
;;       (let ((l (save-excursion (beginning-of-defun 1) (point)))
;;             (r (save-excursion (end-of-defun 1) (point))))
;;         (cl-letf (((symbol-function 'message) #'ignore))
;;           (indent-region l r))))))
;; (defun endless/activate-aggressive-indent ()
;;   "Locally add `endless/indent-defun' to `post-command-hook'."
;;   (add-hook 'post-command-hook #'endless/indent-defun nil 'local)
;;   ;; (remove-hook 'post-command-hook #'endless/indent-defun 'local)
;;   )
;; (add-hook 'emacs-lisp-mode-hook #'endless/activate-aggressive-indent)
;; ;; (remove-hook 'emacs-lisp-mode-hook #'endless/activate-aggressive-indent)
