;; Time-stamp: <2014-07-01 14:17:23 kmodi>

;; Matlab

(prepend-path "~/.emacs.d/matlab-emacs")

(require 'matlab-load)

(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

;; (define-key matlab-shell-mode-map (kbd "C-p") 'matlab-shell-previous-matching-input-from-input)
;; (define-key matlab-shell-mode-map (kbd "C-n") 'matlab-shell-next-matching-input-from-input)

(defun my-matlab-mode-customizations ()
  ;; Override the matlab-mode binding for `C-j'; use my custom global key binding
  (define-key matlab-mode-map (kbd "C-j") nil)
  (when (boundp 'setup-linum-loaded)
    (nlinum-mode 1))
  )
(add-hook 'matlab-mode-hook 'my-matlab-mode-customizations)


(setq setup-matlab-loaded t)
(provide 'setup-matlab)
