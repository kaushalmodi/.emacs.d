;; Time-stamp: <2014-03-04 09:18:51 kmodi>

;; Matlab

(prepend-path "~/.emacs.d/matlab-emacs")

(require 'matlab-load)

(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

;; (define-key matlab-shell-mode-map (kbd "C-p") 'matlab-shell-previous-matching-input-from-input)
;; (define-key matlab-shell-mode-map (kbd "C-n") 'matlab-shell-next-matching-input-from-input)


(provide 'setup-matlab)
