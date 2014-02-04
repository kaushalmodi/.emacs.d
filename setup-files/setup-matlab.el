;; Time-stamp: <2013-12-02 17:07:12 kmodi>

;; Matlab

(prepend-path "~/.emacs.d/matlab-emacs")

(require 'matlab-load)

(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

(provide 'setup-matlab)
