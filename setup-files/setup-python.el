;; Time-stamp: <2017-06-08 08:24:50 kmodi>

;; Python

;; Emacs built-in `python' mode
(use-package python
  :mode (("\\.py\\'" . python-mode))
  :bind (:map python-mode-map
         ("<f9>" . python-shell-send-buffer))
  :config
  (progn
    (defvar modi/python-use-ipython t
      "When non-nil, use Ipython as the python interpreter instead of python3.")

    ;; Don't warn if guessing the indention fails, just set it to the value
    ;; of `python-indent-offset'.
    (setq python-indent-guess-indent-offset-verbose nil)

    (if (and (executable-find "ipython")
             modi/python-use-ipython)
        (progn
          (setq python-shell-buffer-name "Ipython")
          (setq python-shell-interpreter "ipython")
          ;; https://emacs.stackexchange.com/q/24453/115
          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25306
          (setq python-shell-interpreter-args "--simple-prompt -i"))
      (setq python-shell-interpreter "python3")))) ;Default to python 3.x


(provide 'setup-python)

;; | C-c C-p | Start the python shell        |
;; | C-c C-c | Send current buffer to python |
