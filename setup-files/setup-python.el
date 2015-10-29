;; Time-stamp: <2015-10-29 10:49:51 kmodi>

;; Python

;; Emacs built-in `python' mode
(use-package python
  :mode (("\\.py\\'" . python-mode))
  :interpreter (("python" . python-mode)))

;; https://github.com/emacsmirror/python-mode
(use-package python-mode
  :disabled
  :config
  (progn
    ;; ;; Use IPython
    ;; (setq-default py-shell-name "ipython")
    ;; (setq-default py-which-bufname "IPython")
    ;; ;; Use python3
    ;; ;; Use the wx backend, for both mayavi and matplotlib
    ;; (setq py-python-command-args
    ;;   '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
    ;; (setq-default py-shell-name "python3")
    ;; (setq-default py-which-bufname "Python3")
    (setq py-force-py-shell-name-p t)
    ;; Switch to the interpreter after executing code
    (setq py-shell-switch-buffers-on-execute-p t)
    ;; Don't switch the code buffer to python shell
    (setq py-switch-buffers-on-execute-p nil)
    ;; Split windows
    (setq py-split-windows-on-execute-p t)
    ;; Try to automagically figure out indentation
    (setq py-smart-indentation t)))


(provide 'setup-python)
