;; Time-stamp: <2017-07-06 18:09:49 kmodi>

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

    ;; Change the default symbol prettification
    (setcdr (assoc "and" python--prettify-symbols-alist) ?&) ;Default ?^
    (setcdr (assoc "or" python--prettify-symbols-alist) ?|)  ;Default ?∨
    (with-eval-after-load 'setup-font-check
      (when (modi/is-font "Pragmata")
        (setcdr (assoc "and" python--prettify-symbols-alist) ?)
        (setcdr (assoc "or" python--prettify-symbols-alist) ?)))

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
