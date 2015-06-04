;; Time-stamp: <2015-06-04 16:15:46 kmodi>

;; Htmlize

(use-package htmlize
  :config
  (progn
    (defvar modi/htmlize-initial-fci-state nil
      "Variable to store the state of `fci-mode' when `htmlize-buffer' is called.")
    (defvar modi/htmlize-initial-flyspell-state nil
      "Variable to store the state of `flyspell-mode' when `htmlize-buffer' is called.")

    (defun modi/htmlize-before-hook-fn ()
      ;; It is required to disable `fci-mode' when `htmlize-buffer' is called;
      ;; otherwise the invisible fci characters show up as funky looking
      ;; visible characters in the source code blocks in the html file
      ;; http://lists.gnu.org/archive/html/emacs-orgmode/2014-09/msg00777.html
      (when (fboundp 'fci-mode)
        (setq modi/htmlize-initial-fci-state fci-mode)
        (when fci-mode
          (fci-mode -1)))
      ;; `flyspell-mode' also has to be disabled because depending on the
      ;; theme, the squiggly underlines can either show up in the html file
      ;; or cause elisp errors
      (when (fboundp 'flyspell-mode)
        (setq modi/htmlize-initial-flyspell-state flyspell-mode)
        (when flyspell-mode
          (flyspell-mode -1))))
    (add-hook 'htmlize-before-hook #'modi/htmlize-before-hook-fn)

    (defun modi/htmlize-after-hook-fn ()
      (when (fboundp 'fci-mode)
        (when modi/htmlize-initial-fci-state
          (fci-mode 1)))
      (when (fboundp 'flyspell-mode)
        (when modi/htmlize-initial-flyspell-state
          (flyspell-mode 1))))
    (add-hook 'htmlize-after-hook #'modi/htmlize-after-hook-fn)

    (defvar modi/htmlize-temp-html-dir (concat user-home-directory "temp/")
      "Directory where the `modi/htmlize-region-as-html-file' saves the html files.")
    (defun modi/htmlize-region-as-html-file (start end)
      (interactive "r")
      (let ((html-file (concat modi/htmlize-temp-html-dir "htmlize_temp.html"))
            (html-string (htmlize-region-for-paste start end)))
        (with-temp-buffer
          (insert html-string)
          (write-file html-file))))))


  (provide 'setup-htmlize)
