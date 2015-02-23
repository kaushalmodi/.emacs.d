;; Time-stamp: <2015-02-23 11:42:32 kmodi>

;; PDF

;; Source: http://www.sigmafield.org/2009/10/03/using-doc-view-with-auto-revert-to-view-latex-pdf-output-in-emacs
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; pdf-tools
;; To install:
;; - git clone https://github.com/politza/pdf-tools
;; - ./configure --prefix=$HOME/usr_local
;;   + poppler-glib ( http://poppler.freedesktop.org/ ) is REQUIRED
;; - make -s
;; - make install-package
(use-package pdf-tools
  :config
  (progn

    (defun my/get-latest-pdf-tools-dir ()
      "Get the full directory path of the latest installed version of
pdf-tools package."
      (interactive)
      ;; Get a list of directories and files in `package-user-dir'
      (let ((my/package-dirs (directory-files package-user-dir)))
        ;; `break' implementation in elisp
        ;; http://ergoemacs.org/emacs/elisp_break_loop.html
        (catch 'break
          (dotimes (index (safe-length my/package-dirs))
            (let ((dir-name (pop my/package-dirs))
                  full-dir-name)
              ;; (message "%s" dir-name) ; debug
              ;; Find a directory name that matches "pdf-tools-*"
              (when (string-match "pdf\\-tools\\-.*" dir-name)
                (setq full-dir-name (concat package-user-dir "/" dir-name))
                ;; To ensure that the directory is valid, ensure that it
                ;; contains "pdf-tools.el"
                (when (locate-file "pdf-tools.el" (list full-dir-name))
                  ;; break the `dotimes' loop on finding this directory
                  ;; and return its full path
                  (throw 'break full-dir-name))))))))

    (defun my/pdf-tools-install ()
      (interactive)
      ;; Update the `pdf-info-epdfinfo-program' variable to point to
      ;; the directory containing the latest version of `pdf-tools'
      (setq pdf-info-epdfinfo-program
            (concat (my/get-latest-pdf-tools-dir) "/epdfinfo"))
      ;; Call the original `pdf-tools-install' function after updating the
      ;; `pdf-info-epdfinfo-program' variable
      (pdf-tools-install))

    (my/pdf-tools-install)))


(provide 'setup-pdf)
