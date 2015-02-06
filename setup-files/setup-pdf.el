;; Time-stamp: <2015-02-06 15:41:50 kmodi>

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
(req-package pdf-tools
  :config
  (progn
    (pdf-tools-install)))


(provide 'setup-pdf)
