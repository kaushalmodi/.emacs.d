;; Time-stamp: <2014-05-23 17:27:51 kmodi>

;; LaTeX
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq LaTeX-command "latex -shell-escape")

;; Source: http://www.sigmafield.org/2009/10/03/using-doc-view-with-auto-revert-to-view-latex-pdf-output-in-emacs
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
;; Source: http://www.gnu.org/software/auctex/manual/auctex/Multifile.html
(setq TeX-PDF-mode t
      TeX-auto-save t
      TeX-parse-self t
      TeX-save-query nil)

(setq-default TeX-master nil) ; Query for master file.

(provide 'setup-latex)
