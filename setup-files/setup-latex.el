;; Time-stamp: <2014-11-07 13:52:39 kmodi>
;;
;; LaTeX
;;
;; NOTE: auctex has to be installed from outside emacs for the below `load's
;; to work.
;;
;; 1. Download the latest auctex from http://www.gnu.org/software/auctex/download-for-unix.html
;; 2. tar xvzf auctex-VERSION.tar.gz
;; 3. ./configure --prefix=/home/kmodi/usr_local --with-lispdir=/home/kmodi/.emacs.d/auctex/ --with-texmf-dir=/home/kmodi/texlive/texmf-local/
;;    - prefix <- Location of your /usr/local
;;    - with-lispdir <- I prefer to keep auctex elisp stuff in my ~/.emacs.d
;;    - with-texmf-dir <- Location of texlive texmf directory
;; 4. make
;; 5. make install

(prepend-path (concat user-emacs-directory "/auctex"))

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
