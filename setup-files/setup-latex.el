;; Time-stamp: <2015-05-22 11:46:04 kmodi>
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

(defvar auctex-install-dir (concat user-emacs-directory "auctex/") ; must end with /
  "AucTeX install directory.")

(when (file-exists-p auctex-install-dir)
  (prepend-path auctex-install-dir)
  (load "auctex.el" nil t t)
  (load "preview-latex.el" nil t t)

  (if (executable-find "xelatex")
      (setq LaTeX-command "xelatex -shell-escape")
    (setq LaTeX-command "latex -shell-escape"))

  ;; http://www.gnu.org/software/auctex/manual/auctex/Multifile.html
  (setq TeX-PDF-mode   t)
  (setq TeX-auto-save  t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)

  (setq-default TeX-master nil)) ; Query for master file.


(provide 'setup-latex)
