;; Time-stamp: <2015-01-27 17:16:38 kmodi>

;; Package management
;; Loading of packages at startup

(require 'cl)
(require 'package)

(defun prepend-path ( my-path )
  (setq load-path (cons (expand-file-name my-path) load-path)))

(defun append-path ( my-path )
  (setq load-path (append load-path (list (expand-file-name my-path)))))

(prepend-path user-emacs-directory)
(prepend-path (concat user-emacs-directory "/setup-files"))

;; Create the package install directory if it doesn't exist
(setq package-user-dir (concat user-emacs-directory "/elpa_"
                               emacs-version-short)) ; default = ~/.emacs.d/elpa
(unless (file-exists-p package-user-dir)
  (make-directory package-user-dir))

;; add all sub directories under package install dir to the load-path
(let ((default-directory package-user-dir))
  (normal-top-level-add-subdirs-to-load-path))
;; add all sub directories under from-git/ to the load-path
(let ((default-directory (concat user-emacs-directory "/from-git"))) ;; packages not on Melpa
  (normal-top-level-add-subdirs-to-load-path))

;; add theme paths
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "/from-git/zenburn-emacs/"))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "/from-git/smyx/"))

;; Add melpa package source when using package list
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize) ;; Load emacs packages and activate them

;; Auto install the required packages
;; Source: https://github.com/bbatsov/prelude/blob/master/core/prelude-packages.el
;; Source: http://toumorokoshi.github.io/emacs-from-scratch-part-2-package-management.html
;; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))


(provide 'setup-packages)
