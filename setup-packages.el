;; Time-stamp: <2015-02-24 08:48:20 kmodi>

;; Package management
;; Loading of packages at startup

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
;; add all sub directories under elisp/ to the load-path
(let ((default-directory (concat user-emacs-directory "/elisp"))) ; packages not on Melpa
  (normal-top-level-add-subdirs-to-load-path))

;; add theme paths
(add-to-list 'custom-theme-load-path
             (concat user-emacs-directory "/elisp/zenburn-emacs/"))
(add-to-list 'custom-theme-load-path
             (concat user-emacs-directory "/elisp/smyx/"))

;; Add melpa package source when using package list
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Mark packages to not be updated
;; http://emacs.stackexchange.com/a/9342/115
(defvar package-menu-exclude-packages '()
  "List of packages for which the package manager should not look for updates.")
(defun package-menu--remove-excluded-packages (orig)
  (let ((included (-filter
                   (lambda (entry)
                     (let ((name (symbol-name (package-desc-name (car entry)))))
                       (not (member name package-menu-exclude-packages))))
                   tabulated-list-entries)))
    (setq-local tabulated-list-entries included)
    (funcall orig)))
(advice-add 'package-menu--find-upgrades :around #'package-menu--remove-excluded-packages)

(package-initialize) ;; Load emacs packages and activate them

;; Auto install the required packages
;; https://github.com/bbatsov/prelude/blob/master/core/prelude-packages.el
;; http://toumorokoshi.github.io/emacs-from-scratch-part-2-package-management.html
;; Method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Paradox - https://github.com/Bruce-Connor/paradox
(require 'paradox)
;; The ".paradox-token.el" file is supposed to contain this line:
;;     (setq paradox-github-token "<YOUR_TOKEN>")
(load ".paradox-token.el" :noerror)
(setq paradox-lines-per-entry 1)

(paradox-enable)


(provide 'setup-packages)

;; Paradox
;; |----------+---------------------------------------|
;; | Shortcut | Description                           |
;; |----------+---------------------------------------|
;; | v        | Visit the package's homepage          |
;; | l        | View a list of recent commits         |
;; | f r      | filters by regexp (occur);            |
;; | f u      | display only packages with upgrades;  |
;; | f k      | filters by keyword (emacs 24.4 only). |
;; | f c      | clear filters                         |
;; | h        | See all keys                          |
;; | s        | Star/unstar package                   |
;; |----------+---------------------------------------|
;;
;; Use paradox-require instead of require to automatically install absent packages.
