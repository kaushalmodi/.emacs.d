;; Time-stamp: <2016-02-20 01:56:02 kmodi>

;; Package management
;; Loading of packages at startup

;; Load newer version of .el and .elc if both are available
(setq load-prefer-newer t)

(require 'package)

(>=e "25.0"
    (setq package-menu-async t)) ; If non-nil, do activities asynchronously, like refreshing menu

(add-to-list 'load-path (concat user-emacs-directory "elisp/"))
(add-to-list 'load-path (concat user-emacs-directory "setup-files/"))

;; Create the package install directory if it doesn't exist
(setq package-user-dir (concat user-emacs-directory "elpa_"
                               emacs-version-short "/")) ; default = ~/.emacs.d/elpa/

;; add theme paths
(add-to-list 'custom-theme-load-path
             (concat user-emacs-directory "elisp/zenburn-emacs/"))
(add-to-list 'custom-theme-load-path
             (concat user-emacs-directory "elisp/smyx/"))

;; Add melpa package source when using package list
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; For org-plus-contrib
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Load emacs packages and activate them
;; This must come before configurations of installed packages.
;; Don't delete this line.
(package-initialize)
;; `package-initialize' call is required before any of the below
;; can happen

;; Auto install the required packages
;; https://github.com/bbatsov/prelude/blob/master/core/prelude-packages.el
;; http://toumorokoshi.github.io/emacs-from-scratch-part-2-package-management.html
(defvar modi/missing-packages '()
  "List populated at each startup that contains the list of packages that need
to be installed.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (add-to-list 'modi/missing-packages p)))

(when modi/missing-packages
  (message "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  ;; Install the missing packages
  (dolist (p modi/missing-packages)
    (message "Installing `%s' .." p)
    (package-install p nil))
  (setq modi/missing-packages '()))

;; Mark packages to not be updated
;; http://emacs.stackexchange.com/a/9342/115
(defvar package-menu-exclude-packages '()
  "List of packages for which the package manager should not look for updates.")
(defun package-menu--remove-excluded-packages (orig-fun &rest args)
  (let ((included (-filter
                   (lambda (entry)
                     (let ((name (symbol-name (package-desc-name (car entry)))))
                       (not (member name package-menu-exclude-packages))))
                   tabulated-list-entries)))
    (setq-local tabulated-list-entries included)
    (apply orig-fun args)))
(advice-add 'package-menu--find-upgrades :around #'package-menu--remove-excluded-packages)

;; Inspired from paradox.el
(defun my/package-upgrade-packages (&optional no-fetch)
  "Upgrade all packages.  No questions asked.
This function is equivalent to `list-packages', followed by a
`package-menu-mark-upgrades' and a `package-menu-execute'.  Except
the user isn't asked to confirm deletion of packages.

The NO-FETCH prefix argument is passed to `list-packages'.  It
prevents re-download of information about new versions.  It does
not prevent downloading the actual packages (obviously)."
  (interactive "P")
  (let ((package-menu-async nil)) ; This variable was introduced in emacs 25.0
    (save-window-excursion
      (package-list-packages no-fetch)
      (package-menu-mark-upgrades)
      (package-menu-execute 'noquery))))


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
