;; Time-stamp: <2017-05-17 13:02:34 kmodi>

;; Package management
;; Loading of packages at startup

;; Load newer version of .el and .elc if both are available
(setq load-prefer-newer t)

(setq package-user-dir (format "%selpa_%s/"
                               user-emacs-directory emacs-major-version)) ; default = ~/.emacs.d/elpa/
;; Below require will auto-create `package-user-dir' it doesn't exist.
(require 'package)

(>=e "25.0"
    (setq package-menu-async t)) ; If non-nil, do activities asynchronously, like refreshing menu

(add-to-list 'load-path (concat user-emacs-directory "elisp/"))
(add-to-list 'load-path (concat user-emacs-directory "setup-files/"))

(defvar modi/default-lisp-directory
  (let* ((bin-dir (when (and invocation-directory
                             (file-exists-p invocation-directory))
                    invocation-directory))
         (prefix-dir (when bin-dir
                       (replace-regexp-in-string "bin/\\'" "" bin-dir)))
         (lisp-dir (when prefix-dir
                     (concat prefix-dir "share/emacs/"
                             ;; If `emacs-version' is x.y.z.w, remove the ".w" portion
                             ;; Though, this is not needed and also will do nothing in emacs 26+
                             ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=22b2207471807bda86534b4faf1a29b3a6447536
                             (replace-regexp-in-string "\\([0-9]+\\.[0-9]+\\.[0-9]+\\).*" "\\1" emacs-version)
                             "/lisp/"))))
    (when (file-exists-p lisp-dir)
      lisp-dir))
  "Directory containing emacs lisp code installed with emacs.")

;; Add theme paths
(add-to-list 'custom-theme-load-path
             (concat user-emacs-directory "elisp/zenburn-emacs/"))
(add-to-list 'custom-theme-load-path
             (concat user-emacs-directory "elisp/smyx/"))

;; Add melpa package source when using package list
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; For org-plus-contrib
(unless (bound-and-true-p org-load-version-dev)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (add-to-list 'my-packages 'org-plus-contrib)) ;Latest stable version of org-mode, includes org-eww

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
    (package-install p))
  (setq modi/missing-packages '()))

;; Mark packages to not be updated
;; http://emacs.stackexchange.com/a/9342/115
(defvar modi/package-menu-dont-update-packages '()
  "List of packages for which the package manager should not look for updates.
   Example: '(org org-plus-contrib) ")

(defun modi/package-menu-remove-excluded-packages (orig-fun &rest args)
  "Remove the packages listed in `modi/package-menu-dont-update-packages' from
the `tabulated-list-entries' variable."
  (let ((included (-filter
                   (lambda (entry)
                     (let ((pkg-name (package-desc-name (car entry))))
                       (not (member pkg-name modi/package-menu-dont-update-packages))))
                   tabulated-list-entries)))
    (setq-local tabulated-list-entries included)
    (apply orig-fun args)))
(advice-add 'package-menu--find-upgrades :around #'modi/package-menu-remove-excluded-packages)
;; (advice-remove 'package-menu--find-upgrades #'modi/package-menu-remove-excluded-packages)

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

;; http://emacs.stackexchange.com/a/26513/115
(defun modi/package-dependency-check-ignore (orig-ret)
  "Remove the `black listed packages' from ORIG-RET.

Packages listed in the let-bound `pkg-black-list' will not be auto-installed
even if they are found as dependencies.

It is known that this advice is not effective when installed packages
asynchronously using `paradox'. Below is effective on synchronous
package installations."
  (let ((pkg-black-list '(org))
        new-ret
        pkg-name)
    ;; (message "before %S" orig-ret)
    (dolist (pkg-struct orig-ret)
      (setq pkg-name (package-desc-name pkg-struct))
      (if (member pkg-name pkg-black-list)
          (message (concat "Package `%s' will not be installed. "
                           "See `modi/package-dependency-check-ignore'.")
                   pkg-name)
        ;; (message "Package to be installed: %s" pkg-name)
        (push pkg-struct new-ret)))
    ;; Tue Apr 11 17:48:16 EDT 2017 - kmodi
    ;; It's *very* critical that the order of packages stays the same in NEW-RET
    ;; as in ORIG-RET. The `push' command flips the order, so use `reverse'
    ;; to flip the order back to the original.
    ;;   Without this step, you will get errors like below when installing
    ;; packages with dependencies:
    ;;   Debugger entered--Lisp error: (error "Unable to activate package ‘nim-mode’.
    ;;   Required package ‘flycheck-28’ is unavailable")
    (setq new-ret (reverse new-ret))
    ;; (message "after  %S" new-ret)
    new-ret))
(advice-add 'package-compute-transaction :filter-return #'modi/package-dependency-check-ignore)
;; (advice-remove 'package-compute-transaction #'modi/package-dependency-check-ignore)

(defun modi/byte-recompile-elpa ()
  "Force byte-compile every `.el' file in `package-user-dir'.
The `.el' files are re-compiled even if the corresponding `.elc' files exist,
in all the sub-directories under `package-user-dir'.

If the `.elc' file does not exist, this function *does not* compile the
corresponding `.el' file."
  (interactive)
  (byte-recompile-directory package-user-dir nil :force))


(provide 'setup-packages)
