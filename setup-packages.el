;; Time-stamp: <2018-01-05 14:02:30 kmodi>

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

(let* ((bin-dir (when (and invocation-directory
                           (file-exists-p invocation-directory))
                  (file-truename invocation-directory)))
       (prefix-dir (when bin-dir
                     (replace-regexp-in-string "bin/\\'" "" bin-dir)))
       (share-dir (when prefix-dir
                    (concat prefix-dir "share/")))
       (lisp-dir-1 (when share-dir ;Possibility where the lisp dir is something like ../emacs/26.0.50/lisp/
                     (concat share-dir "emacs/"
                             ;; If `emacs-version' is x.y.z.w, remove the ".w" portion
                             ;; Though, this is not needed and also will do nothing in emacs 26+
                             ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=22b2207471807bda86534b4faf1a29b3a6447536
                             (replace-regexp-in-string "\\([0-9]+\\.[0-9]+\\.[0-9]+\\).*" "\\1" emacs-version)
                             "/lisp/")))
       (lisp-dir-2 (when share-dir ;Possibility where the lisp dir is something like ../emacs/25.2/lisp/
                     (concat share-dir "emacs/"
                             (replace-regexp-in-string "\\([0-9]+\\.[0-9]+\\).*" "\\1" emacs-version)
                             "/lisp/"))))
  ;; (message "setup-packages:: bin-dir: %s" bin-dir)
  ;; (message "setup-packages:: prefix-dir: %s" prefix-dir)
  ;; (message "setup-packages:: share-dir: %s" share-dir)
  ;; (message "setup-packages:: lisp-dir-1: %s" lisp-dir-1)
  ;; (message "setup-packages:: lisp-dir-2: %s" lisp-dir-2)
  (defvar modi/default-share-directory (when (file-exists-p share-dir)
                                         share-dir)
    "Share directory for this Emacs installation.")
  (defvar modi/default-lisp-directory (cond
                                       ((file-exists-p lisp-dir-1)
                                        lisp-dir-1)
                                       ((file-exists-p lisp-dir-2)
                                        lisp-dir-2)
                                       (t
                                        nil))
    "Directory containing lisp files for the Emacs installation.

This value must match the path to the lisp/ directory of the
Emacs installation.  If Emacs is installed using
--prefix=\"${PREFIX_DIR}\" this value would typically be
\"${PREFIX_DIR}/share/emacs/<VERSION>/lisp/\"."))

;; Add theme paths
(add-to-list 'custom-theme-load-path
             (concat user-emacs-directory "elisp/zenburn-emacs/"))
(add-to-list 'custom-theme-load-path
             (concat user-emacs-directory "elisp/smyx/"))

;; Add melpa package source when using package list
;; Fri Nov 17 17:05:12 EST 2017 - kmodi
;; The `system-type' for emacs in WSL is `gnu/linux' but it could be built
;; without TLS support. So make the below condition for `no-ssl' an OR condition
;; instead of an AND condition.
(let* ((no-ssl (or (memq system-type '(windows-nt ms-dos))
                   (not (gnutls-available-p))))
       (protocol (if no-ssl
                     "http"
                   "https"))
       (melpa-url (concat protocol "://melpa.org/packages/"))
       (orgelpa-url (concat protocol "://orgmode.org/elpa/")))
  (add-to-list 'package-archives (cons "melpa" melpa-url) :append)
  ;; Install `org-plus-contrib' only when it's set to use the Elpa version of Org.
  (when (eq modi/org-version-select 'elpa)
    (add-to-list 'package-archives (cons "org" orgelpa-url))
    (add-to-list 'my-packages 'org-plus-contrib))) ;Latest stable version of org-mode, includes org-eww

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
  (unless (package-installed-p p)
    (add-to-list 'modi/missing-packages p :append)))

(when modi/missing-packages
  (message "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  ;; Install the missing packages
  (dolist (p modi/missing-packages)
    (message "Installing `%s' .." p)
    (package-install p))
  (setq modi/missing-packages '()))

;; Mark packages to *not* to be updated
;; http://emacs.stackexchange.com/a/9342/115
(defvar modi/package-menu-dont-update-packages '(org)
  "List of packages for which the package manager should not look for updates.
Example: '(org org-plus-contrib).")
;; Do not upgrade Org using the package manager if it's set to *not* use the
;; Elpa version of Org.
(unless (eq modi/org-version-select 'elpa)
  (add-to-list 'modi/package-menu-dont-update-packages 'org-plus-contrib))

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
