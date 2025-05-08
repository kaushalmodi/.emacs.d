;; Time-stamp: <2024-10-31 23:42:17 kmodi>

;; Package management
;; Loading of packages at startup

;; Load newer version of .el and .elc if both are available
(setq load-prefer-newer t)

;; Workaround to get the packages installed -- 2024/10/22
;; The solution presented here did not work: https://emacs.stackexchange.com/questions/233/how-to-proceed-on-package-el-signature-check-failure
(setq package-check-signature nil)

(>=e "27.0"
    nil
  ;; Lower-level `package.el' variables like `package-user-dir' need to be set
  ;; in early-init.el starting emacs 27.x.
  ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=24acb31c04b4048b85311d794e600ecd7ce60d3b
  (setq package-user-dir (let ((elpa-dir-name (format "elpa_%s" emacs-major-version))) ;default = "elpa"
                           (file-name-as-directory (expand-file-name elpa-dir-name user-emacs-directory)))))
;; Below require will auto-create `package-user-dir' it doesn't exist.
(require 'package)

(>=e "25.0"
    (setq package-menu-async t)) ; If non-nil, do activities asynchronously, like refreshing menu

(defvar modi/elisp-directory (file-name-as-directory (expand-file-name "elisp" user-emacs-directory))
  "Directory containing my custom elisp code.")

(add-to-list 'load-path modi/elisp-directory)
(add-to-list 'load-path (file-name-as-directory (expand-file-name "setup-files" user-emacs-directory)))

;; <prefix-dir>
;;      ├── bin (`invocation-directory')
;;      ├── share (`modi/default-share-directory')
;;      │   ├── emacs
;;      │   │   ├── site-lisp
;;      │   │   ├── 27.0.50
;;      │   │   │   ├── lisp (`modi/default-lisp-directory')
;;      │   │   │   └── ..
;;      │   │   └── ..
;;      │   └── ..
;;      └── ..
(defvar modi/default-share-directory nil
  "Share directory for this Emacs installation.")
(defvar modi/default-lisp-directory nil
  "Directory containing lisp files for the Emacs installation.

This value must match the path to the lisp/ directory of the
Emacs installation.  If Emacs is installed using
--prefix=\"${PREFIX_DIR}\" this value would typically be
\"${PREFIX_DIR}/share/emacs/<VERSION>/lisp/\".")
(let* ((bin-dir (when (and invocation-directory
                           (file-exists-p invocation-directory))
                  (file-truename invocation-directory)))
       (prefix-dir (when bin-dir        ;Because bin-dir = prefix-dir + "bin/"
                     (file-name-directory (directory-file-name bin-dir))))
       (share-dir (when prefix-dir
                    (let ((share-dir-1 (file-name-as-directory (expand-file-name "share" prefix-dir))))
                      (when (file-exists-p share-dir-1)
                        (setq modi/default-share-directory share-dir-1))
                      share-dir-1)))
       (version-dir (when share-dir
                      (let* ((emacs-dir (file-name-as-directory (expand-file-name "emacs" share-dir)))
                             ;; Possibility where the lisp dir is something like
                             ;; ../emacs/26.0.50/lisp/.  If `emacs-version' is
                             ;; x.y.z.w, remove the ".w" portion.
                             ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=22b2207471807bda86534b4faf1a29b3a6447536
                             (version (replace-regexp-in-string "\\([0-9]+\\.[0-9]+\\.[0-9]+\\).*" "\\1" emacs-version))
                             (version-dir-1 (file-name-as-directory (expand-file-name version emacs-dir))))
                        (if (file-exists-p version-dir-1)
                            version-dir-1
                          ;; Possibility where the lisp dir is something like
                          ;; ../emacs/25.2/lisp/.  If `emacs-version' is x.y.z,
                          ;; remove the ".z" portion.
                          (setq version (replace-regexp-in-string "\\([0-9]+\\.[0-9]+\\).*" "\\1" emacs-version))
                          (setq version-dir-1 (file-name-as-directory (expand-file-name version emacs-dir)))
                          (when (file-exists-p version-dir-1)
                            version-dir-1))))))
  ;; (message "setup-packages:: bin-dir: %s" bin-dir)
  ;; (message "setup-packages:: prefix-dir: %s" prefix-dir)
  ;; (message "setup-packages:: share-dir: %s" share-dir)
  ;; (message "setup-packages:: version-dir: %s" version-dir)
  (when version-dir
    (let ((lisp-dir-1 (file-name-as-directory (expand-file-name "lisp" version-dir))))
      (when (file-exists-p lisp-dir-1)
        (setq modi/default-lisp-directory lisp-dir-1)))))

;; Add theme paths
(add-to-list 'custom-theme-load-path
             (file-name-as-directory (expand-file-name "zenburn-emacs" modi/elisp-directory)))
(add-to-list 'custom-theme-load-path
             (file-name-as-directory (expand-file-name "smyx" modi/elisp-directory)))

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
       (melpa-url (concat protocol "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" melpa-url) :append))

(>=e "27.0"
    nil           ;`package-initialize' call is not needed in emacs 27+
  ;; Load emacs packages and activate them
  ;; This must come before configurations of installed packages.
  ;; Don't delete this line.
  (package-initialize)
  ;; `package-initialize' call is required before any of the below
  ;; can happen.
  )

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

(defun modi/byte-recompile-elpa ()
  "Force byte-compile every `.el' file in `package-user-dir'.
The `.el' files are re-compiled even if the corresponding `.elc' files exist,
in all the sub-directories under `package-user-dir'.

If the `.elc' file does not exist, this function *does not* compile the
corresponding `.el' file."
  (interactive)
  (byte-recompile-directory package-user-dir nil :force))


(provide 'setup-packages)
