;; Time-stamp: <2016-10-31 16:50:03 kmodi>

;; Collection of general purposes defuns and macros

;; Contents:
;;
;;  Emacs version check
;;  Aliases
;;  Get symbol at point
;;  Quitting emacs
;;  Fringe face setting
;;  Default ag arguments
;;  Default rg arguments
;;  Emacs version and git branch

;;; Emacs version check
(defmacro >=e (V &rest body)
  "The BODY can contain both
'if'   (emacs version at least version V) and
'else' (emacs version older than V) blocks.

Usage: (>=e \"25.0\"
           (defun-compatible-with-25.0)
         (defun-not-compatible-in-older-version))"
  (declare (indent 2)) ; `if'-style indentation where this macro is used
  `(if (version<= ,V emacs-version)
       ,@body))

;;; Aliases
;; Alias ^ as a function to calculate exponents
;; (^ 2 15) `C-x C-e' -> 32768
(defalias '^ 'expt)

;; Easier to remember aliases
;; Set symbol's function definition to nil
(defalias 'undefun 'fmakunbound)
;; Set symbol's value to nil
(defalias 'unsetq  'makunbound)
;; Cyclically replace "A" with "X", "Y", "Z", "X", "Y", ..
(defalias 'query-replace-regexp-cyclic 'map-query-replace-regexp)

;;; Get symbol at point
;; https://github.com/Wilfred/ag.el
(defun modi/get-symbol-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

;;; Quitting emacs
;; Based on `tv-stop-emacs' function from
;; http://lists.gnu.org/archive/html/emacs-devel/2011-11/msg00348.html
(defun modi/quit-emacs (skip-desktop-save)
  "Kill emacs when running in daemon mode or not.
If `desktop-save-mode' is non-nil, save the desktop before killing emacs.

If SKIP-DESKTOP-SAVE is non-nil, do not save the desktop. "
  (interactive "P")
  (when skip-desktop-save
    (desktop-save-mode -1))
  (when desktop-save-mode
    (desktop-save-in-desktop-dir))
  (if (daemonp)
      (save-buffers-kill-emacs)
    (save-buffers-kill-terminal)))

(defun modi/quit-emacs-no-desktop-save ()
  "Kill emacs when running in daemon mode or not without saving the desktop."
  (interactive)
  (modi/quit-emacs :skip-desktop-save))

;; Quitting emacs via `C-x C-c` or the GUI 'X' button
(setq confirm-kill-emacs #'y-or-n-p)

;;; Fringe face setting
;; http://emacs.stackexchange.com/a/5343/115
(defun modi/blend-fringe ()
  (interactive)
  "Set the fringe foreground and background color to that of the theme."
  (set-face-attribute 'fringe nil
                      :foreground (if (string= (face-foreground 'default) "unspecified-fg")
                                      "#f7f7f7" (face-foreground 'default))
                      :background (if (string= (face-background 'default) "unspecified-bg")
                                      "#282828" (face-background 'default))))

;;; Default ag arguments
;; https://github.com/ggreer/the_silver_searcher
(defconst modi/ag-arguments
  '("--nogroup" ; mandatory argument for ag.el as per https://github.com/Wilfred/ag.el/issues/41
    "--skip-vcs-ignores"               ; Ignore files/dirs ONLY from `.ignore'
    "--numbers"                        ; line numbers
    "--smart-case"
    ;; "--one-device"                      ; do not cross mounts when searching
    "--follow")                        ; follow symlinks
  "Default ag arguments used in the functions in `ag', `counsel' and `projectile'
packages.")

;;; Default rg arguments
;; https://github.com/BurntSushi/ripgrep
(defconst modi/rg-arguments
  '("--no-ignore-vcs"                  ; Ignore files/dirs ONLY from `.ignore'
    "--line-number"                    ; line numbers
    "--smart-case"
    "--follow"                          ; follow symlinks
    "--ignore-file ~/.ignore")
  "Default rg arguments used in the functions in `projectile' package.")

;;; Emacs version and git branch
(defvar emacs-build-hash emacs-repository-version
  "Git hash of the commit at which this version of emacs was built.")

(defvar emacs-git-branch
  (when (and emacs-repository-version
             (file-exists-p source-directory))
    (let ((shell-return
           (replace-regexp-in-string
            "[\n)]" " " ; Replace newline and ) chars with spaces
            (shell-command-to-string
             (concat "cd " source-directory " && "
                     "git branch --contains "
                     emacs-repository-version)))))
      ;; Below regexp is tested for following "git branch --contains" values
      ;; Output for a commit in master branch too
      ;;   "* (HEAD detached at origin/emacs-25)
      ;;     master
      ;;   "
      ;; Output for a commit only in emacs-25 branch
      ;;   "* (HEAD detached at origin/emacs-25)
      ;;   "
      ;; (message "%S" shell-return)
      (when (not (string= "" shell-return))
	(string-match ".*[/ ]\\([^ ]+?\\)\\s-*$" shell-return)
	(match-string-no-properties 1 shell-return))))
  "Name of git branch from which the current emacs is built.")

(defun emacs-version-dev (here)
  "Display emacs build info and also save it to the kill-ring.
If HERE is non-nil, also insert the string at point."
  (interactive "P")
  (let ((emacs-build-info
         (concat "Emacs version: " (emacs-version) ","
                 " built using commit " emacs-repository-version ".\n\n"
                 "./configure options:\n  " system-configuration-options "\n\n"
                 "Features:\n  " system-configuration-features "\n")))
    (kill-new emacs-build-info)
    (message "%s" emacs-build-info)
    (when here
      (insert emacs-build-info))
    emacs-build-info))

(defmacro emacs-pkg-debug-setup (pkg-alist &rest body)
  "Install packages in PKG-ALIST and evaluate BODY.
Each element of PKG-ALIST has the form (((ID . LOCATION) . (PKG1 PKG2 ..)) ..).
The ID and LOCATION are the same as the ones in `package-archives'.
PKG1, PKG2, .. are package names from the ID archive.

Example usage:

1. Launch 'emacs -Q'.
2. Copy this macro definition to its scratch buffer and evaluate it.
3. Evaluate a minimum working example using this macro as below:
     (emacs-pkg-debug-setup '(;; Install hydra from GNU Elpa
                              (nil . (hydra))
                              ;; Install org from Org Elpa
                              ((\"org\" . \"http://orgmode.org/elpa/\") . (org)))
       ;; Then evaluate the below forms
       (org-mode)) "
  (declare (indent 1) (debug t))
  `(progn
     (require 'package)
     (setq user-emacs-directory (concat temporary-file-directory
                                        (getenv "USER") "/" ".emacs.d-debug/"))
     (setq package-user-dir (concat user-emacs-directory "elpa/"))
     (let (archive pkgs)
       (dolist (archive-alist ,pkg-alist)
	 (setq archive (car archive-alist))
	 (when archive
           (add-to-list 'package-archives archive :append))
         (setq pkgs (append pkgs (cdr archive-alist))))
       (package-initialize)
       (package-refresh-contents)

       (dolist (pkg pkgs)
         (when (and pkg (not (package-installed-p pkg)))
           (package-install pkg))
         (require pkg))

       ,@body)))
;; (emacs-pkg-debug-setup '((nil . (hydra))
;;                          (("melpa" . "http://melpa.org/packages/") . (use-package)))
;;   (message "Done!"))

;; http://stackoverflow.com/a/20747279/1219634
(defun modi/read-file (f)
  "Return the contents of file F as a string."
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-substring-no-properties (point-min) (point-max))))


(provide 'general)
