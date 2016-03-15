;; Time-stamp: <2016-03-15 10:25:42 kmodi>

;; Collection of general purposes defuns and macros

;; Contents:
;;
;;  Emacs version check
;;  Aliases
;;  Get symbol at point
;;  Quitting emacs
;;  Fringe face setting
;;  Default ag arguments
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
  (when (and (not skip-desktop-save)
             (bound-and-true-p desktop-save-mode))
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
(defconst modi/ag-arguments
  '("--nogroup" ; mandatory argument for ag.el as per https://github.com/Wilfred/ag.el/issues/41
    "--skip-vcs-ignores" ; Ignore files/dirs ONLY from `.agignore'
    "--numbers" ; line numbers
    "--smart-case"
    "--follow") ; follow symlinks
  "Default ag arguments used in the functions in `ag', `counsel' and `projectile'
packages.")

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
    (message emacs-build-info)
    (when here
      (insert emacs-build-info))
    emacs-build-info))


(provide 'general)
