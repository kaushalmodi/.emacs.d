;; Time-stamp: <2022-07-22 09:18:53 kmodi>

;; Collection of general purposes defuns and macros

;; Contents:
;;
;;  Emacs version check
;;  Aliases
;;  Get symbol at point, maybe
;;  Quitting emacs
;;  Fringe face setting
;;  Default ag arguments
;;  Default rg arguments
;;  Emacs version and git branch

;;; Emacs version check
(defmacro >=e (version &rest body)
  "Emacs VERSION check wrapper around BODY.
BODY can contain both `if' block (for stuff to execute if emacs
is equal or newer than VERSION) and `else' block (for stuff to
execute if emacs is older than VERSION).

Example:
  (>=e \"25.0\"
      (defun-compatible-with-25.0)
    (defun-not-compatible-in-older-version))"
  (declare (indent 2))          ;`if'-style indentation where this macro is used
  `(if (version<= ,version emacs-version)
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

;;; Get symbol at point, maybe
(defun modi/get-selected-text-or-symbol-at-point ()
  "Get the text in region or symbol at point.

If region is active, return the text in that region.  Else if the
point is on a symbol, return that symbol name.  Else return nil."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties (thing-at-point 'symbol)))
        (t
         nil)))

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
  '("--nogroup" ;mandatory argument for ag.el as per https://github.com/Wilfred/ag.el/issues/41
    "--skip-vcs-ignores"                ;Ignore files/dirs ONLY from `.ignore'
    "--numbers"                         ;Line numbers
    "--smart-case"
    ;; "--one-device"                      ;Do not cross mounts when searching
    "--follow"                          ;Follow symlinks
    "--ignore" "#*#") ;Adding "*#*#" or "#*#" to .ignore does not work for ag (works for rg)
  "Default ag arguments used in the functions in `ag', `counsel' and `projectile'
packages.")

;;; Default rg arguments
;; https://github.com/BurntSushi/ripgrep
(defconst modi/rg-arguments
  `("--no-ignore-vcs"                   ;Ignore files/dirs ONLY from `.ignore'
    "--line-number"                     ;Line numbers
    "--smart-case"
    "--follow"                 ;Follow symlinks
    "--max-columns" "150"      ;Emacs doesn't handle long line lengths very well
    "--ignore-file" ,(expand-file-name ".ignore" (getenv "HOME")))
  "Default rg arguments used in the functions in `counsel' and `projectile'
packages.")

;;; Emacs version and git branch
(defvar emacs-build-hash emacs-repository-version
  "Git hash of the commit at which this version of emacs was built.")

(defvar emacs-git-branch
  (when (and emacs-repository-version
             (file-exists-p source-directory))
    (let ((shell-return
           (replace-regexp-in-string
            "[\n)]" " "                 ;Replace newline and ) chars with spaces
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
  "Install packages in PKG-ALIST and evaluate BODY in a temporary directory.

The packages are installed in
\"`temporary-file-directory'/`user-login-name'/.emacs.d-debug/elpa_`emacs-major-version'/\".

Each element of PKG-ALIST has the form (((ID . LOCATION) . (PKG1 PKG2 ..)) ..).
The ID and LOCATION are the same as the ones in `package-archives'.
PKG1, PKG2, .. are package names from the ID archive.

If a package is available in one of the default archives from
`package-archives', leave the ID as nil.

Example usage:

1. Launch 'emacs -Q'.
2. Copy this macro definition to its scratch buffer and evaluate it.
3. Evaluate a minimum working example using this macro as below:
     (emacs-pkg-debug-setup \\='(;; Install Hydra from GNU Elpa.
                              (nil . (hydra))
                              ;; Install ox-hugo from Melpa.
                              ((\"melpa\" . \"https://melpa.org/packages/\") . (ox-hugo)))
       ;; Then evaluate the below forms.
       (org-version nil :full)) "
  (declare (indent 1) (debug t))
  `(progn
     (require 'package)
     (setq user-emacs-directory (let* ((dir-1 (file-name-as-directory (expand-file-name user-login-name temporary-file-directory)))
                                       (dir (file-name-as-directory (expand-file-name ".emacs.d-debug" dir-1))))
                                  dir))
     (setq package-user-dir (let ((elpa-dir-name (format "elpa_%s" emacs-major-version))) ;default = ~/.emacs.d/elpa/
                              (file-name-as-directory (expand-file-name elpa-dir-name user-emacs-directory))))
     (let (pkgs)
       (dolist (archive-alist ,pkg-alist)
         (when-let* ((archive (car archive-alist)))
           (add-to-list 'package-archives archive :append))
         (setq pkgs (append pkgs (cdr archive-alist))))
       ;; The `package-initialize' call is needed even in Emacs 27+
       ;; because we need to add our custom `package-user-directory'
       ;; and the underlying packages to the `load-path'.
       (package-initialize)
       (package-refresh-contents)

       (dolist (pkg pkgs)
         (when (and pkg (not (package-installed-p pkg)))
           (package-install pkg))
         (require pkg))

       ,@body)))
;; (emacs-pkg-debug-setup '((("melpa" . "https://melpa.org/packages/") . (use-package ox-hugo)))
;;   (message "Done!"))

;; http://stackoverflow.com/a/20747279/1219634
(defun modi/read-file (f)
  "Return the contents of file F as a string."
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-substring-no-properties (point-min) (point-max))))

;; Tue Jul 11 12:37:11 EDT 2017 - kmodi
;; Added the below variable watching code out of necessity when debugging
;; debbugs # 27647: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27647#14
(defvar modi/variables-to-be-watched ()
  "List of variables to be watched.
Used by `modi/set-variable-watchers' and
`modi/unset-variable-watchers'")

(defun modi/variable-watcher-fn (symbol newval operation where)
  "Print message when the value of variable SYMBOL changes.
The message shows the NEWVAL it changed to, the OPERATION that
caused that, and the buffer WHERE that happened if the value
change was buffer-local."
  (message (format "[Watcher: %s] Now set to %S, by `%S'%s"
                   (symbol-name symbol)
                   newval
                   operation
                   (if where
                       (format " in %S" where)
                     ""))))

(defun modi/set-variable-watchers ()
  "Enable printing messages when any watched variable changes.
The variables to be watched should be added to
`modi/variables-to-be-watched'."
  (interactive)
  (dolist (var modi/variables-to-be-watched)
    (add-variable-watcher var #'modi/variable-watcher-fn)))

(defun modi/unset-variable-watchers ()
  "Disable variable watchers.
Variable watching will be disabled for the list of variables set
in `modi/variables-to-be-watched'."
  (interactive)
  (dolist (var modi/variables-to-be-watched)
    (remove-variable-watcher var #'modi/variable-watcher-fn)))


(provide 'general)
