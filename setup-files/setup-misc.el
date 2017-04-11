;; Time-stamp: <2017-03-30 08:08:33 kmodi>

;; Miscellaneous config not categorized in other setup-* files

(fset 'yes-or-no-p 'y-or-n-p) ; Use y or n instead of yes or no

;; Delete stuff to a trash directory
(setq delete-by-moving-to-trash t)
(setq trash-directory
      (let ((dir (concat temporary-file-directory
                         (getenv "USER") "/.trash_emacs/"))) ; must end with /
        (make-directory dir :parents)
        dir))

;; Make apropos commands search more extensively
(setq apropos-do-all t)

;; On saving, automatically make a file an executable if it begins with "#!"
;; Example: #!/bin/csh
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Uncompress->edit->save->compress .gz, .bz2, .Z files on the fly
(auto-compression-mode 1)

;; Send mail using `Sendmail' package
(setq send-mail-function #'sendmail-send-it)

;; url
;; First delete the old `url/' directory if present
(let ((url-dir-old (concat user-emacs-directory "url/")))
  (when (file-exists-p url-dir-old)
    (delete-directory url-dir-old :recursive)))
;; Solve the issue with `sx.el' when using that package simultaneously in
;; different emacs versions
(setq url-configuration-directory (let ((dir (concat user-emacs-directory
                                                     "url_" emacs-version-short "/")))
                                    (make-directory dir :parents)
                                    dir))

;; Don't ask if I want to visit a sym-linked file under VC. I always do!
(setq vc-follow-symlinks t)

;; Execute the script in current buffer
;; http://ergoemacs.org/emacs/elisp_run_current_file.html
(defun modi/run-current-file (&optional eval-init)
  "Execute the current file.

If EVAL-INIT is non-nil, load the `init.el'.

For example, if the current buffer is the file xx.py, then it'll call
“python xx.py” in a shell. The file can be php, perl, python, ruby,
javascript, bash, ocaml, vb, elisp.  File extension is used to determine
what program to run.

If the file is modified, user is asked to save it first.

If the buffer major-mode is `emacs-lisp-mode', run `eval-buffer'.
If the buffer major-mode is `clojure-mode', run `cider-load-buffer'."
  (interactive "P")
  (if eval-init
      (load (locate-user-emacs-file "init.el"))
    (progn
      (when (buffer-modified-p)
        (when (y-or-n-p "Buffer modified. Do you want to save first?")
          (save-buffer)))

      (cond
       ((derived-mode-p 'emacs-lisp-mode)
        (eval-buffer) ; also works for .el.gz and .el.gpg files
        (message "Evaluated `%0s'." (buffer-name)))
       ((and (featurep 'cider)
             (derived-mode-p 'clojure-mode))
        (cider-load-buffer))
       (t
        (let* ((suffix-map '(("py" . "python")
                             ("rb" . "ruby")
                             ("sh" . "bash")
                             ("csh" . "tcsh")
                             ("pl" . "perl")
                             ("tex" . "pdflatex")
                             ("latex" . "pdflatex")
                             ("d" . "dmd -de -w -unittest -run")
                             ("nim" . "nim c -r --verbosity:0")))
               (file-name (progn
                            ;; Save buffer as a file if it's not already a file.
                            (when (null (buffer-file-name)) (save-buffer))
                            (buffer-file-name)))
               (file-ext (file-name-extension file-name))
               (prog-name (cdr (assoc file-ext suffix-map)))
               (cmd-str (concat prog-name " \"" file-name "\"")))
          (if prog-name
              (progn
                ;; (view-echo-area-messages)
                (message "Running ...")
                (shell-command cmd-str "*run-current-file output*" ))
            (message "No recognized program file suffix for this file."))))))))
(bind-to-modi-map "l" #'modi/run-current-file)

;; Set the major mode for plain text/log files
(use-package text-mode
  :mode (("\\.log\\'" . text-mode)
         ("\\.f\\'" . text-mode)) ; I never need to code in Fortran
  :config
  (progn
    ;; http://emacs.stackexchange.com/a/16854/115
    (defun modi/text-mode-comments ()
      "Make text beginning with # look like comments only in `text-mode'."
      (when (equal major-mode 'text-mode)
        (font-lock-add-keywords nil '(("#.+" . font-lock-comment-face)))))
    (add-hook 'text-mode-hook #'modi/text-mode-comments)))

;; Help Functions +
;; http://www.emacswiki.org/emacs/HelpPlus
(use-package help-fns+
  :defer 20
  :config
  (progn
    (bind-keys
     :map help-map
      ("c"   . describe-key-briefly)
      ("C-c" . describe-command))
    (>=e "25.0"
        (bind-keys
         :map help-map
          ("o" . describe-symbol)))))

(>=e "25.0"
    (use-package saveplace
      :defer t
      :init
      (save-place-mode 1)))

(use-package browse-url
  :defer t
  :config
  (progn
    ;; Set firefox as the default web browser if available
    (when (executable-find "firefox")
      (setq browse-url-browser-function 'browse-url-firefox))))

;; Unset keys
(global-unset-key (kbd "C-z")) ; It is bound to `suspend-frame' by default.
;; `suspend-frame' can be called using C-x C-z too.
;; C-z is used as prefix key by me in tmux. So removing the C-z binding from
;; emacs makes it possible to use emacs in -nw (no-window) mode in tmux without
;; any key binding contention.

;; http://endlessparentheses.com/sweet-new-features-in-24-4.html
;; Hook `eval-expression-minibuffer-setup-hook' is run by `eval-expression'
;; on entering the minibuffer.
;; Below enables ElDoc inside the `eval-expression' minibuffer.
;; Call `M-:' and type something like `(message.' to see what ElDoc does :)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;; https://github.com/kaushalmodi/.emacs.d/issues/7
(defun modi/startup-time()
  (message (format "init.el loaded in %s." (emacs-init-time))))
(add-hook 'emacs-startup-hook #'modi/startup-time)

;; Don't put the build system info "built on <IP>" in emacs bug reports
(defun modi/advice-report-emacs-bug-without-build-system (orig-fun &rest args)
  "Do not insert the user system IP when creating emacs bug report."
  (let (emacs-build-system) ; Temporarily set `emacs-build-system' to nil.
    (apply orig-fun args)))
(advice-add 'report-emacs-bug :around #'modi/advice-report-emacs-bug-without-build-system)

(use-package calendar
  :defer t
  :config
  (progn
    ;; Highlight today's date in the calendar
    (add-hook 'calendar-today-visible-hook 'calendar-mark-today)))

;;;;;;;;;;
;; Organize the order of minor mode lighters
(defvar mode-line-space-mode-lighter " "
  "Lighter for `mode-line-space-mode'." )
(define-minor-mode mode-line-space-mode
  "A minor mode whose sole purpose is to retain a space between the major mode
name and the bunch of minor mode lighters in the mode line."
  :init-value t
  :lighter    mode-line-space-mode-lighter
  :global     t)
(mode-line-space-mode)

(defun modi/organize-minor-mode-lighters ()
  "The `multiple-cursors-mode' lighter is very useful in showing how many cursors
are created or if multiple-cursors-mode is enabled. Move that lighter to the
foremost position in the `minor-mode-alist'.

Move the `mode-line-space-mode' lighter to the second-foremost position
in the mode line."
  (interactive)

  ;; If `mode-line-space-mode' is not the first in `minor-mode-alist' ..
  (when (not (equal 'mode-line-space-mode (car (car minor-mode-alist))))
    ;; First remove it from the alist
    (setq minor-mode-alist (assq-delete-all 'mode-line-space-mode minor-mode-alist))
    ;; Now add it back but to the beginning of the alist
    (add-to-list 'minor-mode-alist '(mode-line-space-mode mode-line-space-mode-lighter)))

  ;; If `multiple-cursors-mode' is not the first in `minor-mode-alist' ..
  (with-eval-after-load 'multiple-cursors
    (when (not (equal 'multiple-cursors-mode (car (car minor-mode-alist))))
      ;; First remove it from the alist
      (setq minor-mode-alist (assq-delete-all 'multiple-cursors-mode minor-mode-alist))
      ;; Now add it back but to the beginning of the alist
      (add-to-list 'minor-mode-alist '(multiple-cursors-mode mc/mode-line)))))

(modi/organize-minor-mode-lighters)
;; Also add the above fn to `after-revert-hook'. So in the event you don't find
;; the minor-mode lighters in right order, simply revert the buffer.
(add-hook 'after-revert-hook #'modi/organize-minor-mode-lighters)

(bind-keys
 ;; Override the default binding of C-x C-c to `save-buffer-kill-terminal'
 ;; `save-buffers-kill-terminal' kills only the current frame; it will NOT
 ;; kill the emacs server.
 ("C-x C-c" . modi/quit-emacs)
 ("C-x M-c" . modi/quit-emacs-no-desktop-save))

(defun modi/restore-imp-keys ()
  "Basic `global-map' bindings saved to `modi-mode-map'.
This comes helpful in the event that the former gets wiped off due to a bug or
by mistake.
https://lists.gnu.org/archive/html/emacs-devel/2016-07/msg00519.html "
  (interactive)
  (bind-keys
   :map modi-mode-map
    ("M-w" . kill-ring-save)
    ("C-y" . yank)
    ("C-p" . previous-line)
    ("C-n" . next-line)
    ("C-b" . backward-char)
    ("C-f" . forward-char)
    ("C-c t" . hydra-toggle/body)
    ("C-h l" . view-lossage)
    ("C-x C-c" . modi/quit-emacs)
    ("C-x M-c" . modi/quit-emacs-no-desktop-save)))
(bind-key "C-c ;" #'modi/restore-imp-keys)


(provide 'setup-misc)

;; TIPS
;;
;; (1) Un-define a symbol/variable
;; this will make the symbol my-nasty-variable's value void
;; (makunbound 'my-nasty-variable)
;;
;; (2) Un-define a function
;; this will make the symbol my-nasty-function's
;; function definition void
;; (fmakunbound 'my-nasty-function)
;;
;; (3) See a variable value
;; `C-h v`, enter the variable name, Enter
;; Example: `C-h v tooltip-mode`
;;
;; (4) Killing buffers from an emacsclient frame
;; `C-x #`   Kills the buffer in emacsclient frame without killing the frame
;; `C-x 5 0` Kills the emacsclient frame
;;
;; (5) `C-q' is bound to `quoted-insert'
;; Example: Pressing `C-q C-l' inserts the `^l' character (form feed):  
;;
;; (6) The way to figure out how to type a particular key combination or to know
;; what a particular key combination does, do help on a key `C-h k`, and type
;; the keystrokes you're interested in. What Emacs shows in the Help buffer is
;; the string you can pass to the macro 'kbd.
;;
;; (7) How to know what the current major mode is?
;; `C-h v major-mode`
;;
;; (8) Put this line at the top of an anything.gpg file to prevent it from
;; asking for the password on each save
;; -*- epa-file-encrypt-to: ("<MY_EMAIL>") -*-
;;
;; (9) Replace a string with string suffixed with incrementing numbers
;; http://www.reddit.com/r/emacs/comments/355bm0/til_after_so_long_using_emacs/cr1l6gy
;; Let's say you want to replace all the instances of "Here" with "Here1" "Here2" "Here3" ..
;;    1. M-x query-replace-regexp
;;    2. regexp: \bHere\b
;;    3. replacement: Here\,(1+ \#)
;;  \, lets you put in some elisp to evaluate, which is incredibly
;; useful. In the s-expression here, \# is a variable available to
;; query-replace-regexp that holds the number of replacements done so
;; far (i.e. before the first replacement it's 0, before the second
;; replacement it's 1, and so on). To start the numbering at one, we
;; just add one to it with the function 1+.
;;
;; (10) C-x =     <-- `what-cursor-position' ; default binding
;;      C-u C-x = <-- `describe-char'
