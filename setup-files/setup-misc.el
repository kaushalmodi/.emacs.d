;; Time-stamp: <2016-01-14 17:31:30 kmodi>

;; Miscellaneous config not categorized in other setup-* files

(fset 'yes-or-no-p 'y-or-n-p) ; Use y or n instead of yes or no

(setq delete-by-moving-to-trash t)
(setq trash-directory (concat "/tmp/trash/" (getenv "USER") "/emacs/"))

;; Do not make mouse wheel accelerate its action (example: scrolling)
(setq mouse-wheel-progressive-speed nil)

;; Quitting emacs via `C-x C-c` or the GUI 'X' button
(setq confirm-kill-emacs 'y-or-n-p)

;; Clipboard
;; after copy Ctrl+c in X11 apps, you can paste by 'yank' in emacs"
(>=e "25.0"
    (setq select-enable-clipboard t)  ; if emacs 25.0 or newer
  (setq x-select-enable-clipboard t)) ; if older
;; after mouse selection in X11, you can paste by 'yank' in emacs"
(>=e "25.0"
    (setq select-enable-primary t)  ; if emacs 25.0 or newer
  (setq x-select-enable-primary t)) ; if older

;; Save stuff copied from an external program to the kill ring before killing
;; new stuff from within emacs
(setq save-interprogram-paste-before-kill t)

;; Make apropos commands search more extensively
(setq apropos-do-all t)

;; On saving, automatically make a file an executable if it begins with "#!"
;; Example: #!/bin/csh
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

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
                             ("d" . "dmd -de -w -unittest -run")))
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
  :mode (("\\.log\\'" . text-mode))
  :config
  (progn
    ;; http://emacs.stackexchange.com/a/16854/115
    (defun modi/text-mode-comments ()
      (font-lock-add-keywords nil '(("#.+" . font-lock-comment-face))))
    (add-hook 'text-mode-hook #'modi/text-mode-comments)))

;; Help Functions +
;; http://www.emacswiki.org/emacs/HelpPlus
(use-package help-fns+
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
      :config
      (save-place-mode 1)))

;; Unset keys
(global-unset-key (kbd "C-z")) ;; it is bound to `suspend-frame' by default
;; suspend-frame can be called using `C-x C-z` too. And `C-z` is used as prefix
;; key in tmux. So removing the `C-z` binding from emacs makes it possible to
;; use emacs in -nw (no-window) mode in tmux if needed without any key binding
;; contention.

;; http://endlessparentheses.com/sweet-new-features-in-24-4.html
;; Hook `eval-expression-minibuffer-setup-hook' is run by ;; `eval-expression'
;; on entering the minibuffer.
;; Below enables ElDoc inside the `eval-expression' minibuffer.
;; Call `M-:' and type something like `(message.' to see what ElDoc does :)
(>=e "24.4"
    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

;; Avert the "auto-notify not working" bug in emacs 24.5+
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=20441
(>=e "24.4" ; The `auto-revert-use-notify' variable was introduced in emacs 24.4
    (with-eval-after-load 'autorevert
      (setq-default auto-revert-use-notify nil)))

;; Set firefox as the default web browser
(setq browse-url-generic-program (executable-find "firefox"))
(setq browse-url-browser-function 'browse-url-generic)

(defvar emacs-build-hash emacs-repository-version
  "Git hash of the commit at which this version of emacs was built.")

(defun modi/browse-current-build-emacs-git (log)
  "Browse to the emacs git page for the current build commit details.
Also save the url to the kill-ring.

If LOG is non-nil, browse to the log page having the current build commit."
  (interactive "P")
  (let* ((commit-or-log (if log "log" "commit"))
         (url (concat "http://git.savannah.gnu.org/cgit/emacs.git/"
                      commit-or-log
                      "/?id=" emacs-repository-version)))
    (kill-new url)
    (browse-url url)))

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

;; https://github.com/kaushalmodi/.emacs.d/issues/7
(defun modi/startup-time()
  (message (format "init.el loaded in %s." (emacs-init-time))))
(add-hook 'emacs-startup-hook #'modi/startup-time)

;; Toggles
;; http://endlessparentheses.com/the-toggle-map-and-wizardry.html
(autoload 'dired-toggle-read-only "dired" nil t)
(defhydra hydra-toggle (:color blue
                        :hint  nil)
  "
    TOGGLE ...
_ai_ aggressive indent      _d_/_D_ debug on error/entry      indent _g_uide                 _i_menu list        _p_resentation          _C-t_ theme
_aw_ adaptive wrap          _e_debug^^                        _hl_ hl-line                   _k_ey chord         _r_ead only             _v_isible mode
menu _b_ar                  _f_ill^^                          _hs_ hideshow                  _l_ine num          _S_tripe buffer         _<SPC>_ whitespace
cycle _c_ase                _F_ollow^^                        _H_ardcore (allow arrows)      _m_odi mode         _t_ truncate lines
"
  ("ai"    aggressive-indent-mode)
  ("aw"    adaptive-wrap-prefix-mode)
  ("b"     modi/toggle-menu-bar)
  ("c"     xah-cycle-letter-case :color red)
  ("d"     toggle-debug-on-error)
  ("D"     modi/toggle-debug)
  ("e"     modi/toggle-edebug)
  ("f"     toggle-fill-unfill)
  ("F"     follow-mode)
  ("g"     indent-guide-mode)
  ("hl"    hl-line-mode)
  ("hs"    modi/hideshow-mode)
  ("H"     hardcore-mode)
  ("i"     modi/imenu-list-display-toggle)
  ("k"     key-chord-mode :color red)
  ("l"     modi/toggle-linum)
  ("m"     modi-mode)
  ("n"     endless/narrow-or-widen-dwim)
  ("N"     neotree-toggle)
  ("o"     modi/toggle-one-window :color red)
  ("p"     prez-mode)
  ("r"     dired-toggle-read-only) ; generalized `read-only-mode'
  ("S"     stripe-buffer-mode)
  ("t"     toggle-truncate-lines :color red)
  ("C-t"   toggle-theme)
  ("v"     visible-mode)
  ("<SPC>" whitespace-mode :color red)
  ("q"     nil "cancel" :color blue))
(bind-key "s-t" #'hydra-toggle/body)
(bind-key "C-c t" #'hydra-toggle/body)
(key-chord-define-global "hj" #'hydra-toggle/body)

;; Launcher
(defhydra hydra-launch-freq (:color teal
                             :hint  nil)
  "
._a_lias     _e_macs init     ._g_pms     _i_ndex.org     _j_ournal     _o_rg Manual     ._t_mux.conf     IEEE system_v_erilog Std     _V_erilog-mode.el
"
  ("a" (find-file
        (expand-file-name ".alias" user-home-directory)))
  ("e" (find-file
        (expand-file-name "init.el" user-emacs-directory)))
  ("g" (find-file
        (expand-file-name ".gpms" (concat user-home-directory "scripts/gpms/"))))
  ("i" (find-file
        (expand-file-name "index.org" (concat user-home-directory "public_html/"))))
  ("j" (find-file
        (expand-file-name "journal.org" org-directory)))
  ("o" (find-file
        (expand-file-name "org_man.org" (concat org-directory "org_man/"))))
  ("t" (find-file
        (expand-file-name ".tmux.conf" user-home-directory)))
  ("v" (find-file
        (expand-file-name "IEEE_STD_1800-2012_SystemVerilog.pdf"
                          (concat user-home-directory "docs/"))))
  ("V" (eww "http://www.veripool.org/ftp/verilog-mode.el"))
  ("q" nil "cancel" :color blue))
(defhydra hydra-launch (:color teal
                        :columns 7)
  "Launcher"
  ("c"       calc "calc")
  ("qc"      quick-calc "quick calc")
  ("rc"      rpn-calc "RPN calc")
  ("a"       counsel-ag "ag cwd")
  ("b"       bookmark-jump "bookmark jump")
  ("d"       dired-single-magic-buffer-current-dir "dired cwd")
  ("ed"      modi/ediff-dwim "ediff dwim")
  ("ee"      eww "eww")
  ("eb"      eww-list-bookmarks "eww bookmarks")
  ("el"      modi/eww-im-feeling-lucky "eww Lucky")
  ("eu"      (eww (browse-url-url-at-point)) "eww url at point")
  ("ev"      modi/eww-browse-url-of-file "eww current file")
  ("ff"      browse-url-firefox "firefox")
  ("fv"      browse-url-of-file "firefox current file")
  ("g"       magit-status "magit status")
  ("h"       hl-line-flash "flash line")
  ("i"       counsel-git-grep "git grep")
  ("l"        modi/run-current-file "load current file")
  ("L"       (modi/run-current-file 4) "load emacs init")
  ("m"       woman "man/woman")
  ("n"       neotree-toggle "neotree")
  ("o"       org-capture "org capture")
  ("p"       paradox-list-packages "packages")
  ;; chmod usage: s-SPC 644 P, s-SPC 400 P
  ("P"       modi/set-file-permissions "file permissions")
  ("sa"      async-shell-command "shell async cmd")
  ("ss"      shell-command "shell cmd")
  ("se"      (sx-tab-all-questions nil "emacs") "emacs.SE")
  ("t"       multi-term "terminal")
  ("u"       paradox-upgrade-packages "upgrade packages")
  ("w"       sunshine-quick-forecast "weather quick")
  ("W"       sunshine-forecast "weather full")
  ("<SPC>"   hydra-launch-freq/body "launch freq")
  ("<s-SPC>" hydra-launch-freq/body nil)
  (":"       eval-expression "eval")
  ("C-g"     nil "cancel" :color blue))
(bind-key "<s-SPC>" #'hydra-launch/body modi-mode-map)
;; Bind C-c SPC in global-map so that the org-mode-map binding is not overridden.
(bind-key "C-c SPC" #'hydra-launch/body)
(bind-key "C-c l" #'hydra-launch/body modi-mode-map)
(key-chord-define-global "jk" #'hydra-launch/body)

;; Organize The Order Of Minor Mode Lighters
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
