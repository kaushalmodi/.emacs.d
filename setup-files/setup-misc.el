;; Time-stamp: <2015-02-20 14:45:23 kmodi>

;; Miscellaneous config not categorized in other setup-* files

(fset 'yes-or-no-p 'y-or-n-p) ;; Use y or n instead of yes or no

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

;; Load newer version of .el and .elc if both are available
(>=e "24.4"
     (setq load-prefer-newer t))

;; url
;; Solve the issue with `sx.el' when using that package simultaneously in
;; different emacs versions
(setq url-configuration-directory (locate-user-emacs-file
                                   (concat "url_"
                                           emacs-version-short
                                           "/") "url/"))

;; Execute the script in current buffer
;; Source: http://ergoemacs.org/emacs/elisp_run_current_file.html
(defun xah-run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file xx.py,
then it'll call “python xx.py” in a shell.
The file can be php, perl, python, ruby, javascript, bash, ocaml, vb, elisp.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.

If the file is emacs lisp, run the byte compiled version if exist."
  (interactive)
  (let* (
         (suffixMap
          `(
            ("py"  . "python")
            ("rb"  . "ruby")
            ("sh"  . "bash")
            ("csh" . "tcsh")
            ("pl"  . "perl")))
         (fName (buffer-file-name))
         (fSuffix (file-name-extension fName))
         (progName (cdr (assoc fSuffix suffixMap)))
         (cmdStr (concat progName " \""   fName "\""))
         IsGPG)

    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
        (save-buffer) ) )

    (when (string-equal fSuffix "gpg")
      (setq IsGPG t)
      (setq fName (replace-regexp-in-string "\.gpg$" "" fName))
      (setq fSuffix (file-name-extension fName)))

    (if (string-equal fSuffix "el") ; special case for emacs lisp
        (if IsGPG
            (load (buffer-file-name)) ; special case for .el.gpg files
          (load (file-name-sans-extension fName)))
      (if progName
          (progn
            ;; (view-echo-area-messages)
            (message "Running…")
            (shell-command cmdStr "*xah-run-current-file output*" ))
        (message "No recognized program file suffix for this file.")))))
(bind-to-modi-map "l" xah-run-current-file)

;; Help Functions +
(require 'help-fns+)
(bind-keys
 :map help-map
 ("c"   . describe-key-briefly)
 ("C-c" . describe-command))

;; Unset keys
(global-unset-key (kbd "C-z")) ;; it is bound to `suspend-frame' by default
;; suspend-frame can be called using `C-x C-z` too. And `C-z` is used as prefix
;; key in tmux. So removing the `C-z` binding from emacs makes it possible to
;; use emacs in -nw (no window) mode in tmux if needed without any key binding
;; contention.

;; Source: http://endlessparentheses.com/sweet-new-features-in-24-4.html
;; Hook `eval-expression-minibuffer-setup-hook' is run by ;; `eval-expression'
;; on entering the minibuffer.
;; Below enables ElDoc inside the `eval-expression' minibuffer.
;; Call `M-:' and type something like `(message.' to see what ElDoc does :)
(>=e "24.4"
     (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

;; Turn off dir local variables
(setq enable-dir-local-variables nil)

;; Toggles
;; http://endlessparentheses.com/the-toggle-map-and-wizardry.html
(autoload 'dired-toggle-read-only "dired" nil t)
(defhydra hydra-toggle (:color teal)
  "toggle"
  ("b"     modi/toggle-menu-bar         "menu bar")
  ("c"     xah-cycle-letter-case        "letter case" :color red)
  ("d"     toggle-debug-on-error        "debug on error")
  ("e"     modi/toggle-edebug-defun     "edebug")
  ("E"     modi/toggle-debug-defun      "debug on entry")
  ("f"     toggle-fill-unfill           "fill/unfill")
  ("F"     auto-fill-mode               "auto fill")
  ("g"     indent-guide-mode            "indent guide")
  ("i"     fci-mode                     "fci")
  ("k"     key-chord-mode               "key chord" :color red)
  ("l"     modi/toggle-linum            "linum")
  ("m"     modi-mode                    "modi mode")
  ("n"     endless/narrow-or-widen-dwim "narrow/widen")
  ("N"     neotree-toggle               "neotree")
  ("o"     modi/toggle-one-window       "one window" :color red)
  ("p"     prez-mode                    "presentation")
  ("r"     dired-toggle-read-only       "read only") ; generalized `read-only-mode'
  ("s"     transpose-frame              "split horz/vert" :color red)
  ("t"     toggle-truncate-lines        "truncate" :color red)
  ("s-t"   toggle-theme                 "theme")
  ("T"     toggle-theme                 "theme")
  ("v"     recenter-top-bottom          "recenter" :color red)
  ("<SPC>" whitespace-mode              "whitespace" :color red)
  ("<f11>" toggle-frame-maximized       "fullscreen")
  ("q"     nil                          "cancel" :color blue))
(key-chord-define-global "qq"  #'hydra-toggle/body)
(bind-key                "s-t" #'hydra-toggle/body modi-mode-map)

;; Launcher
(defhydra hydra-launch-freq (:color teal)
  "open"
  ("a" (find-file
        (concat user-home-directory
                "/.alias"))                                    ".alias")
  ("e" (find-file
        (concat user-emacs-directory
                "/init.el"))                                   "init.el")
  ("g" (find-file
        (concat user-home-directory
                "/scripts/gpms/.gpms"))                        ".gpms")
  ("i" (find-file
        (concat user-home-directory
                "/public_html/index.html"))                    "index.html")
  ("j" (find-file
        (concat org-directory
                "/journal.org"))                               "org journal")
  ("t" (find-file
        (concat user-home-directory
                "/.tmux.conf"))                                ".tmux.conf")
  ("v" (find-file
        (concat user-home-directory
                "/docs/IEEE_STD_1800-2012_SystemVerilog.pdf")) "IEEE-SV")
  ("q" nil                                                     "cancel" :color blue))
(defhydra hydra-launch (:color teal)
  "launch"
  ("a"       ag-regexp-cwd                         "ag here") ; ag in current dir
  ("b"       bookmark-jump                         "bookmark")
  ("c"       quick-calc                            "quick-calc")
  ("C"       calc                                  "calc")
  ("s-c"     rpn-calc                              "rpn-calc")
  ("D"       dired-single-magic-buffer-current-dir "dired")
  ("d"       ediff-buffers                         "ediff")
  ("ed"      ediff-buffers                         "ediff")
  ("ee"      eww                                   "eww")
  ("el"      modi/eww-im-feeling-lucky             "eww lucky")
  ("eu"      (eww (browse-url-url-at-point))       "open url in eww")
  ("f"       browse-url-firefox                    "firefox")
  ("m"       man                                   "man")
  ("n"       neotree-toggle                        "neotree")
  ("o"       org-capture                           "org capture")
  ("p"       paradox-list-packages                 "packages")
  ;; chmod usage: s-SPC 644 P, s-SPC 400 P
  ("P"       modi/set-file-permissions             "chmod")
  ("sa"      async-shell-command                   "async shell cmd")
  ("ss"      shell-command                         "shell cmd")
  ("w"       sunshine-quick-forecast               "weather (quick)")
  ("W"       sunshine-forecast                     "weather (full)")
  ("<s-SPC>" hydra-launch-freq/body                "freq files")
  ("<SPC>"   ace-jump-mode                         "ace jump")
  (":"       eval-expression                       "eval")
  ("q"       nil                                   "cancel" :color blue))
(bind-key "<s-SPC>" #'hydra-launch/body)

;; Vi-mode
;; http://oremacs.com/2015/02/05/amaranth-hydra/
(req-package setup-iregister) ; To get the defalias definitions
(defun hydra-vi/pre ()
  (set-cursor-color "#e52b50"))
(defun hydra-vi/post ()
  "`hcz-set-cursor-color-color' variable is set in `setup-visual.el'"
  (set-cursor-color hcz-set-cursor-color-color))
(defun hydra-vi/end-of-buffer (&optional arg)
  (interactive "P")
  (let* ((numeric-arg (if (consp arg) (car arg) arg))
         (pos-arg (if (and arg (< numeric-arg 0)) (- 0 numeric-arg) numeric-arg)))
    (if arg
        (goto-line pos-arg) ; go to a line if argument is specified
      (goto-char (point-max))))) ; end of buffer
(defun hydra-vi/beginning-of-buffer (&optional arg)
  (interactive "P")
  (let* ((numeric-arg (if (consp arg) (car arg) arg))
         (pos-arg (if (and arg (< numeric-arg 0)) (- 0 numeric-arg) numeric-arg)))
    (if arg
        (goto-line pos-arg) ; go to a line if argument is specified
      (goto-char (point-min))))) ; beginning of buffer

(defhydra hydra-vi (:pre hydra-vi/pre :post  hydra-vi/post :color amaranth)
  "vi"
  ;; basic navigation
  ("l"        forward-char                 nil)
  ("h"        backward-char                nil)
  ("j"        next-line                    nil)
  ("k"        previous-line                nil)
  ;; mark
  ("m"        set-mark-command             "mark")
  ("C-o"      (set-mark-command 4)         "jump to prev location")
  ;; beginning/end of line
  ("a"        back-to-indentation-or-beginning-of-line "beg of line/indentation")
  ("^"        back-to-indentation-or-beginning-of-line "beg of line/indentation")
  ("$"        move-end-of-line             "end of line")
  ;; word navigation
  ("e"        forward-word                 "end of word")
  ("w"        modi/forward-word-begin      "beg of next word")
  ("b"        backward-word                "beg of word")
  ;; page scrolling
  ("<prior>"  scroll-down-command          "page up")
  ("<next>"   scroll-up-command            "page down")
  ;; delete/cut/copy/paste
  ("x"        delete-forward-char          "del char")
  ("d"        my/iregister-cut             "cut/del")
  ("D"        smart-kill-whole-line        "cut/del line")
  ("y"        my/iregister-copy            "copy")
  ("p"        yank                         "paste" :color blue)
  ;; beginning/end of buffer and go to line
  ("g"        hydra-vi/beginning-of-buffer "beg of buffer/goto line")
  ("G"        hydra-vi/end-of-buffer       "end of buffer/goto line")
  ("<return>" goto-line                    "goto line")
  ;; undo/redo
  ("u"        undo-tree-undo               "undo")
  ("C-r"      undo-tree-redo               "redo")
  ;; misc
  ("<SPC>"    ace-jump-mode                "ace jump" :color blue)
  ;; quit
  ("q"        nil                          "quit" :color blue))
(bind-key "C-:" #'hydra-vi/body modi-mode-map)


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
;; Example: `C-h v`, `tooltip-mode`, Enter
;;
;; (4) How to insert superscript
;; `C-x 8 ^ 2` inserts ²
;;
;; (5) Killing buffers from an emacsclient frame
;; `C-x #`   Kills the buffer in emacsclient frame without killing the frame
;; `C-x 5 0` Kills the emacsclient frame
;;
;; (6)
;; `C-q' is bound to `quoted-insert'
;; Example: Pressing `C-q C-l' inserts the `^l' character (form feed):  
;;
;; (7)
;; The way to figure out how to type a particular key combination or to know
;; what a particular key combination does, do help on a key `C-h k`, and type
;; the keystrokes you're interested in. What Emacs shows in the Help buffer is
;; the string you can pass to the macro 'kbd.
;;
;; (8) How to know what the current major mode is?
;; Do `M-:`, type the following `(message "%s" major-mode)` and press Return.
;;
;; (9) Put this line at the top of an anything.gpg file to prevent it from
;; asking for the password on each save
;; -*- epa-file-encrypt-to: ("<MY_EMAIL>") -*-
;;
;;
