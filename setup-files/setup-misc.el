;; Time-stamp: <2015-03-24 13:31:32 kmodi>

;; Miscellaneous config not categorized in other setup-* files

(fset 'yes-or-no-p 'y-or-n-p) ;; Use y or n instead of yes or no

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

;; url
;; Solve the issue with `sx.el' when using that package simultaneously in
;; different emacs versions
(setq url-configuration-directory (locate-user-emacs-file
                                   (concat "url_"
                                           emacs-version-short
                                           "/") "url/"))

;; Execute the script in current buffer
;; Source: http://ergoemacs.org/emacs/elisp_run_current_file.html
(defun xah-run-current-file (&optional eval-init)
  "Execute the current file.
For example, if the current buffer is the file xx.py, then it'll call
“python xx.py” in a shell. The file can be php, perl, python, ruby,
javascript, bash, ocaml, vb, elisp.  File suffix is used to determine
what program to run.

If the file is modified, ask if you want to save first.

If the file is emacs lisp, run the byte compiled version if exist.

If universal arg is used, load the `init.el'."
  (interactive "P")
  (let* ((suffixMap `(("py"  . "python")
                      ("rb"  . "ruby")
                      ("sh"  . "bash")
                      ("csh" . "tcsh")
                      ("pl"  . "perl")))
         (fName    (if eval-init
                       (concat user-emacs-directory "/init.el")
                     (buffer-file-name)))
         (fSuffix  (file-name-extension fName))
         (progName (cdr (assoc fSuffix suffixMap)))
         (cmdStr   (concat progName " \""   fName "\""))
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
(use-package help-fns+
    :config
  (progn
    (bind-keys
     :map help-map
     ("c"   . describe-key-briefly)
     ("C-c" . describe-command))))

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
(defhydra hydra-toggle (:color blue
                        :hint  nil)
  "
    TOGGLE ...
_a_ggressive indent^^             _e_debug^^                         fill col _i_ndicator      _p_resentation         _t_runcate lines
menu _b_ar^^                      _f_/_F_ fill/autofill              _k_ey chord               _r_ead only            _s-t_/_T_ theme
cycle _c_ase^^                    indent _g_uide^^                   _l_ine num                _s_plit horz/vert      _<SPC>_ whitespace
_d_/_D_ debug on error/entry      _H_ardcore (allow arrows)^^        _m_odi mode               _S_tripe buffer
"
  ("a"     aggressive-indent-mode)
  ("b"     modi/toggle-menu-bar)
  ("c"     xah-cycle-letter-case :color red)
  ("d"     toggle-debug-on-error)
  ("D"     modi/toggle-debug-defun)
  ("e"     modi/toggle-edebug-defun)
  ("f"     toggle-fill-unfill)
  ("F"     auto-fill-mode)
  ("g"     indent-guide-mode)
  ("H"     hardcore-mode)
  ("i"     fci-mode)
  ("k"     key-chord-mode :color red)
  ("l"     modi/toggle-linum)
  ("m"     modi-mode)
  ("n"     endless/narrow-or-widen-dwim)
  ("N"     neotree-toggle)
  ("o"     modi/toggle-one-window :color red)
  ("p"     prez-mode)
  ("r"     dired-toggle-read-only) ; generalized `read-only-mode'
  ("s"     transpose-frame :color red)
  ("S"     stripe-buffer-mode)
  ("t"     toggle-truncate-lines :color red)
  ("s-t"   toggle-theme)
  ("T"     toggle-theme)
  ("<SPC>" whitespace-mode :color red)
  ("q"     nil "cancel" :color blue))
(key-chord-define-global "qq"  #'hydra-toggle/body)
(bind-key                "s-t" #'hydra-toggle/body modi-mode-map)

;; Launcher
(defhydra hydra-launch-freq (:color teal
                             :hint  nil)
  "
._a_lias     _e_macs init     ._g_pms     _i_ndex.html     _j_ournal     ._t_mux.conf     IEEE system_v_erilog Std     _V_erilog-mode.el
"
  ("a" (find-file
        (concat user-home-directory
                "/.alias")))
  ("e" (find-file
        (concat user-emacs-directory
                "/init.el")))
  ("g" (find-file
        (concat user-home-directory
                "/scripts/gpms/.gpms")))
  ("i" (find-file
        (concat user-home-directory
                "/public_html/index.html")))
  ("j" (find-file
        (concat org-directory
                "/journal.org")))
  ("t" (find-file
        (concat user-home-directory
                "/.tmux.conf")))
  ("v" (find-file
        (concat user-home-directory
                "/docs/IEEE_STD_1800-2012_SystemVerilog.pdf")))
  ("V" (eww "http://www.veripool.org/ftp/verilog-mode.el"))
  ("q" nil "cancel" :color blue))
(defhydra hydra-launch (:color teal
                        :hint  nil)
  "
_a_g cwd             _d_ired current dir      _h_l line flash          _o_rg capture              _sa_ ^^ Async shell cmd
_b_ookmark jump      _ed_iff dwim             _l_oad current file      _p_ackage list             _ss_ ^^ Shell cmd
_cq_ Quick calc      _ee_ eww                 _L_oad init.el           _u_pgrade packages         _se_ ^^ emacs.SE
_cc_ Calc            _el_ eww Lucky           _m_an                    _P_ermissions (chmod)      _w_/_W_ quick/full weather
_cr_ Rpn calc        _f_irefox                _n_eotree                _<SPC>_ ace jump           _:_  ^^ eval
"
  ("a"       ag-regexp-cwd)
  ("b"       bookmark-jump)
  ("cq"      quick-calc)
  ("cc"      calc)
  ("cr"      rpn-calc)
  ("d"       dired-single-magic-buffer-current-dir)
  ("ed"      modi/ediff-dwim)
  ("ee"      eww)
  ("el"      modi/eww-im-feeling-lucky)
  ("eu"      (eww (browse-url-url-at-point)))
  ("f"       browse-url-firefox)
  ("h"       hl-line-flash)
  ("l"       xah-run-current-file)
  ("L"       (xah-run-current-file 4))
  ("m"       man)
  ("n"       neotree-toggle)
  ("o"       org-capture)
  ("p"       paradox-list-packages)
  ;; chmod usage: s-SPC 644 P, s-SPC 400 P
  ("P"       modi/set-file-permissions)
  ("sa"      async-shell-command)
  ("ss"      shell-command)
  ("se"      (sx-tab-newest nil "emacs"))
  ("u"       paradox-upgrade-packages)
  ("w"       sunshine-quick-forecast)
  ("W"       sunshine-forecast)
  ("<s-SPC>" hydra-launch-freq/body "Frequently Accessed Stuff")
  ("<SPC>"   ace-jump-mode)
  (":"       eval-expression)
  ("q"       nil "cancel" :color blue))
(bind-key "<s-SPC>" #'hydra-launch/body)

;; Organize The Order Of Minor Mode Lighters
(when (featurep 'multiple-cursors)
  (defun modi/promote-multiple-cursors-mode-line ()
    (interactive)
    ;; The `multiple-cursors-mode' lighter is very useful in showing how many
    ;; cursors are created or if multiple-cursors-mode is enabled. Move that
    ;; lighter to the foremost position in the mode-line. That is done by
    ;; moving it to the beginning of the `minor-mode-alist'.

    ;; If `multiple-cursors-mode is not the first in `minor-mode-alist' ..
    (when (not (equal 'multiple-cursors-mode (car (car minor-mode-alist))))
      ;; First remove it from the alist
      (remove-from-alist-matching-car minor-mode-alist multiple-cursors-mode)
      ;; Now add it back but to the beginning of the alist
      (add-to-list 'minor-mode-alist '(multiple-cursors-mode mc/mode-line))))

  (modi/promote-multiple-cursors-mode-line)
  ;; Also add the above fn to `after-revert-hook'. So in the event you don't find
  ;; `multiple-cursors-mode' to be the primary minor mode in the mode-line,
  ;; simply revert the buffer
  (add-hook 'after-revert-hook #'modi/promote-multiple-cursors-mode-line))


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
