;; Time-stamp: <2014-08-14 12:13:28 kmodi>

;; Miscellaneous config not categorized in other setup-* files

(fset 'yes-or-no-p 'y-or-n-p) ;; Use y or n instead of yes or no

;; Enable conversion of the selected region to upper case using `C-x C-u`
(put 'upcase-region 'disabled nil)

;; Enable conversion of the selected region to lower case using `C-x C-l`
(put 'downcase-region 'disabled nil)

;; Do not make mouse wheel accelerate its action (example: scrolling)
(setq mouse-wheel-progressive-speed nil)

;; Quitting emacs via `C-x C-c` or the GUI 'X' button
(setq confirm-kill-emacs 'y-or-n-p)

;; Enable shell-script mode for these files automatically
(setq auto-mode-alist
      (append
       '(
         ("\\.setup\\'" . shell-script-mode)
         ("\\.cfg\\'"   . shell-script-mode)
         ) auto-mode-alist))

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
         (cmdStr (concat progName " \""   fName "\"")))

    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
        (save-buffer) ) )

    (if (string-equal fSuffix "el") ; special case for emacs lisp
        (load (file-name-sans-extension fName))
      (if progName
          (progn
            ;; (view-echo-area-messages)
            (message "Running…")
            (shell-command cmdStr "*xah-run-current-file output*" ))
        (message "No recognized program file suffix for this file.")))))

;; Print to printer defined by env var `PRINTER'
(bind-to-modi-map "p" ps-print-buffer-with-faces)
(bind-to-modi-map "l" xah-run-current-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unset keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-unset-key (kbd "C-z")) ;; it is bound to `suspend-frame' by default
;; suspend-frame can be called using `C-x C-z` too. And `C-z` is used as prefix
;; key in tmux. So removing the `C-z` binding from emacs makes it possible to
;; use emacs in -nw (no window) mode in tmux if needed without any key binding
;; contention.

;; Kill emacs when running in daemon mode or not
;; Source: http://lists.gnu.org/archive/html/emacs-devel/2011-11/msg00348.html
(defun tv-stop-emacs ()
  (interactive)
  (if (daemonp)
      (save-buffers-kill-emacs)
    (save-buffers-kill-terminal)))

;; The emacs-quitting feature is useful whether or not my minor map is loaded
;; So bind the keys globally instead of to the minor mode map.
(if desktop-save-mode
  (bind-keys
   ("C-x C-c" . save-desktop-save-buffers-kill-emacs)
   ("C-x M-c" . tv-stop-emacs))
  (bind-key "C-x C-c" 'tv-stop-emacs))


(provide 'setup-misc)


;; TIPS

;; (1) Un-define a symbol/variable
;; this will make the symbol my-nasty-variable's value void
;; (makunbound 'my-nasty-variable)

;; (2) Un-define a function
;; this will make the symbol my-nasty-function's
;; function definition void
;; (fmakunbound 'my-nasty-function)

;; (3) See a variable value
;; `C-h v`, enter the variable name, Enter
;; Example: `C-h v`, `tooltip-mode`, Enter

;; (4) How to insert superscript
;; `C-x 8 ^ 2` inserts ²

;; (5) Killing buffers from an emacsclient frame
;; `C-x #`   Kills the buffer in emacsclient frame without killing the frame
;; `C-x 5 0` Kills the emacsclient frame

;; (6)
;; `C-q' is boudn to `quoted-insert'
;; Example: Pressing `C-q C-l' inserts the `^l' character (form feed):  

;; (7)
;; The way to figure out how to type a particular key combination or to know
;; what a particular key combination does, do help on a key `C-h k`, and type
;; the keystrokes you're interested in. What Emacs shows in the Help buffer is
;; the string you can pass to the macro 'kbd.

;; (8) How to know what the current major mode is?
;; Do `M-:`, type the following `(message "%s" major-mode)` and press Return.
