;; Time-stamp: <2017-09-01 07:54:51 kmodi>

;; Standard ML

(use-package sml-mode
  :ensure t
  :mode (("\\.sml\\'" . sml-mode))
  :config
  (progn
    ;; Make typing '|' insert a literal '|' only.
    (setq sml-electric-pipe-mode nil)

    ;; Undefine all the default abbrevs defined in `sml-mode.el'.
    ;; I cannot use `clear-abbrev-table' because that will clear out my
    ;; personally defined abbrevs too!
    (dolist (abbrev '("let"
                      "if"
                      "local"
                      "case"
                      "signature"
                      "structure"
                      "functor"
                      "datatype"
                      "abstype"
                      "struct"
                      "sig"
                      "val"
                      "fn"
                      "fun"))
      (define-abbrev sml-mode-abbrev-table abbrev nil))

    (setcdr (assoc "andalso" sml-font-lock-symbols-alist) "&")
    (setcdr (assoc "orelse" sml-font-lock-symbols-alist) "|")

    (defun modi/sml-mode-customization ()
      "My customization for `sml-mode'."
      ;; ;; In SML, it is perfectly fine to have the statements not ending in
      ;; ;; semicolons as below (in files, *not* in REPL!):
      ;; ;;   val x = 34
      ;; ;; But when doing so, newline with auto-indentation does not work
      ;; ;; correctly. Newline + auto-indentation works fine only if the statements
      ;; ;; are ended in semi-colons. So `electric-indent-mode will have to be
      ;; ;; disabled for this major mode.
      ;; (electric-indent-local-mode -1)
      (auto-fill-mode))
    (add-hook 'sml-mode-hook #'modi/sml-mode-customization)

    (defun modi/sml-indent-new-comment-line ()
      "Pretty block comments.

With point | in comment

  (* test| *)

calling this command will result in:

  (* test
   * |
   *)

Calling this command again will result in:

  (* test
   *
   * |
   *) "
      (interactive)
      (call-interactively #'indent-new-comment-line)
      (let (nested-empty-comment)
        ;; If the previous command results in inserting "(* *)" within the
        ;; comment block, delete that, and replace with just "*".
        ;;   (* |*)  --indent-new-comment-line-->  (*        (*
        ;;                                         (* *)  ->  * |
        ;;                                         |*)        *)
        ;;           --modi/sml-indent-new-comment-line------>
        (save-excursion
          (forward-line -1)
          (when (re-search-forward "(\\*[[:blank:]]+\\*)[[:blank:]]*" (line-end-position) :noerror)
            (setq nested-empty-comment t)
            (replace-match "*")
            (indent-according-to-mode)
            (delete-blank-lines)))
        (if nested-empty-comment
            (progn
              (indent-according-to-mode)
              (previous-line 1)
              (move-end-of-line 1))
          ;; By default "*)" is inserted at the end of the current line and the
          ;; point is moved to the next line. So now we need to remove that "*)"
          ;; on the previous line.
          ;;   (* abc| *)  --indent-new-comment-line-->  (* abc *)    (* abc
          ;;                                             (* |*)    ->  * |
          ;;                                                           *)
          ;;           --modi/sml-indent-new-comment-line----------->
          (forward-line -1)
          (re-search-forward "\\*)[[:blank:]]*\n[[:blank:]]*(\\*")
          (replace-match "\n*")
          (indent-according-to-mode)
          ;; Move the ending "*)" to its own line
          (when (looking-at-p " \\*)")
            (save-excursion
              (newline-and-indent))))
        (insert " ")))
    (bind-key "M-j" #'modi/sml-indent-new-comment-line sml-mode-map)

    (defun modi/restart-sml-and-run ()
      "Restarts the SML REPL and tries to load the correct .sml file.

The sml file loaded is the one at the top of the list returned by
`buffer-list'.

If this command is executed while in an sml buffer, the point is returned back
to that buffer after restarting the REPL and loading that file."
      (interactive)
      (let* ((in-sml-buf (derived-mode-p 'sml-mode))
             (repl-buf (get-buffer "*sml*"))
             (sml-file (if in-sml-buf
                           (buffer-file-name)
                         (catch 'break
                           (dolist (buf (buffer-list))
                             (let ((file (buffer-file-name buf)))
                               (when (and (stringp file)
                                          (string-match-p ".*\\.sml\\'" file))
                                 ;; Return the first .sml file name from the list
                                 ;; and end the loop.
                                 (throw 'break file)))))))
             (msg "Restarted SML"))
        (when repl-buf
          (when in-sml-buf
            ;; If the REPL buffer already exists and you are not in it, switch
            ;; to it.
            (switch-to-buffer-other-window repl-buf))
          ;; If the REPL has a live process, kill it
          (when (get-buffer-process (current-buffer))
            (goto-char (point-max)) ; First go to the EOB
            (comint-delchar-or-maybe-eof 1) ; Then kill the REPL (C-d)
            (sleep-for 0.1))) ; Somehow this is needed between kill and respawn
        (if sml-file
            (setq msg (concat msg (format " and now loading `%s'"
                                          (file-name-nondirectory sml-file))))
          ;; If no sml file was found in the buffer list
          (setq sml-file ""))
        (setq msg (concat msg "."))
        (message msg)
        ;; Then start a new process
        (sml-run "sml" sml-file) ; C-c C-s + typing "use foo.sml" in REPL
        ;; Switch back to the sml buffer if you executed this command from there
        (when in-sml-buf
          (switch-to-buffer-other-window (get-file-buffer sml-file)))))
    (bind-key "C-c <C-return>" #'modi/restart-sml-and-run inferior-sml-mode-map)
    (bind-key "C-c <C-return>" #'modi/restart-sml-and-run sml-mode-map)

    ;; Do not bind M-SPC to `sml-electric-space' in `sml-mode-map'.
    ;; I prefer the default binding to `just-one-space'.
    (bind-key "M-SPC" nil sml-mode-map)))


(provide 'setup-sml)
