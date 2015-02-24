;; Time-stamp: <2015-02-24 09:53:55 kmodi>

;; Updating the #+INCLUDE source code line numbers automatically
;; http://emacs.stackexchange.com/q/64/115

;; Here is another option. This one is let's you customize the regular
;; expressions on a per-include basis. It should fit better with some
;; workflows as you're not limited to extension-based definitions.

;; * To Use *
;; Do something like the following in your org-file. (The :lines keyword
;; is optional)

;; #+INCLUDE: "code/my-class.sv" :src systemverilog :range-begin "^class"
;;  :range-end "^endclass" :lines "14-80"

;; The function will visit "my-class.sv" and search for those two
;; regexps, and then it will update the :lines keyword according with the
;; match result.

;; If :range-begin is missing, the range will be "-80".
;; If :range-end is missing, the range will be "14-".
;; If both of the above are missing, the `:lines' won't be auto updated

;; Execute the `endless/update-includes' function just before saving the file
(add-hook 'before-save-hook #'endless/update-includes)

;; * The Updater *
;; This is the function that goes through the buffer. You can bind it
;; to a key, or add it to a hook. The following code updates the lines
;; whenever you save the file, but if your use case is different, just
;; find out which hook you need! (org-mode is full of hooks)

(defun endless/update-includes (&rest ignore)
  "Update the line numbers of #+INCLUDE:s in current buffer.
Only looks at INCLUDEs that have either :range-begin or :range-end.
This function does nothing if not in org-mode, so you can safely
add it to `before-save-hook'."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              "^\\s-*#\\+INCLUDE: *\"\\([^\"]+\\)\".*:range-\\(begin\\|end\\)"
              nil 'noerror)
        (let* ((file (expand-file-name (match-string-no-properties 1)))
               lines begin end)
          (forward-line 0)
          (when (looking-at "^.*:range-begin *\"\\([^\"]+\\)\"")
            (setq begin (match-string-no-properties 1)))
          (when (looking-at "^.*:range-end *\"\\([^\"]+\\)\"")
            (setq end (match-string-no-properties 1)))
          (setq lines (endless/decide-line-range file begin end))
          (when lines
            (if (looking-at ".*:lines *\"\\([-0-9]+\\)\"")
                (replace-match lines :fixedcase :literal nil 1)
              (goto-char (line-end-position))
              (insert " :lines \"" lines "\""))))))))

;; * The background worker *
;; This is the guy that does most of the work.

(defun endless/decide-line-range (file begin end)
  "Visit FILE and decide which lines to include.
BEGIN and END are regexps which define the line range to use."
  (let (l r)
    (save-match-data
      (with-temp-buffer
        (insert-file file)
        (goto-char (point-min))
        (if (null begin)
            (setq l "")
          (search-forward-regexp begin)
          (setq l (line-number-at-pos (match-beginning 0))))
        (if (null end)
            (setq r "")
          (search-forward-regexp end)
          (setq r (1+ (line-number-at-pos (match-end 0)))))
        (format "%s-%s" l r)))))


(provide 'org-include-src-lines)
