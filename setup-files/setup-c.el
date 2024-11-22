;; Time-stamp: <2024-11-22 15:50:55 kmodi>

;; C/C++

(use-package cc-mode
  :mode (("\\.pss\\'" . c++-mode)         ;Portable Stimulus files
         ("\\.psf\\'" . c++-mode)
         ("\\.sln\\'" . c++-mode))
  :config
  (progn
    (setq-default c-basic-offset 3)

    (defun modi/sanitize-pss-file ()
      (interactive)
      (when (and (stringp (buffer-file-name))
                 (string= "pss" (file-name-extension (buffer-file-name))))
        ;; Add semi-colons after closing braces; that fixes the
        ;; auto-indentation.
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^\\(\\s-*\\}\\)\\s-*\\(//.*\\)?$" nil :noerror)
            (replace-match "\\1; \\2")))

        ;; Always a single space after commas, not before!
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "\\s-*,\\s-*" nil :noerror)
            (replace-match ", ")))

        ;; Always `bit[N]' and not `bit [N]'.
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "bit\\s-+\\[" nil :noerror)
            (replace-match "bit[")))

        ;; Single space on both sides of the `:' when deriving
        ;; actions, components, structs, etc.
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "\\(^\\|<\\)\\(\\s-*\\([a-z_]+\\s-+\\)?[a-z_]+\\s-+[a-z0-9A-Z_]+\\)\\s-*:\\s-*\\([a-z0-9A-Z_]+\\)" nil :noerror)
            ;;                              pure         component   der_c                   base_c
            (replace-match "\\1\\2 : \\4")))

        ;; ;; Auto-indent the file
        ;; (indent-region (point-min) (point-max))
        ))

    (defun modi/cc-mode-customization ()
      "My customization for `cc-mode'."
      (add-hook 'before-save-hook #'modi/sanitize-pss-file nil :local)

      ;; Replace tabs with spaces when saving files in a C/C++ mode.
      (add-hook 'before-save-hook #'modi/untabify-buffer nil :local))
    (add-hook 'c++-mode-hook #'modi/cc-mode-customization)))


(provide 'setup-c)
