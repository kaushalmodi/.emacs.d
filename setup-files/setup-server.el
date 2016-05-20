;; Time-stamp: <2016-05-19 22:06:42 kmodi>

;; server/daemon setup

(use-package server
  :init
  (progn
    (setq server-auth-dir
          (let ((dir (concat user-emacs-directory
                             "server_" emacs-version-short
                             ;; Prevent server file clashes when the same emacs
                             ;; config is shared simultaneously across different
                             ;; machines (e.g. via Dropbox)
                             "_" (>=e "25.0" (system-name) system-name)
                             "/"))) ; must end with /
            (make-directory dir :parents)
            dir)))
  :config
  (progn
    ;; (setq server-use-tcp t)
    (when (equal window-system 'w32)
      ;; Suppress error "directory  ~/.emacs.d/server is unsafe". It is needed
      ;; needed for the server to start on Windows.
      ;;   On Windows, also set the EMACS_SERVER_FILE environment variable to
      ;; point to the `server' file. For example, for emacs 25.0, that location
      ;; would be "PATH\TO\.emacs.d\server_25_0\server".
      (defun server-ensure-safe-dir (dir) "Noop" t)

      ;; http://www.emacswiki.org/emacs/Edit_with_Emacs
      ;; `edit-server' package to edit the text boxes in Chrome browser using
      ;; emacsclient
      (use-package edit-server
        :ensure t
        :config
        (progn
          ;; Don't pop up a new frame
          (setq edit-server-new-frame nil)
          (setq edit-server-default-major-mode 'org-mode)
          (setq edit-server-url-major-mode-alist
                '(("github\\.com" . markdown-mode)
                  ("stackexchange\\.com" . markdown-mode)
                  ("stackoverflow\\.com" . markdown-mode)
                  ("reddit\\.com" . markdown-mode)))

          ;; Integration with Gmail
          (use-package edit-server-htmlize
            :ensure t
            :defer t
            :config
            (progn
              (add-hook 'edit-server-start-hook #'edit-server-maybe-dehtmlize-buffer)
              (add-hook 'edit-server-done-hook  #'edit-server-maybe-htmlize-buffer)))

          (edit-server-start))))

    ;; Start a server if (server-running-p) does not return t (e.g. if it
    ;; returns nil or :other)
    (or (eq (server-running-p) t)
        (server-start))))


(provide 'setup-server)
