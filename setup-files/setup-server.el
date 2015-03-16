;; Time-stamp: <2015-03-03 09:19:56 kmodi>

;; server setup

;; Start emacs server
;; Now all new opened files will open in the same emacs instance if
;; opened using `emacsclient FILENAME&`

;; create the server directory if it doesn't exist
(use-package server
  :init
  (progn
    (setq server-auth-dir (concat user-emacs-directory "/server_"
                                  emacs-version-short))

    (unless (file-exists-p server-auth-dir)
      (make-directory server-auth-dir)))
  :config
  (progn
    ;; Suppress error "directory  ~/.emacs.d/server is unsafe" when
    ;; running on cygwin
    (>=e "23.0"
         (when (equal window-system 'x)
           (setq server-use-tcp t)
           (defun server-ensure-safe-dir (dir) "Noop" t)))

    ;; start a server only if one is not already running
    ;; `server-running-p' returns "t" if a server is already running
    (defvar modi/server-temp nil
      "If t, start a “temp” server if a server is already running;
otherwise do nothing.")

    (if (not (server-running-p))
        (progn
          (server-start))
      (progn
        (when modi/server-temp
          (setq server-name "temp")
          (server-start))))))


(provide 'setup-server)
