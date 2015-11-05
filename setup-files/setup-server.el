;; Time-stamp: <2015-11-05 12:12:04 kmodi>

;; server/daemon setup

(use-package server
  :init
  (progn
    (setq server-auth-dir (let ((dir (concat user-emacs-directory
                                             "server_" emacs-version-short
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
      (defun server-ensure-safe-dir (dir) "Noop" t))

    ;; Start a server only if one is not already running
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
