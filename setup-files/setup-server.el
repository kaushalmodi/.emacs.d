;; Time-stamp: <2014-08-13 11:26:09 kmodi>

;; server setup

;; Start emacs server
;; Now all new opened files will open in the same emacs instance if
;; opened using `emacsclient FILENAME&`

;; create the server directory if it doesn't exist
(setq server-dir (concat user-emacs-directory "/server"))
(unless (file-exists-p server-dir)
  (make-directory server-dir))

(require 'server)

;; Suppress error "directory  ~/.emacs.d/server is unsafe" when
;; running on cygwin
(when (and (>= emacs-major-version 23)
           (equal window-system 'x))
  (setq server-use-tcp t)
  (defun server-ensure-safe-dir (dir) "Noop" t))

;; start a server only if one is not already running
;; `server-running-p' returns "t" if a server is already running
(if (not (server-running-p))
    (progn
      (server-start))
  (progn
    (setq server-name "temp")
    (server-start)))


(provide 'setup-server)
