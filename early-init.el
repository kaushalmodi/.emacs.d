;; -*- lexical-binding: t; -*-
;; Time-stamp: <2018-02-20 14:49:15 kmodi>
;; Emacs 27.x: http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=24acb31c04b4048b85311d794e600ecd7ce60d3b

;; This config uses features that do not exist in earlier releases; fail
;; here instead of at a random `void-function' later.
(when (version< emacs-version "30.1")
  (error "This Emacs config requires Emacs 30.1 or newer; this is Emacs %s"
         emacs-version))

(setq package-user-dir (let ((elpa-dir-name (format "elpa_%s" emacs-major-version))) ;default = ~/.emacs.d/elpa/
                         (file-name-as-directory (expand-file-name elpa-dir-name user-emacs-directory))))
