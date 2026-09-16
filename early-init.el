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

;; Create the initial frame without the menu bar, tool bar and scroll bars
;; rather than removing them from setup-visual.el after the frame is already
;; on screen, which makes the frame visibly resize while starting up.
;; `menu-bar-mode', `tool-bar-mode' and `scroll-bar-mode' still toggle them
;; interactively.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Never resize the frame implicitly, in particular when a font or one of the
;; bars above changes.
(setq frame-inhibit-implied-resize t)
