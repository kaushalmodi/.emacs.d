;; Time-stamp: <2014-02-10 17:40:12 kmodi>
;; Author: Kaushal Modi

;; Global variables (symbols)
(setq user-emacs-directory "~/.emacs.d"
      setup-packages-file (expand-file-name "setup-packages.el" user-emacs-directory)
      custom-file         (expand-file-name "custom.el" user-emacs-directory)
      )

(defvar my-packages
  '(
    ;; projectile
    ;; header2 ;; INSTR_UNCOMMENT_THIS_LINE
    ;; highlight-symbol ;; The highlight-global package does a better job
    ace-jump-mode
    auto-complete fuzzy
    color-theme-sanityinc-solarized
    color-theme-sanityinc-tomorrow
    dired+ dired-single
    drag-stuff
    expand-region
    fill-column-indicator
    guide-key
    hardcore-mode
    helm helm-swoop
    hl-line+
    ido-vertical-mode flx-ido ido-ubiquitous
    iy-go-to-char ;; Go to next char which is similar to "f" and "t" in vim
    key-chord ;; map pairs of simultaneously pressed keys to commands
    magit ;; for git management
    markdown-mode
    multiple-cursors
    org ;; Get the latest org-mode package from Melpa
    popwin ;; Open windows like *Help*, *Completions*, etc in minibuffer
    rainbow-delimiters
    smart-compile
    smart-mode-line popup
    smex ;; smart M-x
    soft-stone-theme
    stripe-buffer
    visual-regexp
    web-mode
    yasnippet
    zenburn-theme
    )
  "A list of packages to ensure are installed at launch.")

(load setup-packages-file) ;; Load the packages
(load custom-file) ;; Load the emacs `M-x customize` generated file

;; Set up the looks of emacs
(require 'setup-visual)

;; Set up extensions/packages
(eval-after-load 'ido '(require 'setup-ido))
(require 'setup-ace-jump-mode)
(require 'setup-auto-complete)
(require 'setup-dired)
(require 'setup-drag-stuff)
(require 'setup-expand-region)
(require 'setup-fci)
(require 'setup-guide-key)
(require 'setup-hardcore)
(require 'setup-header2)
(require 'setup-helm)
(require 'setup-highlight)
(require 'setup-hl-line+)
(require 'setup-key-chord)
(require 'setup-magit)
(require 'setup-multiple-cursors)
(require 'setup-org)
(require 'setup-popwin)
(require 'setup-rainbow-delimiters)
(require 'setup-server)
(require 'setup-smart-compile)
(require 'setup-smart-mode-line)
(require 'setup-smex)
(require 'setup-stripe-buffer)
(require 'setup-visual-regexp)
(require 'setup-yasnippet)
;; (require 'setup-linum)
;; (require 'setup-projectile)

;; Languages
(require 'setup-verilog)
(require 'setup-perl)
(require 'setup-python)
(require 'setup-matlab)
(require 'setup-markdown)
(require 'setup-web-mode)
;; (require 'setup-latex)
;; (require 'setup-tcl)
;; (require 'setup-hspice)

;; custom packages
(require 'setup-sos) ;; INSTR_DELETE_THIS_LINE
(require 'setup-windows-buffers)
(require 'setup-registers)
(require 'setup-navigation)
(require 'setup-editing)
(require 'setup-search)
(require 'setup-print)
(require 'setup-desktop)
(require 'setup-misc)
;; (require 'setup-occur) ;; not required as the helm-multi-swoop-all does awesome job

;; NOTE: Load below ONLY after loading all the packages
(require 'setup-key-bindings)

(setq emacs-initialized t)
