;; Time-stamp: <2014-05-08 11:22:40 kmodi>
;; Author: Kaushal Modi

;; Global variables (symbols)
(setq user-emacs-directory "~/.emacs.d"
      setup-packages-file (expand-file-name "setup-packages.el" user-emacs-directory)
      custom-file         (expand-file-name "custom.el" user-emacs-directory)
      )

;; A list of packages to ensure are installed at launch
(setq my-packages
  '(
    ;; projectile
    ;; header2 ;; INSTR_UNCOMMENT_THIS_LINE
    ace-jump-mode
    ag wgrep wgrep-ag ;; ag > ack > grep, wgrep+wgrep-ag allow editing files directly in ag buffer
                      ;; You need to have ag installed on your machine
    anzu ;; shows total search hits in mode line, better query-replace alternative
    auto-complete fuzzy
    auto-highlight-symbol
    benchmark-init
    bookmark+ ;; able to bookmark desktop sessions
    cperl-mode
    dired+ dired-single
    drag-stuff
    etags-select etags-table ctags-update ;; ac-etags
    expand-region
    fill-column-indicator
    guide-key
    hardcore-mode
    helm helm-swoop
    hl-line+
    ido-vertical-mode flx-ido ido-ubiquitous
    iregister ;; Interactive access to registers
    iy-go-to-char ;; Go to next char which is similar to "f" and "t" in vim
    key-chord ;; map pairs of simultaneously pressed keys to commands
    leuven-theme ;; awesome white background theme
    magit ;; for git management
    manage-minor-mode
    markdown-mode
    multiple-cursors
    mwe-log-commands ;; for logging commands; useful when demoing emacs
    nlinum ;; reviews say it's better than linum
    org ;; Get the latest org-mode package from Melpa
    popwin ;; Open windows like *Help*, *Completions*, etc in minibuffer
    rainbow-delimiters
    region-bindings-mode ;; complements really well with multiple-cursors
    smart-compile
    smart-mode-line popup
    smex ;; smart M-x
    stripe-buffer
    sublimity ;; smooth scrolling
    undo-tree ;; supercool undo visualization
    visual-regexp
    volatile-highlights
    w3m ;; web-browsing in emacs
    web-mode
    xkcd ;; comic
    yaml-mode ;; Useful for editing Octopress' _config.yml
    yasnippet
    zenburn-theme
    ))

(load setup-packages-file) ;; Load the packages
(load custom-file) ;; Load the emacs `M-x customize` generated file

(require 'benchmark-init)

;; Set up the looks of emacs
(require 'setup-popwin) ;; require popwin first as packages might depend on it
(require 'setup-visual)

;; Set up extensions/packages
(eval-after-load 'ido '(require 'setup-ido))
(require 'setup-ace-jump-mode)
(require 'setup-ag)
(require 'setup-auto-complete)
(require 'setup-bookmark+)
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
(require 'setup-iregister)
(require 'setup-key-chord)
(require 'setup-linum)
(require 'setup-magit)
(require 'setup-manage-minor-mode)
(require 'setup-multiple-cursors)
(require 'setup-org)
(require 'setup-rainbow-delimiters)
(require 'setup-server)
(require 'setup-smart-compile)
(require 'setup-smart-mode-line)
(require 'setup-smex)
(require 'setup-stripe-buffer)
(require 'setup-sublimity)
(require 'setup-undo-tree)
(require 'setup-visual-regexp)
(require 'setup-w3m)
(require 'setup-xkcd)
(require 'setup-yasnippet)
;; (require 'setup-projectile)

;; Languages
(require 'setup-verilog)
(require 'setup-perl)
(require 'setup-python)
(require 'setup-matlab)
(require 'setup-markdown)
(require 'setup-web-mode)
(require 'setup-yaml-mode)
;; (require 'setup-latex)
;; (require 'setup-tcl)
;; (require 'setup-hspice)

(require 'setup-sos) ;; INSTR_DELETE_THIS_LINE
(require 'setup-windows-buffers)
(require 'setup-registers)
(require 'setup-navigation)
(require 'setup-editing)
(require 'setup-search)
(require 'setup-print)
(require 'setup-desktop)
(require 'setup-ctags)
(require 'setup-misc)
;; (require 'setup-occur) ;; not required as the helm-multi-swoop-all does awesome job

;; Stuff that can't be committed publicly on github
(require 'setup-secret) ;; INSTR_DELETE_THIS_LINE
;;

;; NOTE: Load below ONLY after loading all the packages
;; region-bindings-mode has bindings for multiple-cursors, visual-regexp, anzu
(require 'setup-region-bindings-mode)
(require 'setup-key-bindings)

(setq emacs-initialized t)
