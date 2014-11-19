;; Time-stamp: <2014-11-19 11:33:30 kmodi>
;; Author: Kaushal Modi

;; Record the start time
(defvar *emacs-load-start* (current-time))

;; Global variables (symbols)
(setq user-home-directory  (getenv "HOME")
      user-emacs-directory (concat user-home-directory "/.emacs.d")
      org-directory        (concat user-home-directory "/org")
      setup-packages-file  (expand-file-name "setup-packages.el" user-emacs-directory)
      custom-file          (expand-file-name "custom.el" user-emacs-directory))

;; A list of packages to ensure are installed at launch
(setq my-packages
      '(
        ;; helm helm-swoop ;; Replaced with swoop
        ;; etags-select etags-table ctags-update ;; Replacing these with ggtags
        ;; zenburn-theme ;; Using my own forked version
        ;; workgroups2 ;; tested but unstable at the time of testing
        ;; fiplr ; quick file search in a project (marked by folders like .git)
        ;; ox-reveal ;; Using branch 'stable' from github; used to export to HTML slides
        ace-jump-mode
        ace-jump-zap
        ace-window
        ag wgrep wgrep-ag s ; ag > ack > grep, wgrep+wgrep-ag allow editing files directly in ag buffer
                                        ; You need to have ag installed on your machine
        anzu   ; shows total search hits in mode line, better query-replace alternative
        auto-complete fuzzy
        auto-highlight-symbol
        benchmark-init
        bookmark+ ; able to bookmark desktop sessions
        buffer-move
        cperl-mode
        csv-nav ; editing csv files
        dired+ dired-single
        discover-my-major ; Discover key bindings for the major mode
        drag-stuff
        eimp ; required by org-show
        elfeed
        elisp-slime-nav ; tag based code navigation for elisp; works even for compressed code
        expand-region
        eww-lnum ; jump to links in eww buffer ace-jump style
        fill-column-indicator
        ggtags
        guide-key
        hardcore-mode
        header2
        help-fns+
        hl-line+
        hungry-delete
        ido-vertical-mode flx-ido ido-ubiquitous
        iregister ; Interactive access to registers
        iy-go-to-char ; Go to next char which is similar to "f" and "t" in vim
        key-chord ; map pairs of simultaneously pressed keys to commands
        leuven-theme ; awesome white background theme
        linum-relative
        magit ; for git management
        manage-minor-mode
        markdown-mode
        multiple-cursors
        mwe-log-commands ; for logging commands; useful when demoing emacs
        neotree
        nlinum ; reviews say it's better than linum
        number ; number manipulation
        org htmlize poporg ; Get the latest org-mode package from MELPA
                                        ; poporg to edit comments from any other mode in org mode
        outshine outorg navi-mode ; org-mode like structure in other major modes
        paradox ; package menu improvements
        page-break-lines ; Convert the ^L (form feed) chars to horizontal lines
        popwin ; Open windows like *Help*, *Completions*, etc in minibuffer
        projectile ;; Better than fiplr
        rainbow-delimiters
        rainbow-mode
        rectangle-utils
        region-bindings-mode ; complements really well with multiple-cursors
        req-package ; optimize package loading
        smart-compile
        smart-mode-line popup rich-minority
        smex ; smart M-x
        smyx-theme ; dark theme
        stripe-buffer
        swoop
        undo-tree ; supercool undo visualization
        visual-regexp
        volatile-highlights
        web-mode
        wrap-region ; wrap selection with punctuations, tags (org-mode, markdown-mode, ..)
        xkcd ; comic
        yafolding ; indentation detected code folding
        yaml-mode ; Useful for editing Octopress' _config.yml
        yasnippet
        ))

(load custom-file) ;; Load the emacs `M-x customize` generated file
(load setup-packages-file) ;; Load the packages

(require 'defuns)
(require 'benchmark-init)
(require 'req-package)

(req-package setup-region-bindings-mode)
(req-package setup-key-chord)
(req-package modi-mode)
(req-package temp-mode)

(require 'setup-secret nil t) ;; No error if not found

;; Set up the looks of emacs
(req-package setup-popwin) ;; require popwin first as packages might depend on it
(req-package setup-smart-mode-line)
(req-package setup-visual)

;; Set up extensions/packages
(req-package setup-ace-jump-mode)
(req-package setup-ace-jump-zap)
(req-package setup-ace-window)
(req-package setup-ag)
(req-package setup-auto-complete)
(req-package setup-bookmark+)
(req-package setup-buffer-move)
(req-package setup-dired)
(req-package setup-discover-my-major)
(req-package setup-drag-stuff)
(req-package setup-elfeed)
(>=e244
 (req-package setup-eww)  ;; if emacs version >= 24.4
 (req-package setup-w3m)) ;; if emacs version <= 24.3
(req-package setup-elisp-slime-nav)
(req-package setup-expand-region)
(req-package setup-fci)
(req-package setup-guide-key)
(req-package setup-hardcore)
(req-package setup-header2 nil t) ;; No error if not found
(req-package setup-highlight)
(req-package setup-hl-line+)
(req-package setup-hungry-delete)
(req-package setup-ido)
(req-package setup-iregister)
(req-package setup-linum)
(req-package setup-magit)
(req-package setup-manage-minor-mode)
(req-package setup-multiple-cursors)
(req-package setup-neotree)
(req-package setup-number)
(req-package setup-org)
(req-package setup-orgstruct)
(req-package setup-outshine)
(req-package setup-page-break-lines)
(req-package setup-paradox)
(req-package setup-poporg)
(req-package setup-projectile)
(req-package setup-rainbow-delimiters)
(req-package setup-rainbow-mode)
(req-package setup-server)
(req-package setup-smart-compile)
(req-package setup-smex)
(req-package setup-stripe-buffer)
(req-package setup-undo-tree)
(req-package setup-wrap-region)
(req-package setup-xkcd)
(req-package setup-yafolding)
(req-package setup-yasnippet)
;; (req-package setup-fiplr)
;; (req-package setup-workgroups2)
;; (req-package setup-helm) ;; Not required; replaced with swoop

;; Languages
(req-package setup-verilog)
(req-package setup-perl)
(req-package setup-python)
(req-package setup-matlab)
(req-package setup-markdown)
(req-package setup-web-mode)
(req-package setup-yaml-mode)
(req-package setup-latex)
(req-package setup-spice)
;; (req-package setup-tcl)

(req-package setup-editing)
(req-package setup-windows-buffers)
(req-package setup-registers)
(req-package setup-navigation)
(req-package setup-search)
(req-package setup-print)
(req-package setup-gtags)
(req-package setup-spell)
(req-package setup-desktop)
(req-package setup-image)
;; (req-package setup-ctags) ; Using gtags instead

(req-package setup-misc) ; This package must be the last required package

(req-package-finish) ; Start loading packages in right order
(global-modi-mode t)
(funcall default-theme)

(setq emacs-initialized t)

;; Write out a message indicating how long it took to process the init script
(message "init.el loaded in %ds"
         (destructuring-bind (hi lo ms ps) (current-time)
           (- (+ hi lo)
              (+ (first *emacs-load-start*)
                 (second *emacs-load-start*)))))
