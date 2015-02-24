;; Time-stamp: <2015-02-24 11:38:40 kmodi>
;; Author: Kaushal Modi

;; Record the start time
(defvar *emacs-load-start* (current-time))

;; Global variables (symbols)
(setq user-home-directory  (getenv "HOME")
      user-emacs-directory (concat user-home-directory "/.emacs.d")
      emacs-version-short  (replace-regexp-in-string
                            "\\([0-9]+\\)\\.\\([0-9]+\\).*"
                            "\\1_\\2" emacs-version) ; 25.0.50.1 -> 25_0
      org-directory        (concat user-home-directory "/org")
      setup-packages-file  (expand-file-name
                            "setup-packages.el"
                            user-emacs-directory)
      custom-file          (expand-file-name
                            (concat "custom_" emacs-version-short ".el")
                            user-emacs-directory))

;; A list of packages to ensure are installed at launch
(setq my-packages
      '(
        ;; etags-select etags-table ctags-update ; Replacing these with ggtags
        ;; zenburn-theme ; Using my own forked version
        ;; smyx-theme ; dark theme; Using my own forked version
        ace-jump-mode
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
        dired-single dired+
        discover-my-major ; Discover key bindings for the major mode
        drag-stuff
        eimp ; required by org-show
        elfeed
        elisp-slime-nav ; tag based code navigation for elisp; works even for compressed code
        expand-region
        eww-lnum ; jump to links in eww buffer ace-jump style
        fill-column-indicator
        gist
        git-gutter git-gutter-fringe git-gutter+ git-gutter-fringe+
        ggtags
        guide-key
        hardcore-mode
        header2
        helm helm-swoop
        help-fns+
        hl-anything
        hl-line+
        hungry-delete
        hydra
        ibuffer-projectile
        ido-vertical-mode flx-ido ido-ubiquitous
        iregister ; Interactive access to registers
        iy-go-to-char ; Go to next char which is similar to "f" and "t" in vim
        key-chord ; map pairs of simultaneously pressed keys to commands
        leuven-theme ; awesome white background theme
        linum-relative
        magit ; for git management
        manage-minor-mode
        markdown-mode
        multi-term
        multiple-cursors
        mwe-log-commands ; for logging commands; useful when demoing emacs
        neotree
        nlinum ; reviews say it's better than linum
        number ; number manipulation
        org htmlize poporg ; Get the latest org-mode package from MELPA
                                        ; poporg to edit comments from any other mode in org mode
        org-tree-slide
        ox-reveal ; Using branch 'stable' from github; used to export to HTML slides
        outshine outorg navi-mode ; org-mode like structure in other major modes
        paradox ; package menu improvements
        page-break-lines ; Convert the ^L (form feed) chars to horizontal lines
        popwin ; Open windows like *Help*, *Completions*, etc in minibuffer
        projectile ;; Better than fiplr
        rainbow-delimiters
        rainbow-mode
        rectangle-utils
        region-bindings-mode ; complements really well with multiple-cursors
        smart-compile
        smart-mode-line popup rich-minority
        smex ; smart M-x
        stripe-buffer
        sx
        tiny
        undo-tree ; supercool undo visualization
        use-package ; optimize package loading
        visual-regexp
        volatile-highlights
        web-mode
        wrap-region ; wrap selection with punctuations, tags (org-mode, markdown-mode, ..)
        xkcd ; comic
        yafolding ; indentation detected code folding
        yaml-mode ; Useful for editing Octopress' _config.yml
        yasnippet
        ))

;; Basic requires
(require 'cl)
(require 'cl-lib)

(load custom-file)         ; Load the emacs `M-x customize` generated file
(load setup-packages-file) ; Load the packages

(require 'use-package)

(use-package benchmark-init)

(use-package defuns)
(use-package modi-mode)
(use-package temp-mode)

(use-package setup-region-bindings-mode)
(use-package setup-key-chord)
(use-package setup-hydra)
;; End of basic requires

;; Set up the looks of emacs
(use-package setup-popwin) ; require popwin first as packages might depend on it
(use-package setup-smart-mode-line)
(use-package setup-visual)

;; Set up extensions/packages
(use-package setup-ace-jump-mode)
(use-package setup-ace-window)
(when (executable-find "ag")
  (use-package setup-ag))
(use-package setup-auto-complete)
(use-package setup-big-fringe)
(use-package setup-bookmark+)
(use-package setup-buffer-move)
(use-package setup-de-ansi)
(use-package setup-dired)
(use-package setup-discover-my-major)
(use-package setup-drag-stuff)
(use-package setup-elfeed)
(>=e "24.4"
     (use-package setup-eww)  ; if emacs version >= 24.4
     (use-package setup-w3m)) ; if emacs version <= 24.3
(use-package setup-elisp-slime-nav)
(use-package setup-expand-region)
(use-package setup-fci)
(use-package setup-gist)
(use-package setup-git-gutter)
(use-package setup-guide-key)
(use-package setup-hardcore)
(use-package setup-highlight)
(use-package setup-hungry-delete)
(use-package setup-ibuffer)
(use-package setup-ido)
(use-package setup-iregister)
(when (executable-find "git")
  (use-package setup-magit))
(use-package setup-manage-minor-mode)
(use-package setup-multiple-cursors)
(use-package setup-neotree)
(use-package setup-number)
(use-package setup-org)
(use-package setup-orgstruct)
(use-package setup-outshine)
(use-package setup-page-break-lines)
(use-package setup-pcache)
(use-package setup-poporg)
(use-package setup-projectile)
(use-package setup-rainbow-delimiters)
(use-package setup-rainbow-mode)
(use-package setup-server)
(use-package setup-smart-compile)
(use-package setup-smex)
(use-package setup-stripe-buffer)
(use-package setup-sunshine)
(use-package setup-sx)
(use-package setup-term)
(use-package setup-tiny)
(use-package setup-undo-tree)
(use-package setup-wrap-region)
(use-package setup-xkcd)
(use-package setup-yafolding)
(use-package setup-yasnippet)
;; (use-package setup-fiplr)
;; (use-package setup-workgroups2)

;; Languages
(use-package setup-verilog)
(use-package setup-perl)
(use-package setup-python)
(when (executable-find "matlab")
  (use-package setup-matlab))
(use-package setup-markdown)
(use-package setup-web-mode)
(use-package setup-shell)
(use-package setup-elisp)
(use-package setup-yaml-mode)
(use-package setup-latex)
(use-package setup-spice)
;; (use-package setup-tcl)

(use-package setup-editing)
(use-package setup-windows-buffers)
(use-package setup-registers)
(use-package setup-navigation)
(use-package setup-search)
(use-package setup-print)
(use-package setup-pdf)
(when (executable-find "global")
  (use-package setup-gtags))
(when (executable-find "hunspell")
  (use-package setup-spell))
(use-package setup-calc)
(use-package setup-desktop)
(use-package setup-image)
;; (use-package setup-ctags) ; Using gtags instead

;; require `setup-work' but don't trigger error if not found
(require 'setup-work nil t)

(if (daemonp)
    (add-hook 'window-setup-hook
              (λ (message ">> Daemon mode")
                ;; It is mandatory to load linum AFTER the frames are set up
                ;; Else, I get "*ERROR*: Invalid face: linum"
                (use-package setup-linum)))
  (progn
    (message ">> Non daemon mode")
    (use-package setup-linum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package setup-misc) ; This MUST be the last required package

(global-modi-mode t)

(when (bound-and-true-p emacs-initialized)
  (funcall default-theme))

(setq emacs-initialized t)

;; Write out a message indicating how long it took to process the init script
(message "init.el loaded in %ds"
         (destructuring-bind (hi lo ms ps) (current-time)
           (- (+ hi lo)
              (+ (first *emacs-load-start*)
                 (second *emacs-load-start*)))))
