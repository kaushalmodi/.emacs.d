;; Time-stamp: <2015-03-31 10:15:10 kmodi>
;; Author: Kaushal Modi

;; Record the start time
(setq *emacs-load-start* (current-time))

;; (setq debug-on-message "Making tags")

;; Global variables
(setq user-home-directory  (getenv "HOME"))
(setq user-emacs-directory (concat user-home-directory "/.emacs.d"))
(setq emacs-version-short  (replace-regexp-in-string
                            "\\([0-9]+\\)\\.\\([0-9]+\\).*"
                            "\\1_\\2" emacs-version)) ; 25.0.50.1 -> 25_0
(setq org-directory        (concat user-home-directory "/org"))
(setq setup-packages-file  (expand-file-name
                            "setup-packages.el"
                            user-emacs-directory))
(setq custom-file          (expand-file-name
                            (concat "custom_" emacs-version-short ".el")
                            user-emacs-directory))

;; A list of packages to ensure are installed at launch
(setq my-packages
      '(
        ;; git-gutter git-gutter-fringe git-gutter+ git-gutter-fringe+ ; < diff-hl
        ace-jump-mode
        ace-window
        ag wgrep wgrep-ag s ; ag > ack > grep
                                        ; wgrep+wgrep-ag allow editing files
                                        ; directly in ag buffer
        aggressive-indent
        anzu   ; shows total search hits in mode line, > query-replace
        auto-complete fuzzy
        auto-highlight-symbol
        benchmark-init
        bookmark+ ; able to bookmark desktop sessions
        buffer-move
        cperl-mode
        csv-nav ; editing csv files
        diff-hl
        dired-single dired+
        discover-my-major ; Discover key bindings for the major mode
        drag-stuff
        eimp ; required by org-show
        elfeed
        elisp-slime-nav ; tag based code navigation for elisp; works even for compressed code
        expand-region
        eww-lnum ; jump to links in eww buffer ace-jump style
        fill-column-indicator
        fold-this
        gist
        gplusify ; copy region with formatting for G+ posts
        etags-select etags-table ctags-update ggtags helm-gtags
        guide-key
        hardcore-mode
        header2
        helm helm-swoop
        help-fns+
        ;; hl-anything ; temporarily removing it, bugs
        hl-line+
        hungry-delete
        hydra
        ibuffer-projectile
        ido-vertical-mode flx-ido ido-ubiquitous
        indent-guide
        interleave ; takes notes associated to pdf files in org mode
        iregister ; Interactive access to registers
        iy-go-to-char ; Go to next char which is similar to "f" and "t" in vim
        key-chord ; map pairs of simultaneously pressed keys to commands
        keyfreq ; find which commands you use the most
        kurecolor ; library to tweak colors
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
        projectile ; Better than fiplr
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
        zop-to-char

        ;; Themes
        ;; zenburn-theme ; < forked version
        ;; smyx-theme ; < forked version
        ample-theme ; ample, ample-flat, ample-light
        darktooth-theme ; coffee
        leuven-theme ; awesome white background theme
        planet-theme ; dark blue
        twilight-bright-theme
        twilight-anti-bright-theme
        ))

;; Basic requires
(require 'cl)
(require 'cl-lib)

(load custom-file nil :nomessage) ; Load the emacs `M-x customize` generated file

(load setup-packages-file nil :nomessage) ; Load the packages

;; Optional file containing `setq' statements to disable loading of selected
;; packages. Format to be used: (setq disable-pkg-PKGNAME t)
;; Example: (setq disable-pkg-pdf-tools t)
;; Place `setup-disables.el' with `(provide 'setup-disables)' in `setup-files/'
(require 'setup-disables nil :noerror)

;; Start `benchmark-init' as soon as possible
(require 'benchmark-init)

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure nil))
(require 'bind-key)
(require 'defuns)
(require 'modi-mode)
(require 'temp-mode)
(require 'setup-paradox)

(require 'setup-region-bindings-mode)
(require 'setup-key-chord)
(require 'setup-hydra)
(require 'setup-tags) ; It's important that this is required up-front, at least
                      ; before requiring `setup-projectile'
;; End of basic requires

;; Set up the looks of emacs
(require 'setup-popwin) ; require popwin first as packages might depend on it
(require 'setup-smart-mode-line)
(require 'setup-visual)

;; Set up extensions/packages
(require 'setup-ace-jump-mode)
(require 'setup-ace-window)
(when (executable-find "ag")
  (require 'setup-ag))
(require 'setup-aggressive-indent)
(require 'setup-auto-complete)
(require 'setup-big-fringe)
(require 'setup-bookmark+)
(require 'setup-buffer-move)
(require 'setup-de-ansi)
(require 'setup-dired)
(require 'setup-discover-my-major)
(require 'setup-drag-stuff)
(require 'setup-elfeed)
(>=e "24.4"
     (require 'setup-eww)  ; if emacs version >= 24.4
     (require 'setup-w3m)) ; if emacs version <= 24.3
(require 'setup-elisp-slime-nav)
(require 'setup-expand-region)
(require 'setup-fci)
(require 'setup-fold-this)
(require 'setup-gist)
(require 'setup-guide-key)
(require 'setup-hardcore)
(require 'setup-highlight)
(require 'setup-hungry-delete)
(require 'setup-ibuffer)
(require 'setup-ido)
(require 'setup-iregister)
(when (executable-find "git")
  (require 'setup-magit)
  (require 'setup-diff-hl)
  (require 'setup-git-link))
(require 'setup-keyfreq)
(require 'setup-kurecolor)
(require 'setup-manage-minor-mode)
(require 'setup-multiple-cursors)
(require 'setup-neotree)
(require 'setup-number)
(require 'setup-org)
(require 'setup-orgstruct)
(require 'setup-outshine)
(require 'setup-page-break-lines)
(require 'setup-pcache)
(require 'setup-poporg)
(require 'setup-projectile)
(require 'setup-rainbow-delimiters)
(require 'setup-rainbow-mode)
(require 'setup-server)
(require 'setup-smart-compile)
(require 'setup-smex)
(require 'setup-stripe-buffer)
(require 'setup-sunshine)
(require 'setup-sx)
(require 'setup-term)
(require 'setup-tiny)
(require 'setup-undo-tree)
(require 'setup-which-func)
(require 'setup-wrap-region)
(require 'setup-xkcd)
(require 'setup-yafolding)
(require 'setup-yasnippet)
;; (require 'setup-fiplr)
;; (require 'setup-workgroups2)

;; Languages
(require 'setup-verilog)
(require 'setup-perl)
(require 'setup-python)
(when (executable-find "matlab")
  (require 'setup-matlab))
(require 'setup-markdown)
(require 'setup-web-mode)
(require 'setup-shell)
(require 'setup-elisp)
(require 'setup-yaml-mode)
(require 'setup-latex)
(require 'setup-spice)
(require 'setup-tcl)

(require 'setup-editing)
(require 'setup-windows-buffers)
(require 'setup-registers)
(require 'setup-navigation)
(require 'setup-search)
(require 'setup-print)
(require 'setup-pdf)
(when (or (executable-find "aspell")
          (executable-find "hunspell"))
  (require 'setup-spell))
(require 'setup-calc)
(require 'setup-desktop)
(require 'setup-image)

(require 'setup-work nil :noerror)

(defun post-window-setup-stuff ()
  ;; It is mandatory to load linum AFTER the frames are set up
  ;; Else, I get "*ERROR*: Invalid face: linum"
  (require 'setup-linum)

  ;; Place `setup-personal.el' with `(provide 'setup-personal)' in `setup-files/'
  (require 'setup-personal nil :noerror))

(if (daemonp)
    (add-hook 'window-setup-hook
              (λ (message ">> Daemon mode")
                (post-window-setup-stuff)))
  (progn
    (message ">> Non daemon mode")
    (post-window-setup-stuff)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'setup-misc) ; This MUST be the last required package

(global-modi-mode t)

(when (and (bound-and-true-p emacs-initialized)
           (featurep 'setup-visual))
  (funcall default-theme-fn)) ; defined in setup-visual.el

(setq emacs-initialized t)

;; Write out a message indicating how long it took to process the init script
(message "init.el loaded in %ds"
         (destructuring-bind (hi lo ms ps) (current-time)
           (- (+ hi lo)
              (+ (first *emacs-load-start*)
                 (second *emacs-load-start*)))))
