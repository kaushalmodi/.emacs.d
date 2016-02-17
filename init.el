;; Time-stamp: <2016-02-17 13:46:36 kmodi>
;; Author: Kaushal Modi

;; Global variables
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq gc-cons-threshold--orig gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024)) ; 100 MB before garbage collection

(setq user-home-directory     (concat (getenv "HOME") "/")) ; must end with /
(setq user-emacs-directory    (concat user-home-directory ".emacs.d/")) ; must end with /
(setq emacs-version-short     (replace-regexp-in-string
                               "\\([0-9]+\\)\\.\\([0-9]+\\).*"
                               "\\1_\\2" emacs-version)) ; 25.0.50.1 -> 25_0
(setq org-directory           (let ((dir (concat user-home-directory
                                                 "org/"))) ; must end with /
                                (make-directory dir :parents)
                                dir))
(setq setup-packages-file     (locate-user-emacs-file "setup-packages.el"))
(setq custom-file             (locate-user-emacs-file
                               (concat "custom_" emacs-version-short ".el")))
(setq user-personal-directory (let ((dir (concat user-emacs-directory
                                                 "personal/"))) ; must end with /
                                (make-directory dir :parents)
                                dir))

;; A list of packages to ensure are installed at launch
(setq my-packages
      '(
        ;; git-gutter git-gutter-fringe git-gutter+ git-gutter-fringe+ ; < diff-hl
        ;; popwin ; < shackle
        ;; ox-reveal ; used to export to HTML slides; using latest from git
        ;; helm helm-swoop ; < swiper
        ;; helm-gtags ; < ggtags
        ace-window
        adaptive-wrap ; indented line wrapping
        ag wgrep wgrep-ag s ; ag > ack > grep
                                        ; wgrep+wgrep-ag allow editing files
                                        ; directly in ag buffer
        aggressive-indent
        all all-ext ; edit ALL lines matching regex
        anzu   ; shows total search hits in mode line, > query-replace
        ascii-art-to-unicode
        auto-complete fuzzy
        auto-highlight-symbol
        avy ; > ace-jump-mode
        beacon ; visual flash to show the cursor position
        benchmark-init
        bm ; visual bookmarks
        bookmark+ ; able to bookmark desktop sessions
        buffer-move
        command-log-mode ; for logging commands; useful when demoing emacs
        cperl-mode
        csv-nav ; editing csv files
        deft ; quick note taking and management
        diff-hl
        dired-single dired+
        discover-my-major ; Discover key bindings for the major mode
        drag-stuff
        easy-escape ; Make the \\ escape chars more pleasant looking in elisp regexps
        elfeed
        elisp-slime-nav ; tag based code navigation for elisp; works even for compressed code
        engine-mode ; search engines
        expand-region
        eww-lnum ; jump to links in eww buffer ace-jump style
        fill-column-indicator
        fold-this
        gist
        git-link ; get git links with line numbers and commit-hash/branch
        git-timemachine ; walk through git revisions
        ggtags etags-select etags-table ctags-update
        hardcore-mode
        header2
        help-fns+
        hideshow-org hideshowvis
        htmlize
        hl-line+
        hungry-delete
        hydra
        ibuffer-projectile
        ido-vertical-mode flx-ido ido-ubiquitous
        imenu-list
        indent-guide
        info+
        interleave ; takes notes associated to pdf files in org mode
        iregister ; Interactive access to registers
        isend-mode ; used in setup-perl.el
        iy-go-to-char ; Go to next char which is similar to "f" and "t" in vim
        key-chord ; map pairs of simultaneously pressed keys to commands
        keyfreq ; find which commands you use the most
        kurecolor ; library to tweak colors
        linum-relative
        list-environment
        magit ; for git management
        manage-minor-mode
        markdown-mode
        minibuffer-line
        multi-term
        multiple-cursors
        neotree
        nlinum ; reviews say it's better than linum
        org-cliplink ; paste copied links as well-formatted org-mode links with desc
        org-plus-contrib ; latest stable version of org-mode, includes org-eww
        org-tree-slide
        outorg navi-mode ; supporting packages for outshine
        outshine ; org-mode navigation and organization outside org-mode
        ox-twbs ; export to twitter bootstrap html
        paradox ; package menu improvements
        page-break-lines ; Convert the ^L (form feed) chars to horizontal lines
        pomodoro
        poporg ; edit comments from any other mode in org mode
        projectile ; Better than fiplr
        rainbow-delimiters
        rainbow-mode
        region-bindings-mode ; complements really well with multiple-cursors
        rectangle-utils
        rpn-calc
        shackle
        smart-compile
        smart-mark
        smart-mode-line popup rich-minority
        smex ; smart M-x
        stripe-buffer
        sunshine forecast ; weather
        swiper counsel
        sx
        tiny
        transpose-frame ; for the priceless `rotate-frame' and `transpose-frame'
        undo-tree ; supercool undo visualization
        use-package use-package-chords ; optimize package loading
        visual-regexp
        volatile-highlights
        web-mode
        which-key ; > guide-key
        wordnut ; offline dictionary (requires installing Wordnet wn application)
        wrap-region ; wrap selection with punctuations, tags (org-mode, markdown-mode, ..)
        writegood-mode ; highlight passive voice, weasel words and duplicates
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
        tao-theme ; monochrome
        twilight-bright-theme
        twilight-anti-bright-theme
        ))

;; Basic requires
(require 'cl)
(require 'cl-lib)

;; Place `setup-var-overrides.el' with `(provide 'setup-var-overrides)' in
;; `user-personal-directory'
(add-to-list 'load-path user-personal-directory)
(require 'setup-var-overrides nil :noerror)

(load custom-file :noerror :nomessage) ; Load the emacs `M-x customize` generated file

(load setup-packages-file nil :nomessage) ; Load the packages
;; (package-initialize) ; Do NOT delete this comment
;;   In emacs 25+, the `package-initialize' call is auto-added to the top of
;; init.el unless the user already has a commented or uncommented
;; `(package-initialize)' line present in their init.el.
;;   I call this function in setup-packages.el and so am keeping the
;; commented out version here so that package.el does not add it again.

;; Start `benchmark-init' as soon as possible
(require 'benchmark-init)

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure nil))
(require 'bind-key)
(require 'use-package-chords)
(require 'defuns)

;; Enable `modi-mode' unless `disable-pkg-modi-mode' is set to `t' in
;; `setup-var-overrides.el'.
(when (not (bound-and-true-p disable-pkg-modi-mode))
  (require 'modi-mode))
(require 'temp-mode)

(require 'setup-paradox)
(require 'setup-region-bindings-mode)
(require 'setup-key-chord)
(require 'setup-hydra)
(require 'setup-tags) ; It's important that this is required at least
                                        ; before requiring `setup-projectile'
;; End of basic requires

;; Set up the looks of emacs
(require 'setup-mode-line)
(require 'setup-visual)
(if (bound-and-true-p disable-pkg-shackle)
    (require 'setup-popwin)
  (require 'setup-shackle))

;; Set up packages
(require 'setup-abbrev)
(require 'setup-ace-window)
(when (executable-find "ag")
  (require 'setup-ag))
(require 'setup-aggressive-indent)
(require 'setup-all)
(require 'setup-artist)
(require 'setup-auto-complete)
(require 'setup-beacon)
(require 'setup-big-fringe)
(require 'setup-bookmarks)
(require 'setup-buffer-move)
(require 'setup-calc)
(require 'setup-command-log-mode)
(require 'setup-counsel)
(require 'setup-de-ansi)
(require 'setup-deft)
(require 'setup-dired)
(require 'setup-discover-my-major)
(require 'setup-drag-stuff)
(require 'setup-elfeed)
(>=e "24.4"
    (require 'setup-eww))
(require 'setup-elisp-slime-nav)
(require 'setup-engine-mode)
(require 'setup-expand-region)
(require 'setup-fci)
(require 'setup-fold)
(require 'setup-gist)
(when (executable-find "git")
  (require 'setup-diff)
  (require 'setup-git-link)
  (require 'setup-git-timemachine)
  (require 'setup-magit))
(require 'setup-hardcore)
(require 'setup-header2)
(require 'setup-highlight)
(require 'setup-htmlize)
(require 'setup-hungry-delete)
(require 'setup-ibuffer)
(require 'setup-ido)
(require 'setup-imenu-list)
(require 'setup-indent-guide)
(require 'setup-iregister)
(require 'setup-ivy)
(require 'setup-keyfreq)
(require 'setup-kurecolor)
(require 'setup-list-environment)
(require 'setup-manage-minor-mode)
(require 'setup-multiple-cursors)
(require 'setup-neotree)
(require 'setup-org)
(require 'setup-orgstruct)
(require 'setup-outshine)
(require 'setup-page-break-lines)
(require 'setup-pcache)
(require 'setup-pomodoro)
(require 'setup-poporg)
(require 'setup-projectile)
(require 'setup-rainbow-delimiters)
(require 'setup-rainbow-mode)
(require 'setup-server)
(require 'setup-smart-compile)
(require 'setup-smex)
(require 'setup-stripe-buffer)
(require 'setup-sx)
(require 'setup-term)
(require 'setup-tiny)
(require 'setup-undo-tree)
(require 'setup-weather)
(require 'setup-which-func)
(require 'setup-which-key)
(when (executable-find "wn")
  (require 'setup-wordnut))
(require 'setup-wrap-region)
(require 'setup-writegood)
(require 'setup-xkcd)
(require 'setup-yasnippet)

;; Languages
(when (executable-find "lein")
  (require 'setup-clojure))
(require 'setup-conf)
(when (executable-find "dmd")
  (require 'setup-d))
(require 'setup-elisp)
(require 'setup-latex)
(require 'setup-markdown)
(when (executable-find "matlab")
  (require 'setup-matlab))
(require 'setup-perl)
(require 'setup-python)
(require 'setup-shell)
(require 'setup-spice)
(require 'setup-tcl)
(require 'setup-verilog)
(require 'setup-web-mode)
(require 'setup-yaml-mode)

;; Blend of other setup
(require 'setup-backup)
(require 'setup-editing)
(require 'setup-image)
(require 'setup-navigation)
(require 'setup-pdf)
(require 'setup-print)
(require 'setup-registers)
(require 'setup-search)
(when (or (executable-find "aspell")
          (executable-find "hunspell"))
  (require 'setup-spell))
(require 'setup-unicode)
(require 'setup-windows-buffers)

;; Place `setup-work.el' with `(provide 'setup-work)' in `user-personal-directory'
(require 'setup-work nil :noerror)

;; Place `setup-personal.el' with `(provide 'setup-personal)' in `user-personal-directory'
(require 'setup-personal nil :noerror)

;; Load certain setup files after a 1 second idle time after emacs has loaded.
;; This files need the emacs frame to be set up properly. For example, linum,
;; font detection does not work when emacs is launched in daemon mode while
;; the emacs frame has yet to load. So do those things after a safe estimate
;; delay of 1 second by which the frame should have loaded.
(use-package setup-info    :defer 1)
(use-package setup-linum   :defer 1)
(use-package setup-symbola :defer 1)

;; Do desktop setup after linum setup so that the desktop loaded files will show
;; linum if enabled for that major mode or if enabled globally
(with-eval-after-load 'setup-linum
  (require 'setup-desktop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'setup-misc) ; This MUST be the last required package

(when (and (bound-and-true-p emacs-initialized)
           (featurep 'setup-visual))
  (funcall default-theme-fn)) ; defined in `setup-visual.el'

(setq emacs-initialized t)

(run-with-idle-timer 5 nil (lambda () (setq gc-cons-threshold gc-cons-threshold--orig)))
