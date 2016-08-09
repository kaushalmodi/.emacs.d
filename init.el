;; Time-stamp: <2016-08-09 11:54:34 kmodi>
;; Author: Kaushal Modi

;; Global variables
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(defvar gc-cons-threshold--orig gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024)) ; 100 MB before garbage collection

(defvar user-home-directory (concat (getenv "HOME") "/")) ; must end with /

(defvar emacs-version-short (format "%s_%s"
                                    emacs-major-version emacs-minor-version)
  "A variable to store the current emacs versions as <MAJORVER>_<MINORVER>.
So, for emacs version 25.0.50.1, this variable will be 25_0.")

(defvar user-personal-directory (let ((dir (concat user-emacs-directory
                                                   "personal/"))) ; must end with /
                                  (make-directory dir :parents)
                                  dir)
  "User's personal directory to contain non-git-controlled files.")

(setq user-emacs-directory (concat user-home-directory ".emacs.d/")) ; must end with /
(setq org-directory (let ((dir (concat user-home-directory
                                       "org/"))) ; must end with /
                      (make-directory dir :parents)
                      dir))
(setq custom-file (expand-file-name "custom.el" user-personal-directory))

(defconst my-packages
  '(ace-window
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
    drag-stuff
    easy-escape ; Make the \\ escape chars more pleasant looking in elisp regexps
    elfeed
    elisp-slime-nav ; tag based code navigation for elisp; works even for compressed code
    engine-mode ; search engines
    expand-region
    eww-lnum ; jump to links in eww buffer ace-jump style
    fill-column-indicator
    flyspell-correct-ivy ; ivy interface for correct spelling suggestions
    fold-this
    gist
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
    imenu-list
    indent-guide
    info+
    interleave ; takes notes associated to pdf files in org mode
    isend-mode ; used in setup-perl.el
    ivy swiper counsel
    iy-go-to-char ; Go to next char which is similar to "f" and "t" in vim
    key-chord ; map pairs of simultaneously pressed keys to commands
    keyfreq ; find which commands you use the most
    magit ; for git management
    manage-minor-mode
    markdown-mode
    minibuffer-line
    multi-term
    multiple-cursors
    neotree
    nlinum ; better performance than linum
    org-cliplink ; paste copied links as well-formatted org-mode links with desc
    org-plus-contrib ; latest stable version of org-mode, includes org-eww
    org-tree-slide
    outorg navi-mode ; supporting packages for outshine
    outshine ; org-mode navigation and organization outside org-mode
    paradox ; package menu improvements
    page-break-lines ; Convert the ^L (form feed) chars to horizontal lines
    pomodoro
    poporg ; edit comments from any other mode in org mode
    projectile ; Better than fiplr
    rainbow-delimiters
    rainbow-mode
    region-bindings-mode ; complements really well with multiple-cursors
    rpn-calc
    shackle
    smart-compile
    smart-mark
    smart-mode-line popup rich-minority
    smex ; smart M-x
    sunshine forecast ; weather
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
    ;; zenburn-theme ; < fork
    ;; smyx-theme ; < fork
    ample-theme ; ample, ample-flat, ample-light
    darktooth-theme ; coffee
    leuven-theme ; awesome white background theme
    planet-theme ; dark blue
    tao-theme ; monochrome
    twilight-bright-theme
    twilight-anti-bright-theme
    ;; Crypt
    ;; ox-twbs ; export to twitter bootstrap html < fork (supports org 9.0+)
    ;; ox-reveal ; used to export to HTML slides; < git clone
    ;; git-link ; get git links with line numbers and commit-hash/branch ; < fork
    ;; ido-vertical-mode flx-ido ido-ubiquitous ; < ivy, counsel
    ;; git-gutter git-gutter-fringe git-gutter+ git-gutter-fringe+ ; < diff-hl
    ;; popwin ; < shackle
    ;; helm helm-swoop ; < swiper
    ;; helm-gtags ; < ggtags
    )
  "A list of packages to ensure are installed at launch")

;; Basic requires
;; Place `setup-var-overrides.el' with `(provide 'setup-var-overrides)' in
;; `user-personal-directory'
(add-to-list 'load-path user-personal-directory)
(require 'setup-var-overrides nil :noerror)

(load custom-file :noerror :nomessage) ; Load the `M-x customize` generated file
(load (locate-user-emacs-file "general.el") nil :nomessage)
(load (locate-user-emacs-file "setup-packages.el") nil :nomessage)
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

;; Enable `modi-mode' unless `disable-pkg-modi-mode' is set to `t' in
;; `setup-var-overrides.el'.
(when (not (bound-and-true-p disable-pkg-modi-mode))
  (require 'modi-mode))
(require 'temp-mode)

(require 'setup-paradox)
(require 'setup-region-bindings-mode)
(require 'setup-key-chord)
(require 'setup-hydra)
(require 'setup-tags)
;; End of basic requires

;; Set up the looks of emacs
(require 'setup-mode-line)
(require 'setup-visual)
(require 'setup-shackle)

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
(require 'setup-bookmarks)
(require 'setup-buffer-move)
(require 'setup-calc)
(require 'setup-command-log-mode)
(require 'setup-counsel)
(require 'setup-de-ansify)
(require 'setup-deft)
(require 'setup-dired)
(require 'setup-drag-stuff)
(require 'setup-elfeed)
(require 'setup-eww)
(require 'setup-elisp-slime-nav)
(require 'setup-engine-mode)
(require 'setup-expand-region)
;; Below will cause emacs to freeze on evaluating "(string-match-p "." nil)"
;; on emacs 25.1 or older.
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=23949
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
(if (bound-and-true-p disable-pkg-ivy)
    (require 'setup-ido)
  (require 'setup-ivy))
(require 'setup-imenu-list)
(require 'setup-indent-guide)
(require 'setup-info)
(require 'setup-keyfreq)
(require 'setup-linum)
(require 'setup-manage-minor-mode)
(require 'setup-multiple-cursors)
(require 'setup-neotree)
(require 'setup-news)
(require 'setup-org)
(require 'setup-orgstruct)
(require 'setup-outshine)
(require 'setup-page-break-lines)
(require 'setup-pcache)
(require 'setup-pomodoro)
(require 'setup-poporg)
(with-eval-after-load 'setup-tags
  ;; Below causes `help-function-arglist' error on evaluating "(string-match-p "." nil)"
  ;; on emacs 25.1 or older.
  ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=23949
  (require 'setup-projectile))
(require 'setup-rainbow-delimiters)
(require 'setup-rainbow-mode)
(require 'setup-server)
(require 'setup-smart-compile)
(require 'setup-smex)
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
  (require 'setup-clojure)) ; cider
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
(when (executable-find "sml")
  (require 'setup-sml))
(require 'setup-tcl)
(require 'setup-verilog)
(require 'setup-web-mode)
(require 'setup-yaml-mode)

;; Blend of other setup
(require 'setup-backup)
(require 'setup-editing)
(require 'setup-image)
(require 'setup-launcher)
(require 'setup-navigation)
(require 'setup-pdf)
(require 'setup-print)
(require 'setup-registers)
(require 'setup-search)
(when (or (executable-find "aspell")
          (executable-find "hunspell"))
  (require 'setup-spell))
(require 'setup-toggles)
(require 'setup-unicode)
(require 'setup-windows-buffers)

;; Place `setup-work.el' with `(provide 'setup-work)' in `user-personal-directory'
(require 'setup-work nil :noerror)

;; Place `setup-personal.el' with `(provide 'setup-personal)' in `user-personal-directory'
(require 'setup-personal nil :noerror)

;; The `setup-misc' must be the last package to be required except for
;; `setup-desktop'.
(require 'setup-misc)

;; Delay desktop setup by a second.
;; - This speeds up emacs init, and
;; - Also (n)linum and other packages would already be loaded which the files
;;   being loaded from the saved desktop might need.
(use-package setup-desktop :defer 1)

(defun modi/font-check ()
  "Do font check, then remove self from `focus-in-hook'; need to run this just once."
  (require 'setup-font-check)
  (remove-hook 'focus-in-hook #'modi/font-check))
;; For non-daemon, regular emacs launches, the frame/fonts are loaded *before*
;; the emacs config is read. But when emacs is launched as a daemon (using
;; emacsclient, the fonts are not actually loaded until the point when the
;; `after-make-frame-functions' hook is run. But even at that point, the frame
;; is not yet selected (for the daemon case). Without a selected frame, the
;; `find-font' will not work correctly. So we do the font check in
;; `focus-in-hook' instead by which all the below are true:
;;  - Fonts are loaded (in both daemon and non-daemon cases).
;;  - The frame is selected and so `find-font' calls work correctly.
(add-hook 'focus-in-hook #'modi/font-check)

(when (and (bound-and-true-p emacs-initialized)
           (featurep 'setup-visual))
  (funcall default-theme-fn)) ; defined in `setup-visual.el'

(setq emacs-initialized t)

(run-with-idle-timer 5 nil (lambda () (setq gc-cons-threshold gc-cons-threshold--orig)))
