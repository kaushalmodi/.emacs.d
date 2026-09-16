;; -*- lexical-binding: t; -*-
;; Time-stamp: <2016-06-23 23:29:36 kmodi>
;;
;; Spell check
;; ispell, flyspell
;; aspell, hunspell
;;
;; NOTE: You need to have `aspell' or `hunspell' installed first
;;
;; Aspell Setup:
;; 1. Install aspell from http://aspell.net/
;;    - Install using ./configure --prefix=~/usr_local/bin, make, make install
;; 2. Download the latest dictionary from ftp://ftp.gnu.org/gnu/aspell/dict/0index.html
;;    and extract it.
;;    - Install the dictionary using ./configure, make, make install
;;
;; Hunspell Setup:
;; 1. Install hunspell from http://hunspell.sourceforge.net/
;; 2. Download openoffice dictionary extension from
;;    http://extensions.openoffice.org/en/project/english-dictionaries-apache-openoffice
;; 3. That is download `dict-en.oxt'. Rename that to `dict-en.zip' and unzip
;;    the contents to a temporary folder.
;; 4. Copy `en_US.dic' and `en_US.aff' files from there to a folder where you
;;    save dictionary files; I saved it to `~/usr_local/share/hunspell/'
;; 5. Add that path to shell env variable `DICPATH':
;;     setenv DICPATH $MYLOCAL/share/hunspell
;; 6. Restart emacs so that when hunspell is run by ispell/flyspell, that env
;;    variable is effective.
;;
;; hunspell will search for a dictionary called `en_US' in the path specified by
;; `$DICPATH'
;;
;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
;;
;; Jinx is used when its dependencies are present, and flyspell otherwise.
;; See `modi/jinx-available-p' below.

(defconst modi/jinx-available-p
  (and module-file-suffix                ;Emacs built with dynamic modules
       (or
        ;; jinx-mod was compiled already, so nothing else is needed.
        (locate-library (file-name-with-extension "jinx-mod" module-file-suffix) :nosuffix)
        ;; Else jinx has to compile it on first use, which needs a C
        ;; compiler and libenchant.
        (and (seq-find #'executable-find '("gcc" "clang" "cc"))
             (seq-find #'executable-find '("pkg-config" "pkgconf"))
             (eq 0 (call-process "pkg-config" nil nil nil "--exists" "enchant-2"))))
       t)
  "Non-nil if `jinx' can be used on this machine.
`jinx-mode' compiles and loads a dynamic module linked against
libenchant, and signals an error if either is missing. Check for
that up front so that the flyspell setup can be used instead.")

(use-package ispell
  :if (not (bound-and-true-p disable-pkg-ispell))
  :defer 15
  :config
  (progn
    (cond
     ((executable-find "aspell")
      (setq ispell-program-name "aspell")
      (setq ispell-extra-args   '("--sug-mode=ultra"
                                  "--lang=en_US")))
     ((executable-find "hunspell")
      (setq ispell-program-name "hunspell")
      (setq ispell-extra-args   '("-d en_US"))))

    ;; Save a new word to personal dictionary without asking
    (setq ispell-silently-savep t)

    ;; Flyspell is the fallback for when jinx cannot be used; see
    ;; `modi/jinx-available-p'. `:if' does not prevent the `:ensure' of
    ;; `flyspell-correct-ivy' below, hence the `unless' wrapper.
    (unless modi/jinx-available-p
      (use-package flyspell
        :init
        (progn
          ;; Below variables need to be set before `flyspell' is loaded.
          (setq flyspell-use-meta-tab nil)
          ;; Binding for `flyspell-auto-correct-previous-word'.
          (setq flyspell-auto-correct-binding (kbd "<S-f12>")))
        :config
        (progn
          (add-hook 'prog-mode-hook #'flyspell-prog-mode)
          ;; https://github.com/larstvei/dot-emacs#flyspell
          (add-hook 'text-mode-hook #'flyspell-mode)
          (add-hook 'org-mode-hook  #'flyspell-mode)

          ;; Flyspell signals an error if there is no spell-checking tool is
          ;; installed. We can advice `flyspell-mode' and `flyspell-prog-mode'
          ;; to try to enable flyspell only if a spell-checking tool is available.
          (defun modi/ispell-not-avail-p (&rest args)
            "Return `nil' if `ispell-program-name' is available; `t' otherwise."
            (not (executable-find ispell-program-name)))
          (advice-add 'flyspell-mode      :before-until #'modi/ispell-not-avail-p)
          (advice-add 'flyspell-prog-mode :before-until #'modi/ispell-not-avail-p)

          ;; https://github.com/d12frosted/flyspell-correct
          (use-package flyspell-correct-ivy
            :after flyspell-correct
            :bind (:map modi-mode-map
                   ("<f12>" . flyspell-correct-wrapper)))

          (bind-keys
           :map flyspell-mode-map
           ;; Stop flyspell overriding other key bindings
           ("C-," . nil)
           ("C-." . nil)
           ("<C-f12>" . flyspell-goto-next-error)))))))

;; https://github.com/minad/jinx
;; Unlike flyspell, jinx checks all the visible text at once, and does
;; it asynchronously, so misspellings show up while reading and not
;; only after point has moved past them. It spell-checks through
;; libenchant, which on macOS reaches the system dictionary via its
;; AppleSpell backend, so no aspell/hunspell dictionary is needed.
(when modi/jinx-available-p
  (use-package jinx
    :bind (:map modi-mode-map
           ("<f12>" . jinx-correct)     ;Same binding as `flyspell-correct-wrapper'
           ("<C-f12>" . jinx-next))     ;Same binding as `flyspell-goto-next-error'
    :init
    (progn
      ;; Set the dictionary explicitly. `jinx-languages' otherwise
      ;; defaults to whatever `current-locale-environment' or $LANG says
      ;; at load time, which is unset for a GUI Emacs started from the
      ;; Dock or Finder; `jinx--dicts' then ends up empty and the
      ;; correction UI fails with "Invalid dictionary".
      (setq jinx-languages "en_US")

      ;; `jinx-mode' does the module loading, so enable it from `:init'
      ;; rather than `:config'; the latter would not run until one of
      ;; the keys above was pressed.
      (dolist (hook '(text-mode-hook
                      prog-mode-hook
                      org-mode-hook
                      conf-mode-hook))
        (add-hook hook #'jinx-mode)))))


(provide 'setup-spell)

;; How to add a new word to the dictionary?
;; 1. Run ispell-word when the cursor is over the word ( `M-$' )
;; 2. Press `i' to add the word to the dictionary
;; 3. Done!
;;
;; For `aspell', the new words are auto added to `~/.aspell.en.pws'.
;; For `hunspell', the new words are auto added to `~/.hunspell_en_US'.
;;
;; If the word does not auto-correct properly, call the function
;; `flyspell-auto-correct-previous-word' repeatedly till you find the
;; right match. It is easy if a key is bound to call that function.
