;; Time-stamp: <2015-03-17 09:36:58 kmodi>
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

(use-package ispell
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

      (use-package flyspell
          :config
        (progn
          (setq flyspell-use-meta-tab nil)
          ;; Stop flyspell overriding other key bindings
          (define-key flyspell-mode-map (kbd "C-;") nil)
          (define-key flyspell-mode-map (kbd "C-,") nil)
          (define-key flyspell-mode-map (kbd "C-.") nil)
          (define-key flyspell-mode-map "\M-\t"     nil)

          (add-hook 'prog-mode-hook #'flyspell-prog-mode)
          (with-eval-after-load 'auto-complete
            (ac-flyspell-workaround))
          ;; https://github.com/larstvei/dot-emacs#flyspell
          (add-hook 'text-mode-hook #'turn-on-flyspell)
          (add-hook 'org-mode-hook  #'turn-on-flyspell)

          ;; Flyspell signals an error if there is no spell-checking tool is installed.
          ;; We can advice turn-on=flyspell and flyspell-prog-mode to only try to enable
          ;; flyspell if a spell-checking tool is available.
          (defadvice turn-on-flyspell (around check nil activate)
            "Turns on flyspell only if a spell-checking tool is installed."
            (when (executable-find ispell-program-name)
              ad-do-it))
          (defadvice flyspell-prog-mode (around check nil activate)
            "Turns on flyspell only if a spell-checking tool is installed."
            (when (executable-find ispell-program-name)
              ad-do-it))

          (bind-keys
           :map modi-mode-map
           ("<f12>" . flyspell-auto-correct-previous-word))))))


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
