;; Time-stamp: <2024-11-01 09:02:50 kmodi>

;; Auto completion

;; corfu -- COmpletion in Region FUnction
;; UI for in-buffer completion; it does not come with completion backends
;; https://github.com/minad/corfu
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)                   ;Enable auto completion
  (corfu-quit-no-match 'separator) ;Quit completion eagerly, such that the completion popup stays
                                        ;out of your way when it appeared unexpectedly.
  ;; (corfu-cycle t)                 ;Enable cycling for `corfu-next/previous'
  ;; (corfu-separator ?\s)           ;Orderless field separator
  ;; (corfu-quit-at-boundary nil)    ;Never quit at completion boundary
  ;; (corfu-preview-current nil)     ;Disable current candidate preview
  ;; (corfu-preselect 'prompt)       ;Preselect the prompt
  ;; (corfu-on-exact-match nil)      ;Configure handling of exact matches
  ;; (corfu-scroll-margin 5)         ;Use scroll margin

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (progn
    (global-corfu-mode))
  :config
  (progn
    (keymap-unset corfu-map "RET")))    ;Free the RET key for less intrusive behavior

(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; cape -- Provides completion backends for corfu
;; https://github.com/minad/cape
(use-package cape
  :ensure t
  :bind ("M-=" . cape-prefix-map)       ;Press M-= ? to for help.
  :init
  (progn
    ;; Add to the global default value of
    ;; `completion-at-point-functions' which is used by
    ;; `completion-at-point'.  The order of the functions matters, the
    ;; first function returning a result wins.  Note that the list of
    ;; buffer-local completion functions takes precedence over the
    ;; global list.
    (add-hook 'completion-at-point-functions #'cape-dabbrev)      ;Complete word from current buffers
    (add-hook 'completion-at-point-functions #'cape-file)         ;Complete file name
    (add-hook 'completion-at-point-functions #'cape-elisp-block)  ;Complete Elisp in Org or Markdown code block
    (add-hook 'completion-at-point-functions #'cape-history)      ;Complete from Eshell, Comint or minibuffer history
    (add-hook 'completion-at-point-functions #'cape-dict)         ;Complete word from dictionary file
    (add-hook 'completion-at-point-functions #'cape-emoji)        ;Complete Emoji
    (add-hook 'completion-at-point-functions #'cape-keyword)      ;Complete programming language keyword
    ;; (add-hook 'completion-at-point-functions #'cape-abbrev)       ;Complete abbreviation (add-global-abbrev, add-mode-abbrev)
    ;; (add-hook 'completion-at-point-functions #'cape-elisp-symbol) ;Complete Elisp symbol
    ;; (add-hook 'completion-at-point-functions #'cape-line)         ;Complete entire line from file
    ;; (add-hook 'completion-at-point-functions #'cape-rfc1345)      ;Complete Unicode char using RFC 1345 mnemonics
    ;; (add-hook 'completion-at-point-functions #'cape-sgml)         ;Complete Unicode char from SGML entity, e.g., &alpha
    ;; (add-hook 'completion-at-point-functions #'cape-tex)          ;Complete Unicode char from TeX command, e.g. \hbar
    ))


(provide 'setup-completion)
