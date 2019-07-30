;; Time-stamp: <2019-07-31 07:58:02 kmodi>

;;;; Fold setup

(defface modi/fold-face
  '((t (:foreground "deep sky blue" :slant italic)))
  "Face used for fold ellipsis.")

;;; Fold This - Fold selected
;; https://github.com/magnars/fold-this.el
(use-package fold-this
  :init
  (progn
    (setq fold-this-persistent-folds-file (locate-user-emacs-file "folds-saved")))
  :config
  (progn
    (setq fold-this-persistent-folds t)

    (defvar modi/fold-this--last-overlay nil
      "Store the last overlay created by `fold-this'.")

    ;; Patch the original `fold-this' command to save the overlay to the var
    ;; `modi/fold-this--last-overlay' and tweak the 'display property of the
    ;; overlay
    (defun fold-this (beg end)
      (interactive "r")
      (let ((o (make-overlay beg end nil t nil)))
        (overlay-put o 'type 'fold-this)
        (overlay-put o 'invisible t)
        (overlay-put o 'keymap fold-this-keymap)
        (overlay-put o 'face 'modi/fold-face)
        (overlay-put o 'modification-hooks '(fold-this--unfold-overlay))
        (overlay-put o 'display (propertize " « » " 'face 'modi/fold-face))
        (overlay-put o 'evaporate t)
        (setq modi/fold-this--last-overlay o))
      (deactivate-mark))

    (bind-keys
     :map fold-this-keymap
     ("<mouse-1>" . fold-this-unfold-at-point)) ; left-click on ellipsis to unfold

    (bind-keys
     :map region-bindings-mode-map
     ("&" . fold-this))))

;;; Yet Another Folding - Folding code blocks based on indentation
;; https://github.com/zenozeng/yafolding.el
(use-package yafolding
  :config
  (progn
    (setq yafolding-ellipsis-content " ... ")
    (set-face-attribute 'yafolding-ellipsis-face nil :inherit 'modi/fold-face)))

;;; Hide-show
(use-package hideshow
  :commands (modi/hideshow-mode)
  :config
  (progn

    (defconst modi/hs-minor-mode-hooks '(emacs-lisp-mode-hook
                                         verilog-mode-hook
                                         cperl-mode-hook)
      "List of hooks of major modes in which hs-minor-mode should be enabled.")

    (setq hs-isearch-open 'code) ; default 'code, options: 'comment, t, nil

    ;; org-style folding/unfolding in hideshow
    (use-package hideshow-org
      :init
      (progn
        (setq hs-org/trigger-keys-block (list (kbd "TAB")
                                              (kbd "<C-tab>")) )
        (setq hs-org/trigger-keys-all (list [S-tab]
                                            [S-iso-lefttab]
                                            [(shift tab)]
                                            [backtab]))))

    ;; Show hideshow foldable sections in the buffer
    (use-package hideshowvis
      :load-path "elisp/manually-synced/hideshowvis" ;This package is not on Melpa yet.
      :if (display-graphic-p) ; no fringe in terminal mode
      :config
      (progn
        ;; + bitmap
        (define-fringe-bitmap 'hs-expand-bitmap [0   ; 0 0 0 0 0 0 0 0
                                                 24  ; 0 0 0 ▮ ▮ 0 0 0
                                                 24  ; 0 0 0 ▮ ▮ 0 0 0
                                                 126 ; 0 ▮ ▮ ▮ ▮ ▮ ▮ 0
                                                 126 ; 0 ▮ ▮ ▮ ▮ ▮ ▮ 0
                                                 24  ; 0 0 0 ▮ ▮ 0 0 0
                                                 24  ; 0 0 0 ▮ ▮ 0 0 0
                                                 0]) ; 0 0 0 0 0 0 0 0

        (defface modi/hs-fringe-face
          '((t (:foreground "#888"
                :box (:line-width 2 :color "grey75" :style released-button))))
          "Face used to highlight the fringe on folded regions"
          :group 'hideshow)

        (defun modi/display-code-line-counts (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (let* ((marker-string "*fringe-dummy*"))
              ;; Place the + bitmap in the left fringe
              (put-text-property 0 (length marker-string)
                                 'display
                                 '(left-fringe hs-expand-bitmap modi/hs-fringe-face)
                                 marker-string)
              (overlay-put ov
                           'before-string
                           marker-string)
              (overlay-put ov
                           'display
                           (propertize
                            (format " ... [%d] "
                                    (count-lines (overlay-start ov)
                                                 (overlay-end ov)))
                            'face 'modi/fold-face))
              (overlay-put ov
                           'help-echo
                           (buffer-substring (overlay-start ov)
                                             (overlay-end ov))))))
        (setq hs-set-up-overlay #'modi/display-code-line-counts)))

    (defun modi/turn-on-hs-minor-mode ()
      "Turn on hs-minor-mode only for specific modes."
      (interactive)
      (dolist (hook modi/hs-minor-mode-hooks)
        (add-hook hook #'hs-minor-mode)
        (add-hook hook #'hs-org/minor-mode)
        (add-hook hook #'hideshowvis-minor-mode)))

    (defun modi/turn-off-hs-minor-mode ()
      "Turn off hs-minor-mode only for specific modes."
      (interactive)
      (dolist (hook modi/hs-minor-mode-hooks)
        (remove-hook hook #'hs-minor-mode)
        (remove-hook hook #'hs-org/minor-mode)
        (remove-hook hook #'hideshowvis-minor-mode)))

    (define-minor-mode modi/hideshow-mode
      "Minor mode to toggle the `hs-minor-mode' and related modes in the
current buffer."
      :global     nil
      :init-value nil
      :lighter    " ḤṢ"
      (if modi/hideshow-mode
          (progn
            (hs-minor-mode 1)
            (hs-org/minor-mode 1)
            (hideshowvis-minor-mode 1))
        (progn
          (hs-minor-mode -1)
          (hs-org/minor-mode -1)
          (hideshowvis-minor-mode -1))))))

;;; DWIM
(defvar modi/fold-dwim--last-fn nil
  "Store the symbol of the last function called using `modi/fold-dwim'.")

(defun modi/fold-dwim (&optional beg end)
  "If region is selected use `fold-this', else use `yafolding'.
If prefix argument is used, `set-selective-display' to the current column."
  (interactive "r")
  (let ((message-log-max nil))
    (if (region-active-p)
        (progn
          (fold-this beg end)
          (setq modi/fold-dwim--last-fn #'fold-this)
          (message "Folded the selected region using %s."
                   (propertize
                    (symbol-name modi/fold-dwim--last-fn)
                    'face 'font-lock-function-name-face)))
      (if current-prefix-arg
          (progn
            (set-selective-display (current-column))
            (setq modi/fold-dwim--last-fn #'set-selective-display)
            (message "Folded using %s at column %d."
                     (propertize
                      (symbol-name modi/fold-dwim--last-fn)
                      'face 'font-lock-function-name-face)
                     (current-column)))
        (progn
          (yafolding-toggle-element)
          (setq modi/fold-dwim--last-fn #'yafolding-toggle-element)
          (message "Folded at current indent level using %s."
                   (propertize
                    (symbol-name modi/fold-dwim--last-fn)
                    'face 'font-lock-function-name-face)))))))
;; Below binding is made in global map and not in my minor mode as I want
;; other modes like org-mode to override that binding
(bind-key "C-c C-f" #'modi/fold-dwim)
(bind-key "C-c &"   #'modi/fold-dwim modi-mode-map)

(defun modi/unfold-if-last-command-fold (&rest args)
  "If the `last-command' was a folding command, undo that fold."
  (let ((message-log-max nil)
        (msg "Undid the last fold.")
        last-cmd-was-fold)
    (cond
     ;; if last command was a fold using `fold-this'
     ((or (and (eq modi/fold-dwim--last-fn #'fold-this)
               (eq last-command #'modi/fold-dwim))
          (eq last-command #'fold-this))
      (delete-overlay modi/fold-this--last-overlay)
      (setq last-cmd-was-fold t))
     ;; if last command was a fold using `yafolding'
     ((or (and (eq modi/fold-dwim--last-fn #'yafolding-toggle-element)
               (eq last-command #'modi/fold-dwim))
          (eq last-command #'yafolding-toggle-element))
      (yafolding-toggle-element)
      (setq last-cmd-was-fold t))
     ;; if last command was a fold using `set-selective-display'
     ((or (and (eq modi/fold-dwim--last-fn #'set-selective-display)
               (eq last-command #'modi/fold-dwim))
          (eq last-command #'set-selective-display))
      (set-selective-display nil) ; clear selective display with `nil' arg
      (setq last-cmd-was-fold t))
     ;; otherwise do nothing
     (t
      (setq msg nil)))
    (when msg
      (message msg))
    last-cmd-was-fold))
;; Advice `undo-tree-undo' to unfold the previous fold
(with-eval-after-load 'undo-tree
  (advice-add 'undo-tree-undo :before-until #'modi/unfold-if-last-command-fold))
;; Advice `undo' to unfold the previous fold
(with-eval-after-load 'undo-tree
  (advice-add 'undo :before-until #'modi/unfold-if-last-command-fold))


(provide 'setup-fold)

;; `fold-this'
;; Fold the selected region using `fold-this' binding.
;; If you move point into the ellipsis and press `RET' or `C-g', it is unfolded.
