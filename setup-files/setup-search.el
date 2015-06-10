;; Time-stamp: <2015-06-10 15:50:43 kmodi>

;; Search / Replace

(setq-default case-fold-search t) ; Ignore case when searching

;; replace.el patches
(load (expand-file-name
       "replace.el"
       (concat user-emacs-directory "elisp/patches/"))
      nil :nomessage)

;; Anzu mode
;; https://github.com/syohex/emacs-anzu
(use-package anzu
  :config
  (progn
    (global-anzu-mode 1)

    (set-face-attribute 'anzu-mode-line nil :foreground "lightblue" :weight 'bold)
    (setq anzu-mode-lighter                "")
    (setq anzu-search-threshold            1000)
    (setq anzu-replace-to-string-separator " => ")

    (bind-keys
     :map modi-mode-map
      ("C-c r" . anzu-query-replace))

    (when (featurep 'region-bindings-mode)
      (bind-keys
       :map region-bindings-mode-map
        ("]" . anzu-query-replace-at-cursor-thing)))))

;; Visual Regular Expression search/replace
(use-package visual-regexp
  :config
  (progn
    (setq vr/default-feedback-limit 300)

    (bind-keys
     :map modi-mode-map
      ("C-c q" . vr/query-replace))

    (when (featurep 'multiple-cursors)
      (bind-keys
       :map modi-mode-map
        ("C-c M" . vr/mc-mark)))

    (when (featurep 'region-bindings-mode)
      (bind-keys
       :map region-bindings-mode-map
        ("}" . vr/query-replace)))))

;; Inspired from http://www.emacswiki.org/emacs/QueryExchange and definition of
;; `query-replace-regexp' from replace.el
(defun query-exchange (string-1 string-2 &optional delimited start end)
  "Exchange string-1 and string-2 interactively.
The user is prompted at each instance like query-replace. Exchanging
happens within a region if one is selected."
  (interactive
   (let ((common
          (query-replace-read-args
           (concat "Query replace"
                   (if current-prefix-arg " word" "")
                   " regexp"
                   (if (and transient-mark-mode mark-active) " in region" ""))
           t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
           ;; These are done separately here
           ;; so that command-history will record these expressions
           ;; rather than the values they had this time.
           (if (and transient-mark-mode mark-active)
               (region-beginning))
           (if (and transient-mark-mode mark-active)
               (region-end)))))
  (perform-replace
   (concat "\\(" string-1 "\\)\\|" string-2)
   '(replace-eval-replacement replace-quote
                              (if (match-string 1) string-2 string-1))
   t t delimited nil nil start end))

;; Swiper
;; https://github.com/abo-abo/swiper
(use-package swiper
  :config
  (progn
    (defun modi/swiper-dwim (arg)
      "Start swiper with input as the selected region or symbol at point by default.

C-u     -> `ivy-resume' (resume from where you last left off swiper)
C-u C-u -> Start swiper without any arguments (stock behavior)"
      (interactive "P")
      (cl-case (car arg)
        (4  (ivy-resume)) ; C-u
        (16 (swiper)) ; C-u C-u
        (t  (swiper (modi/get-symbol-at-point)))))

    (defun isearch-swiper ()
      "Invoke `swiper' from isearch.

https://github.com/ShingoFukuyama/helm-swoop/blob/f67fa8a4fe3b968b7105f8264a96da61c948a6fd/helm-swoop.el#L657-668
"
      (interactive)
      (let (($query (if isearch-regexp
                        isearch-string
                      (regexp-quote isearch-string))))
        (isearch-exit)
        (swiper $query)))

    (bind-key "M-i"   #'isearch-swiper isearch-mode-map) ; isearch > swiper
    (bind-key "M-a" #'swiper-avy swiper-map) ; swiper > avy

    (key-chord-define-global "'/" #'modi/swiper-dwim)
    (bind-key "M-i" #'modi/swiper-dwim modi-mode-map)))

;; Helm Swoop
;; https://github.com/ShingoFukuyama/helm-swoop
(use-package helm-swoop
  ;; Fix free variable warning
  :preface
  (progn
    (defvar helm-swoop-pattern            nil)
    (defvar helm-swoop-last-prefix-number nil))
  :commands (helm-swoop helm-multi-swoop-all helm-swoop-from-isearch)
  :init
  (progn
    (if (featurep 'swiper)
        (progn
          (bind-key "M-I" #'helm-swoop-from-isearch isearch-mode-map) ; isearch > helm-swoop
          (bind-key "M-I" #'helm-swoop modi-mode-map))
      (progn
        (bind-key "M-i" #'helm-swoop-from-isearch isearch-mode-map) ; isearch > helm-swoop
        (bind-key "M-i" #'helm-swoop modi-mode-map))))
  :config
  (progn
    ;; Disable helm
    (with-eval-after-load 'setup-ido
      (helm-mode -1)
      (ido-mode (if (bound-and-true-p disable-pkg-ivy) 1 -1)))
    (setq helm-swoop-split-direction 'split-window-vertically)
    (setq helm-swoop-speed-or-color nil) ; If nil, boosts speed in exchange for color
    ;; While doing `helm-swoop` press `C-c C-e` to edit mode, apply changes to
    ;; original buffer by `C-x C-s`
    ))

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-isearch.el
;; DEL during isearch should edit the search string, not jump back to
;; the previous result
(define-key isearch-mode-map [remap isearch-delete-char] #'isearch-del-char)

;; Search for the highlighted string in ALL buffers `search-all-buffers'
;; http://stackoverflow.com/a/2642655/1219634
(require 'grep)
(defcustom search-all-buffers-ignored-files (list (rx-to-string
                                                   '(and bos
                                                         (or ".bash_history"
                                                             "TAGS")
                                                         eos)))
  "Files to ignore when searching buffers via \\[search-all-buffers]."
  :type 'editable-list)
(defun search-all-buffers (regexp prefix)
  "Searches file-visiting buffers for occurence of REGEXP.  With
prefix > 1 (i.e., if you type C-u \\[search-all-buffers]),
searches all buffers."
  (interactive (list (grep-read-regexp)
                     current-prefix-arg))
  (message "Regexp is %s; prefix is %s" regexp prefix)
  (multi-occur
   (if (member prefix '(4 (4)))
       (buffer-list)
     (remove-if
      (lambda (b)
        (some
         (lambda (rx)
           (string-match rx (file-name-nondirectory
                             (buffer-file-name b))))
         search-all-buffers-ignored-files))
      (remove-if-not 'buffer-file-name (buffer-list))))
   regexp))
(bind-to-modi-map "s" search-all-buffers)

(defun modi/isearch-backward-symbol-at-point ()
  "Do incremental search backward for a symbol found near point.
Like ordinary incremental search except that the symbol found at point
is added to the search string initially as a regexp surrounded
by symbol boundary constructs \\_< and \\_>.
See the command `isearch-backward-symbol' for more information."
  (interactive)
  (isearch-mode (not :forward) nil nil nil 'isearch-symbol-regexp)
  (let ((bounds (find-tag-default-bounds)))
    (cond
     (bounds
      (when (< (car bounds) (point))
        (goto-char (car bounds)))
      (isearch-yank-string
       (buffer-substring-no-properties (car bounds) (cdr bounds))))
     (t
      (setq isearch-error "No symbol at point")
      (isearch-update)))))

(bind-keys
 :map modi-mode-map
  ("C-S-s" . isearch-forward-symbol-at-point)
  ("C-S-r" . modi/isearch-backward-symbol-at-point))


(provide 'setup-search)
