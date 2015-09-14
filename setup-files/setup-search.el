;; Time-stamp: <2015-09-13 23:35:52 kmodi>

;; Search / Replace

(setq-default case-fold-search t) ; Ignore case when searching

;; replace.el patches
(>=e "25.0"
    (load (expand-file-name
           "replace.el"
           (concat user-emacs-directory "elisp/patches/"))
          nil :nomessage))

;; Anzu mode
;; https://github.com/syohex/emacs-anzu
(use-package anzu
  :init
  (progn
    (bind-keys
     :map modi-mode-map
      ("C-c r" . anzu-query-replace)))
  :config
  (progn
    (setq anzu-mode-lighter                "")
    (setq anzu-search-threshold            1000)
    (setq anzu-replace-to-string-separator " => ")

    (global-anzu-mode 1)))

;; Visual Regular Expression search/replace
(use-package visual-regexp
  :commands (vr/query-replace vr/mc-mark vr/replace)
  :init
  (progn
    (bind-keys
     :map modi-mode-map
      ("C-c q" . vr/query-replace))
    (with-eval-after-load 'multiple-cursors
      (bind-keys
       :map modi-mode-map
        ("C-c M" . vr/mc-mark))))
  :config
  (progn
    (setq vr/default-feedback-limit 300)))

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
  :commands (modi/swiper swiper swiper-from-isearch swiper-avy)
  :init
  (progn
    (bind-key "M-i" #'swiper-from-isearch isearch-mode-map) ; isearch > swiper
    (bind-key "M-a" #'swiper-avy swiper-map) ; swiper > avy
    (key-chord-define-global "'/" #'modi/swiper)
    (bind-key "M-i" #'modi/swiper modi-mode-map))
  :config
  (progn
    (defun modi/swiper (arg)
      "Start swiper with input as the selected region.

If a region is not selected and,
  - If ARG is nil, start swiper with the symbol at point as input.
  - Elseswiper without any arguments (stock behavior)."
      (interactive "P")
      (if (use-region-p)
          (let ((b (region-beginning))
                (e (region-end)))
            (deactivate-mark)
            (swiper (buffer-substring-no-properties b e)))
        (if arg
            (swiper) ; C-u
          (swiper (modi/get-symbol-at-point)))))))

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-isearch.el
;; DEL during isearch should edit the search string, not jump back to
;; the previous result
(define-key isearch-mode-map [remap isearch-delete-char] #'isearch-del-char)

;; Search for the highlighted string in ALL buffers `search-all-buffers'
;; http://stackoverflow.com/a/2642655/1219634
(use-package grep
  :config
  (progn
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
    (bind-to-modi-map "s" #'search-all-buffers)))

(defun modi/isearch-backward-symbol-at-point ()
  "Do incremental search backward for a symbol found near point.
Like ordinary incremental search except that the symbol found at point
is added to the search string initially as a regexp surrounded
by symbol boundary constructs \\_< and \\_>.
See the command `isearch-forward-symbol' for more information."
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
