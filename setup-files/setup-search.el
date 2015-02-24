;; Time-stamp: <2015-02-23 23:39:15 kmodi>

;; Search

(setq case-fold-search t) ;; Ignore case when searching

;; Anzu mode
;; Source: https://github.com/syohex/emacs-anzu
(use-package anzu
  :config
  (progn
    (global-anzu-mode +1)

    (set-face-attribute 'anzu-mode-line nil :foreground "lightblue" :weight 'bold)
    (setq anzu-mode-lighter                "")
    (setq anzu-search-threshold            1000)
    (setq anzu-replace-to-string-separator " => ")

    (bind-keys
     :map modi-mode-map
     ("M-%"   . anzu-query-replace) ; override binding for `query-replace'
     ("C-c r" . anzu-query-replace))

    (when (featurep 'region-bindings-mode)
      (bind-keys
       :map region-bindings-mode-map
       ("]" . anzu-query-replace-at-cursor-thing)))))

;; Visual Regular Expression search/replace
(use-package visual-regexp
  :config
  (progn
    (setq vr--feedback-limit nil)

    (bind-keys
     :map modi-mode-map
     ("C-M-%" . vr/query-replace) ; override binding for `query-replace-regexp'
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

;; Helm Swoop
(defvar helm-swoop-last-prefix-number nil) ; Fix free variable warning
(use-package helm-swoop
  :commands (helm-swoop helm-multi-swoop-all helm-swoop-from-isearch)
  :init
  (progn
    (bind-keys
     :map modi-mode-map
     ("M-i" . helm-swoop)
     ("M-I" . helm-multi-swoop-all))
    ;; isearch > press [M-i] > helm-swoop
    (bind-key "M-i" #'helm-swoop-from-isearch isearch-mode-map))
  :config
  (progn
    ;; Disable helm
    (with-eval-after-load 'setup-ido
      (defun modi/disable-helm-enable-ido ()
        (interactive)
        (helm-mode -1)
        (ido-mode 1)
        (ido-ubiquitous-mode 1))
      (modi/disable-helm-enable-ido))
    (setq helm-swoop-split-direction 'split-window-vertically)
    (setq helm-swoop-speed-or-color nil) ; If nil, boosts speed in exchange for color
    ;; helm-swoop  > press [M-i] > helm-multi-swoop-all
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


(provide 'setup-search)
