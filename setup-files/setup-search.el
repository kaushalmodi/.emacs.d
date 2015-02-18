;; Time-stamp: <2015-02-18 00:06:02 kmodi>

;; Search

(setq case-fold-search t) ;; Ignore case when searching

;; Anzu mode
;; Source: https://github.com/syohex/emacs-anzu
(req-package anzu
  :require (region-bindings-mode)
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

    (bind-keys
     :map region-bindings-mode-map
     ("]" . anzu-query-replace-at-cursor-thing))))

;; Visual Regular Expression search/replace
(req-package visual-regexp
  :require (region-bindings-mode)
  :config
  (progn
    (setq vr--feedback-limit nil)

    (bind-keys
     :map modi-mode-map
     ("C-M-%" . vr/query-replace) ; override binding for `query-replace-regexp'
     ("C-c q" . vr/query-replace)
     ("C-c M" . vr/mc-mark))

    (bind-keys
     :map region-bindings-mode-map
     ("}" . vr/query-replace))))

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
(req-package helm-swoop
  :require (ido ido-ubiquitous)
  :config
  (progn
    ;; Disable helm
    (defun modi/disable-helm-enable-ido ()
      (interactive)
      (helm-mode -1)
      (ido-mode 1)
      (ido-ubiquitous-mode 1))
    (modi/disable-helm-enable-ido)
    (bind-keys
     :map modi-mode-map
     ("M-i" . helm-swoop)
     ("M-I" . helm-multi-swoop-all))
    ;; Transition
    ;; isearch     > press [M-i] > helm-swoop
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    ;; helm-swoop  > press [M-i] > helm-multi-swoop-all
    (setq helm-swoop-split-direction 'split-window-vertically)
    (setq helm-swoop-speed-or-color nil) ; If nil, boosts speed in exchange for color
    ;; While doing `helm-swoop` press `C-c C-e` to edit mode, apply changes to
    ;; original buffer by `C-x C-s`
    ))

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-isearch.el
;; DEL during isearch should edit the search string, not jump back to
;; the previous result
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)


(provide 'setup-search)
