;; Time-stamp: <2014-10-23 09:33:12 kmodi>

;; Search

(setq case-fold-search t) ;; Ignore case when searching

;; Source: http://www.emacswiki.org/emacs/SearchAtPoint

(defun isearch-yank-regexp (regexp)
  "Pull REGEXP into search regexp."
  (let ((isearch-regexp nil)) ;; Dynamic binding of global.
    (isearch-yank-string regexp))
  (isearch-search-and-update))

(defun isearch-yank-symbol (&optional partialp backward)
  "Put symbol at current point into search string.

    If PARTIALP is non-nil, find all partial matches."
  (interactive "P")

  (let (from to bound sym)
    (setq sym
                                        ; this block taken directly from find-tag-default
                                        ; we couldn't use the function because we need the internal from and to values
          (when (or (progn
                      ;; Look at text around `point'.
                      (save-excursion
                        (skip-syntax-backward "w_") (setq from (point)))
                      (save-excursion
                        (skip-syntax-forward "w_") (setq to (point)))
                      (> to from))
                    ;; Look between `line-beginning-position' and `point'.
                    (save-excursion
                      (and (setq bound (line-beginning-position))
                           (skip-syntax-backward "^w_" bound)
                           (> (setq to (point)) bound)
                           (skip-syntax-backward "w_")
                           (setq from (point))))
                    ;; Look between `point' and `line-end-position'.
                    (save-excursion
                      (and (setq bound (line-end-position))
                           (skip-syntax-forward "^w_" bound)
                           (< (setq from (point)) bound)
                           (skip-syntax-forward "w_")
                           (setq to (point)))))
            (buffer-substring-no-properties from to)))
    (cond ((null sym)
           (message "No symbol at point"))
          ((null backward)
           (goto-char (1+ from)))
          (t
           (goto-char (1- to))))
    (isearch-search)
    (if partialp
        (isearch-yank-string sym)
      (isearch-yank-regexp
       (concat "\\_<" (regexp-quote sym) "\\_>")))))

;; Search for the highlighted string in ALL buffers `search-all-buffers'
;; Source: http://stackoverflow.com/questions/2641211/emacs-interactively-search-open-buffers
(require 'cl)
(defcustom search-all-buffers-ignored-files (list (rx-to-string '(and bos (or ".bash_history" "TAGS") eos)))
  "Files to ignore when searching buffers via \\[search-all-buffers]."
  :type 'editable-list)

(require 'grep)
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
         (lambda (rx) (string-match rx
                                    (file-name-nondirectory
                                     (buffer-file-name b))))
         search-all-buffers-ignored-files))
      (remove-if-not 'buffer-file-name (buffer-list))))
   regexp))

;; Anzu mode
;; Source: https://github.com/syohex/emacs-anzu
(req-package anzu
  :require (region-bindings-mode)
  :config
  (progn
    (global-anzu-mode +1)
    ;; color of search count shown in the mode-line by anzu
    (set-face-attribute 'anzu-mode-line nil
                        :foreground "lightblue" :weight 'bold)
    (setq anzu-mode-lighter                "" ;; String to show in the mode-line, default is " Anzu"
          anzu-search-threshold            1000 ;; anzu stops searching after reaching 1000 matches
          anzu-replace-to-string-separator " => "
          )
    (bind-keys
     :map region-bindings-mode-map
     ("]" . anzu-query-replace-at-cursor-thing))
    (bind-keys
     :map modi-mode-map
     ("M-%"   . anzu-query-replace) ;; replace the emacs default `query-replace'
     ("C-c r" . anzu-query-replace)
     )))
;;     ("C-c q" . anzu-query-replace-regexp))))

;; Visual Regular Expression search/replace
(req-package visual-regexp
  :require (region-bindings-mode)
  :config
  (progn
    (setq vr--feedback-limit nil)
    (bind-keys
     :map modi-mode-map
     ("C-M-%" . vr/query-replace) ;; replace the emacs default query-replace-regexp
     ("C-c q" . vr/query-replace)
     ("C-c m" . vr/mc-mark))

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

;; Swoop
;; https://github.com/ShingoFukuyama/emacs-swoop
(req-package swoop
  :config
  (progn
    (bind-keys
     :map modi-mode-map
     ("M-i" . swoop)
     ("M-I" . swoop-multi)
     ("M-o" . swoop-pcre-regexp))
    ;; Transition
    ;; isearch     > press [C-o] > swoop
    (define-key isearch-mode-map (kbd "C-o") 'swoop-from-isearch)
    ;; swoop       > press [C-o] > swoop-multi
    (define-key swoop-map (kbd "C-o")        'swoop-multi-from-swoop)
    ;; Resume
    ;; C-u M-x swoop : Use last used query
    ;; Swoop Edit Mode
    ;; During swoop, press [C-c C-e]
    ;; You can edit synchronously
    ))

;; Source: https://github.com/purcell/emacs.d/blob/master/lisp/init-isearch.el
;; DEL during isearch should edit the search string, not jump back to the previous result
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(bind-to-modi-map "s" search-all-buffers)


(provide 'setup-search)
