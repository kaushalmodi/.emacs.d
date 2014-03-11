;; Time-stamp: <2014-03-11 13:58:35 kmodi>

;; Interactively Do Things
;; Source: http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
;; Source: https://github.com/magnars/.emacs.d/blob/master/setup-ido.el

(require 'ido)
(setq ido-enable-flex-matching t ;; enable fuzzy search
      ido-everywhere t
      ido-create-new-buffer 'always ;; create a new buffer if no buffer matches substring
      ido-file-extensions-order '(".sv" ".v" ".svh" ".tv" ".m" ".c" ".cpp" ".emacs")
      ;;  customize the order in which files are sorted when Ido displays them in
      ;; the minibuffer. There are certain file extensions I use more than others,
      ;; so I tell Ido to emphasize those
      ido-use-filename-at-point 'guess ;; find file at point using ido
      ido-auto-merge-work-directories-length 0 ;; look into other directories if
                       ;; the entered filename doesn't exist in current directory
      ;; ido-auto-merge-work-directories-length -1 ;; do NOT look into other directories if
      ;;                  ;; the entered filename doesn't exist in current directory
      )
(ido-mode 1)

;; (setq ido-enable-prefix nil
;;       ido-case-fold nil
;;       ido-use-filename-at-point nil
;;       ido-max-prospects 10)

;; Use flx-ido for better flex matching between words
(require 'flx-ido)
(flx-ido-mode 1)
(setq ido-use-faces nil) ;; disable ido faces to see flx highlights

;; flx-ido looks better with ido-vertical-mode
(require 'ido-vertical-mode)
(ido-vertical-mode 1)

;; (defun ido-disable-line-truncation ()
;;   (set (make-local-variable 'truncate-lines) nil))
;; (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

(defun ido-define-keys ()
  ;; C-n/p  and up/down keys are more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<up>") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

;; ;; Use C-w to go back up a dir to better match normal usage of C-w
;; ;; - insert current file name with C-x C-w instead.
;; (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
;; (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)

;; (define-key ido-file-dir-completion-map (kbd "C-w") 'ido-delete-backward-updir)
;; (define-key ido-file-dir-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name))

;; ;; Always rescan buffer for imenu
;; (set-default 'imenu-auto-rescan t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ido at point (Replacement for auto-complete)
;; https://github.com/katspaugh/ido-at-point
(require 'ido-at-point)
(ido-at-point-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use ido everywhere, example: when searching for var name `C-h v`, searching
;; for function name `C-h f`, etc
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; ;; Fix ido-ubiquitous for newer packages
;; (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
;;   `(eval-after-load ,package
;;      '(defadvice ,cmd (around ido-ubiquitous-new activate)
;;         (let ((ido-ubiquitous-enable-compatibility nil))
;;           ad-do-it))))

;; (ido-ubiquitous-use-new-completing-read webjump 'webjump)
;; (ido-ubiquitous-use-new-completing-read yas-expand 'yasnippet)
;; (ido-ubiquitous-use-new-completing-read yas-visit-snippet-file 'yasnippet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sort ido filelist by mtime instead of alphabetically
;; source: http://www.emacswiki.org/emacs/InteractivelyDoThings
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook  'ido-sort-mtime)
(defun ido-sort-mtime ()
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (time-less-p
                 (sixth (file-attributes (concat ido-current-directory b)))
                 (sixth (file-attributes (concat ido-current-directory a)))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (and (char-equal (string-to-char x) ?.) x))
              ido-temp-list))))


(setq setup-ido-loaded t)
(provide 'setup-ido)
