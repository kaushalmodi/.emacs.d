;; Time-stamp: <2014-07-03 20:14:03 kmodi>

;; Ag
;; https://github.com/Wilfred/ag.el

(require 'ag)

;; Set default ag arguments
(setq ag-arguments (list "--smart-case" "--nogroup" "--column"))
;; As per https://github.com/Wilfred/ag.el/issues/41, it is mandatory
;; to use --nogroup and --column options for ag.el compatibility.

;; Add more ag arguments
(setq ag-arguments (append '(
                             "--follow"
;; NOTE: It looks like the ~/.agignore is used when launching ag from emacs too.
;;       So the ignores from agignore don't have to be set here again.
;;                              "--ignore" "*~"
;;                              "--ignore" "*/*~"
;;                              "--ignore" "*/*/*~"
;;                              "--ignore" "*/*/*/*~"
;;                              "--ignore" "*/*/*/*/*~"
;;                              "--ignore" "*/*/*/*/*/*~"
;;                              "--ignore" "*/*/*/*/*/*/*~"
;;                              "--ignore" "*/*/*/*/*/*/*/*~"
;;                              "--ignore" "*#*#"
;;                              "--ignore" "*/*#*#"
;;                              "--ignore" "*/*/*#*#"
;;                              "--ignore" "*/*/*/*#*#"
;;                              "--ignore" "*/*/*/*/*#*#"
;;                              "--ignore" "*/*/*/*/*/*#*#"
;;                              "--ignore" "*/*/*/*/*/*/*#*#"
;;                              "--ignore" "*/*/*/*/*/*/*/*#*#"
;;                              "--ignore" ".SOS"
;;                              "--ignore" ".snapshot"
;;                              "--ignore" "pei_stuff"
;;                              "--ignore" "gnuarm-4.0.2/"
;;                              "--ignore" "png32:-"
;;                              "--ignore" "INCA_libs/"
;;                              "--ignore" "INCA_libs.r/"
;;                              "--ignore" "coverage/"
;;                              "--ignore" "opencores/"
;;                              "--ignore" "dump/"
                             ) ag-arguments))

(setq ag-highlight-search t)

;; By default, ag.el will open results in a different window in the frame, so
;; the results buffer is still visible. You can override this so the results
;; buffer is hidden and the selected result is shown in its place:
(setq ag-reuse-window nil)

;; reuse the same *ag* buffer for all your searches
(setq ag-reuse-buffers t)

(define-key ag-mode-map (kbd "i") 'wgrep-change-to-wgrep-mode)
(define-key ag-mode-map (kbd "/") 'isearch-forward)
(define-key ag-mode-map (kbd "n") 'isearch-repeat-forward)
(define-key ag-mode-map (kbd "N") 'isearch-repeat-backward)
(define-key ag-mode-map (kbd "q") 'ag-kill-buffers) ;; quits ag buffer

;; Redefine the ag-regexp function where the default search pattern is
;; word at point
(defun ag-regexp (string directory)
  "Search using ag in a given DIRECTORY for a given search REGEXP,
with REGEXP defaulting to the symbol under point.
Search using ag in a given directory for a given regexp.

If called with a prefix, prompts for flags to pass to ag."
  (interactive (list (read-from-minibuffer "Search regexp: " (ag/dwim-at-point))
                     (read-directory-name "Directory: ")))
  (ag/search string directory :regexp t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep-ag : To allow editing in *ag* buffer
;; https://github.com/mhayashi1120/Emacs-wgrep

(require 'wgrep-ag)
(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)

;; ;; To save buffer automatically when `wgrep-finish-edit'
;; (setq wgrep-auto-save-buffer t)

;; Default key binding when in wgrep mode
;; (define-key wgrep-mode-map (kbd "C-c C-c") 'wgrep-finish-edit) ;; Apply changes to file buffers
;; (define-key wgrep-mode-map (kbd "C-c C-e") 'wgrep-finish-edit)
;; (define-key wgrep-mode-map (kbd "C-x C-s") 'wgrep-finish-edit)
(define-key wgrep-mode-map (kbd "C-x s") 'wgrep-save-all-buffers) ;; Save the buffers that wgrep changed
;; (define-key wgrep-mode-map (kbd "C-c C-d") 'wgrep-mark-deletion)
;; (define-key wgrep-mode-map (kbd "C-c C-p") 'wgrep-toggle-readonly-area)
;; (define-key wgrep-mode-map (kbd "C-c C-r") 'wgrep-remove-change)
;; (define-key wgrep-mode-map (kbd "C-c C-u") 'wgrep-remove-all-change)
;; (define-key wgrep-mode-map (kbd "C-c C-[") 'wgrep-remove-all-change)
;; (define-key wgrep-mode-map (kbd "C-c C-k") 'wgrep-abort-changes)
;; (define-key wgrep-mode-map (kbd "C-x C-q") 'wgrep-exit)


(setq setup-ag-loaded t)
(provide 'setup-ag)

;; NOTES
;; Simply put an empty folder called `.git' at a location that you want to consider
;; as a "project root"; even if that is not revision controlled in git.
;; After that, if you are in any file under that path or under any of that path's
;; sub-directories, `ag-project' or `ag-project-regexp' will work right away!
