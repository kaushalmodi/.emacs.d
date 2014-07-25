;; Do NOT manually edit the below custom-set-variables block
;; Comments added manually in above custom-set-variables block are not
;; retained. To add comments, do `M-x customize` search for that variable, click
;; on State button for that variable and click on "Add comment". After doing
;; that, click on State button once again and click on "Save for future sessions".
;; *Manually added comments get deleted when emacs updates this block.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ignores (quote ("/" "//" "/*" "//*" "///" "////")))
 '(ac-stop-words (quote ("/" "//" "/*" "//*" "///" "////")))
 '(after-save-hook (quote (swoop-cache-clear executable-make-buffer-file-executable-if-script-p)))
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(auto-compression-mode t nil (jka-compr) "uncompress->edit->save->compress .gz, .bz2, .Z files on the fly")
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(bzg-big-fringe-mode nil t)
 '(case-fold-search t nil nil "ignore case when searching; searching using an all-lowercase string will match the same strings with uppercase characters too")
 '(column-number-mode t)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "de594fe35e10e54f44faf0b8c037ba066900d22cc9e69faf8d238d04cf860131" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e")))
 '(custom-theme-directory "~/.emacs.d/")
 '(custom-theme-load-path (quote ("/home/kmodi/.emacs.d/" "/home/kmodi/.emacs.d/elpa/leuven-theme-20140520.542/" "/home/kmodi/.emacs.d/from-git/zenburn-emacs/" "/home/kmodi/.emacs.d/elpa/smart-mode-line-20140701.112/" custom-theme-directory t)))
 '(default-input-method "rfc1345")
 '(delete-selection-mode t nil nil "typing anything after highlighting text overwrites that text; source: http://emacsredux.com/blog/2013/04/12/delete-selection-on-insert/")
 '(display-time-mode t)
 '(doc-view-continuous t)
 '(fci-rule-color "#383838")
 '(fringe-mode nil nil (fringe))
 '(indent-tabs-mode nil nil nil "use spaces instead of tabs for indentation")
 '(indicate-buffer-boundaries (quote ((top . right) (bottom . right))) nil nil "show frame boundaries in the fringe; as the fringe is activated only for the right-hand side, the buffer boundaries (top, bottom) are marked on the right side")
 '(indicate-empty-lines nil)
 '(keyboard-coding-system (quote utf-8-unix) nil nil "default EOL system = that of Unix")
 '(magit-auto-revert-mode nil)
 '(major-mode (quote text-mode) nil nil "If the default value of major-mode is nil, the major mode is taken from the previously current buffer")
 '(menu-bar-mode nil)
 '(next-line-add-newlines nil nil nil "Do not auto-add newlines at the end of the file on pressing `C-n` or down arrow")
 '(org-confirm-elisp-link-not-regexp "\\(.*switch\\-to\\-buffer.*\\|org-show\\)")
 '(require-final-newline nil nil nil "Do not auto-add a final newline (if one is not present) when saving/visiting a file")
 '(safe-local-variable-values (quote ((org-export-allow-bind-keywords . t) (org-confirm-babel-evaluate) (lisp-backquote-indentation . t) (eval when (require (quote rainbow-mode) nil t) (rainbow-mode 1)) (py-indent-offset . 4) (header-auto-update-enabled))))
 '(save-place t nil (saveplace) "save the last cursor location for each file")
 '(show-paren-mode t nil (paren) "allow one to see matching pairs of parentheses; when point is on one of the paired characters, the other is highlighted")
 '(sml/theme (quote automatic))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(visible-bell t nil nil "enable the visible bell or screen blink to happen when there's any error")
 '(visual-line-fringe-indicators (quote (nil nil)))
 '(w3m-mode-hook (quote (bookmark-w3m-prepare (lambda nil (set (make-local-variable (quote bookmark-make-record-function)) (quote bmkp-make-w3m-record))))) t)
 '(x-select-enable-clipboard t nil nil "after copy Ctrl+c in X11 apps, you can paste by 'yank' in emacs")
 '(x-select-enable-primary t nil nil "after mouse selection in X11, you can paste by 'yank' in emacs"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "#3F3F3F"))))
 '(stripe-hl-line ((t (:inherit nil :overline "gray" :underline "gray" :weight bold :height 1.1))))
 '(yafolding-ellipsis-face ((t (:foreground "deep sky blue" :slant italic :weight bold :height 1.1))) t))
