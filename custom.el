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
 '(after-save-hook (quote (swoop-cache-clear executable-make-buffer-file-executable-if-script-p sml/generate-buffer-identification)))
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(auto-compression-mode t nil (jka-compr) "uncompress->edit->save->compress .gz, .bz2, .Z files on the fly")
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(bzg-big-fringe-mode nil t)
 '(case-fold-search t nil nil "ignore case when searching; searching using an all-lowercase string will match the same strings with uppercase characters too")
 '(column-number-mode t)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("75c9f0b0499ecdd0c856939a5de052742d85af81814e84faa666522c2bba7e85" "6fe6ab4abe97a4f13533e47ae59fbba7f2919583f9162b440dd06707b01f7794" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "025354235e98db5e7fd9c1a74622ff53ad31b7bde537d290ff68d85665213d85" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "b1471d88b39cad028bd621ae7ae1e8e3e3fca2c973f0dfe3fd6658c194a542ff" "c2cfe2f1440d9ef4bfd3ef4cf15bfe35ff40e6d431264b1e24af64f145cffb11" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" default)))
 '(default-input-method "rfc1345")
 '(delete-selection-mode t nil nil "typing anything after highlighting text overwrites that text; source: http://emacsredux.com/blog/2013/04/12/delete-selection-on-insert/")
 '(display-time-mode t)
 '(doc-view-continuous t)
 '(fci-rule-color "dim gray")
 '(fringe-mode (quote (0)) nil (fringe))
 '(indent-tabs-mode nil nil nil "use spaces instead of tabs for indentation")
 '(indicate-buffer-boundaries (quote ((top . right) (bottom . right))) nil nil "show frame boundaries in the fringe; as the fringe is activated only for the right-hand side, the buffer boundaries (top, bottom) are marked on the right side")
 '(keyboard-coding-system (quote utf-8-unix) nil nil "default EOL system = that of Unix")
 '(magit-auto-revert-mode nil)
 '(major-mode (quote text-mode) nil nil "If the default value of major-mode is nil, the major mode is taken from the previously current buffer")
 '(next-line-add-newlines nil nil nil "Do not auto-add newlines at the end of the file on pressing `C-n` or down arrow")
 '(require-final-newline nil nil nil "Do not auto-add a final newline (if one is not present) when saving/visiting a file")
 '(safe-local-variable-values (quote ((py-indent-offset . 4) (header-auto-update-enabled))))
 '(save-place t nil (saveplace) "save the last cursor location for each file")
 '(show-paren-mode t nil (paren) "allow one to see matching pairs of parentheses; when point is on one of the paired characters, the other is highlighted")
 '(tool-bar-mode nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(visible-bell t nil nil "enable the visible bell or screen blink to happen when there's any error")
 '(w3m-mode-hook (quote (bookmark-w3m-prepare (lambda nil (set (make-local-variable (quote bookmark-make-record-function)) (quote bmkp-make-w3m-record))))) t)
 '(x-select-enable-clipboard t nil nil "after copy Ctrl+c in X11 apps, you can paste by 'yank' in emacs")
 '(x-select-enable-primary t nil nil "after mouse selection in X11, you can paste by 'yank' in emacs"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
