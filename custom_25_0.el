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
 '(ac-stop-words (quote ("/" "//" "/*" "//*" "///" "////")))
 '(after-save-hook
   (quote
    (executable-make-buffer-file-executable-if-script-p)))
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#282828" "#c75646" "#cdee69" "#d0b03c" "#72b3cc" "#992222" "#9cd9f0" "#f7f7f7"])
 '(auto-compression-mode t nil (jka-compr) "uncompress->edit->save->compress .gz, .bz2, .Z files on the fly")
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(default-input-method "rfc1345")
 '(delete-selection-mode t nil nil "typing anything after highlighting text overwrites that text; source: http://emacsredux.com/blog/2013/04/12/delete-selection-on-insert/")
 '(fci-rule-color "#5d5d5d")
 '(fringe-mode nil nil (fringe))
 '(indicate-buffer-boundaries (quote ((top . right) (bottom . right))) nil nil "show frame boundaries in the fringe; as the fringe is activated only for the right-hand side, the buffer boundaries (top, bottom) are marked on the right side")
 '(keyboard-coding-system (quote utf-8-unix) nil nil "default EOL system = that of Unix")
 '(next-line-add-newlines nil nil nil "Do not auto-add newlines at the end of the file on pressing `C-n` or down arrow")
 '(org-confirm-elisp-link-not-regexp "\\(.*switch\\-to\\-buffer.*\\|org-show\\)")
 '(package-selected-packages
   (quote
    (command-log-mode edit-server-htmlize edit-server org-plus-contrib d-mode org-mode cider ox-reveal poporg beacon forecast adice-mode xsos-fns sos-fns defproject org-cliplink pacmacs wordnut use-package-chords info+ smart-mark ox-twbs pomodoro drag-stuff which-key ascii-art-to-unicode bm all all-ext help-fns+ org-trello engine-mode tao-theme list-environment imenu-list git-timemachine easy-escape writegood-mode hideshow-org hideshowvis deft counsel interleave git-link shackle minibuffer-line zop-to-char yasnippet yaml-mode yafolding xkcd wrap-region wgrep-ag web-mode volatile-highlights visual-regexp use-package undo-tree twilight-bright-theme twilight-anti-bright-theme tiny sx swiper sunshine stripe-buffer smex smart-mode-line smart-compile rpn-calc region-bindings-mode rectangle-utils rainbow-mode rainbow-delimiters planet-theme paradox page-break-lines outshine org-tree-slide nlinum neotree navi-mode multiple-cursors multi-term manage-minor-mode magit linum-relative leuven-theme kurecolor keyfreq key-chord iy-go-to-char isend-mode iregister indent-guide ido-vertical-mode ido-ubiquitous ibuffer-projectile hydra hungry-delete htmlize hl-line+ header2 hardcore-mode gplusify gist ggtags fuzzy fold-this flx-ido fill-column-indicator expand-region eww-lnum etags-table etags-select elisp-slime-nav elfeed discover-my-major dired-single dired+ diff-hl darktooth-theme ctags-update csv-nav buffer-move bookmark+ benchmark-init auto-highlight-symbol auto-complete anzu ample-theme aggressive-indent ag adaptive-wrap ace-window)))
 '(safe-local-variable-values
   (quote
    ((eval define-key temp-mode-map
           (kbd "<C-f10>")
           (function dv-docs-dis))
     (eval define-key temp-mode-map
           (kbd "<S-f10>")
           (function dv-docs-ci))
     (eval define-key temp-mode-map
           (kbd "<s-f10>")
           (function modi/org-export-to-html-txt-pdf))
     (eval define-key temp-mode-map
           (kbd "<f10>")
           (function dv-docs-co))
     (do-not-delete-trailing-whitespace . t)
     (checkdoc-minor-mode . t)
     (eval when
           (featurep
            (quote aggressive-indent))
           (aggressive-indent-mode -1))
     (eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))
     (org-export-allow-bind-keywords . t)
     (org-confirm-babel-evaluate)
     (lisp-backquote-indentation . t)
     (eval when
           (require
            (quote rainbow-mode)
            nil t)
           (rainbow-mode 1))
     (py-indent-offset . 4)
     (header-auto-update-enabled)
     (lisp-indent-function . lisp-indent-function))))
 '(send-mail-function (quote sendmail-send-it))
 '(show-paren-mode t nil (paren) "allow one to see matching pairs of parentheses; when point is on one of the paired characters, the other is highlighted")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(visible-bell t nil nil "enable the visible bell or screen blink to happen when there's any error"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
