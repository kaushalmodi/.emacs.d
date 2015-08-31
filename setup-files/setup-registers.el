;; Time-stamp: <2015-08-31 12:50:09 kmodi>

;; Registers

;; http://stackoverflow.com/questions/12558019/shortcut-to-open-a-specific-file-in-emacs
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/File-Registers.html
;; Save the frequently accessed file locations in registers for quick access


(set-register ?e (cons 'file (expand-file-name "init.el" user-emacs-directory))) ; C-x r j e

(set-register ?j (cons 'file (expand-file-name "journal.org" org-directory))) ; C-x r j j

(set-register ?a (cons 'file (expand-file-name ".alias" user-home-directory ))) ; C-x r j a
(set-register ?t (cons 'file (expand-file-name ".tmux.conf" user-home-directory ))) ; C-x r j t
(set-register ?i (cons 'file (expand-file-name
                              "index.org"
                              (concat user-home-directory "public_html/")))) ; C-x r j i

(set-register ?v (cons 'file (expand-file-name
                              "IEEE_STD_1800-2012_SystemVerilog.pdf"
                              (concat user-home-directory "docs/")))) ; C-x r j v


(provide 'setup-registers)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Text-Registers.html

;;     C-x r s REG <- Copy region to register REG
;; C-u C-x r s REG <- CUT region and move to register REG
;;     C-x r i REG <- Insert text from register REG
;;     C-x r a REG <- Append region to text in register REG
;;     C-x r + REG <- Append region to text in register REG if REG already
;;                    contains text; but increments the content of REG if the
;;                    content is a number.
