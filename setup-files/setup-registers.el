;; Time-stamp: <2014-11-13 13:07:37 kmodi>

;; Source: http://stackoverflow.com/questions/12558019/shortcut-to-open-a-specific-file-in-emacs
;; Source: http://www.gnu.org/software/emacs/manual/html_node/emacs/File-Registers.html#File-Registers
;; Save the frequently accessed file locations in registers for quick access


(set-register ?e (cons 'file (concat user-emacs-directory "/init.el"))) ;; C-x r j e

(set-register ?j (cons 'file (concat org-directory "/journal.org"))) ;; C-x r j j

(set-register ?a (cons 'file (concat user-home-directory "/.alias"))) ;; C-x r j a
(set-register ?t (cons 'file (concat user-home-directory "/.tmux.conf"))) ;; C-x r j t
(set-register ?i (cons 'file (concat user-home-directory "/public_html/index.html"))) ;; C-x r j i

(set-register ?v (cons 'file (concat user-home-directory "/docs/IEEE_STD_1800-2012_SystemVerilog.pdf"))) ;; C-x r j v


(provide 'setup-registers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Text-Registers.html#Text-Registers
;;     C-x r s REG <- Copy region to register REG
;; C-u C-x r s REG <- CUT region and move to register REG
;;     C-x r i REG <- Insert text from register REG
;;     C-x r a REG <- Append region to text in register REG
;;     C-x r + REG <- Append region to text in register REG if REG already
;;                    contains text; but increments the content of REG if the
;;                    content is a number.
