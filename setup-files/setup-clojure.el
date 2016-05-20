;; Time-stamp: <2016-05-19 22:14:20 kmodi>

;; Clojure

(use-package cider
  :ensure t
  :defer t)


(provide 'setup-clojure)

;; (eww "http://www.braveclojure.com/introduction")
;;
;; * Clojure Buffer Key Bindings
;;
;; |-------------+------------------------------------------------------|
;; | Keys        | Description                                          |
;; |-------------+------------------------------------------------------|
;; | C-c M-n     | Switch to namespace of current buffer.               |
;; | C-x C-e     | Evaluate expression immediately preceding point.     |
;; | C-c C-k     | Compile current buffer (like `eval-buffer').         |
;; | C-c C-d C-d | Display documentation for symbol under point.        |
;; | M-. and M-, | Navigate to source code for symbol under point and   |
;; |             | return to your original buffer.                      |
;; | C-c C-d C-a | Apropros search; find arbitrary text across function |
;; |             | names and documentation.                             |
;; |-------------+------------------------------------------------------|
;;
;; * REPL Buffer Key Bindings
;;
;; |-------------+---------------------------------|
;; | Keys        | Description                     |
;; |-------------+---------------------------------|
;; | M-p and M-n | Cycle through REPL history.     |
;; | C-RET       | Close parentheses and evaluate. |
;; |-------------+---------------------------------|
