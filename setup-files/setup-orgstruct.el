;; Time-stamp: <2014-10-28 16:54:45 kmodi>

;; Orgstruct

(eval-after-load "org"
  '(progn
     ))


(provide 'setup-orgstruct)

;;|-----------------+---------------------------------------------------------|
;;| Binding         | Description                                             |
;;|-----------------+---------------------------------------------------------|
;;| C-c C-p         | Go to previous visible heading                          |
;;| C-c C-n         | Go to next visible heading                              |
;;| C-c C-b         | Go to previous heading, same level                      |
;;| C-c C-f         | Go to next heading, same level                          |
;;|-----------------+---------------------------------------------------------|
;;| M-up            | Move the current section up                             |
;;| M-down          | Move the current section down                           |
;;|-----------------+---------------------------------------------------------|
;;| TAB             | Expand/collapse current section                         |
;;| S-TAB           | Cycle visibility of all sections (except archived ones) |
;;| C-u C-u C-u TAB | Show-all, even archived sections                        |
;;|-----------------+---------------------------------------------------------|
