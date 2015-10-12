((text-mode . ((hi-lock-file-patterns . (;; seed
                                         ("^\\(SVSEED set from command line: \\([0-9]+\\)\\)"
                                          (1 '(:inherit default
                                               :foreground "white"
                                               :background "blue")
                                             prepend)
                                          (2 '(:inherit default
                                               :foreground "white"
                                               :background "blue"
                                               :height 1.1
                                               :weight bold)
                                             prepend))
                                         ;; standard use case
                                         ("\\(Standard Use Case \\([0-9]+\\)\\)\\s-+:STD_USE_CASE"
                                          (1 '(:inherit default
                                               :foreground "white"
                                               :background "blue")
                                             prepend)
                                          (2 '(:inherit default
                                               :foreground "white"
                                               :background "blue"
                                               :height 1.1
                                               :weight bold)
                                             prepend))
                                         ;; FFT failures
                                         ("^\\s-*\\(\\([a-z_]+\\)\\s-+=.*?, delta =\\s-+\\([-.0-9]+\\).*FAIL.*$\\)"
                                          (1 '(:inherit default
                                               :foreground "black"
                                               :background "salmon")
                                             prepend)
                                          (2 '(:inherit default
                                               :foreground "white"
                                               :background "red")
                                             prepend)
                                          (3 '(:inherit default
                                               :foreground "white"
                                               :background "red")
                                             prepend))
                                         ;; UVM_WARNING
                                         ("^\\s-*\\(UVM_WARNING\\)\\s-+[^:]"
                                          (1 '(:inherit default
                                               :foreground "black"
                                               :background "orange"
                                               :height 1.1)
                                             prepend))
                                         ("^\\s-*\\(UVM_WARNING\\s-+:\\s-+[1-9][0-9]*\\)"
                                          (1 '(:inherit default
                                               :foreground "black"
                                               :background "orange"
                                               :box (:line-width 2 :color "green")
                                               :height 1.1
                                               :weight bold)
                                             prepend))
                                         ;; UVM_ERROR
                                         ("^\\s-*\\(UVM_ERROR\\)\\s-+[^:]"
                                          (1 '(:inherit default
                                               :foreground "white"
                                               :background "red"
                                               :height 1.1)
                                             prepend))
                                         ("^\\s-*\\(UVM_ERROR\\s-+:\\s-+[1-9][0-9]*\\)"
                                          (1 '(:inherit default
                                               :foreground "white"
                                               :background "red"
                                               :box (:line-width 2 :color "green")
                                               :height 1.1
                                               :weight bold)
                                             prepend))
                                         ;; UVM_FATAL
                                         ("^\\s-*\\(UVM_FATAL\\)\\s-+[^:]"
                                          (1 '(:inherit default
                                               :foreground "black"
                                               :background "red"
                                               :height 1.1)
                                             prepend))
                                         ("^\\s-*\\(UVM_FATAL\\s-+:\\s-+[1-9][0-9]*\\)"
                                          (1 '(:inherit default
                                               :foreground "black"
                                               :background "red"
                                               :box (:line-width 2 :color "green")
                                               :height 1.1
                                               :weight bold)
                                             prepend))
                                         )))))
