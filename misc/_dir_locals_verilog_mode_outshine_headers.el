(("digital/tb/scoreboards"
  . ((verilog-mode . ((hi-lock-file-patterns
                       . (("\\(^// \\**\\)\\(\\* *.*\\)"
                           (1 'org-hide prepend)
                           (2 '(:inherit org-level-1
                                :height 1.3
                                :weight bold
                                :overline t
                                :underline t)
                              prepend))))
                      ))))
 )
