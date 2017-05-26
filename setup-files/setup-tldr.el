;; Time-stamp: <2017-05-26 11:08:19 kmodi>

;; TLDR
;; https://github.com/tldr-pages/tldr
;; https://github.com/kuanyui/tldr.el
(use-package tldr
  :init
  (progn
    (setq tldr-directory-path (concat temporary-file-directory
                                      (getenv "USER") "/tldr/")) ;must end with /
    (setq tldr-saved-zip-path (concat temporary-file-directory
                                      (getenv "USER") "/tldr-source.zip"))))


(provide 'setup-tldr)
