;; Time-stamp: <2014-05-09 11:07:52 kmodi>

;; Sublimity
;; Source: https://github.com/zk-phi/sublimity

(require 'sublimity)
(require 'sublimity-scroll)
;; (require 'sublimity-map)
;; (require 'sublimity-attractive)

;; Smooth scroll configuration
(setq sublimity-scroll-weight 3
      sublimity-scroll-drift-length 1)
;; Scroll is basically divided into (weight + drift-length) steps.
;; (setq sublimity-scroll-weight 10
;;       sublimity-scroll-drift-length 5)
;; For example, with the configuration above, 100 lines of scrolling is divided
;; into 15 (= 10 + 5) steps
;; (17 15 14 12 11 8 7 5 4 2 1 1 1 1 1)
;; and it looks smoother than 1 step scroll. Note that last 5 steps are all 1
;; line scrolls, because drift-length is set 5.

;; So you may make scroll more
;; smooth by setting drift-length greater. try :
;; (setq sublimity-scroll-weight 5
;;       sublimity-scroll-drift-length 10)
;; With the configuration above, scroll is divided into 15 steps again,
;; (30 24 18 12 6 1 1 1 1 1 1 1 1 1 1)
;; but the last 10 steps are all 1 line scrolls. this looks smoother but
;; perhaps more annoying for some users.

(sublimity-mode 1)


(setq setup-sublimity-loaded t)
(provide 'setup-sublimity)
