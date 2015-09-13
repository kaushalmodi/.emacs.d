;; Time-stamp: <2015-09-13 13:01:52 kmodi>

;; Unicode

(use-package iso-transl
  :config
  (progn
    ;; Add custom bindings to "C-x 8" map
    (dolist (binding
             '(;; >
               (">"       . nil) ; First unbind ">" from the map
               (">="      . [?≥]) ; greater than or equal to
               (">>"      . [?≫]) ; much greater than
               (">\""     . [?»]) ; right-pointing double angle quotation mark
               (">'"      . [?›]) ; single right-pointing angle quotation mark
               (">h"      . [?☛]) ; black right pointing index
               ;; <
               ("<"       . nil) ; First unbind "<" from the map
               ("<="      . [?≤]) ; less than or equal to
               ("<<"      . [?≪]) ; much less than
               ("<\""     . [?«]) ; left-pointing double angle quotation mark
               ("<'"      . [?‹]) ; single left-pointing angle quotation mark
               ("<h"      . [?☚]) ; black left pointing index
               ;; "
               ("\"`"     . [?“]) ; left double quotation mark
               ("\"'"     . [?”]) ; right double quotation mark
               ;; arrows
               ("<right>" . [?→]) ; rightwards arrow
               ("<left>"  . [?←]) ; leftwards arrow
               ("<up>"    . [?↑]) ; upwards arrow
               ("<down>"  . [?↓]) ; downwards arrow
               ;; misc
               ("r"       . [?▯]) ; white vertical rectangle
               ("R"       . [?▮]) ; black vertical rectangle
               ("*r"      . [?₹]) ; indian rupee sign
               ("e"       . [?↵]) ; downwards arrow with corner leftwards
               ("E"       . [?⏎]) ; return symbol
               ("1/3"     . [?⅓]) ; fraction one third
               ("0"       . [?​]))) ; zero width space
      (define-key iso-transl-ctl-x-8-map (kbd (car binding)) (cdr binding)))))


(provide 'setup-unicode)

;; Unicode chars that can be entered using C-x 8 binding

;; |------------------+-------------------------|
;; | C-x 8 prefix map | Unicode                 |
;; | binding          | character               |
;; |------------------+-------------------------|
;; | {                | “                       |
;; | }                | ”                       |
;; | ^1               | ¹                       |
;; | ^2               | ²                       |
;; | ^3               | ³                       |
;; | .                | ·                       |
;; | **               | •                       |
;; | o                | ° (degree)              |
;; | ~=               | ≈                       |
;; | /=               | ≠                       |
;; | +                | ±                       |
;; | /o               | ø                       |
;; | 1/2              | ½                       |
;; | 1/4              | ¼                       |
;; | 3/4              | ¾                       |
;; | R                | ®                       |
;; | C                | ©                       |
;; | m                | µ                       |
;; | 'a               | á                       |
;; | 'e               | é                       |
;; | 'i               | í                       |
;; | 'o               | ó                       |
;; | 'u               | ú                       |
;; | "u               | ü                       |
;; | ~n               | ñ                       |
;; | ?                | ¿                       |
;; | !                | ¡                       |
;; | _n               | – (en dash)             |
;; | _m               | — (em dash)             |
;; | _h               | ‐ (hyphen)              |
;; | _H               | ‑ (non-breaking hyphen) |
;; | _-               | − (minus sign)          |
;; | a>               | →                       |
;; | a<               | ←                       |
;; |------------------+-------------------------|
