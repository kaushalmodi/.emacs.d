;; Time-stamp: <2020-09-29 23:30:40 kmodi>

;; Calc settings file
;; https://www.gnu.org/software/emacs/manual/html_node/calc/Customizing-Calc.html

;; (setq calc-twos-complement-mode t)
(setq calc-twos-complement-mode nil)

;; Calculator output value format
;; (setq calc-float-format '(eng 4)) ; Engineering notation
(setq calc-float-format '(float 0))
;; |------------+--------------------------------------------------------------+-------------|
;; | value      | Description                                                  | Key Binding |
;; |------------+--------------------------------------------------------------+-------------|
;; | (float 0)  | Floating point format, display full precision.               | d n         |
;; | (float N)  | N > 0: Floating point format, at most N significant figures. |             |
;; | (float -N) | -N < 0: Floating point format, calc-internal-prec - N figs.  |             |
;; | (fix N)    | N >= 0: Fixed point format, N places after decimal point.    | d f         |
;; | (sci 0)    | Scientific notation, full precision.                         | d s         |
;; | (sci N)    | N > 0: Scientific notation, N significant figures.           |             |
;; | (sci -N)   | -N < 0: Scientific notation, calc-internal-prec - N figs.    |             |
;; | (eng 0)    | Engineering notation, full precision.                        | d e         |
;; | (eng N)    | N > 0: Engineering notation, N significant figures.          |             |
;; | (eng -N)   | -N < 0: Engineering notation, calc-internal-prec - N figs."  |             |
;; |------------+--------------------------------------------------------------+-------------|
