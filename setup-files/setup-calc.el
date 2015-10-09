;; Time-stamp: <2015-10-09 13:14:57 kmodi>

;; Calculator

(use-package calc
  :commands (calc quick-calc
                  hydra-launch/calc-and-exit hydra-launch/quick-calc-and-exit)
  :bind (:map modi-mode-map
         ("C-`" . modi/calc))
  :init
  (progn
    (setq calc-settings-file (expand-file-name
                              "setup-calc-defaults.el"
                              (concat user-emacs-directory "setup-files/"))))
  :config
  (progn
    (defun modi/calc (regular-calc)
      "Launch `quick-calc' by default.

But if REGULAR-CALC is 1 (C-1 or M-1 prefix),launch `calc`.

Note that `C-u COMMAND' will launch `quick-calc', but
also paste the results of the calculations in the current buffer."
      (interactive "P")
      (if (eq regular-calc 1) ; C-1 prefix
          (let (current-prefix-arg)
            (call-interactively #'calc))
        (call-interactively #'quick-calc)))

    ;; Make alog10(0.3) give the antilog(base 10) in quick-calc
    ;; Same as exp10(0.3)
    (defalias 'calcFunc-alog10 'calcFunc-exp10)

    (defun calcFunc-dbv (x)
      "Return 20*log10(abs(x))
       Usage in quick-calc: dbv(2)"
      (math-mul 20 (calcFunc-log10 (math-abs x))))

    (defun calcFunc-dbp (x)
      "Return 10*log10(abs(x))
       Usage in quick-calc: dbp(2)"
      (math-mul 10 (calcFunc-log10 (math-abs x))))

    (defun calcFunc-dbinv (x)
      "Return 10^(x/20)
       Usage in quick-calc: dbinv(6)"
      (calcFunc-alog10 (math-div x 20)))

    (defun calcFunc-dbinvp (x)
      "Return 10^(x/10)
       Usage in quick-calc: dbinvp(3)"
      (calcFunc-alog10 (math-div x 10)))

    (defun calcFunc-atan (x)
      "Return arctan in radians."
      (math-mul (calcFunc-arctan x) (math-div (math-pi) 180)))

    (defun calcFunc-next2pow (x)
      "Return the next 2's power index. If the input is 7, output is 3
because 2^3 = 8 comes next after 7 |  ceil(log(x)/log(2))"
      (calcFunc-ceil (math-div (calcFunc-log10 x) (calcFunc-log10 2))))

    (use-package calc-yank
      :commands (calc-yank)
      :config
      (progn
        ;; Patch `calc-yank' to prefix radix notation when used with specific
        ;; numeric prefixes.
        ;; http://emacs.stackexchange.com/a/16888/115
        (defun calc-yank (radix)
          "Yank a value into the Calculator buffer.

Valid numeric prefixes for RADIX: 0, 2, 6, 8
No radix notation is prepended for any other numeric prefix.

If RADIX is 2, prepend \"2#\"  - Binary.
If RADIX is 8, prepend \"8#\"  - Octal.
If RADIX is 0, prepend \"10#\" - Decimal.
If RADIX is 6, prepend \"16#\" - Hexadecimal.

If RADIX is a non-nil list (created using \\[universal-argument]), the user
will be prompted to enter the radix in the minibuffer.

If RADIX is nil or if the yanked string already has a calc radix prefix, the
yanked string will be passed on directly to the Calculator buffer without any
alteration."
          (interactive "P")
          (calc-wrapper
           (calc-pop-push-record-list
            0 "yank"
            (let* (radix-num
                   radix-notation
                   valid-num-regexp
                   (thing-raw
                    (if (fboundp 'current-kill)
                        (current-kill 0 t)
                      (car kill-ring-yank-pointer)))
                   (thing
                    (if (or (null radix)
                            ;; Support up to base 36 numbers
                            (string-match-p "\\`\\-*[0-9a-zA-Z]+#" thing-raw))
                        thing-raw
                      (progn
                        (if (listp radix)
                            (progn
                              (setq radix-num
                                    (read-number
                                     "Set radix for yanked content (2-36): "))
                              (when (not (and (integerp radix-num)
                                              (<= 2 radix-num)
                                              (>= 36 radix-num)))
                                (error (concat "The radix has to be an "
                                               "integer between 2 and 36."))))
                          (setq radix-num
                                (cond ((eq radix 2) 2)
                                      ((eq radix 8) 8)
                                      ((eq radix 0) 10)
                                      ((eq radix 6) 16)
                                      (t (message
                                          (concat "No radix prepended "
                                                  "for invalid *numeric* "
                                                  "prefix %0d.")
                                          radix)
                                         nil))))
                        (if radix-num
                            (progn
                              (setq radix-notation
                                    (concat (number-to-string radix-num) "#"))
                              (setq valid-num-regexp
                                    (cond
                                     ;; radix 2 to 10
                                     ((and (<= 2 radix-num)
                                           (>= 10 radix-num))
                                      (concat "[0-"
                                              (number-to-string (1- radix-num))
                                              "]+"))
                                     ;; radix 11
                                     ((= 11 radix-num) "[0-9aA]+")
                                     ;; radix 12+
                                     (t
                                      (concat "[0-9"
                                              "a-" (format "%c" (+ 86 radix-num))
                                              "A-" (format "%c" (+ 54 radix-num))
                                              "]+"))))
                              ;; Ensure that the radix-notation is prefixed
                              ;; correctly even for multi-line yanks like below,
                              ;;   111
                              ;;   1111
                              (replace-regexp-in-string
                               valid-num-regexp
                               (concat radix-notation "\\&")
                               thing-raw))
                          thing-raw)))))
              (if (eq (car-safe calc-last-kill) thing)
                  (cdr calc-last-kill)
                (if (stringp thing)
                    (let ((val (math-read-exprs (calc-clean-newlines thing))))
                      (if (eq (car-safe val) 'error)
                          (progn
                            (setq val (math-read-exprs thing))
                            (if (eq (car-safe val) 'error)
                                (error "Bad format in yanked data")
                              val))
                        val))))))))
        ))))

(use-package rpn-calc
  :commands (hydra-launch/rpn-calc-and-exit)
  :bind (:map modi-mode-map
         ("C-~" . rpn-calc)))


(provide 'setup-calc)

;; You can enter hex numbers in two ways (the same applies to numbers in other
;; bases too)
;; - Enter the hex number directly without switching base as: `16#12cafe'
;; - Switch to hex base by doing `d6', and then enter a hex number as `#12cafe'
;;   When you enter `#', it automatically becomes `16#' as you switch the base
;;   to hex by doing `d6'.

;; calc bindings
;; |--------------------+-----------------------------------------------|
;; | Binding            | Function                                      |
;; |--------------------+-----------------------------------------------|
;; | d e                | Engineering notation                          |
;; | &                  | 1/x                                           |
;; | H L                | log10                                         |
;; | H E                | exp10 or alog10                               |
;; | g                  | Grouping on/off                               |
;; | b w <RET> 16 <RET> | Set binary word to 16 bits                    |
;; | b w <RET> 32 <RET> | Set binary word to 32 bits                    |
;; | d 6                | Convert to hex                                |
;; | O d 6              | Convert to hex (show -ve's in 2's complement) |
;; | d 0                | Convert to dec                                |
;; | d 2                | Convert to bin                                |
;; | O d 2              | Convert to bin (show -ve's in 2's complement) |
;; | n                  | Negate e.g. convert 5 to -5                   |
;; |--------------------+-----------------------------------------------|
;;
;; Basic keys:
;;   Letter keys: Negate; Precision; Yank; Why; Xtended cmd; Quit
;;   Letter keys: SHIFT + Undo, reDo; Inverse, Hyperbolic, Option
;;   Letter keys: SHIFT + sQrt; Sin, Cos, Tan; Exp, Ln, logB
;;   Letter keys: SHIFT + Floor, Round; Abs, conJ, arG; Pi
;;   Letter keys: SHIFT + Num-eval; More-recn; eXec-kbd-macro; Keep-args
;;   Other keys: +, -, *, /, ^, \ (int div), : (frac div)
;;   Other keys: & (1/x), | (concat), % (modulo), ! (factorial)
;;   Other keys: ' (alg-entry), = (eval), ` (edit); M-RET (last-args)
;;   Other keys: SPC/RET (enter/dup), LFD (over); < > (scroll horiz)
;;   Other keys: DEL (drop), M-DEL (drop-above); { } (scroll vert)
;;   Other keys: TAB (swap/roll-dn), M-TAB (roll-up)
;;   Other keys: [ , ; ] (vector), ( , ) (complex), ( ; ) (polar)
;;   Prefix keys: Algebra, Binary/business, Convert, Display
;;   Prefix keys: Functions, Graphics, Help, J (select)
;;   Prefix keys: Kombinatorics/statistics, Modes, Store/recall
;;   Prefix keys: Trail/time, Units/statistics, Vector/matrix
;;   Prefix keys: Z (user), SHIFT + Z (define)
;;   Prefix keys: prefix + ? gives further help for that prefix
;;
;; Inverse-modified keys:
;;   I + S (arcsin), C (arccos), T (arctan); Q (square)
;;   I + E (ln), L (exp), B (alog: B^X); f E (lnp1), f L (expm1)
;;   I + F (ceiling), R (truncate); a S (invert func)
;;   I + a m (match-not); c h (from-hms); k n (prev prime)
;;   I + f G (gamma-Q); f e (erfc); k B (etc., lower-tail dists)
;;   I + V S (reverse sort); V G (reverse grade)
;;   I + v s (remove subvec); v h (tail)
;;   I + t + (alt sum), t M (mean with error)
;;   I + t S (pop std dev), t C (pop covar)
;;
;; Hyperbolic-modified keys:
;;   H + S (sinh), C (cosh), T (tanh); E (exp10), L (log10)
;;   H + F (float floor), R (float round); P (constant "e")
;;   H + a d (total derivative); k c (permutations)
;;   H + k b (bern-poly), k e (euler-poly); k s (stirling-2)
;;   H + f G (gamma-g), f B (beta-B); v h (rhead), v k (rcons)
;;   H + v e (expand w/filler); V H (weighted histogram)
;;   H + a S (general solve eqn), j I (general isolate)
;;   H + a R (widen/root), a N (widen/min), a X (widen/max)
;;   H + t M (median), t S (variance), t C (correlation coef)
;;   H + c f/F/c (pervasive float/frac/clean)
;;
;; Inverse-Hyperbolic-modified keys:
;;   I H + S (arcsinh), C (arccosh), T (arctanh)
;;   I H + E (log10), L (exp10); f G (gamma-G)
;;   I H + F (float ceiling), R (float truncate)
;;   I H + t S (pop variance)
;;   I H + a S (general invert func); v h (rtail)
;;
;; Option-modified keys:
;;
;;
;; `a' prefix (algebra) keys:
;;   Simplify, Extended-simplify, eVal; " (exp-formula)
;;   eXpand, Collect, Factor, Apart, Norm-rat
;;   GCD, /, \, % (polys); Polint
;;   Derivative, Integral, Taylor; _ (subscr)
;;   suBstitute; Rewrite, Match
;;   SHIFT + Solve; Root, miN, maX; Poly-roots; Fit
;;   SHIFT + Map; Tabulate, + (sum), * (prod); num-Integ
;;   relations: =, # (not =), <, >, [ (< or =), ] (> or =)
;;   logical: & (and), | (or), ! (not); : (if)
;;   misc: { (in-set); . (rmeq)
;;
;; `b' prefix (binary/business) keys:
;;   And, Or, Xor, Diff, Not; Wordsize, Clip
;;   Lshift, Rshift, roTate; SHIFT + signed Lshift, Rshift
;;   SHIFT + business: Pv, Npv, Fv, pMt, #pmts, raTe, Irr
;;   SHIFT + business: Sln, sYd, Ddb; %ch
;;
;; `c' prefix (convert) keys:
;;   Deg, Rad, HMS; Float; Polar/rect; Clean, 0-9; %
;;   SHIFT + Fraction
;;
;; `d' prefix (display) keys:
;;   Group, ","; Normal, Fix, Sci, Eng, "."; Over
;;   Radix, Zeros, 2, 8, 0, 6; Hms; Date; Complex, I, J
;;   Why; Line-nums, line-Breaks; <, =, > (justify); Plain
;;   " (strings); Truncate, [, ]; SPC (refresh), RET, @
;;   SHIFT + language: Normal, One-line, Big, Unformatted
;;   SHIFT + language: C, Pascal, Fortran; TeX, LaTeX, Eqn
;;   SHIFT + language: Yacas, X=Maxima, A=Giac
;;   SHIFT + language: Mathematica, W=Maple
;;
;; `f' prefix (functions) keys:
;;   miN, maX; Hypot; Im, Re; Sign; [, ] (incr/decr)
;;   Gamma, Beta, Erf, besselJ, besselY
;;   SHIFT + int-sQrt; Int-log, Exp(x)-1, Ln(x+1); arcTan2
;;   SHIFT + Abssqr; Mantissa, eXponent, Scale
;;   SHIFT + incomplete: Gamma-P, Beta-I
;;
;; `g' prefix (graphics) keys:
;;   Fast; Add, Delete, Juggle; Plot, Clear; Quit
;;   Header, Name, Grid, Border, Key; View-commands, X-display
;;   x-axis: Range, Title, Log, Zero; lineStyle
;;   SHIFT + y-axis: Range, Title, Log, Zero; pointStyle
;;   SHIFT + Print; Device, Output-file; X-geometry
;;   SHIFT + Num-pts; Command, Kill, View-trail
;;   SHIFT + 3d: Fast, Add; CTRL + z-axis: Range, Title, Log
;;
;; `h' prefix (help) keys:
;;   Help; Bindings; Info, Tutorial, Summary; News
;;   describe: Key, C (briefly), Function, Variable
;;
;; `j' prefix (selection) keys:
;;   Select, Additional, Once; eVal, Formula; Rewrite
;;   More, Less, 1-9, Next, Previous
;;   Unselect, Clear; Display; Enable; Breakable
;;   ' (replace), ` (edit), +, -, *, /, RET (grab), DEL
;;   SHIFT + swap: Left, Right; maybe: Select, Once
;;   SHIFT + Commute, Merge, Distrib, jump-Eqn, Isolate
;;   SHIFT + Negate, & (invert); Unpack
;;
;; `k' prefix (combinatorics/statistics) keys:
;;   GCD, LCM; Choose (binomial), Double-factorial
;;   Random, random-Again, sHuffle
;;   Factors, Prime-test, Next-prime, Totient, Moebius
;;   Bernoulli, Euler, Stirling
;;   SHIFT + Extended-gcd
;;   SHIFT + dists: Binomial, Chi-square, F, Normal
;;   SHIFT + dists: Poisson, student's-T
;;
;; `l' prefix (log units) keys:
;;   Quantity, DB level, Np level
;;   +, -, *, /
;;   Scientific pitch notation, Midi number, Frequency
;;
;; `m' prefix (mode) keys:
;;   Deg, Rad, HMS; Frac; Polar; Inf; Alg, Total; Symb; Vec/mat
;;   Working; Xtensions; Mode-save; preserve Embedded modes
;;   SHIFT + Shifted-prefixes, mode-Filename; Record; reCompute
;;   SHIFT + simplify: Off, Num, basIc, Algebraic, Bin, Ext, Units
;;
;; `r' prefix (recall/register) keys:
;;   digits 0-9: recall, same as `s r 0-9'
;;   Save to register, Insert from register
;;
;; `s' prefix (store) keys:
;;   Store, inTo, Xchg, Unstore; Recall, 0-9; : (:=); = (=>)
;;   Let; Copy, K=copy constant; Declare; Insert, Perm; Edit
;;   Negate, +, -, *, /, ^, &, |, [, ]; Map
;;   SHIFT + Decls, GenCount, TimeZone, Holidays; IntegLimit
;;   SHIFT + LineStyles, PointStyles, plotRejects; Units
;;   SHIFT + Eval-, AlgSimp-, ExtSimp-, FitRules
;;
;; `t' prefix (trail/time) keys:
;;   Display; Fwd, Back; Next, Prev, Here, [, ]; Yank
;;   Search, Rev; In, Out; <, >; Kill; Marker; . (abbrev)
;;   SHIFT + time: Now; Part; Date, Julian, Unix, Czone
;;   SHIFT + time: newWeek, newMonth, newYear; Incmonth
;;   SHIFT + time: +, - (business days)
;;   digits 0-9: store-to, same as `s t 0-9'
;;
;; `u' prefix (units/statistics) keys:
;;   Simplify, Convert, Temperature-convert, Base-units
;;   Autorange; Remove, eXtract; Explain; View-table; 0-9
;;   Define, Undefine, Get-defn, Permanent
;;   SHIFT + View-table-other-window
;;   SHIFT + stat: Mean, G-mean, Std-dev, Covar, maX, miN
;;   SHIFT + stat: + (sum), - (asum), * (prod), # (count)
;;
;; `v' or `V' prefix (vector/matrix) keys:
;;   Pack, Unpack, Identity, Diagonal, indeX, Build
;;   Row, Column, Subvector; Length; Find; Mask, Expand
;;   Transpose, Arrange, reVerse; Head, Kons; rNorm
;;   SHIFT + Det, & (inverse), LUD, Trace, conJtrn, Cross
;;   SHIFT + Sort, Grade, Histogram; cNorm
;;   SHIFT + Apply, Map, Reduce, accUm, Inner-, Outer-prod
;;   SHIFT + sets: V (union), ^ (intersection), - (diff)
;;   SHIFT + sets: Xor, ~ (complement), Floor, Enum
;;   SHIFT + sets: : (span), # (card), + (rdup)
;;   <, =, > (justification); , (commas); [, {, ( (brackets)
;;   } (matrix brackets); . (abbreviate); / (multi-lines)
;;
;; `Z' prefix (user) keys:
;;   Define, Undefine, Formula, Kbd-macro, Edit, Get-defn
;;   Composition, Syntax; Invocation; Permanent; Timing
;;   kbd-macros: [ (if), : (else), | (else-if), ] (end-if)
;;   kbd-macros: < > (repeat), ( ) (for), { } (loop)
;;   kbd-macros: / (break)
;;   kbd-macros: ` (save), ' (restore)
