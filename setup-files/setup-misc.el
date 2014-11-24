;; Time-stamp: <2014-11-20 14:28:22 kmodi>

;; Miscellaneous config not categorized in other setup-* files

(fset 'yes-or-no-p 'y-or-n-p) ;; Use y or n instead of yes or no

;; Enable conversion of the selected region to upper case using `C-x C-u`
(put 'upcase-region 'disabled nil)

;; Enable conversion of the selected region to lower case using `C-x C-l`
(put 'downcase-region 'disabled nil)

;; Do not make mouse wheel accelerate its action (example: scrolling)
(setq mouse-wheel-progressive-speed nil)

;; Quitting emacs via `C-x C-c` or the GUI 'X' button
(setq confirm-kill-emacs 'y-or-n-p)

;; Enable shell-script mode for these files automatically
(setq auto-mode-alist
      (append
       '(
         ("\\.setup\\'" . shell-script-mode)
         ("\\.cfg\\'"   . shell-script-mode)
         ) auto-mode-alist))

;; Load newer version of .el and .elc if both are available
(>=e244
 (setq load-prefer-newer t))

;; Execute the script in current buffer
;; Source: http://ergoemacs.org/emacs/elisp_run_current_file.html
(defun xah-run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file xx.py,
then it'll call “python xx.py” in a shell.
The file can be php, perl, python, ruby, javascript, bash, ocaml, vb, elisp.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.

If the file is emacs lisp, run the byte compiled version if exist."
  (interactive)
  (let* (
         (suffixMap
          `(
            ("py"  . "python")
            ("rb"  . "ruby")
            ("sh"  . "bash")
            ("csh" . "tcsh")
            ("pl"  . "perl")))
         (fName (buffer-file-name))
         (fSuffix (file-name-extension fName))
         (progName (cdr (assoc fSuffix suffixMap)))
         (cmdStr (concat progName " \""   fName "\"")))

    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
        (save-buffer) ) )

    (if (string-equal fSuffix "el") ; special case for emacs lisp
        (load (file-name-sans-extension fName))
      (if progName
          (progn
            ;; (view-echo-area-messages)
            (message "Running…")
            (shell-command cmdStr "*xah-run-current-file output*" ))
        (message "No recognized program file suffix for this file.")))))

;; Print to printer defined by env var `PRINTER'
(bind-to-modi-map "p" ps-print-buffer-with-faces)
(bind-to-modi-map "l" xah-run-current-file)

;; Help Functions +
(req-package help-fns+
  :config
  (progn
    (bind-keys
     :map help-map
     ("c"   . describe-key-briefly)
     ("C-c" . describe-command))))

(req-package calc
  :commands (calc quick-calc)
  :init
  (progn
    (bind-keys
     :map modi-mode-map
     ("C-`" . quick-calc)))
  :config
  (progn
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
      (math-mul (calcFunc-arctan x) (math-div (math-pi) 180)))))


;; Unset keys
(global-unset-key (kbd "C-z")) ;; it is bound to `suspend-frame' by default
;; suspend-frame can be called using `C-x C-z` too. And `C-z` is used as prefix
;; key in tmux. So removing the `C-z` binding from emacs makes it possible to
;; use emacs in -nw (no window) mode in tmux if needed without any key binding
;; contention.

;; Kill emacs when running in daemon mode or not
;; Source: http://lists.gnu.org/archive/html/emacs-devel/2011-11/msg00348.html
(defun tv-stop-emacs ()
  (interactive)
  (if (daemonp)
      (save-buffers-kill-emacs)
    (save-buffers-kill-terminal)))

;; The emacs-quitting feature is useful whether or not my minor map is loaded
;; So bind the keys globally instead of to the minor mode map.
(if desktop-save-mode
    (bind-keys
     ("C-x C-c" . save-desktop-save-buffers-kill-emacs)
     ("C-x M-c" . tv-stop-emacs))
  (bind-key "C-x C-c" 'tv-stop-emacs))

;; Source: http://endlessparentheses.com/sweet-new-features-in-24-4.html
;; Hook `eval-expression-minibuffer-setup-hook' is run by ;; `eval-expression'
;; on entering the minibuffer.
;; Below enables ElDoc inside the `eval-expression' minibuffer.
;; Call `M-:' and type something like `(message.' to see what ElDoc does :)
(>=e244
 (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

;; Turn on ElDoc mode in emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)


(provide 'setup-misc)


;; TIPS
;;
;; (1) Un-define a symbol/variable
;; this will make the symbol my-nasty-variable's value void
;; (makunbound 'my-nasty-variable)
;;
;; (2) Un-define a function
;; this will make the symbol my-nasty-function's
;; function definition void
;; (fmakunbound 'my-nasty-function)
;;
;; (3) See a variable value
;; `C-h v`, enter the variable name, Enter
;; Example: `C-h v`, `tooltip-mode`, Enter
;;
;; (4) How to insert superscript
;; `C-x 8 ^ 2` inserts ²
;;
;; (5) Killing buffers from an emacsclient frame
;; `C-x #`   Kills the buffer in emacsclient frame without killing the frame
;; `C-x 5 0` Kills the emacsclient frame
;;
;; (6)
;; `C-q' is bound to `quoted-insert'
;; Example: Pressing `C-q C-l' inserts the `^l' character (form feed):  
;;
;; (7)
;; The way to figure out how to type a particular key combination or to know
;; what a particular key combination does, do help on a key `C-h k`, and type
;; the keystrokes you're interested in. What Emacs shows in the Help buffer is
;; the string you can pass to the macro 'kbd.
;;
;; (8) How to know what the current major mode is?
;; Do `M-:`, type the following `(message "%s" major-mode)` and press Return.
;;
;; (9) calc bindings
;; |---------+----------------------|
;; | Binding | Function             |
;; |---------+----------------------|
;; | d e     | Engineering notation |
;; | &       | 1/x                  |
;; | H L     | log10                |
;; | H E     | exp10 or alog10      |
;; | g       | Grouping on/off      |
;; |---------+----------------------|
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
