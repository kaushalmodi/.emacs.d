;; Time-stamp: <2015-07-26 17:36:56 kmodi>

;; https://github.com/abo-abo/tiny

(use-package tiny
  :config
  (progn
    (defun modi/tiny-helper (&optional end-val begin-val sep op fmt)
      "Helper function for `tiny-expand'.

If `tiny' expansion is possible at point, do it. Otherwise activate the helper
to generate a valid “tiny expression” and expand that.

Usage: M-x COMMAND ↵↵↵↵↵            -> 0 1 2 3 4 5 6 7 8 9
       M-x COMMAND 9↵2↵_↵+1*x2↵↵    -> 5_7_9_11_13_15_17_19
       M-x COMMAND 15↵1↵↵-30*2x↵%x↵ -> 1c 1a 18 16 14 12 10 e c a 8 6 4 2 0
"
      (interactive
       (when (null (tiny-mapconcat)) ; enable the helper only if tiny expansion is
                                        ; not possible at point.
         (list (number-to-string (string-to-number
                                  (read-string
                                   (concat "END value "
                                           "[default=0; auto-set to 9 if both "
                                           "begin and end values are 0]: "))))
               (number-to-string (string-to-number
                                  (read-string
                                   (concat "BEGIN value "
                                           "[default=0; this has to be "
                                           "*smaller* than the end value]: "))))
               (read-string (concat "Separator "
                                    "[default=Space; "
                                    "no math operators like - or = allowed]: "))
               (read-string (concat "Lisp Operation "
                                    "[parentheses are optional; "
                                    "eg: *xx | (+ x ?A) | *2+3x]: "))
               (read-string (concat "Format "
                                    "[eg: %x | 0x%x | %c | %s | %(+ x x) | "
                                    "%014.2f | %03d; parentheses required "
                                    "here for sexps]: ")))))
      (when (null (tiny-mapconcat)) ; enable the helper only if tiny expansion is
                                        ; not possible at point.
        (let ((tiny-key-binding (or (substitute-command-keys "\\[modi/tiny-helper]")
                                    (substitute-command-keys "\\[tiny-expand]")))
              (begin-val-num (string-to-number begin-val))
              (end-val-num (string-to-number end-val))
              tiny-expr)
          ;; Begin and end values cannot be same
          (when (= end-val-num begin-val-num)
            (if (zerop end-val-num) ; if both are zero, set the end value to 9
                (setq end-val "9")
              (error (concat "Begin and end values cannot be same; "
                             "Begin value = " begin-val
                             ", End value = " end-val "."))))
          ;; End value has to be greater than the begin value
          (when (< end-val-num begin-val-num)
            (error (concat "End value has to be greater than the begin value; "
                           "Begin value = " begin-val
                           ", End value = " end-val ".")))
          (when (string= sep "")
            (setq sep " ")) ; `sep' cannot be empty string
          (when (not (string= fmt ""))
            ;; When non-nil, prefix `fmt' with the `|' char for reading clarity
            (setq fmt (concat "|" fmt)))
          (when (string= begin-val "0")
            (setq begin-val "") ; it's OK to not specify begin-val if it is 0
            (when (string= sep " ")
              (setq sep "")))
          (setq tiny-expr (concat "m" begin-val sep end-val op fmt))
          (message "%s" (concat "This "
                                (propertize "tiny"
                                            'face 'font-lock-function-name-face)
                                " expansion can also be done by typing "
                                (propertize tiny-expr
                                            'face 'font-lock-keyword-face)
                                " and then "
                                (propertize tiny-key-binding
                                            'face 'font-lock-keyword-face)
                                (when (null tiny-key-binding)
                                  (concat
                                   (propertize "M-x modi/tiny-helper"
                                               'face 'font-lock-keyword-face)
                                   " or "
                                   (propertize "M-x tiny-expand"
                                               'face 'font-lock-keyword-face)))
                                "."))
          (insert tiny-expr)
          (undo-boundary)))
      (tiny-expand))

    (bind-key "C-c \\" #'modi/tiny-helper modi-mode-map)
    (key-chord-define-global "]\\" #'modi/tiny-helper)))


(provide 'setup-tiny)

;; General Format
;; mBSEO|F
;;  ││││―
;;  │││││└──> (optional) Format - %x | 0x%x | %c | %s | %(+ x x) | %014.2f | %03d | %(date "Jan 16" (* x 7))
;;  ││││└───> (optional) Pipe character used if Format specified for reading clarity
;;  │││└────> (optional) Lisp Operation - *xx | (* x x) | (+ x ?A) | *2+3x | (* 2 (+ 3 x))
;;  ││└─────> End value
;;  │└──────> (optional) Separator - Space | , | \n (default=Space)
;;  └───────> (optional) Begin value (default=0)
;; - No space allowed between 'm' and 'B'
;; - No space allowed between 'E' and 'O'

;; ================================================================================
;; m10 or m0 10
;; TINY HELPER 10↵↵↵↵↵
;; --------------------------------------------------------------------------------
;; 0 1 2 3 4 5 6 7 8 9 10

;; ================================================================================
;; m\n10 or m0\n10
;; TINY HELPER 10↵↵\n↵↵↵
;; --------------------------------------------------------------------------------
;; 0
;; 1
;; 2
;; 3
;; 4
;; 5
;; 6
;; 7
;; 8
;; 9
;; 10

;; ================================================================================
;; m5 10
;; TINY HELPER 10↵5↵↵↵↵
;; --------------------------------------------------------------------------------
;; 5 6 7 8 9 10

;; ================================================================================
;; m5,10
;; TINY HELPER 10↵5↵,↵↵↵
;; --------------------------------------------------------------------------------
;; 5,6,7,8,9,10

;; ================================================================================
;; m5 10*xx or m5 10* x x or m5 10(* x x)
;; TINY HELPER 10↵5↵↵*xx↵↵
;; --------------------------------------------------------------------------------
;; 25 36 49 64 81 100

;; ================================================================================
;; m10-10x or m0 10(- 10 x) (decrementing numbers)
;; TINY HELPER 10↵↵↵-10x↵↵
;; --------------------------------------------------------------------------------
;; 10 9 8 7 6 5 4 3 2 1 0

;; ================================================================================
;; m5 15*xx%x
;; TINY HELPER 15↵5↵↵*xx↵%x↵
;; --------------------------------------------------------------------------------
;; 19 24 31 40 51 64 79 90 a9 c4 e1

;; ================================================================================
;; m5 10*xx|0x%x
;; TINY HELPER 10↵5↵↵*xx↵0x%x↵
;; --------------------------------------------------------------------------------
;; 0x19 0x24 0x31 0x40 0x51 0x64

;; ================================================================================
;; m25+x?a%c or m25(+ x ?a) | %c
;; TINY HELPER 25↵↵↵+x?a↵%c↵
;; --------------------------------------------------------------------------------
;; a b c d e f g h i j k l m n o p q r s t u v w x y z

;; ================================================================================
;; m25+x?A%c or m25(+ x ?A) | %c or m25+x65%c
;; TINY HELPER 25↵↵↵+x?A↵%c↵
;; --------------------------------------------------------------------------------
;; A B C D E F G H I J K L M N O P Q R S T U V W X Y Z

;; ================================================================================
;; m97,122(string x)
;; TINY HELPER 122↵97↵,↵stringx↵↵
;; --------------------------------------------------------------------------------
;; a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z

;; ================================================================================
;; m97,122stringxx
;; TINY HELPER 122↵97↵,↵stringxx↵↵
;; --------------------------------------------------------------------------------
;; aa,bb,cc,dd,ee,ff,gg,hh,ii,jj,kk,ll,mm,nn,oo,pp,qq,rr,ss,tt,uu,vv,ww,xx,yy,zz

;; ================================================================================
;; m97,120stringxupcasex
;; TINY HELPER 120↵97↵,↵stringxupcasex↵↵
;; --------------------------------------------------------------------------------
;; aA,bB,cC,dD,eE,fF,gG,hH,iI,jJ,kK,lL,mM,nN,oO,pP,qQ,rR,sS,tT,uU,vV,wW,xX

;; ================================================================================
;; m97,120stringxupcasex)x (Note that the parenthesis separating the last two x's.
;;                          That is required as we do NOT want to upcase the last
;;                          x too.)
;; TINY HELPER 120↵97↵,↵stringxupcasex)x↵↵
;; --------------------------------------------------------------------------------
;; aAa,bBb,cCc,dDd,eEe,fFf,gGg,hHh,iIi,jJj,kKk,lLl,mMm,nNn,oOo,pPp,qQq,rRr,sSs,tTt,uUu,vVv,wWw,xXx

;; ================================================================================
;; m\n;; 10|%(+ x x) and %(* x x) and %s
;; TINY HELPER 10↵↵\n;; ↵↵%(+ x x) and %(* x x) and %s↵
;; --------------------------------------------------------------------------------
;; ;; 0 and 0 and 0
;; ;; 2 and 1 and 1
;; ;; 4 and 4 and 2
;; ;; 6 and 9 and 3
;; ;; 8 and 16 and 4
;; ;; 10 and 25 and 5
;; ;; 12 and 36 and 6
;; ;; 14 and 49 and 7
;; ;; 16 and 64 and 8
;; ;; 18 and 81 and 9
;; ;; 20 and 100 and 10

;; ================================================================================
;; m10*2+3x or m10(* 2 (+ 3 x))
;; TINY HELPER 10↵↵↵*2+3x↵↵
;; --------------------------------------------------------------------------------
;; 6 8 10 12 14 16 18 20 22 24 26

;; ================================================================================
;; m\n;; 10expx
;; TINY HELPER 10↵↵\n;; ↵expx↵↵
;; --------------------------------------------------------------------------------
;; ;; 1.0
;; ;; 2.718281828459045
;; ;; 7.38905609893065
;; ;; 20.085536923187668
;; ;; 54.598150033144236
;; ;; 148.4131591025766
;; ;; 403.4287934927351
;; ;; 1096.6331584284585
;; ;; 2980.9579870417283
;; ;; 8103.083927575384
;; ;; 22026.465794806718

;; ================================================================================
;; m5\n;; 20expx%014.2f
;; TINY HELPER 20↵5↵\n;; ↵expx↵%014.2f↵
;; --------------------------------------------------------------------------------
;; ;; 00000000148.41
;; ;; 00000000403.43
;; ;; 00000001096.63
;; ;; 00000002980.96
;; ;; 00000008103.08
;; ;; 00000022026.47
;; ;; 00000059874.14
;; ;; 00000162754.79
;; ;; 00000442413.39
;; ;; 00001202604.28
;; ;; 00003269017.37
;; ;; 00008886110.52
;; ;; 00024154952.75
;; ;; 00065659969.14
;; ;; 00178482300.96
;; ;; 00485165195.41

;; ================================================================================
;; m7|%(expt 2 x)
;; TINY HELPER 7↵↵↵↵%(expt 2 x)↵ (Note that parentheses are required for
;;                                    sexps in Format)
;; --------------------------------------------------------------------------------
;; 1 2 4 8 16 32 64 128

;; ================================================================================
;; m, 7|0x%02x
;; TINY HELPER 7↵↵, ↵↵0x%02x↵
;; --------------------------------------------------------------------------------
;; 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07

;; ================================================================================
;; m9|%0.2f
;; TINY HELPER ↵↵↵↵%0.2f↵
;; --------------------------------------------------------------------------------
;; 0.00 1.00 2.00 3.00 4.00 5.00 6.00 7.00 8.00 9.00

;; ================================================================================
;; m1\n14|*** TODO http://emacsrocks.com/e%02d.html
;; TINY HELPER 14↵1↵\n↵↵*** TODO http://emacsrocks.com/e%02d.html↵
;; --------------------------------------------------------------------------------
;; *** TODO http://emacsrocks.com/e01.html
;; *** TODO http://emacsrocks.com/e02.html
;; *** TODO http://emacsrocks.com/e03.html
;; *** TODO http://emacsrocks.com/e04.html
;; *** TODO http://emacsrocks.com/e05.html
;; *** TODO http://emacsrocks.com/e06.html
;; *** TODO http://emacsrocks.com/e07.html
;; *** TODO http://emacsrocks.com/e08.html
;; *** TODO http://emacsrocks.com/e09.html
;; *** TODO http://emacsrocks.com/e10.html
;; *** TODO http://emacsrocks.com/e11.html
;; *** TODO http://emacsrocks.com/e12.html
;; *** TODO http://emacsrocks.com/e13.html
;; *** TODO http://emacsrocks.com/e14.html

;; ================================================================================
;; m1\n10|convert img%s.jpg -monochrome -resize 50%% -rotate 180 img%s_mono.pdf
;; TINY HELPER 10↵1↵\n↵↵convert img%s.jpg -monochrome -resize 50%% -rotate 180 img%s_mono.pdf↵
;; --------------------------------------------------------------------------------
;; convert img1.jpg -monochrome -resize 50% -rotate 180 img1_mono.pdf
;; convert img2.jpg -monochrome -resize 50% -rotate 180 img2_mono.pdf
;; convert img3.jpg -monochrome -resize 50% -rotate 180 img3_mono.pdf
;; convert img4.jpg -monochrome -resize 50% -rotate 180 img4_mono.pdf
;; convert img5.jpg -monochrome -resize 50% -rotate 180 img5_mono.pdf
;; convert img6.jpg -monochrome -resize 50% -rotate 180 img6_mono.pdf
;; convert img7.jpg -monochrome -resize 50% -rotate 180 img7_mono.pdf
;; convert img8.jpg -monochrome -resize 50% -rotate 180 img8_mono.pdf
;; convert img9.jpg -monochrome -resize 50% -rotate 180 img9_mono.pdf
;; convert img10.jpg -monochrome -resize 50% -rotate 180 img10_mono.pdf

;; ================================================================================
;; (setq foo-list '(m1 11+x96|?%c))
;; (setq foo-list '(TINY HELPER 11↵1↵↵+x96↵?%c))↵
;; --------------------------------------------------------------------------------
;; (setq foo-list '(?a)) ?b)) ?c)) ?d)) ?e)) ?f)) ?g)) ?h)) ?i)) ?j)) ?k))

;; ================================================================================
;; m1\n10listx+x96|convert img%s.jpg -monochrome -resize 50%% -rotate 180 img%c_mono.pdf
;; TINY HELPER 10↵1↵\n↵listx+x96↵convert img%s.jpg -monochrome -resize 50%% -rotate 180 img%c_mono.pdf
;; --------------------------------------------------------------------------------
;; convert img1.jpg -monochrome -resize 50% -rotate 180 imga_mono.pdf
;; convert img2.jpg -monochrome -resize 50% -rotate 180 imgb_mono.pdf
;; convert img3.jpg -monochrome -resize 50% -rotate 180 imgc_mono.pdf
;; convert img4.jpg -monochrome -resize 50% -rotate 180 imgd_mono.pdf
;; convert img5.jpg -monochrome -resize 50% -rotate 180 imge_mono.pdf
;; convert img6.jpg -monochrome -resize 50% -rotate 180 imgf_mono.pdf
;; convert img7.jpg -monochrome -resize 50% -rotate 180 imgg_mono.pdf
;; convert img8.jpg -monochrome -resize 50% -rotate 180 imgh_mono.pdf
;; convert img9.jpg -monochrome -resize 50% -rotate 180 imgi_mono.pdf
;; convert img10.jpg -monochrome -resize 50% -rotate 180 imgj_mono.pdf

;; ================================================================================
;; m1\n10(- (+ x ?A) 1)|;; convert img%(+ x 0).jpg -monochrome -resize 50%% -rotate 180 img%c_mono.pdf
;; TINY HELPER 10↵1↵\n↵(- (+ x ?A) 1)↵;; convert img%(+ x 0).jpg -monochrome -resize 50%% -rotate 180 img%c_mono.pdf
;;  - Here parentheses are used in the Lisp Operation section for clarity
;; --------------------------------------------------------------------------------
;; convert img1.jpg -monochrome -resize 50% -rotate 180 imgA_mono.pdf
;; convert img2.jpg -monochrome -resize 50% -rotate 180 imgB_mono.pdf
;; convert img3.jpg -monochrome -resize 50% -rotate 180 imgC_mono.pdf
;; convert img4.jpg -monochrome -resize 50% -rotate 180 imgD_mono.pdf
;; convert img5.jpg -monochrome -resize 50% -rotate 180 imgE_mono.pdf
;; convert img6.jpg -monochrome -resize 50% -rotate 180 imgF_mono.pdf
;; convert img7.jpg -monochrome -resize 50% -rotate 180 imgG_mono.pdf
;; convert img8.jpg -monochrome -resize 50% -rotate 180 imgH_mono.pdf
;; convert img9.jpg -monochrome -resize 50% -rotate 180 imgI_mono.pdf
;; convert img10.jpg -monochrome -resize 50% -rotate 180 imgJ_mono.pdf

;; ================================================================================
;; m\n;; 16list*xxx)*xx%s:%s:%s
;; TINY HELPER 16↵↵\n;; ↵list*xxx)*xx↵%s:%s:%s
;; --------------------------------------------------------------------------------
;; ;; 0:0:0
;; ;; 1:1:1
;; ;; 8:4:2
;; ;; 27:9:3
;; ;; 64:16:4
;; ;; 125:25:5
;; ;; 216:36:6
;; ;; 343:49:7
;; ;; 512:64:8
;; ;; 729:81:9
;; ;; 1000:100:10
;; ;; 1331:121:11
;; ;; 1728:144:12
;; ;; 2197:169:13
;; ;; 2744:196:14
;; ;; 3375:225:15
;; ;; 4096:256:16

;; ================================================================================
;; m\n8|**** TODO Learning from Data Week %(+ x 2) \nSCHEDULED: <%(date "Oct 7" (* x 7))> DEADLINE: <%(date "Oct 14" (* x 7))>
;; TINY HELPER 8↵↵\n;; ↵↵**** TODO Learning from Data Week %(+ x 2) \nSCHEDULED: <%(date "Oct 7" (* x 7))> DEADLINE: <%(date "Oct 14" (* x 7))>
;; --------------------------------------------------------------------------------
;; **** TODO Learning from Data Week 2
;; SCHEDULED: <2015-10-07 Wed> DEADLINE: <2015-10-14 Wed>
;; **** TODO Learning from Data Week 3
;; SCHEDULED: <2015-10-14 Wed> DEADLINE: <2015-10-21 Wed>
;; **** TODO Learning from Data Week 4
;; SCHEDULED: <2015-10-21 Wed> DEADLINE: <2015-10-28 Wed>
;; **** TODO Learning from Data Week 5
;; SCHEDULED: <2015-10-28 Wed> DEADLINE: <2015-11-04 Wed>
;; **** TODO Learning from Data Week 6
;; SCHEDULED: <2015-11-04 Wed> DEADLINE: <2015-11-11 Wed>
;; **** TODO Learning from Data Week 7
;; SCHEDULED: <2015-11-11 Wed> DEADLINE: <2015-11-18 Wed>
;; **** TODO Learning from Data Week 8
;; SCHEDULED: <2015-11-18 Wed> DEADLINE: <2015-11-25 Wed>
;; **** TODO Learning from Data Week 9
;; SCHEDULED: <2015-11-25 Wed> DEADLINE: <2015-12-02 Wed>
;; **** TODO Learning from Data Week 10
;; SCHEDULED: <2015-12-02 Wed> DEADLINE: <2015-12-09 Wed>

;; ================================================================================
;; m\n8%(date "Jan 16" (* x 7))
;; TINY HELPER 8↵↵\n↵↵%(date "Jan 16" (* x 7))
;; --------------------------------------------------------------------------------
;; 2015-01-16 Fri
;; 2015-01-23 Fri
;; 2015-01-30 Fri
;; 2015-02-06 Fri
;; 2015-02-13 Fri
;; 2015-02-20 Fri
;; 2015-02-27 Fri
;; 2015-03-06 Fri
;; 2015-03-13 Fri

;; ================================================================================
;; m-7\n0%(date "Jan 16" (* x 7))
;; TINY HELPER ↵-7↵\n↵↵%(date "Jan 16" (* x 7))
;; --------------------------------------------------------------------------------
;; 2014-11-28 Fri
;; 2014-12-05 Fri
;; 2014-12-12 Fri
;; 2014-12-19 Fri
;; 2014-12-26 Fri
;; 2015-01-02 Fri
;; 2015-01-09 Fri
;; 2015-01-16 Fri
