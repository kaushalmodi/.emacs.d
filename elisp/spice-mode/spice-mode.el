;;; spice-mode.el --- major mode providing a spice mode hook for fontification
;;; $Id: spice-mode.el,v 1.100 2007/01/03 16:46:46 vdplasg Exp $

;; Emacs Lisp Archive Entry
;; Author: Geert A. M. Van der Plas <geert_vanderplas@email.com> 1999-
;;         Emmanuel Rouat <emmanuel.rouat@wanadoo.fr> 1997-
;;         Carlin J. Vieri, MIT AI Lab <cvieri@ai.mit.edu> 1994
;; Keywords: spice, spice2g6, spice3, eldo, hspice, layla, mondriaan, fasthenry, CDL, spectre compatibility, netlist editing
;; Filename: spice-mode.el
;; Version: 1.2.25
;; Maintainer: Geert A. M. Van der Plas <geert_vanderplas@email.com>
;; Last-Updated: 01 November 2004
;; Description: spice file editing
;; URL: http://spice-mode.4t.com/
;; old-URL: http://www.esat.kuleuven.ac.be/~vdplas/emacs/
;; Compatibility: Emacs2[01]

;; Please send suggestions and bug reports to
;; mailto:Geert_VanderPlas@email.com

;; Copyright (C) 1994, MIT Artificial Intelligence Lab
;; Copyright (C) 1997- Emmanuel Rouat
;; Copyright (C) 1999- Geert A. M. Van der Plas

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; INSTALL:
;; ========

;; byte compile spice-mode.el to spice-mode.elc (see `byte-compile-file')
;; put these two files in an arbitrary, but accesible directory
;; for example: $HOME/emacs, /usr/lib/emacs/site-lisp/ or
;; /usr/local/lib/emacs/site-lisp/

;; If you chose a non-standard place to put the files add the following
;; line to your Emacs start-up file (`.emacs') or custom `site-start.el'
;; file (replace <directory-name> by the directory where you placed
;; spice-mode.el and spice-mode.elc):
;; (setq load-path (cons (expand-file-name "<directory-name>") load-path))

;; To use spice-mode, add either the following to your `.emacs' file. This
;; assumes that you will use the .sp, .cir, .ckt, .mod, ... extensions for
;; your spice source decks and output files:
;; (autoload 'spice-mode "spice-mode" "Spice/Layla Editing Mode" t)
;; (setq auto-mode-alist (append (list (cons "\\.sp$" 'spice-mode)
;; 				       (cons "\\.cir$" 'spice-mode)
;; 				       (cons "\\.ckt$" 'spice-mode)
;; 				       (cons "\\.mod$" 'spice-mode)
;; 				       (cons "\\.cdl$" 'spice-mode)
;; 				       (cons "\\.chi$" 'spice-mode) ;eldo outpt
;; 				       (cons "\\.inp$" 'spice-mode))
;; 				 auto-mode-alist))

;; Other way to load spice mode (by Andrew Scott):
;; ;;; Testing for spice-mode (Another spice mode, also works on Spice,
;; ;;; Eldo, Layla; this one works on GNU Emacs-20.6 issues).
;; (autoload 'spice-mode "spice-mode" "Spice/Layla Editing Mode" t)
;; (add-to-list 'auto-mode-alist '("\\.cir$"          . spice-mode))
;; (add-to-list 'auto-mode-alist '("\\.ckt$"          . spice-mode))
;; (add-to-list 'auto-mode-alist '("\\.inp$"          . spice-mode))
;; (add-to-list 'auto-mode-alist '("\\.spout$"        . spice-mode));hspice out
;; (add-to-list 'auto-mode-alist '("\\.pdir$"         . spice-mode))
;; ;;; Intel formats
;; (add-to-list 'auto-mode-alist '("\\.[sS][pP]$"     . spice-mode))
;; (add-to-list 'auto-mode-alist '("\\.[sm]?t0$"      . spice-mode))
;; (add-to-list 'auto-mode-alist '("\\.[h]?spice$"    . spice-mode))

;; CUSTOMIZATION:
;; ==============

;; use customization to modify the behaviour of spice-mode; add
;; custom-set-variables to your `.emacs' file, for example:
;; (custom-set-variables
;; ; '(spice-initialize-file-function (quote geert-spice-file-header)) ;; use geert-spice-file-header function (not included in this file !)
;;  '(spice-initialize-empty-file t)         ;; initialize empty/new spice file
;; ; '(spice-standard '(spice2g6 (hspice eldo eldorf eldovloga layla))) ;; all 4 modes
;;  '(spice-standard (quote (spice2g6 (hspice eldo))))    ;; hspice and eldo
;;  '(spice-standard (quote (spice2g6 ())))               ;; spice2g6/3 only
;;  '(spice-simulator "Spice3")                           ;; default simulator
;;  '(spice-waveform-viewer "Nutmeg")                     ;; default waveform
;;  '(spice-highlight-keywords nil)                       ;; less highlighting
;;  '(spice-section-alist                                 ;; add own sections
;;    ;; this is ugly, I know ;)
;;    (append (nth 1 (nth 0 (get 'spice-section-alist 'standard-value)))
;; 	      (list
;; 	       (list "My Header"    "MY HEADER"    nil)
;; 	       )))
;;  '(spice-show-describe-mode nil)         ;; don't describe mode at startup
;;  )

;; This can also be achieved interactively through customizations !

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The original spice-mode.el created by C. Vieri has been modified
;; extensively. These changes have been heavily influenced by the eldo-mode.el
;; of Emmanuel Rouat (the tempo-templates for one) and the vhdl-mode.el of
;; Reto Zimmermann and Rodney J. Whitby (the font-locking, the
;; customization, the menus, ...) Help in understanding 'advanced' lisp
;; was provided by G. Debyser <geert.debyser@advalvas.be>, our (common) lisp
;; expert. Since version 0.97 a lot of input/ideas have been provided by
;; Emmanuel Rouat. Just take a look and search for eldo-mode ;)

;; This package provides an Emacs major mode for editing SPICE decks.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;   - customization group spice- needs to be improved
;;     activate customizations needs to be looked at (potentially removed)
;;   - fix problem imenu sometimes truncating filenames of include files
;;     by replacing the functionality with own function
;;   - improve Y instance handling. use builtin macromodels, and own development;;     models
;;   - ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BUGS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  - searching for .subckt could be buggy because of user's file structure
;;  - xinstance highlighting is buggy, and probably always will ?
;;  - font-lock expressions are incredibly complicated, and multiple spice
;;    formats are not handled correctly at the same time...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If you have any questions about this mode, feel free to contact me
;; at the following address:  geert_vanderplas@email.com. If I find the
;; time, I can take a look at the problem

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst spice-version "1.2.25 (03 Jan 2007)"
  "Current version of spice mode (spice2g6/spice3/hspice/eldo(rf&verilog-a)/fasthenry/layla/mondriaan/draccdl/spectre).")

(defconst spice-developer
  "Geert Van der Plas & Emmanuel Rouat (<geert_vanderplas@email.com>, <emmanuel.rouat@wanadoo.fr>)"
  "Current developers/maintainers of spice-mode.")

(defvar spice-tempo-tags nil
  "List of templates used in spice mode.")


;; help function
(defun spice-custom-set (variable value &rest functions)
  "Set variables as in `custom-set-default' and call FUNCTIONS (if
bound) afterwards."
  (if (fboundp 'custom-set-default)
      (custom-set-default variable value)
    (set-default variable value))
  (while functions
    (when (fboundp (car functions)) (funcall (car functions)))
    (setq functions (cdr functions))))


(defgroup spice nil
  "Customizations for Spice mode."
  :prefix "spice-"
  :link  '(url-link "http://spice-mode.4t.com/")
  :group 'languages
  )


;;;###autoload
(defcustom spice-mode-hook nil
  "*List of hook functions run by `spice-mode' (see `run-hooks')."
  :type 'hook
  :group 'spice)


;;;###autoload
(defcustom spice-standard '(spice2g6 (hspice eldo eldorf eldovloga fasthenry)) ; "layla" has been removed
  "*Spice standards used.
Basic standard:
  Spice2g6    : Original Berkeley Spice (leave this always on!)
Additional standards:
  Hspice (TM) : Commercial Spice, formerly Meta Software, now owned by Synopsys
  Eldo (TM)   : Commercial Spice, formerly Anacad, now owned by Mentor Graphics
  EldoRf (TM) : RF Steady State analyses of Eldo (also turn on Eldo!)
  EldoVlogA   : Verilog-A extensions to Eldo netlist language (also turn on Eldo!)
  FastHenry   : Multipole-accelerated inductance analysis program from MIT
  Layla       : KULeuven LAYLA (layout synthesis) extensions to Spice format
  Mondriaan   : KULeuven MONDRIAAN (layout synthesis) extensions (also turn on Layla!)
  DracCDL (TM): Dracula CDL extensions (Cadence LOGLVS netlists)
  Spectre (TM): Spice compatibility of Spectre language (simulator language=spice decks)
"
  :type '(list (choice :tag "Basic standard"
                 (const :tag "Spice2g6" spice2g6))
	       (set :tag "Additional standards" :indent 2
                 (const :tag "Hspice" hspice)
                 (const :tag "Eldo"   eldo)
                 (const :tag "Eldo RF" eldorf)
                 (const :tag "Eldo Verilog-A" eldovloga)
                 (const :tag "FastHenry" fasthenry)
                 (const :tag "Layla"  layla)
                 (const :tag "Mondriaan"  mondriaan)
                 (const :tag "DracCDL"  draccdl)
                 (const :tag "Spectre Spice Compatibility"  spectre)))
  :set (lambda (variable value)
         (spice-custom-set variable value
			   'spice-check-spice-standard
			   'spice-mode-syntax-table-init
			   'spice-keywords-init
			   'spice-font-lock-init
			   'spice-imenu-init
			   'spice-update-existing-buffers))
  :group 'spice)


;;;###autoload
(defcustom spice-imenu-add-to-menubar t
  "*Spice mode adds imenu (Index) item to menubar"
  :group 'spice
  :set (lambda (variable value)
         (spice-custom-set variable value
			   'spice-update-existing-buffers))
  :type 'boolean)

;;;###autoload
(defcustom spice-use-func-menu nil
  "*Spice func menu setting (untested), comparable to imenu"
  :group 'spice
  :set (lambda (variable value)
         (spice-custom-set variable value
			   'spice-update-existing-buffers))
  :type  'boolean)

;;;###autoload
(defcustom spice-show-describe-mode nil ; was t
  "*Spice mode runs `describe-mode' once at start of spice-mode"
  :group 'spice
  :type 'boolean)

;;;###autoload
(defcustom spice-echo-intro t
  "*Spice mode echos introductory message on entry to spice-mode"
  :group 'spice
  :type 'boolean)

(defgroup spice-initialize-file nil
  "Customizations for initialization of empty/new spice files."
  :group 'spice)

;;;###autoload
(defcustom spice-initialize-empty-file nil
  "*Spice initialize empty/new file setting"
  :group 'spice-initialize-file
  :type  'boolean)

;;;###autoload
(defcustom spice-initialize-template-file "~/.spice-default"
  "*File containing the default header that is inserted when opening
an empty file (ie. a new file), see also `spice-initialize-empty-file'"
  :group 'spice-initialize-file
  :type  'file)

;;;###autoload
(defcustom spice-default-header nil
  "*Default header for new Spice netlists, see also `spice-initialize-empty-file'"
  :group 'spice-initialize-file
  :type  'string)

;;;###autoload
(defcustom spice-initialize-file-function 'spice-initialize-empty-file
  "*Optional initialize function for empty/new files, see also
`spice-initialize-empty-file'. If a different function is specified it
should insert a default header/template in the current buffer. This
function should check which submode is in use with `spice-standard-p'
and adapt its output accordingly. It may also use the `spice-default-header'
variable and insert its contents into the buffer."
  :group 'spice-initialize-file
  :type  'function)

(defgroup spice-simulate nil
  "Customizations for simulation."
  :group 'spice)


(defcustom spice-simulator-alist
  '(
    ;; Spice3; spice3 -b <file.cir>
    ;;
    ("Spice3" "spice3 -b" ""
     ("\\s-*Error[\t ]+on[ \t]+line[\t ]+\\([0-9]+\\) +:.+"
      0 1 nil (buffer-file-name))
     ("Circuit: \\(.*\\)$" 1)) ; spice3 hack, is not the filename, but that's no prob
    ;; Hspice; hspice <file.cir>
    ;; **error**:  only 1 connection at node    0:2
    ("Hspice" "hspice" ""
     ("\\s-*\\(..?error..?[: ]\\).+" 0 spice-linenum 1 (buffer-file-name))
     ("[* ]* [iI]nput [fF]ile: +\\([^ \t]+\\).*$" 1))
    ;; Eldo; eldo -i <file.cir>
    ;; ERROR  503: MODEL "NMOS": Undeclared model reference.
    ("Eldo" "eldo -i" ""
     ("\\s-*\\(E[rR][rR][oO][rR] +[0-9]+:\\).*"
      0 spice-linenum 1 (buffer-file-name))
     ("Running \\(eldo\\).*$" 1)) ; eldo hack, true filename can not be found
    ;; Spectre; spectre <file.cir>
    ;;     "viblok2.cir" 7: Unknown dot word `.param'.
    ("Spectre" "spectre" ""
     ("\\s-*\"\\([^ \t\n]+\\)\" +\\([0-9]+\\):.*"
      1 2)
     ("" 0)) ; filename is in regexp of error
    ;;; Add your local simulators here:
    ;;
    )
  "*List of available Spice simulators and their properties.
Each list entry specifies the following items for a simulator:
Simulator:
  Simulator Name   : name used in variable `spice-simulator' to choose
                     simulator
  Simulate Command : command including options used for simulation program
  Extra switches   : extra switches for simulator, after filename
Error Message:
  Regexp           : regular expression to match error messages
  File Subexp Index: index of subexpression that matches the file name
  Line Subexp Index: index of subexpression that matches the line number.
                     use function `spice-linenum' when the line number
                     is not available
  Col Subexp Index : index of subexpression that matches the column number
                     if not available but `spice-linenum' has been
                     supplied in the previous field, use 1
File Message:
  Regexp           : regular expression to match a file name message
  File Subexp Index: index of subexpression that matches the file name

See also variable `spice-simulator-switches' to add options to the
simulate command.

Most simulators do not include the file name in the error message, but print
out a file name message in advance.  In this case, set \"File Subexp Index\"
to 0 and fill out the \"File Message\" entries.
"
  :type '(repeat (list :tag "Simulator" :indent 2
                   (string :tag "Simulator Name    ")
                   (string :tag "Simulate Command  ")
                                        ;(string :tag "From Directory    " "./")
                   (string :tag "Extra Switches    ")
                   (list :tag "Error Message" :indent 4
                     (regexp  :tag "Regexp           ")
                     (integer :tag "File Subexp Index")
                     (integer :tag "Line Subexp Index"))
                   (list :tag "File Message" :indent 4
                     (regexp  :tag "Regexp           ")
                     (integer :tag "File Subexp Index"))))
  :set (lambda (variable value)
         (spice-custom-set variable value
			   'spice-menu-init
			   'spice-compile-variables-init
			   'spice-update-existing-buffers))
  :group 'spice-simulate)


;;;###autoload
(defcustom spice-simulator nil ; example: "Hspice"
  "*Spice command, used when compiling buffer with `compile-mode',
see also `spice-simulator-switches'."
  :group 'spice-simulate
  :type  'string)


;;;###autoload
(defcustom spice-simulator-switches "" ; example "-noconf"
  "*Spice command switches, used when compiling buffer with `compile-mode',
see also `spice-simulator'."
  :group 'spice-simulate
  :type  'string)


(defcustom spice-waveform-viewer-alist
  '(
    ;; Nutmeg; nutmeg <waveformdata.dat>
    ;;
    ("Nutmeg"  "nutmeg" "" spice-run-interactive (".dat" ".ac0" ".tr0")) ; spice3 nutmeg viewer
    ("Gwave"   "gwave"  "" spice-run-silent
     (".raw" ".braw" ".ac0" ".tr0" ".sw0" ".W" ".N" ".acs")) ; gwave viewer
    ("Xelga"   "xelga"  "" spice-run-silent ".cou") ; xelga eldo viewer
    ("Awaves"  "awaves_emacs" "" spice-run-silent (".ac0" ".tr0")) ; awaves hspice viewer, this doesn't work yet and will it ever ? Explanation: awaves is a script that starts an executable/binary in the background. This means the shell starting awaves ends immediately after the forking of the executable; after which all backgrounded programs get killed by emacs since their parent's parent shell (/bin/sh) ends. awaves shouldn't return immediately and then this will work; that's why I called the program awaves_emacs...

    ;;     ("Awaves2" "echo_awaves"  "" spice-run-silent nil) ; awaves hspice viewer
    ;;; Add your local waveform viewers here:
    ;;
    )
  "*List of available Waveform viewers and their properties.
Each list entry specifies the following items for a waveform viewer:
Waveform Viewer:
  Wave Viewer Name : name used in variable `spice-waveform-viewer' to choose
                     a waveform viewer
  Wave Viewer Command : command including options used for waveform viewer
  Extra switches   : extra switches for waveform viewer, after filename

See also variable `spice-waveform-viewer-switches' to add options to the
waveform viewer command.
"
  :type '(repeat (list :tag "Waveform Viewer" :indent 2
                   (string :tag "Waveform Viewer Name     ")
                   (string :tag "Waveform Viewing Command ")
                   (string :tag "Extra Switches           ")
                   (choice :tag "How"
                           :value spice-run-silent
                     (function-item spice-run-silent)
                     (function-item spice-run-interactive)
                     (function :tag "Other"))
                   (choice (string :tag "Filename Suffix          ")
                           (function :tag "Function Deriving Filename")
                           (repeat (list :tag "List of Filename Suffixes")))))
  :set (lambda (variable value)
         (spice-custom-set variable value
			   'spice-menu-init
			   'spice-update-existing-buffers))
  :group 'spice-simulate)


;;;###autoload
(defcustom spice-waveform-viewer nil ; example: "Nutmeg"
  "*Spice command, used when starting waveform viewer,
see also `spice-waveform-viewer-switches'."
  :group 'spice-simulate
  :type  'string)


;;;###autoload
(defcustom spice-waveform-viewer-switches "" ; example "-b"
  "*Spice waveform viewer command switches,
see also `spice-waveform-viewer'."
  :group 'spice-simulate
  :type  'string)


(defgroup spice-commands nil
  "Customizations for commands."
  :group 'spice)

;;;###autoload
(defcustom spice-shell
  (if (memq system-type '(ms-dos emx windows-nt))
      shell-file-name
    "/bin/sh")
  "*Name of shell used to parse spice commands."
  :group 'spice-commands
  :type 'file)

;;;###autoload
(defcustom spice-shell-command-option
  (cond ((memq system-type '(ms-dos emx windows-nt) )
	 (cond ((boundp 'shell-command-option)
		shell-command-option)
	       ((boundp 'shell-command-switch)
		shell-command-switch)
	       (t
		"/c")))
	(t				;Unix & EMX (Emacs 19 port to OS/2)
	 "-c"))
  "*Shell argument indicating that next argument is the command."
  :group 'spice-commands
  :type 'string)


(defgroup spice-hide nil
  "Customizations for hiding of comments."
  :group 'spice)

;;;###autoload
(custom-declare-variable 'spice-hide-line-prefix
			 '(concat
			   (regexp-quote
			    (concat comment-start
				    (if (boundp 'comment-padding)
					(if (integerp comment-padding)
					    (make-string comment-padding ? )
					  comment-padding)
				      " ")))
                           "[a-z\\*!$0-9+\\.]")
                         "*Regexp string describing lines that are commented out and will be
hidden. The regexp is matched to the beginning of a line, the ^ is
added automatically. The initialization of this variable is handled
in `spice-hide-init', which is after the setting of `comment-start'
and `comment-padding' variables."
                         :initialize (lambda (variable value)
                                       (message "Deferring initialization of %s with %s to spice-hide-init" variable value)) ; avoid init now, comment-start and padding aren't set yet.
                         :group 'spice-hide
                         :type 'string)

;;;###autoload
(defcustom spice-auto-hide-comments nil
  "*Boolean indicating automatic hiding of all commented regions at load time."
  :group 'spice-hide
  :type 'boolean)


(defgroup spice-section nil
  "Customizations for sections."
  :group 'spice)

;; sections (entirely different implementation but sections idea has
;; been taken from eldo-mode.el)
;;;###autoload
(defcustom spice-section-alist
  '(
    ;; Libraries
    ("Libraries"     "LIBRARIES"              nil) ;
    ;; Netlist
    ("Netlist"       "NETLIST"                nil) ;
    ;; Main Circuit
    ("Main Circuit"  "MAIN CIRCUIT"           nil) ;
    ;; Options
    ("Options"       "SIMULATION OPTIONS"     nil) ;
    ;; Supplies
    ("Supplies"      "SUPPLIES/REFERENCES"    nil) ;
    ;; Input Signals
    ("Input Signals" "INPUT SIGNALS"          nil) ;
    ;; DC Analysis
    ("DC Analysis"   "DC ANALYSIS"            nil) ;
    ;; AC Analysis
    ("AC Analysis"   "AC ANALYSIS"            nil) ;
    ;; Transient Analysis
    ("Transient Analysis" "TRANSIENT ANALYSIS" nil) ;
    ;;; Add your site-local spice sections here:
    ;;
    )
  "*List of valid sections in a Spice file and their options.
Each list entry specifies the following items for a section:
Section:
  Section Name     : name used in to select/create find section, make this
                     name short and descriptive.
  Section String   : string used in file to start section (usually all
                     uppercase variant of name).
  Extra switches   : extra switches for a section, unspecified for now."
  :type '(repeat (list :tag "Section" :indent 2
                   (string :tag "Section Name        ")
                   (string :tag "Section String      ")
                   (sexp   :tag "Extra Switches (nil)")))
  :set (lambda (variable value)
         (spice-custom-set variable value
			   'spice-keywords-init
			   'spice-font-lock-init
			   'spice-menu-init
			   'spice-imenu-init
			   'spice-update-existing-buffers))
  :group 'spice-section)


(defgroup spice-faces nil
  "Customizations for highlighting."
  :group 'spice)

;;;###autoload
(defcustom spice-highlight-keywords t
  "*Non-nil means highlight SPICE keywords and other standardized words.
The following faces are used:
  `spice-title-face'		: title (first line in a spice file)
  `spice-doc-face'		: doc strings
  `spice-analysis-face'		: analyses
  `spice-instance-name-face'	: instance/element names
  `spice-model-name-face'	: subckt model names
  `spice-layla-function-name-face': layla function names
  `spice-include-file-face'	: include files and libraries
  `font-lock-keyword-face'	: keywords
  `font-lock-warning-face'	: warnings
  `font-lock-comment-face'	: comment
  `font-lock-function-name-face': subcircuit references / names of objects
  `font-lock-type-face'		: types
  `font-lock-string-face'	: strings & include files
  `font-lock-constant-face'	: simulator's options
  `font-lock-variable-name-face': names of .param's & variables
NOTE: Activate the new setting in a spice buffer by re-fontifying it (menu
      entry \"Fontify Buffer\")."
  :type 'boolean
  :group 'spice-faces)

;; We try to use usual/standard font-lock faces, plus a few specific ones:
(custom-add-to-group
 'spice-faces 'font-lock-comment-face 'custom-face)
(custom-add-to-group
 'spice-faces 'font-lock-keyword-face 'custom-face)
(custom-add-to-group
 'spice-faces 'font-lock-type-face 'custom-face)
(custom-add-to-group
 'spice-faces 'font-lock-function-name-face 'custom-face)
(custom-add-to-group
 'spice-faces 'font-lock-variable-name-face 'custom-face)
(custom-add-to-group
 'spice-faces 'font-lock-warning-face 'custom-face)
(custom-add-to-group
 'spice-faces 'font-lock-string-face 'custom-face)


(defgroup spice-output nil
  "Customizations for spice output file handling."
  :group 'spice)

;;;###autoload
(defcustom spice-output-filename-alist
  '(
    ;; Libraries
    (eldo         (concat (file-name-sans-extension (buffer-file-name)) ".chi")) ;
    (hspice       (concat (file-name-sans-extension (buffer-file-name)) ".lis")) ;
    (hspice       (concat (file-name-sans-extension (buffer-file-name)) ".spout")) ;
    (hspice       (concat (file-name-sans-extension (buffer-file-name)) ".hspout")) ;
    (spice2g6     (concat (file-name-sans-extension (buffer-file-name)) ".out")) ;
    )
  "*List of valid output names depending on selected spice standard:
  Spice Standard   : one of spice2g6, hspice, eldo or layla
  Expression       : expression calculating the output filename
"
  :type '(repeat (list :tag "Output Filenames" :indent 2
                   (symbol :tag "Spice Standard      ")
                   (sexp   :tag "Expression          ")))
  :group 'spice-output)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer local vars for spice-mode

;;(defvar spice-standard-local nil
;;  "buffer local version of spice-standard.")

(defvar spice-output-local nil
  "buffer local version of spice-output.")

(defun spice-standard-p (standard)
  "Check if STANDARD is specified as used standard on local variable."
  (or (eq standard (car spice-standard))
      (memq standard (cadr spice-standard))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; regexps for spice mode

(defconst spice-continuation-prefix "+"
  "Continuation prefix for normal spice line")

(defconst spice-line-break "\\(\n\\s-*\\+\\s-*\\)*"
  "Regexp that matches a (possible) line break (\n+)")

(defconst spice-model-name "\\([a-z][^ \t\n=]*\\)" ; "\\([a-z]\\sw*[^ \t\n=]*\\)"
  "Regexp that describes a syntactically correct model or subckt name")

(defconst spice-library-regexp-start
  "^\\.\\(inc\\|include\\|verilog\\|use_veriloga\\|lib\\(\\s-+key=\\w+\\)?\\|libfas\\|subckt\\s-+lib\\|model\\s-+lib\\)\\s-+[\"']?"
  "Regexp that matches the beginning of library or include filename")

(defconst spice-library-regexp-end
  "\\([^ \t\n\"']*\\)"
  "Regexp that matches the end of library or include filename") ; doesn't catch end of buffer

(defconst spice-section-regexp-start "\\(^[*!$]\\s-*\\)"
  "spice mode section header start regexp.")

(defconst spice-spice2g6-keywords
  '(
    "subckt"
    "print" "plot"
    "opt" ; opt is actually a spice3 keyword, not 2g6
    "nodeset"
    "model" "macro" "include" ; include is actually a spice3 keyword, not 2g6
    "ends" "end"
    )
  "List of Spice2g6 keywords")

(defconst spice-spice2g6-analyses
  '(
    "tran" "op" "noise" "four" "disto" "dc" "ac"
    )
  "List of Spice2g6 analysis keywords")

(defconst spice-spice2g6-analysis-modifiers
  '(
    "dec" "lin" "oct" ;; the ubiquitous spice2g6 ac modifiers
    "pol" "zer" "pz" "cur" "vol" ;; pz analysis spice3
    "ac" ;; .sens analysis spice3
    )
  "List of Spice2g6 analysis modifier keywords")

(defconst spice-eldo-keywords
  '(
    "width" "use" "unprotect" "tvinclude" "topcell"
    "table" "subdup" "solve" "sinus" "sigbus"
    "setsoa" "setbus" "save" "restart" "ramp"
    "protect" "probe" "plotbus"
    "param" "overwrite_input" "optwind" "optpwl" "optnoise" "options"
    "option" "optfour"
    "notrc" "nocom" "modlogic"
    "moddup" "meas" "mcmod" "lotgroup" "loop" "load"
    "libfas" "lib" "init" "include" "ic" "hier" "guess" "global"
    "extract" "endl" "enddata" "dspf_include" "distrib" "del"
    "defwave" "defplotdig" "defmac" "data" "d2a" "comchar" "conso" "connect"
    "chrsim" "chrent" "chrand" "checksoa" "checkbus" "alter" "addlib" "a2d"
    )
  "List of Eldo keywords")

(defconst spice-eldo-colon-keywords
  '(
    "param" "pin" "model"
    )
  "List of Eldo colon keywords")

(defconst spice-eldo-macromodel-keywords
  '(
    "opamp0" "opamp0d" "opamp1" "opamp1d" "opamp2" "opamp2d"
    "satr" "satv" "vswitch" "cswitch"
    "tri2sin" "stairgen" "sawgen" "trigen"
    "amm" "pam"
    "sa_ho" "tr_ho"
    "pwm" "vco"
    "peak_d" "lev_d"
    "logamp" "expamp"
    "diff" "integ"
    "add" "sub" "mult" "div"
    "sc_ideal" "sc_i" "sc_n" "sc_p" "sc_s1" "sc_s2"
    "sc_sp1" "sc_sp2" "sc_b" "sc_u"
    )
  "List of Eldo macromodels")

(defconst spice-eldo-analyses
  '(
    "wcase" "tf" "temp"
    "step" "snf" "sens" "pz"
    "noisetran" "mc"
    )
  "List of Eldo analysis keywords")

(defconst spice-eldo-analysis-modifiers
  '(
    )
  "List of Eldo analysis modifier keywords")

(defconst spice-eldorf-keywords
  '(
    "sst"
    )
  "List of Eldo RF keywords")

(defconst spice-eldovloga-keywords
  '(
    "verilog" "use_veriloga"
    )
  "List of Eldo Verilog-A keywords")

(defconst spice-eldovloga-colon-keywords
  '(
    "port" "generic"
    )
  "List of Eldo Verilog-A colon keywords")

(defconst spice-eldorf-analyses
  '(
    "sstac" "sstxf" "sstnoise"
    )
  "List of Eldo RF keywords")

(defconst spice-hspice-keywords
  '(
    "width" "unprot" "unprotect" "uic"
    "title" "sys" "system" "save"
    "sample" "prot" "protect"
    "probe" "pc" "parameter"
    "param" "options" "option"
    "nomod"
    "measure" "meas" "macro"
    "load" "lib" "include" "inc" "ic" "graph" "global"
    "eom" "enddata" "dellib" "delete"
    "del" "dcvolt" "data" "control" "comment"
    "alter"
    "fsoptions" "layerstack" "material" "shape" ; FEM solver for W elements
    )
  "List of Hspice keywords")

(defconst spice-hspice-analyses
  '(
    "disto"
    "fft"
    "tf" "temp"
    "net" ;; S, Z, Y and H parameters
    "sample"
    "sens"
    "pz"
    "noise"
    )
  "List of Hspice analysis keywords")

(defconst spice-hspice-analysis-modifiers
  '(
    "sweep" "poi"
    )
  "List of Hspice analysis modifier keywords")

(defconst spice-fasthenry-keywords
  '(
    "units" "default" "external" "equiv"
    )
  "List of FastHenry keywords")

(defconst spice-fasthenry-analyses
  '(
    "freq"
    )
  "List of FastHenry analysis keywords")

(defconst spice-fasthenry-analysis-modifiers
  '(
    )
  "List of FastHenry analysis modifier keywords")

(defconst spice-layla-keywords
  '(
    "stop" "start"
    "port" "performance" "parameter"
    "param" "options" "option"
    "net"
    "model" "matching"
    "include" "inc"
    "bus"
    )
  "List of Layla keywords")

(defconst spice-mondriaan-keywords
  '(
    "master" "routingarea" "routingchannel" "ports" "portgrid" "net"
    )
  "List of Mondriaan keywords")

(defconst spice-draccdl-keywords
  '(
    "bipolar" "busdelimiter" "capa" "caparea" "capval" "default" "dioarea"
    "dioperi" "diode" "edifdelimiter" "busdelimiter" "equation" "equiv"
    "gnonswap" "ldd" "mega" "nonswap" "nopin" "nosub" "pin" "pininfo"
    "resi" "ressize" "resval" "reverse" "scale" "spice" "unspec"
    "eom" "global" "param" "macro" "swap"
    )
  "List of Dracula CDL keywords")

(defconst spice-spice2g6-types
  '(
    "ac" "dc" "exp" "pulse" "pwl" "sffm" "sin"
    )
  "List of types in spice2g6")

(defconst spice-hspice-types
  '(
    "metal" "dielectric" "pec" "rectangle" "circle" "strip" "polygon" ;; ""
    )
  "List of types in hspice")

(defconst spice-eldo-types
  '(
    "pattern"
    )
  "List of source types in eldo")

(defconst spice-eldorf-types
  '(
    "fpulse" "four" "probe"
    )
  "List of source types in eldorf")

(defconst spice-fasthenry-types
  '(
    "point" "rect" "circle"
    "user1" "user2" "user3" "user4" "user5" "user6" "user7"
    )
  "List of source types in fasthenry")

(defconst spice-special-model-type-names
  '(
    "d" "npn" "pnp" "nmos" "pmos"
    )
  "List of model type names which are excluded for imenu")

(defconst spice-spice2g6-model-type-names
  '(
    "r" "c" "urc" "ltra"
    "njf" "pjf"
    "nmf" "pmf"
    "sw" "csw"
    )
  "List of model type names in spice2g6")

(defconst spice-eldo-model-type-names
  '(
    "res" "cap" "ind"
    "rn" "rp" "lpnp"
    )
  "List of model type names extra in eldo")

(defconst spice-eldo-vloga-model-type-names
  '(
    "macro"
    )
  "List of model type names extra in Eldo Verilog-A")

(defconst spice-hspice-model-type-names
  '(
    "l" "core" "w" "plot"
    )
  "List of model type names extra in hspice")

(defconst spice-spice2g6-output-keywords
  '("print" "plot")
  "List of output keywords in spice2g6")

(defconst spice-spice2g6-output-types
  '("ac" "dc" "tran")
  "List of output types in spice2g6")

(defconst spice-eldo-output-keywords
  '("extract" "meas")
  "List of output keywords in eldo")

(defconst spice-eldo-output-types
  '("dcac" "dcsweep" "dctran" "noise" "four" "sweep")
  "List of output types in eldo")

(defconst spice-eldorf-output-types
  '("fsst" "tsst" "sstac" "sstxf" "ssnoise" )
  "List of output types in eldo")

(defconst spice-hspice-output-keywords
  '("probe" "graph" "measure")
  "List of output keywords in hspice")

(defconst spice-hspice-output-types
  '("noise" "disto")
  "List of output types in hspice")

(defconst spice-spice2g6-options-keywords
  '("trytocompact")
  "List of spice2g6/3 options keywords.")

(defconst spice-eldo-options-keywords
  '("wsf" "wsfascii" "sda"
    "precise" "spi3asc" "spi3bin" "spicedc" "spiout" "libfas" "nolib"
    "ammeter" "msgbias" "nowarn" "ulogic" "aspec" "mod4pins" "modwl" "wl"
    "captab" "coustep" "input" "lcapop" "list" "noascii" "ascii"
    "autostop" "nobound_phase" "nocou" "node" "nomod" "nopage" "nosizechk"
    "notrc" "trap" "smooth" "be" "gear" "newton" "iem" "analog" "digital"
    "osr" "mixed" "pstran" "dptran" "itl6" "itl7" "itl8" "nmaxsize"
    "noconvassist" "nolat" "pivrel" "pivtol" "qtrunc" "noconvassist"
    "nolat" "pivrel" "pivtol" "qtrunc" "relvar" "splitc" "noswitch"
    "unbound" "randmc" "usedefap" "engnot" "nodcpart" "probeop"
    "noprobeop" "histlim" "wbulk" "noinit" "nonoise" "tempcouk" "nofnsiem"
    "icdc" "icdev" "carlo_gauss" "cteprec" "d2dmvl9bit" "defconvmsg"
    "maxnodeord" "notrclib" "motorola" "aex")
  "List of eldo options keywords.")

(defconst spice-eldorf-options-keywords
  '("sst_freqdiv")
  "List of Eldo RF options keywords.")

(defconst spice-hspice-options-keywords
  '("acct" "converge"
    "absh" "acout" "cds" "scale" "cshdc" "alt999" "alt9999" "csdf"
    "accurate" "dvdt" "measout" "acout" "brief" "cshunt" "gshunt" "probe"
    "kcltest" "dctran" "di" "itl5" "list" "sda" "gshunt" "cshunt" "post"
    "node" "noelck" "gshunt" "maxamp" "nomod" "aspec" "icsweep" "relh"
    "nopage" "newtol" "reli" "notop" "parhier" "off" "spice" "wl"
    "dvtr" "nxx" "seed" "risetime" "imax" "nopiv" "imin" "opts" "cscal"
    "vntol" "absv" "pathnum" "badchr" "fmax" "plim" "diagnostic" "pivref"
    "fscal" "autostop" "nowarn" "gscal" "search" "lscal" "bypass" "verify"
    "sparse" "pivot" "pzabs" "cpu" "pztol" "fast" "interp" "h9007" "expli"
    "ritol" "itlpz" "itrprt" "unwrap" "captab" "newtol" "dccap")
  "List of hspice options keywords.")

(defconst spice-spice2g6-options-parameters
  '("gmin" "reltol" "abstol" "vntol" "trtol" "chgtol" "pivtol" "pivrel"
    "tnom" "temp" "lvlcod" "itl1" "itl2" "itl3" "itl4" "itl5" "defl"
    "defw" "defad" "defas" "method")
  "List of spice2g6/3 options parameters.")

(defconst spice-eldo-options-parameters
  '("cptime" "msgnode" "zoomtime" "bsim3ver" "defnrd" "defnrs" "defpd" "defps"
    "gramp" "scale" "scalebsim" "scalm" "soiback" "flicker_noise"
    "thermal_noise" "cousmp" "limprobe" "ascii" "savetime" "simudiv"
    "stat" "timediv" "maxord" "bloc" "epsdig" "absvar" "capanw" "chgtol"
    "dvdt" "eps" "fluxtol" "freqsmp" "ft" "hmin" "hmax" "itol" "lvltim"
    "maxnodes" "maxtran" "maxv" "netsize" "ngtol" "ratprint" "reltrunc"
    "sample" "startsmp" "step" "trtol" "tuning" "vmin" "vntol" "xa"
    "numdgt" "dcpart" "couresol" "defa2d" "defd2a" "vbcsat" "dclog" "epso"
    "optype")
  "List of eldo options parameters.")

(defconst spice-eldorf-options-parameters
  '("sst_start" "sst_stop" "sst_nper"
    "sst_npt" "sst_ovrsmp" "sst_spectrum" "sst_uic"
    "sst_max_liniter" "sst_verbose")
  "List of Eldo RF options parameters.")

(defconst spice-hspice-options-parameters
  '("artist" "dcap" "absh" "absvar" "absi" "absv" "delmax" "absmos"
    "dcfor" "dchold" "fs" "mentor" "absvdc" "dcon" "ft" "co" "reltol" ; "post"
    "cvtol" "di" "dcstep" "imin" "ingold" "imax" "lennam" "psf" "maxamp"
    "dv" "defl" "relh" "gmax" "relvar" "measdgt" "zuken" "defnrd" "reli"
    "gmindc" "rmax" "defnrs" "relmos" "gramp" "rmin" "defpd" "relv"
    "slopetol" "defps" "relvdc" "timeres" "limpts" "scalm" "relq" "numdgt"
    "resmin" "optlst" "trtol" "genk" "pivot" "sparse" "lvltim" "klim"
    "maxord" "post_version" "bkpsiz" "mu" "xmu" "warnlimit" "expli"
    "bytol" "cptime" "epsmin" "mbypass" "expmax" "limtim" "vfloor")
  "List of hspice options parameters.")

(defconst spice-layla-options-parameters
  '("begin_temp_iterations" "begin_acc_prob" "min_temp_scale_factor"
    "max_temp_scale_factor" "max_inner_loop_delta" "max_cost_delta"
    "min_range_scale_factor" "nr_temp_steps"
    "min_stable_inner_loops" "min_local_iterations" "max_local_iterations"
    "max_iterations" "bank_min_ar" "bank_max_ar" "bank_orientations"
    "bank_routing_space" "bus_sides" "bus_layer" "bus_port_width"
    "bus_port_height" "bus_port_distance" "bus_routing_space" "cap_min_ar"
    "cap_max_ar" "cap_orientations" "cap_routing_space" "cap_ar_step"
    "coil_orientations" "coil_routing_space" "alfa_area"
    "alfa_performance" "alfa_aspect_ratio" "alfa_overlap" "alfa_overlap_min"
    "alfa_overlap_max" "kappa_overlap" "device_min_ar" "device_max_ar"
    "device_orientations" "device_routing_space" "device_current"
    "diode_min_ar" "diode_max_ar" "diode_orientations"
    "diode_routing_space" "diode_current" "matching_compute_method"
    "dummy_min_ar" "dummy_max_ar" "dummy_orientations"
    "dummy_routing_space" "dummy_ar_step"
    "mos_min_ar" "mos_max_ar" "mos_orientations" "mos_routing_space"
    "mos_current" "prob_reshape" "prob_reorientation"
    "prob_symmetric_translation" "prob_symmetric_swap"
    "prob_symmetric_flip" "prob_symmetric_flip" "prob_symmetric_shift"
    "prob_independent_translation" "prob_independent_swap"
    "prob_independent_flip" "min_move_range" "net_compute_method"
    "write_init" "write_int" "int_write_interval" "write_fin" "write_final"
    "mentor_write_ipc" "gds2_write_ipc" "construct_net_performance"
    "construct_matching_performance" "aspect_ratio" "field_grow_factor"
    "field_aspect_ratio" "disable_symmetry" "disable_matching"
    "do_kul_routing" "kul_router_program" "kul_router_directives_file"
    "number_of_randomize_loops" "port_inherit_terminals" "bus_priority"
    "port_sides" "port_layer" "port_width" "port_height" "port_distance"
    "port_abutment" "port_routing_space" "port_no_routing" "port_current"
    "res_ar_step" "res_min_ar" "res_max_ar" "res_orientations"
    "res_routing_space" "res_width" "res_current" "disable_couple_flipping"
    "symmetry_max_offset_factor" "print_cost" "placement_name"
    "write_final_ample" "write_final_skill" "write_final_gds2_bin"
    "write_final_gds2_cells" "write_final_tud"
    "write_final_ample_generators" "write_final_ample_cells"
    "write_final_cif" "write_final_rose" "write_final_kul"
    "write_final_text"
    "write_int_ample" "write_int_skill" "write_int_gds2_bin"
    "write_int_gds2_cells" "write_int_tud" "write_int_ample_generators"
    "write_int_ample_cells" "write_int_cif" "write_int_rose"
    "write_int_kul" "write_int_text"
    "write_init_ample" "write_init_skill" "write_init_gds2_bin"
    "write_init_gds2_cells" "write_init_tud" "write_init_ample_generators"
    "write_init_ample_cells" "write_init_cif" "write_init_rose"
    "write_init_kul" "write_init_text" "time_out"
    "random_seed" "anneal_rs" )
  "List of LAYLA options parameters.")

(defconst spice-layla-functions
  '(
    "bus_double_param" "bus_integer_param" "bus_string_param"
    "device_double_param" "device_integer_param" "device_string_param"
    "net_double_param" "net_integer_param" "net_string_param"
    "placement_double_param" "placement_integer_param" "placement_string_param"
    "port_double_param" "port_integer_param" "port_string_param"
    "symmetry_double_param" "symmetry_integer_param" "symmetry_string_param"
    )
  "List of functions in Layla mode")

(defconst spice-draccdl-device-keywords
  '("nonswap" )
  "List of Dracula CDL device keywords.")

(defconst spice-draccdl-device-parameters
  '("w" "l" "sub" "ea" ;; CDL parameters
    "x" "y") ;; added X&Y, dracula extracts them (not CDL!)
  "List of Dracula & CDL device parameters.")

(defconst spice-spice2g6-entity-start-keywords
  '(
    "ends" "macro" "subckt"
    )
  "List of spice2g6 entity start keywords")

(defconst spice-eldo-entity-start-keywords
  '(
    "endl"
    )
  "List of eldo entity start keywords")

(defconst spice-hspice-entity-start-keywords
  '(
    "endl" "eom"
    "fsoptions" "layerstack" "material" "shape" ; FEM solver for W elements
    )
  "List of hspice entity start keywords")

(defconst spice-layla-entity-start-keywords
  '(
    "bus" "net" "symmetry" "performance" "port"
    )
  "List of layla entity start keywords")

(defconst spice-mondriaan-entity-start-keywords
  '(
    "routingarea" "routingchannel" ;; "net" ; already in layla
    )
  "List of mondriaan entity start keywords")

(defconst spice-draccdl-entity-start-keywords
  '(
    "eom"
    )
  "List of DracCDL entity start keywords")

(defvar spice-keywords nil
  "List of spice mode keywords.")

(defvar spice-colon-keywords nil
  "List of spice mode colon keywords.")

(defvar spice-analyses nil
  "List of spice mode analyses.")

(defvar spice-analysis-modifiers nil
  "List of spice mode analysis modifiers.")

(defvar spice-types nil
  "List of spice mode standardized types.")

(defvar spice-functions nil
  "List of spice mode functions.")

(defvar spice-section-headings nil
  "List of spice mode section headings.")

(defvar spice-misc-model-type-names nil
  "List of miscellaneous model type name.")

(defvar spice-model-type-names nil
  "List of all model type name.")

(defvar spice-output-keywords nil
  "List of all output keywords.")

(defvar spice-output-types nil
  "List of all output types.")

(defvar spice-options-keywords nil
  "List of all output options keywords.")

(defvar spice-options-parameters nil
  "List of all output options parameters.")

(defvar spice-entity-start-keywords nil
  "List of entity definition keywords.")

(defvar spice-keywords-regexp nil
  "Regexp for keywords.")

(defvar spice-colon-keywords-regexp nil
  "Regexp for colon keywords.")

(defvar spice-analyses-regexp nil
  "Regexp for analyses.")

(defvar spice-analysis-modifiers-regexp nil
  "Regexp for analysis modifiers.")

(defvar spice-layla-keywords-regexp nil
  "Regexp for layla keywords.")

(defvar spice-mondriaan-keywords-regexp nil
  "Regexp for mondriaan keywords.")

(defvar spice-draccdl-keywords-regexp nil
  "Regexp for dracula CDL keywords.")

(defvar spice-types-regexp nil
  "Regexp for spice mode types.")

(defvar spice-functions-regexp nil
  "Regexp for spice mode functions (Layla).")

(defvar spice-section-headings-regexp nil
  "Regexp for spice mode section headings.")

(defvar spice-xinstance-regexp nil
  "Regexp for x instances.")

(defvar spice-model-name-regexp nil
  "Regexp model names of elements.")

(defvar spice-entity-start-regexp nil
  "Regexp for start of entity definition.")

(require 'font-lock)

(defvar spice-instance-name-face	 'spice-instance-name-face
  "Face name to use spice instances.")

(defface spice-instance-name-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "ForestGreen" :bold t))
    (((class color) (background dark)) (:foreground "Yellow" :bold t))
    (t (:bold t)))
  "Spice mode face used to highlight instances."
  :group 'spice-faces)

(defvar spice-model-name-face	 'spice-model-name-face
  "Face name to use spice instances.")

(defface spice-model-name-face
  '((((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "Red3"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    (t (:bold t)))
  "Spice mode face used to highlight models."
  :group 'spice-faces)

(defvar spice-title-face	'spice-title-face
  "Face name for title string.")

(defface spice-title-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light))
     (:foreground "Yellow" :background "black" :bold t))
    (((class color) (background dark))
     (:foreground "green3":background "lightyellow" :bold t))
    (t (:bold t)))
  "Spice mode face used for title string."
  :group 'spice-faces)

(defvar spice-layla-function-name-face       'spice-layla-function-name-face
  "Face name to use for layla function names.")

(defface spice-layla-function-name-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light))
     (:foreground "Darkgreen"))
    (((class color) (background dark)) (:foreground "green"))
    (t (:bold t)))
  "Spice mode face used to higlight layla functions."
  :group 'spice-faces)


(defvar spice-analysis-face 'spice-analysis-face
  "Face name to highlight spice mode analysis commands.")

(defface spice-analysis-face
  '((((class color) (background light)) (:foreground "Magenta" :bold t :underline t))
    (((class color) (background dark)) (:foreground "Lightgreen" :bold t :underline t))
    (t (:bold t)))
  "Spice mode face used to highlight analysis commands."
  :group 'spice-faces)

(defvar spice-doc-face 'font-lock-string-face
  "Face name to use for doc strings.")

(custom-add-to-group
 'spice-faces 'font-lock-string-face
 'custom-face)


(defvar spice-constant-face (if (facep 'font-lock-constant-face)
                                'font-lock-constant-face
                              'font-lock-reference-face) ; old emacs20.1
  "Face name to use for constants.")

(custom-add-to-group
 'spice-faces (if (facep 'font-lock-constant-face)
                  'font-lock-constant-face
                'font-lock-reference-face)
 'custom-face)

(defvar spice-include-file-face	'font-lock-string-face
  "Face name to use for include files and libraries.")

(custom-add-to-group
 'spice-faces 'font-lock-string-face
 'custom-face)

(custom-add-to-group
 'spice-faces 'font-lock-builtin-face 'custom-face)

(defvar spice-builtin-face 'font-lock-builtin-face
  "Face name for builtin types.")

(defun spice-keywords-init ()
  "Initialize reserved words."
  (setq spice-keywords
	(append spice-spice2g6-keywords
		(when (spice-standard-p 'hspice)
		  spice-hspice-keywords)
		(when (spice-standard-p 'eldo)
		  spice-eldo-keywords)
		(when (spice-standard-p 'eldorf)
		  spice-eldorf-keywords)
		(when (spice-standard-p 'fasthenry)
		  spice-fasthenry-keywords)))
  (setq spice-colon-keywords
	(append (when (spice-standard-p 'eldo)
		  spice-eldo-colon-keywords)
		(when (spice-standard-p 'eldovloga)
		  spice-eldovloga-colon-keywords)))
  (setq spice-analyses
	(append spice-spice2g6-analyses
		(when (spice-standard-p 'hspice)
		  spice-hspice-analyses)
		(when (spice-standard-p 'eldo)
		  spice-eldo-analyses)
		(when (spice-standard-p 'eldorf)
		  spice-eldorf-analyses)
		(when (spice-standard-p 'fasthenry)
		  spice-fasthenry-analyses)))
  (setq spice-analysis-modifiers
	(append spice-spice2g6-analysis-modifiers
		(when (spice-standard-p 'hspice)
		  spice-hspice-analysis-modifiers)
		(when (spice-standard-p 'eldo)
		  spice-eldo-analysis-modifiers)
		(when (spice-standard-p 'fasthenry)
		  spice-fasthenry-analysis-modifiers)))
  (setq spice-misc-model-type-names
	(append spice-spice2g6-model-type-names
		(when (spice-standard-p 'hspice)
		  spice-hspice-model-type-names)
		(when (spice-standard-p 'eldovloga)
		  spice-eldo-vloga-model-type-names)
		(when (spice-standard-p 'eldo)
		  spice-eldo-model-type-names)))
  (setq spice-output-keywords
	(append spice-spice2g6-output-keywords
		(when (spice-standard-p 'hspice)
		  spice-hspice-output-keywords)
		(when (spice-standard-p 'eldo)
		  spice-eldo-output-keywords)))
  (setq spice-output-types
	(append spice-spice2g6-output-types
		(when (spice-standard-p 'hspice)
		  spice-hspice-output-types)
		(when (spice-standard-p 'eldo)
		  spice-eldo-output-types)
		(when (spice-standard-p 'eldorf)
		  spice-eldorf-output-types)))
  (setq spice-model-type-names
	(append spice-misc-model-type-names
		spice-special-model-type-names))
  (setq spice-types
	(append spice-spice2g6-types
		(when (spice-standard-p 'hspice)
		  spice-hspice-types)
		(when (spice-standard-p 'eldo)
		  spice-eldo-types)
		(when (spice-standard-p 'eldorf)
		  spice-eldorf-types)
		(when (spice-standard-p 'fasthenry)
		  spice-fasthenry-types)))
  (setq spice-options-keywords
	(append spice-spice2g6-options-keywords
		(when (spice-standard-p 'hspice)
		  spice-hspice-options-keywords)
		(when (spice-standard-p 'eldo)
		  spice-eldo-options-keywords)
		(when (spice-standard-p 'eldorf)
		  spice-eldorf-options-keywords)))
  (setq spice-options-parameters
	(append spice-spice2g6-options-parameters
		(when (spice-standard-p 'hspice)
		  spice-hspice-options-parameters)
		(when (spice-standard-p 'layla)
		  spice-layla-options-parameters)
		(when (spice-standard-p 'eldo)
		  spice-eldo-options-parameters)
		(when (spice-standard-p 'eldorf)
		  spice-eldorf-options-parameters)))
  (setq spice-functions spice-layla-functions)
  (setq spice-section-headings (list "Changelog")) ; Changelog is special case
  (let ((section-alist spice-section-alist) heading)
    (while section-alist
      (setq heading (downcase (car (cdr (car section-alist)))))
      (setq spice-section-headings (append spice-section-headings
                                           (list heading)))
      (setq section-alist (cdr section-alist))))
  (setq spice-entity-start-keywords
	(append spice-spice2g6-entity-start-keywords
		(when (spice-standard-p 'hspice)
		  spice-hspice-entity-start-keywords)
		(when (spice-standard-p 'layla)
		  spice-layla-entity-start-keywords)
		(when (spice-standard-p 'mondriaan)
		  spice-mondriaan-entity-start-keywords)
		(when (spice-standard-p 'draccdl)
		  spice-draccdl-entity-start-keywords)
		(when (spice-standard-p 'eldo)
		  spice-eldo-entity-start-keywords)))
  (setq spice-section-headings-regexp
	(concat spice-section-regexp-start "\\("
		(regexp-opt spice-section-headings) "\\)\\(.*\\)$"
                                        ; "\\s-*$" ; ??
		))
  (setq spice-keywords-regexp
	(concat "^\\.\\("
		(regexp-opt spice-keywords)
		"\\)\\>"))
  (setq spice-colon-keywords-regexp
	(concat "\\<\\("
		(regexp-opt spice-colon-keywords)
		"\\)\\(:\\)"))
  (setq spice-analyses-regexp
	(concat "^\\s-*"
		"\\(\\.\\)\\("
		(regexp-opt spice-analyses)
		"\\)\\>"))
  (setq spice-analysis-modifiers-regexp
	(concat "\\<\\(" (regexp-opt spice-analysis-modifiers) "\\)\\>"))
  (setq spice-layla-keywords-regexp
	(concat "^\\s-*\\*?"
		"\\.\\("
		(regexp-opt spice-layla-keywords)
		"\\)\\>"))
  (setq spice-mondriaan-keywords-regexp
	(concat "^\\s-*\\*?"
		"\\.\\("
		(regexp-opt spice-mondriaan-keywords)
		"\\)\\>"))
  (setq spice-draccdl-keywords-regexp
	(concat "^\\s-*"
		"\\.\\("
		(regexp-opt spice-draccdl-keywords)
		"\\)\\>"))
  (setq spice-types-regexp
	(concat "\\<\\(" ;"\\(\\s-+\\|\n\\+\\s-*\\)\\<\\("
		(regexp-opt spice-types)
		"\\)\\>"))
  (setq spice-functions-regexp
	(concat "^\\s-*\\(\\*?\\.\\("
		(regexp-opt spice-functions)
		"\\)\\)\\>(\\([^,]+\\),\\([^,]+\\),[^,]+)"))
                                        ; old xinstance regexp
                                        ;		  "\\([ \t]+[^ $!(=\t\n][^ (=\t\n]*\\|\n[+]\\)*" ; $! are hspice & eldo's doc string starters
  (setq spice-xinstance-regexp
        (concat "^\\(x\\S-*\\)"
                "\\(\\([ \t]+[^ *"
                ;; "\\(\\([ \t]*[^ *" ;; should solve problem when no space between + and name, but fails horribly due to CPU hungry recursion in matcher....
                (when (spice-standard-p 'hspice) "$")
                (when (spice-standard-p 'eldo) "!")
                ":(=\t\n][^ :(=\t\n]*\\|[ \t]*\\(\n?[*"
                (when (spice-standard-p 'hspice) "$")
                (when (spice-standard-p 'eldo) "!")
                "].*\\)?\n[+]\\)*\\s-*\\)" ;; \\([a-z]\\w*\\)
                "\\<" spice-model-name "\\>"
                "\\(\\s-*\n\\|\\s-+[^=\n]\\)"
                ))

  (setq spice-model-name-regexp
        (concat "^\\([qmd]\\S-*\\)" ;; only bips, mos and diodes
                "\\([ \t]+[^ *" ;; first '*' was '+' ! but fails see spice-xinstance-regexp...
                (when (spice-standard-p 'hspice) "$")
                (when (spice-standard-p 'eldo) "!")
                "(=\t\n][^ (=\t\n]*\\|[ \t]*\\([*"
                (when (spice-standard-p 'hspice) "$")
                (when (spice-standard-p 'eldo) "!")
                "].*\\)?\n[+]\\)*\\s-*" ;; \\([a-z]\\w*\\)
                "\\<" spice-model-name "\\>"
                "\\(\\s-*\n\\|\\s-+[^=]\\)"
                ))
  (setq spice-entity-start-regexp
	(concat "^\\*?\\.\\("
		(regexp-opt spice-entity-start-keywords)
		"\\)")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font lock initialization
(defvar spice-font-lock-keywords-0 nil
  ;; set in `spice-font-lock-init' because dependent on custom variables
  "For consideration as a value of `spice-font-lock-keywords'.
This does highlighting of keywords and standard identifiers.")

(defvar spice-font-lock-keywords-1 nil
  ;; set in `spice-font-lock-init' because dependent on custom variables
  "For consideration as a value of `spice-font-lock-keywords'.
This does highlighting of keywords and standard identifiers.")

(defvar spice-font-lock-keywords-2 nil
  ;; set in `spice-font-lock-init' because dependent on custom variables
  "For consideration as a value of `spice-font-lock-keywords'.
This does highlighting of keywords and standard identifiers.")

(defvar spice-font-lock-keywords-3 nil
  ;; set in `spice-font-lock-init' because dependent on custom variables
  "For consideration as a value of `spice-font-lock-keywords'.
This does highlighting of keywords and standard identifiers.")

(defvar spice-font-lock-keywords-4 nil
  ;; set in `spice-font-lock-init' because dependent on custom variables
  "For consideration as a value of `spice-font-lock-keywords'.
This does highlighting of keywords and standard identifiers.")

(defvar spice-font-lock-keywords-5 nil
  ;; set in `spice-font-lock-init' because dependent on custom variables
  "For consideration as a value of `spice-font-lock-keywords'.
This does highlighting of keywords and standard identifiers.")

(defvar spice-font-lock-keywords-6 nil
  ;; set in `spice-font-lock-init' because dependent on custom variables
  "For consideration as a value of `spice-font-lock-keywords'.
This does highlighting of keywords and standard identifiers.")

(defvar spice-font-lock-keywords-7 nil
  ;; set in `spice-font-lock-init' because dependent on custom variables
  "For consideration as a value of `spice-font-lock-keywords'.
This does highlighting of keywords and standard identifiers.")

(defvar spice-font-lock-keywords-8 nil
  ;; set in `spice-font-lock-init' because dependent on custom variables
  "For consideration as a value of `spice-font-lock-keywords'.
This does highlighting of keywords and standard identifiers.")

(defvar spice-font-lock-keywords-9 nil
  ;; set in `spice-font-lock-init' because dependent on custom variables
  "For consideration as a value of `spice-font-lock-keywords'.
This does highlighting of keywords and standard identifiers.")

(defvar spice-font-lock-keywords-10 nil
  ;; set in `spice-font-lock-init' because dependent on custom variables
  "For consideration as a value of `spice-font-lock-keywords'.
This does highlighting of keywords and standard identifiers.")

(defvar spice-font-lock-keywords-11 nil
  ;; set in `spice-font-lock-init' because dependent on custom variables
  "For consideration as a value of `spice-font-lock-keywords'.
This does highlighting of keywords and standard identifiers.")

(defvar spice-font-lock-keywords-12 nil
  ;; set in `spice-font-lock-init' because dependent on custom variables
  "For consideration as a value of `spice-font-lock-keywords'.
This does highlighting of keywords and standard identifiers.")

(defvar spice-font-lock-keywords-13 nil
  ;; set in `spice-font-lock-init' because dependent on custom variables
  "For consideration as a value of `spice-font-lock-keywords'.
This does highlighting of keywords and standard identifiers.")


;; fast variable name matcher in parameter=value constructs
(defun spice-match-variable-name (limit)
  "match variable names"
  (let (pos found (start (point)))
    (setq found nil)
    (setq pos (search-forward "=" limit 'end)) ;
    (while (and pos
		(not found))
      (forward-word -1)
      (if (< (point) start) (goto-char pos))
      (if (looking-at "\\([a-z]\\w*\\)\\s-*\\(=\\)")
	  (progn
	    (goto-char pos)
	    ;; (message (match-string 1))
	    (setq found t))
	(progn
	  (goto-char pos)
	  (setq pos (search-forward "=" limit 'end)))))
    found))

;; font-lock aux functions
(defun spice-match-eldo-colon-keywords (limit)
  "match ((param|pin|model|port|generic):)" ;; latter two are eldo verilog-A
  (let (pos found (start (point)))
    (setq found nil)
    (setq pos (search-forward ":" limit 'end)) ;
    (while pos
      (forward-word -1)
      (if (< (point) start) (goto-char pos))
      ;; (message "pos is %d" pos)
      (if (looking-at ;; "\\(\\(param\\|pin\\|model\\|port\\|generic\\):\\)")
	   spice-colon-keywords-regexp)
	  (progn
	    (goto-char pos)
	    (setq pos nil)
	    (setq found t))
	(progn
	  (goto-char pos)
	  (setq pos (search-forward ":" limit 'end)))))
    found))

;; these are try outs to solve the font-locking of problematic xinstances:

(defun spice-match-xinstances-dummy (limit)
  "match xinstances"
  (let (min max pt)
    (setq pt (point))
    (goto-char (point-min))
    (setq min (point))
    (goto-char (point-max))
    (setq max (point))
    (save-excursion
      (set-buffer (get-buffer-create "*Matcher*"))
      (goto-char (point-max))
      (insert (format "Point min is %s\n" min))
      (insert (format "Current point is %s\n" pt))
      (insert (format "Limit is %s\n" limit))
      (insert (format "Point max is %s\n" max))
      )
    )
  nil
  )

(defun spice-idle-font-lock (beg end)
  "runs font-lock on a region"
  (message "rerunning font-lock on %s:%s=%s" beg end (buffer-substring beg end))
  (save-excursion (font-lock-fontify-region beg end)))

(defvar spice-previous-xinstance-match-result nil)
(make-variable-buffer-local 'spice-previous-xinstance-match-result)

(defun spice-match-in-xinstance (limit)
  "checks if in xinstance"
  (let ((pt (point)))
    (if (or (looking-at "^\\s-*[xX]")
	    (re-search-backward "^[xX]" (point-min) t))
	(if (and (looking-at spice-xinstance-regexp)
		 (or (> (match-end 5) pt)
		     (not spice-previous-xinstance-match-result))
		 )
	    (progn
	      (message ".")
	      (goto-char (match-end 5))
	      (if (or (> pt (match-beginning 1))
		      (< limit (match-end 5)))
		  (progn
		    (message "*")
		    ;;(spice-idle-font-lock (match-beginning 1) (match-end 5))
		    (run-with-idle-timer 1 nil 'spice-idle-font-lock
					 (match-beginning 1) (match-end 5))
		    ))
              ;;	      (if (> (match-end 5) limit)
              ;;		  (run-with-idle-timer 1 nil 'spice-idle-font-lock
              ;;				       (match-beginning 1) (match-end 5)))
	      t)
	  (goto-char pt)
	  nil)
      nil)
    ))

(defun spice-match-next-xinstance (limit)
  "checks if there is a next xinstance partly within limit"
  (if (re-search-forward "^[xX]" limit 'end)
      (progn
	(backward-char 1)
	(if (looking-at spice-xinstance-regexp)
	    (progn
	      (message "+")
	      (goto-char (match-end 5))
	      t)
	  (message "-")
	  (forward-char 1)
	  (spice-match-next-xinstance limit))
	)
    nil))

(defun spice-match-xinstance (limit)
  "match xinstances"
  (if (spice-match-in-xinstance limit)
      t
    (spice-match-next-xinstance limit)))

(defun spice-match-xinstances-old (limit)
  "match an xinstance"
  (setq spice-previous-xinstance-match-result (spice-match-xinstance limit))
  spice-previous-xinstance-match-result)

;; this xinstances matcher is a complete parser !

(defun spice-match-xinstances (limit)
  "match xinstance subckt name, this one parses the lines, should work for
all cases, infinite number of comment lines, continuation lines. Could fail
when modifying an xinstance line though, hard to tell."
  (interactive)
  (let ((result nil) match-start)
    (while
	(and (not result)
	     (re-search-forward "^\\(x\\S-*\\)" limit 'end)) ;; should be checked ?
      (setq match-start (match-data))
      ;;(message "match-start is %s" match-start)
      (while
	  (or
	   (and (looking-at "\\([ \t]+\\|\\s-*\n[+]\\s-*\\)\\(\\([a-z][^ :(=\t\n]*\\)\\|\\([^ *$!:(=\t\n][^ :(=\t\n]*\\)\\)\\(\\s-*\n\\|\\s-+[^=\n]\\)")
		(progn
		  (goto-char (match-end 2))
		  (setq result t)))
	   (save-match-data
	     (and
	      (looking-at "\\(\\([ \t]+\\|\\s-*\n\\s-*\\)[$!*].*\\)")
	      (progn
		(goto-char (match-end 1))
		t))))
	nil))
    (when result (set-match-data
		  (append (list (car match-start) (car (cdr (match-data))))
			  (cdr (cdr match-start)) (cdr (cdr (match-data))))))
    ;;(when result (message "Matched %s[%d]" (match-string 3) (match-end 3)))
    (when result (when (< limit (match-end 3)) (setq result nil)))
    result))

(defvar spice-font-lock-keywords nil
  "Regular expressions to highlight in spice mode.")

(defun spice-font-lock-init ()
  "Initialize fontification." ; makes spice-font-lock-keywords valid
  ;; highlight title & titles after .alter & .title (hspice only)
  (setq spice-font-lock-keywords-0
	(append (list ;; first line of spice deck
		 (list "\\`.+$" 0 spice-title-face)
		 (list spice-section-headings-regexp
		       '(1 font-lock-comment-face)
		       '(2 spice-title-face)
		       (list (regexp-opt-depth spice-section-headings-regexp)
			     spice-doc-face 'keep t)))
		;; hspice title in .alter or .title line
		(when (spice-standard-p 'hspice)
		  (list
		   (list "^\\.\\(alter\\|title\\)\\s-+\\(.+\\)$"
			 2 spice-title-face)))))
  ;; highlight layla functions: .xx_yy_param(name,prop,val);
  (setq spice-font-lock-keywords-1
        (list
         (list spice-functions-regexp
               '(1  spice-layla-function-name-face)
               (list (- (regexp-opt-depth spice-functions-regexp) 1)
                     font-lock-function-name-face)
               (list (regexp-opt-depth spice-functions-regexp)
                     font-lock-variable-name-face)
               )))
  ;; highlight spice keywords (mainly .<keyword> & .<analysis>)
  (setq spice-font-lock-keywords-2
	(list ;; .opt, .nodeset, ...
	 (list spice-keywords-regexp    0 font-lock-keyword-face)
	 ;; '+' of continuation lines ? maybe over the top ...
	 ;; (list (concat "^"
	 ;; 		   (when (spice-standard-p 'layla) "\\*?")
	 ;; 		   "\\+")                0 font-lock-keyword-face)
	 ;; .op, .ac, .dc, .tran, ...
	 (list spice-analyses-regexp
	       '(1 font-lock-keyword-face)
	       '(2 spice-analysis-face)
	       (list spice-analysis-modifiers-regexp
		     nil nil '(0 font-lock-type-face)))))

  (setq spice-font-lock-keywords-12
	;; do dracula CDL stuff
	(list
	 ;; add $ docs when they are followed by a space...
	 (list (concat "\\<\\([$]\\)\\s-+\\(.*\\)$")
               (list 1 font-lock-comment-face)
               (list 2 spice-doc-face 'keep))
	 ;; special models on element line
	 (list (concat "\\([$]\\(?:ldd\\)?\\[\\)" spice-model-name "\\(\\]\\)")
	       '(1 font-lock-keyword-face)
	       '(2 spice-model-name-face)
	       '(3 font-lock-keyword-face))
	 ;; normal .model on element line
	 (list (concat "\\([$]\\.model\\)\\s-*=\\s-*" spice-model-name)
	       '(1 font-lock-keyword-face)
	       '(2 spice-model-name-face))
	 ;; element options behind $ syntax
	 (list (concat "\\([$]\\)\\(" (regexp-opt spice-draccdl-device-keywords) "\\)")
	       '(1 font-lock-keyword-face)
	       '(2 font-lock-keyword-face))
	 ;; element parameters behind $ syntax
	 (list (concat "\\([$]\\)\\(" (regexp-opt spice-draccdl-device-parameters) "\\)\\s-*=")
	       '(1 font-lock-keyword-face)
	       '(2 font-lock-variable-name-face))
	 ;; general '.' keywords
	 (list spice-draccdl-keywords-regexp 0 font-lock-keyword-face)))

  ;; highlight layla specific keywords, also if *. syntax is used
  (setq spice-font-lock-keywords-3
	(append (list
		 (list spice-layla-keywords-regexp    0 font-lock-keyword-face)
		 (list "^\\(\\*\\)\\+"    1 font-lock-keyword-face))
                (when (spice-standard-p 'mondriaan)
                  (list
                   (list spice-mondriaan-keywords-regexp    0 font-lock-keyword-face)))))

  (setq spice-font-lock-keywords-13
	;; do spectre stuff
	(list
	 ;; * spectre: + syntax
	 (list "^\\(\\*\\)\\s-+spectre:\\s-+" 1 font-lock-keyword-face)
	 ;; // comments
	 (list "//.*$" 0 font-lock-comment-face)
	 ;; add simulator language=spice
	 (list "^\\s-*\\(simulator\\)\\>"
	       '(1 font-lock-keyword-face))))

  ;; varia: .end, spice instances and '*' comment lines and output lines
  (setq spice-font-lock-keywords-4
	(list
	 ;; elements
	 ;;'("^[a-z]\\S-*"            .     spice-instance-name-face)
	 '("^[a-z][^ \t\n]+"            .     spice-instance-name-face)
                                        ;(list (concat "^\\*\\([^"
                                        ;		(when (spice-standard-p 'layla) "+")
                                        ;		"\n].*\\|\n\\)") 0 font-lock-comment-face)
	 (list (concat "^\\*[^\n"
		       (when (or (spice-standard-p 'hspice)
				 (spice-standard-p 'draccdl)
				 (spice-standard-p 'layla)) "$")
		       (when (spice-standard-p 'eldo) "!")
		       "]*") 0 font-lock-comment-face)
	 (list (concat "^\\.\\(" (regexp-opt spice-output-keywords)
		       "\\)\\s-*" spice-line-break "\\s-+\\("
		       (regexp-opt spice-output-types) "\\)\\>")
	       '(0 font-lock-type-face keep)) ;
	 ))
  ;; highlight additional . unknowns (to detect stupid typing errors)
  (setq spice-font-lock-keywords-5
	(list '("^\\s-*\\.[^ \t\n]*" 0 font-lock-warning-face)))

  ;; highlight additional $ and ! comments, only eldo, hspice, draccdl and layla
  (setq spice-font-lock-keywords-6
	(list
	 (list (concat "\\<\\(["
		       (when (or (spice-standard-p 'hspice)
				 (spice-standard-p 'draccdl)
				 (spice-standard-p 'layla)) "$")
		       (when (spice-standard-p 'eldo) "!")
		       "]\\)\\(.*\\)$")
	       (list 1 font-lock-comment-face)
	       (list 2 spice-doc-face 'append) ;; needs at least 'append or 'keep
	       )
	 (list (concat "\\<\\([*"
		       (when (or (spice-standard-p 'hspice)
				 (spice-standard-p 'draccdl)
				 (spice-standard-p 'layla)) "$")
		       (when (spice-standard-p 'eldo) "!")
		       "]\\)")
	       ;; elisp mode like quotes, for extra clarity
	       (list "['\"]\\([^'\"]+\\)['\"]" nil nil
		     (list 1 spice-constant-face 'prepend)))))

  ;; subcircuit instance names of x instances and model names of mos,bip&diode
  (setq spice-font-lock-keywords-7
	(list
	 (list spice-xinstance-regexp 5 spice-model-name-face)
	 ;; test xinstance function matcher
	 ;;(list 'spice-match-xinstances
	 ;;      '(4 spice-model-name-face keep t)
	 ;;      '(5 font-lock-warning-face keep t))
	 (list spice-model-name-regexp 4 spice-model-name-face)
	 ))

  ;; highlight poly's of inductors and caps
  (setq spice-font-lock-keywords-8
	(list
	 '("^[cl]\\w*\\s-+\\w[^ \t\n]*\\s-+\\w[^ \t\n]*\\s-+\\(poly\\)"
	   1 font-lock-type-face)))

  ;; types, included files, libs and names of subcircuits, ports, libs, models
  (setq spice-font-lock-keywords-9
	(append
	 ;; names and types of models/subckt, lib concept of eldo...
	 (when (spice-standard-p 'eldo)
	   (list
	    (list (concat "^\\s-*\\.\\(model\\|subckt\\|macro\\)\\s-+"
			  "\\(\\(lib\\)\\s-+[^ \t\n]+\\s-+\\)"
			  spice-model-name)
		  '(3 font-lock-keyword-face)
		  '(4 font-lock-function-name-face))))
	 (list
	  ;; types
	  (list "^[+vi.]" '(0 'default) ;; find v & i's and continuation and . lines
		(list spice-types-regexp nil nil '(1 font-lock-type-face)))
	  ;; libs, inc's, ...
	  (list (concat spice-library-regexp-start
			spice-library-regexp-end) 3 spice-include-file-face)
	  ;; names of (defined) entities
	  (list
	   (concat spice-entity-start-regexp
		   "\\(\\s-+\\([a-z]\\w*\\)\\s-*"
		   "\\|\\s-*[\n][+]\\s-*\\([a-z]\\w*\\)\\s-*\\)")
	   (1+ (regexp-opt-depth spice-entity-start-regexp))
	   font-lock-function-name-face)
	  ;; names and types of models
	  (list (concat "^\\s-*\\.model\\s-+"
			spice-model-name
			spice-line-break "\\s-+\\("
			(regexp-opt spice-model-type-names) "\\)\\>")
		'(1 font-lock-function-name-face)
		'(3 font-lock-type-face))
	  ))
        )
  ;; highlight additional eldo reserved words
  (setq spice-font-lock-keywords-10
	(list
	 ;; eldo's labels
	 (list "\\<\\(label\\)\\s-*=\\s-*\\([^\"]\\w*\\)\\>"
	       '(1 font-lock-type-face)
	       '(2 font-lock-variable-name-face))
	 ;; eldo's labels(2)
	 (list "\\<\\(label\\)\\s-*=\\s-*\"\\([^\"]+\\)\""
	       '(1 font-lock-type-face)
	       '(2 font-lock-variable-name-face))
	 ;; eldo's param,model&pin:
	 ;;'("\\<\\(\\(param\\|pin\\|model\\):\\)" 0 font-lock-keyword-face)
	 (list 'spice-match-eldo-colon-keywords
	       (list 1 font-lock-keyword-face)
	       '(2 'default))
	 ;; eldo's builtin macro models
	 (list (concat "^y\\w+\\s-+\\<\\("
		       (regexp-opt spice-eldo-macromodel-keywords) "\\)\\>")
	       '(1 spice-builtin-face))
	 ;; eldo's y instances, type is second word on line
	 (list (concat "^y\\w+\\s-+\\<" spice-model-name "\\>")
	       1 spice-model-name-face)))
  ;; highlight property names property=value & strings (way at the end)
  (setq spice-font-lock-keywords-11
	(list
	 ;; this is 1 second faster when loading 28 files, appx 10000 lines
	 ;; .options keywords & parameters
	 (list (concat "^\\s-*" ;; was "^"
		       (when (spice-standard-p 'layla) "\\*?")
		       "\\.opt\\(ion"
		       (when (or (spice-standard-p 'hspice)
				 (spice-standard-p 'layla))
			 "\\(s\\)?")
		       "\\)?\\s-+")
	       '(0 font-lock-keyword-face) ;; not used, already colored
	       (list (concat "\\<\\(\\("
			     (regexp-opt spice-options-keywords)
			     "\\)\\>\\|\\("
			     (regexp-opt spice-options-parameters)
			     "\\)\\s-*=\\)") nil nil
			     (list 1 spice-constant-face)))
	 ;; param=value
	 ;; '("\\<\\([a-z]\\w*\\)\\s-*=" 1 font-lock-variable-name-face)
	 ;; this function matcher is much faster, it searches for '=' and then
	 ;; returns the word in front of the '='
	 (list 'spice-match-variable-name
	       (list 1 font-lock-variable-name-face 'append)
	       ;;(list 1 font-lock-variable-name-face)
	       '(2 'default 'append)) ;; default is required for XEmacs

	 ;; changelog entries
	 (list
	  (concat
	   "^[*"
	   (when (or (spice-standard-p 'hspice)
		     (spice-standard-p 'layla)) "$")
	   (when (spice-standard-p 'eldo) "!")
	   "]+\\s-+\\([A-Z].*[0-9]\\)\\s-+\\([a-zA-Z].*\\)<\\(.*@.*\\)>$")
	  '(1 font-lock-string-face t)
	  '(2 font-lock-type-face t)
	  '(3 font-lock-variable-name-face t))

	 ;; strings
	 '("\"[^\"]*\""               0 font-lock-string-face)

	 ;; scale factors and powers or is this overkill ?
	 (list (concat
		"\\<[-+]?[0-9.]+\\(\\("
		(regexp-opt '("T" "G" "Meg" "K" "mil" "m" "u" "M" "n" "p" "f"))
		"\\)[a-zA-Z]*\\)\\>")
	       (list 2 spice-constant-face 'append))
	 (list (concat
		"[0-9.]\\(e\\)[-+]?[0-9]+\\>")
	       (list 1 spice-constant-face 'append))
	 ))
  ;; set font-lock-keywords, all of 'em
  (setq spice-font-lock-keywords
	(append spice-font-lock-keywords-0 ;; title first
		(when (spice-standard-p 'layla)
		  spice-font-lock-keywords-1)
		(when spice-highlight-keywords
		  spice-font-lock-keywords-2)
		(when (and
		       spice-highlight-keywords
		       (spice-standard-p 'layla))
		  spice-font-lock-keywords-3)
		(when (spice-standard-p 'spectre)
		  spice-font-lock-keywords-13)
		(when (spice-standard-p 'draccdl)
		  spice-font-lock-keywords-12)
		spice-font-lock-keywords-4
		(when spice-highlight-keywords
		  spice-font-lock-keywords-5)
		(when (or (spice-standard-p 'layla)
			  (spice-standard-p 'hspice)
			  (spice-standard-p 'draccdl)
			  (spice-standard-p 'eldo))
		  spice-font-lock-keywords-6)
		spice-font-lock-keywords-7
		(when spice-highlight-keywords
		  spice-font-lock-keywords-8)
		spice-font-lock-keywords-9
		(when (spice-standard-p 'eldo)
		  spice-font-lock-keywords-10)
		(when spice-highlight-keywords
		  spice-font-lock-keywords-11)
		)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Comments (taken from eldo-mode.el)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; uncomment function, should work for any case now:
(defun spice-uncomment-region (beg end)
  "Uncomment selected region - comment symbol is '*'
Doc comments (starting with '!') are unaffected."
  (interactive "*r")
  (comment-region beg end '(2))) ; 2 is arbitrary, can be any value


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; spice mode map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'tempo) ; ? is this necessary

(defvar spice-mode-map ()
  "Keymap used in spice-mode.")

(if spice-mode-map
    ()
  (let ((map (make-sparse-keymap)))

    ;; key binding for template completion
    (define-key map "\t"       'tempo-complete-tag)
    ;; (define-key map "\S-\t"       'indent-for-tab-command)
    (define-key map [(shift tab)]       'indent-for-tab-command)
    (define-key map [(shift iso-lefttab)]  'indent-for-tab-command)

    ;; key bindings for compile
    (define-key map "\C-c\C-r" 'spice-compile)  ;; r for run
    (define-key map "\C-c\C-k" 'kill-compilation)

    ;; replace global binding
    (define-key map "\C-x`"    'spice-next-error)

    ;; key bindings for waveform viewer
    (define-key map "\C-c\C-v"    'spice-run-waveform-viewer)

    ;; key bindings for output file loading
    (define-key map "\C-c\C-o"    'spice-load-output-file)

    ;; key bindings for include file loading
    (define-key map "\C-c\C-l"    'spice-load-include-files)

    ;; comment region, use auctex-mode bindings...
    (define-key map "\C-c\C-c"    'comment-region)
    ;;(define-key map "\C-c:"    'spice-uncomment-region)  ;; \C-u\C-c\C-c

    ;; .subckt search
    (define-key map "\C-c\C-s" 'spice-search-subckt)

    ;; join lines
    (define-key map "\M-^"     'spice-delete-indentation)

    ;; key bindings for hiding/unhidding comments
    (define-key map "\C-c;"    'spice-hide-all-comments)
    (define-key map "\C-c:"    'spice-unhide-all-comments)

    ;; changelog addition
    (define-key map "\C-c\C-ac"   'spice-add-changelog-entry)

    (setq spice-mode-map map)))


(defvar spice-output-mode-map ()
  "Keymap used in Spice-output mode.")

(if spice-output-mode-map
    ()
  (let ((map (make-sparse-keymap)))
    ;; nothing for now ...

    (setq spice-output-mode-map map)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; libraries & include files (taken & adapted from eldo-mode.el, E. Rouat)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;------------------------------------------------------------
;; Mouse bindings (only used by 'spice-load-file-at-mouse')
;; I separate this from spice-mode-map so that this particular
;; mouse binding doesn't interfere with other bindings

(defvar spice-mode-mouse-map nil
  "Map containing mouse bindings for spice-mode.")

(if spice-mode-mouse-map
    ()
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map spice-mode-map)
    ;; mouse button bindings
    ;;(define-key map "\r" 'ffap)
    (define-key map "\r" 'spice-load-file-at-point)
    (define-key map [mouse-2] 'spice-load-file-at-mouse)
    (define-key map [S-mouse-2] 'mouse-yank-at-click)
    (setq spice-mode-mouse-map map)))


;; overlays and extents are a mess. Depending on emacs/xemacs versions the
;; following code might actually work. What is known now is that this
;; works in: emacs 20.7, 21.1 and 21.2 (Linux versions have been tested),
;; xemacs 21.1 (Windows) and xemacs 21.5 (Linux). You need
;; fsf-compat package for xemacs 21.4.5 (Linux). Anyone still following this ?

;; create set-extent-keymap procedure when it does not exist
(eval-and-compile
  (unless (fboundp 'set-extent-keymap)
    (defun set-extent-keymap (extent keymap)
      "fallback version of set-extent-keymap (for emacs 2[01])"
      (set-extent-property extent 'local-map keymap))))


(defun spice-colorize-libraries (beg end old-len)
  "This function colorises libraries and included files when the mouse
passes over them. Clicking on the middle-mouse button loads them in a buffer.
BEWARE, this feature was hard to implement, and contains (non-fatal) bugs,
primarily because emacs 20 does not have the same support for this as xemacs
has."
  (save-excursion
    (save-match-data
      (let (end-point)
	(goto-char end)
	(end-of-line)
	(setq end-point (point))
	(goto-char beg)
	(beginning-of-line)  ; scan entire line !
	;; delete overlays existing on this line
	(let ((overlays (overlays-in (point) end-point)))
	  (while overlays
	    (if (and (overlay-get (car overlays) 'detachable)
		     (overlay-get (car overlays) 'spice-library))
		(delete-overlay (car overlays))
	      )
	    (setq overlays (cdr overlays)))) ; let
					; make new ones, could reuse deleted one ?
	(while (search-forward-regexp spice-library-regexp-start end-point t)
	  (let (start-lib extent)
	    (setq start-lib (point))
	    (search-forward-regexp spice-library-regexp-end end-point)
					; (let ((end-lib (point)))
	    (or (extent-at (point) (buffer-name) 'mouse-face) ;; not yet extended
		(progn
		  (setq extent (make-extent start-lib (point)))
		  (set-extent-property extent 'start-closed 't)
		  (set-extent-property extent 'end-closed 't)
		  (set-extent-property extent 'detachable 't)
		  (set-extent-property extent 'spice-library 't)
		  (set-extent-property extent 'mouse-face 'highlight)
		  (set-extent-keymap extent spice-mode-mouse-map)))))))))


(defun spice-colorize-libraries-buffer ()
  (interactive)
  ;; (message "running colorize libraries buffer")
  ;; delete overlays
  (let ((overlays (overlays-in (point-min) (point-max))))
    (while overlays
      (if (and
	   (overlay-get (car overlays) 'detachable)
	   (overlay-get (car overlays) 'spice-library))
	  (delete-overlay (car overlays)))
      (setq overlays (cdr overlays)))) ; let
  ;; remake overlays
  (spice-colorize-libraries (point-min) (point-max) nil))


;; ffap needs wrapper to detect end of buffer condition
(defun spice-load-file-at-point ()
  "wrapper for ffap. But if at end of buffer inserts a newline instead"
  (interactive)
  (if (looking-at "\\'")
      (newline) ;; assumes \r is bound to load file...
    (ffap)))


;; ffap-at-mouse isn't available in xemacs < 21
;; so define this function to do more or less the same (primarily
;; wraps ffap-at-mouse, except for xemacs 20)...
(defun spice-load-file-at-mouse (event)
  "loads file under button 2 click. Checks if file is readable."
  (interactive "@e")
  (if (fboundp 'ffap-at-mouse)
      (ffap-at-mouse event)  ;; use ffap-at-mouse if available
    (save-excursion ;; implement a spice specific ffap-at-mouse
      (mouse-set-point event)
      (beginning-of-line)
      (if (looking-at (concat spice-library-regexp-start
			      spice-library-regexp-end))
	  (if (file-readable-p (substitute-in-file-name (match-string 3)))
	      (find-file (substitute-in-file-name (match-string 3)))
	    (progn
	      (message "File '%s' isn't readable, use shift-mouse2 to paste in this field" (match-string 3))))
	))))


;;------------------------------------------------------------
;; Changelog and sections support (taken from eldo-mode, trying
;;  to be compatible :)
;;------------------------------------------------------------

(defun spice-doc-char ()
  "Return doc char that's valid in the selected spice submode"
  (cond
   ((and (spice-standard-p 'eldo)
	 (spice-standard-p 'hspice))
    "*") ; if both eldo and hspice is turned on
   ((spice-standard-p 'eldo)
    "!") ; only eldo
   ((spice-standard-p 'hspice)
    "$") ; only hspice
   (t
    "*"))) ; everything else


(defun spice-find-changelog-point ()
  "Find adequate position to place Changelog entries: just before .end
or if not found at end of buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((pos (re-search-forward
		"^\\.end\\b" nil t)))
      (if pos (progn (forward-line 1)
		     (point))
	(point-max)))))


(defun spice-add-changelog-entry (changelog-entry)
  "Find changelog section (create it if not found) and add an entry for today."
  (interactive "sChangelog entry: ")
  (goto-char (point-min))
  (if (not (re-search-forward
	    (concat spice-section-regexp-start "Changelog") nil t))
      (spice-add-section "Changelog" (spice-find-changelog-point)))

  (spice-goto-section "Changelog")
                                        ; (forward-line 2)
  (let ((string (concat (spice-doc-char)
			(if (equal (spice-doc-char) "*") "* " " ")
			(substring (current-time-string) 0 11)
			(substring (current-time-string) -4) " "
			(user-full-name) " <" user-mail-address ">")))
    (if (not (search-forward string nil t))
	(insert "\n" string "\n\n")
      (forward-line 2))
    (insert (spice-doc-char)
	    (if (equal (spice-doc-char) "*") "*" "")
	    "    - " changelog-entry "\n")))


(defun spice-goto-section (section)
  "Move point to the beginning of the specified section; If the
section is not found, leave point at previous location."
  (interactive "ssection: ")
  (let ((pos (point)))
    (goto-char (point-min))
    (if (not (re-search-forward
	      (concat spice-section-regexp-start section "\\b") nil t))
	(progn (message "Couldn't find section %s" section)
	       (goto-char pos))
      (progn
	(forward-line 2)
	(recenter))))) ;; added recenter


(defun spice-comment-bar (&optional aligned)
  "Insert solid comment bar from column zero to end of line. If optional
argument is provided, bar will be added from current column."
  (interactive)
  (if (not aligned) (beginning-of-line) )
  (insert "*")
  (insert-char ?- (- (1- fill-column) (current-column)))
  (insert "\n"))


(defun spice-add-section (section &optional arg)
  "Add a section in buffer at (optional) point arg"
  (interactive "ssection: ")
  (if arg
      (goto-char arg))
  (spice-comment-bar)
  (insert
   (concat (spice-doc-char) "\t" section " \n"))
  (spice-comment-bar)
  ;;  (unless (assoc section spice-section-alist)
  ;;    ;; new entry
  ;;    (custom-set-variables
  ;;     (quote (spice-section-alist
  ;;	     (append spice-section-alist
  ;;		     (list (list section (upcase section) nil))))))
  ;;    )
  )


(defvar spice-cache-section-alist nil)

(defun spice-cache-section-p (section)
  "checks for all sections in file and remembers if they were present or not"
  (save-excursion
    (setq spice-cache-section-alist nil)
    (goto-char (point-min))
    (while (re-search-forward spice-section-headings-regexp nil t)
      (setq spice-cache-section-alist
            (cons (cons (downcase (spice-match-string-no-properties 2)) t)
                  spice-cache-section-alist)))
    (spice-section-p section)))


(defun spice-section-p (section)
  "checks if named section is in file, returns t if found, nil otherwise,
uses cache generated with the `spice-cache-section-p' function."
  (assoc section spice-cache-section-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Templates (extensive, long code...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tempo)

;;; element templates

;; resistors
(tempo-define-template
 "spice-spice2g6-resistor"
 '("R"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "[val]: "))
 ;; "spice2g6 resistor"
 "R"
 "tempo template for spice2g6 resistor"
 'spice-tempo-tags)


(tempo-define-template
 "spice-spice3-semiconductor-resistor"
 '("R"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "<value>: " value) '(just-one-space)
   (p "<mname>: " mname) '(just-one-space)
   (p "<length>: " l 'noinsert)
   (if (string-equal (tempo-lookup-named 'l) "")
       () (list 'l "L=" '(s l)))
   '(just-one-space)
   (p "<width>: " w 'noinsert)
   (if (string-equal (tempo-lookup-named 'w) "")
       () (list 'l "W=" '(s w)))
   '(just-one-space)
   (p "<temp>: " temp 'noinsert)
   (if (string-equal (tempo-lookup-named 'temp) "")
       () (list 'l "TEMP=" '(s temp)))
   '(just-one-space)
   )
 ;; "spice3 semiconductor resistor"
 "RSS"
 "tempo template for spice3 semiconductor resistor"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-resistor"
 '("R"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "<mname>: " mname) '(just-one-space)
   (if (string-equal (tempo-lookup-named 'mname) "")
       (list 'l "r=" '(p "[val]: "))
     (list 'l '(p "[val]: "))) '(just-one-space)
     (p "<temp coef 1>: " tc1 'noinsert)
     (if (string-equal (tempo-lookup-named 'tc1) "") ()
       (list 'l "TC1=" '(s tc1) '(p "<temp coef 2>: " tc2 'noinsert)))
     '(just-one-space)
     (if (and (tempo-lookup-named 'tc2)
              (not (string-equal (tempo-lookup-named 'tc2) "")))
         (list 'l "TC2=" '(s tc2)))
     '(just-one-space)
     (p "<ac resistance>: " ac 'noinsert)
     (if (string-equal (tempo-lookup-named 'ac) "")
         () (list 'l "AC=" '(s ac)))
     '(just-one-space)
     (p "<temp>: " temp 'noinsert)
     (if (string-equal (tempo-lookup-named 'temp) "")
         () (list 'l "T=" '(s temp)))
     '(just-one-space)
     (p "<m>: " m 'noinsert)
     (if (string-equal (tempo-lookup-named 'm) "")
         () (list 'l "M=" '(s m)))
     '(just-one-space)
     (p "<nonoise in transient [y/n]?>: " nonoise 'noinsert)
     (if (string-equal (tempo-lookup-named 'nonoise) "y")
         (list 'l "NONOISE"))
     '(just-one-space)
     (p "<kf>: " kf 'noinsert)
     (if (string-equal (tempo-lookup-named 'kf) "")
         () (list 'l '(s kf) '(p "<af>: " af 'noinsert)))
     '(just-one-space)
     (if (and (tempo-lookup-named 'af)
              (not (string-equal (tempo-lookup-named 'af) "")))
         (list 'l '(s af)))
     '(just-one-space)
     )
 ;; "eldo resistor"
 "RE"
 "tempo template for eldo resistor"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-expression-resistor"
 '("R"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "[VALUE | TABLE]: " type 'noinsert) '(just-one-space)
   (if (string-equal (tempo-lookup-named 'type) "VALUE")
       (list 'l "VALUE={" '(p "[val, enter expression without {}]: ") "}")
     (list 'l "TABLE={" '(p "[table of values, enter table without {}]: ") "}"))
   '(just-one-space)
   (if (string-equal (tempo-lookup-named 'type) "VALUE")
       (list 'l '(p "<nonoise in transient [y/n]?>: " nonoise 'noinsert)))
   (if (and (tempo-lookup-named 'nonoise)
	    (string-equal (tempo-lookup-named 'nonoise) "y"))
       (list 'l "NONOISE"))
   '(just-one-space)
   (p "<kf>: " kf 'noinsert)
   (if (string-equal (tempo-lookup-named 'kf) "")
       () (list 'l '(s kf) '(p "<af>: " af 'noinsert)))
   '(just-one-space)
   (if (and (tempo-lookup-named 'af)
	    (not (string-equal (tempo-lookup-named 'af) "")))
       (list 'l '(s af)))
   )
 ;; "eldo expression resistor"
 "REE"
 "tempo template for eldo expression resistor"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-semiconductor-resistor"
 '("P"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "[mname]: " mname) '(just-one-space)
   (p "<res>: " r 'noinsert)
   (if (string-equal (tempo-lookup-named 'r) "")
       () (list 'l "R=" '(s r)))
   '(just-one-space)
   (p "<length>: " l 'noinsert)
   (if (string-equal (tempo-lookup-named 'l) "")
       () (list 'l "L=" '(s l)))
   '(just-one-space)
   (p "<contact offset length>: " cl 'noinsert)
   (if (string-equal (tempo-lookup-named 'cl) "")
       () (list 'l "CL=" '(s cl)))
   '(just-one-space)
   (p "<width>: " w 'noinsert)
   (if (string-equal (tempo-lookup-named 'w) "")
       () (list 'l "W=" '(s w)))
   '(just-one-space)
   (p "<contact offset width>: " cw 'noinsert)
   (if (string-equal (tempo-lookup-named 'cw) "")
       () (list 'l "CW=" '(s cw)))
   '(just-one-space)
   (p "<area>: " area 'noinsert)
   (if (string-equal (tempo-lookup-named 'area) "")
       () (list 'l "AREA=" '(s area)))
   '(just-one-space)
   (p "<init cond (voltage, voltage)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "")
       () (list 'l "IC=" '(s ic)))
   '(just-one-space)
   )
 ;; "eldo semiconductor resistor"
 "RES"
 "tempo template for eldo semiconductor resistor"
 'spice-tempo-tags)


(tempo-define-template
 "spice-hspice-resistor"
 '("R"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "<mname>: " mname) '(just-one-space)
   (if (string-equal (tempo-lookup-named 'mname) "")
       (list 'l "R=" '(p "[val]: "))
     (list 'l '(p "<val>: " val 'noinsert)
	   '(if (not (string-equal (tempo-lookup-named 'val) ""))
		(list 'l "R=" '(s val))))) '(just-one-space)
                (p "<temp coef 1>: " tc1 'noinsert)
                (if (string-equal (tempo-lookup-named 'tc1) "") ()
                  (list 'l "TC1=" '(s tc1) '(p "<temp coef 2>: " tc2 'noinsert)))
                '(just-one-space)
                (if (and (tempo-lookup-named 'tc2)
                         (not (string-equal (tempo-lookup-named 'tc2) "")))
                    (list 'l "TC2=" '(s tc2)))
                '(just-one-space)
                (p "<scale>: " scale 'noinsert)
                (if (string-equal (tempo-lookup-named 'scale) "")
                    () (list 'l "SCALE=" '(s scale)))
                '(just-one-space)
                (p "<m>: " m 'noinsert)
                (if (string-equal (tempo-lookup-named 'm) "")
                    () (list 'l "M=" '(s m)))
                '(just-one-space)
                (p "<ac resistance>: " ac 'noinsert)
                (if (string-equal (tempo-lookup-named 'ac) "")
                    () (list 'l "AC=" '(s ac)))
                '(just-one-space)
                (p "<diff temp>: " dtemp 'noinsert)
                (if (string-equal (tempo-lookup-named 'dtemp) "")
                    () (list 'l "DTEMP=" '(s dtemp)))
                '(just-one-space)
                (p "<length>: " l 'noinsert)
                (if (string-equal (tempo-lookup-named 'l) "")
                    () (list 'l "L=" '(s l)))
                '(just-one-space)
                (p "<width>: " w 'noinsert)
                (if (string-equal (tempo-lookup-named 'w) "")
                    () (list 'l "W=" '(s w)))
                '(just-one-space)
                (p "<cap>: " c 'noinsert)
                (if (string-equal (tempo-lookup-named 'c) "")
                    () (list 'l "C=" '(s c)))
                '(just-one-space)
                )
 ;; "hspice resistor"
 "RH"
 "template for hspice resistor tempo templates"
 'spice-tempo-tags)


(tempo-define-template
 "spice-layla-resistor"
 '("R"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
                                        ;   (p "<mname>: ") '(just-one-space)
   (p "[value]: ") '(just-one-space)
   (p "<width>: " w 'noinsert)
   (if (string-equal (tempo-lookup-named 'w) "")
       () (list 'l "width=" '(s w)))
   '(just-one-space)
   "\n+ type=\""
   (p "[type (no quotes)]: ") "\""
   '(just-one-space)
   (p "<symmetry (no quotes)>: " symmetry 'noinsert)
   (if (string-equal (tempo-lookup-named 'symmetry) "")
       () (list 'l "symmetry=\"" '(s symmetry) "\""))
   '(just-one-space)
   (p "<matching (no quotes)>: " matching 'noinsert)
   (if (string-equal (tempo-lookup-named 'matching) "")
       () (list 'l "matching=\"" '(s matching) "\""))
   '(just-one-space)
   (p "<couple>: " couple 'noinsert)
   (if (string-equal (tempo-lookup-named 'couple) "")
       () (list 'l "couple=" '(s couple)))
   '(just-one-space)
   (p "<array (no quotes)>: " array 'noinsert)
   (if (string-equal (tempo-lookup-named 'array) "")
       () (list 'l "array=\"" '(s array) "\""))
   '(just-one-space)
   (p "<orientations (no quotes)>: " orientations 'noinsert)
   (if (string-equal (tempo-lookup-named 'orientations) "")
       () (list 'l "orientations=\"" '(s orientations) "\""))
   '(just-one-space)
   (p "<unit_value>: " unit_value 'noinsert)
   (if (string-equal (tempo-lookup-named 'unit_value) "")
       () (list 'l "unit_value=" '(s unit_value)))
   '(just-one-space)
   (p "<current>: " current 'noinsert)
   (if (string-equal (tempo-lookup-named 'current) "")
       () (list 'l "current=" '(s current)))
   '(just-one-space)
   (p "<power>: " power 'noinsert)
   (if (string-equal (tempo-lookup-named 'power) "")
       () (list 'l "power=" '(s power)))
   '(just-one-space)
   (p "<min_ar>: " min_ar 'noinsert)
   (if (string-equal (tempo-lookup-named 'min_ar) "")
       () (list 'l "min_ar=" '(s min_ar)))
   '(just-one-space)
   (p "<max_ar>: " max_ar 'noinsert)
   (if (string-equal (tempo-lookup-named 'max_ar) "")
       () (list 'l "max_ar=" '(s max_ar)))
   '(just-one-space)
   (p "<routing_space>: " routing_space 'noinsert)
   (if (string-equal (tempo-lookup-named 'routing_space) "")
       () (list 'l "routing_space=" '(s routing_space)))
   '(just-one-space)
   )
 ;; "layla resistor"
 "RL"
 "tempo template for layla resistor"
 'spice-tempo-tags)


;; capacitors
(tempo-define-template
 "spice-spice2g6-capacitor"
 '("C"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "[val]: ") '(just-one-space)
   (p "<initial cond (voltage)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "")
       () (list 'l "ic=" '(s ic)))
   )
 ;; "spice2g6 capacitor"
 "C"
 "tempo template for spice2g6 capacitor"
 'spice-tempo-tags)


(tempo-define-template
 "spice-spice3-semiconductor-capacitor"
 '("C"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "<value>: " value) '(just-one-space)
   (p "<mname>: " mname) '(just-one-space)
   (p "<length>: " l 'noinsert)
   (if (string-equal (tempo-lookup-named 'l) "")
       () (list 'l "L=" '(s l)))
   '(just-one-space)
   (p "<width>: " w 'noinsert)
   (if (string-equal (tempo-lookup-named 'w) "")
       () (list 'l "W=" '(s w)))
   '(just-one-space)
   (p "<initial conditions (Voltage)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "")
       () (list 'l "IC=" '(s ic)))
   '(just-one-space)
   )
 ;; "spice3 semiconductor capacitor"
 "CSS"
 "tempo template for spice3 semiconductor capacitor"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-capacitor"
 '("C"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "<mname | POLY>: " mname) '(just-one-space)
   (if (string-equal (tempo-lookup-named 'mname) "POLY")
       (list 'l '(p "[val and poly coefficients]: "))
     (list 'l '(p "[val]: ")))
   '(just-one-space)
   (p "<m>: " m 'noinsert)
   (if (string-equal (tempo-lookup-named 'm) "")
       () (list 'l "M=" '(s m)))
   '(just-one-space)
   (p "<length>: " l 'noinsert)
   (if (string-equal (tempo-lookup-named 'l) "")
       () (list 'l "L=" '(s l)))
   '(just-one-space)
   (p "<width>: " w 'noinsert)
   (if (string-equal (tempo-lookup-named 'w) "")
       () (list 'l "W=" '(s w)))
   '(just-one-space)
   (p "<diff temp>: " dtemp 'noinsert)
   (if (string-equal (tempo-lookup-named 'dtemp) "")
       () (list 'l "DTEMP=" '(s dtemp)))
   '(just-one-space)
   (p "<temp coef 1>: " tc1 'noinsert)
   (if (string-equal (tempo-lookup-named 'tc1) "") ()
     (list 'l "TC1=" '(s tc1) '(p "<temp coef 2>: " tc2 'noinsert)))
   '(just-one-space)
   (if (and (tempo-lookup-named 'tc2)
	    (not (string-equal (tempo-lookup-named 'tc2) "")))
       (list 'l "TC2=" '(s tc2)))
   '(just-one-space)
   (p "<initial cond (voltage)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "")
       () (list 'l "IC=" '(s ic)))
   )
 ;; "eldo capacitor"
 "CE"
 "tempo template for eldo capacitor"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-expression-capacitor"
 '("C"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "<temp coef 1>: " tc1 'noinsert)
   (if (string-equal (tempo-lookup-named 'tc1) "") ()
     (list 'l "TC1=" '(s tc1)))
   '(just-one-space)
   (p "<temp coef 2>: " tc2 'noinsert)
   (if (string-equal (tempo-lookup-named 'tc2) "") ()
     (list 'l "TC2=" '(s tc2)))
   '(just-one-space)
   (p "<temp coef 3>: " tc3 'noinsert)
   (if (string-equal (tempo-lookup-named 'tc3) "") ()
     (list 'l "TC3=" '(s tc3)))
   '(just-one-space)
   "VALUE={"
   (p "[val enter expression without {}]: ")
   "}"
   )
 ;; "eldo expression capacitor"
 "CEE"
 "tempo template for eldo expression capacitor"
 'spice-tempo-tags)


(tempo-define-template
 "spice-hspice-capacitor"
 '("C"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "<mname | POLY>: " mname) '(just-one-space)
   (if (string-equal (tempo-lookup-named 'mname) "POLY")
       (list 'l '(p "[val and poly coefficients]: " val))
     (if (string-equal (tempo-lookup-named 'mname) "")
	 (list 'l "C=" '(p "[val or expression]: " val))
       (list 'l "C=" '(p "[val]: " val))))
   '(just-one-space)
   (if (and (not (string-equal (tempo-lookup-named 'mname) "POLY"))
	    (char-equal (string-to-char (tempo-lookup-named 'val))
			(string-to-char "'")))
       (list 'l '(p "<ctype>: " ctype 'noinsert)))
   (if (and (tempo-lookup-named 'ctype)
	    (not (string-equal (tempo-lookup-named 'ctype) "")))
       (list 'l "CTYPE=" '(s ctype)))
   '(just-one-space)
   (p "<temp coef 1>: " tc1 'noinsert)
   (if (string-equal (tempo-lookup-named 'tc1) "") ()
     (list 'l "TC1=" '(s tc1) '(p "<temp coef 2>: " tc2 'noinsert)))
   '(just-one-space)
   (if (and (tempo-lookup-named 'tc2)
	    (not (string-equal (tempo-lookup-named 'tc2) "")))
       (list 'l "TC2=" '(s tc2)))
   '(just-one-space)
   (p "<scale>: " scale 'noinsert)
   (if (string-equal (tempo-lookup-named 'scale) "")
       () (list 'l "SCALE=" '(s scale)))
   '(just-one-space)
   (p "<initial cond (voltage)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "")
       () (list 'l "IC=" '(s ic)))
   '(just-one-space)
   (p "<m>: " m 'noinsert)
   (if (string-equal (tempo-lookup-named 'm) "")
       () (list 'l "M=" '(s m)))
   '(just-one-space)
   (p "<width>: " w 'noinsert)
   (if (string-equal (tempo-lookup-named 'w) "")
       () (list 'l "W=" '(s w)))
   '(just-one-space)
   (p "<length>: " l 'noinsert)
   (if (string-equal (tempo-lookup-named 'l) "")
       () (list 'l "L=" '(s l)))
   '(just-one-space)
   (p "<diff temp>: " dtemp 'noinsert)
   (if (string-equal (tempo-lookup-named 'dtemp) "")
       () (list 'l "DTEMP=" '(s dtemp)))
   '(just-one-space)
   )
 ;; "hspice capacitor"
 "CH"
 "tempo template for hspice capacitor"
 'spice-tempo-tags)


(tempo-define-template
 "spice-layla-capacitor"
 '("C"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "[value]: ") '(just-one-space)
   (p "<units_ver>: " units_ver 'noinsert)
   (if (string-equal (tempo-lookup-named 'units_ver) "")
       () (list 'l "units_ver=" '(s units_ver)))
   '(just-one-space)
   (p "<units_hor>: " units_hor 'noinsert)
   (if (string-equal (tempo-lookup-named 'units_hor) "")
       () (list 'l "units_hor=" '(s units_hor)))
   '(just-one-space)
   "\n+ type=\""
   (p "[type (no quotes)]: ") "\""
   '(just-one-space)
   (p "<symmetry (no quotes)>: " symmetry 'noinsert)
   (if (string-equal (tempo-lookup-named 'symmetry) "")
       () (list 'l "symmetry=\"" '(s symmetry) "\""))
   '(just-one-space)
   (p "<matching (no quotes)>: " matching 'noinsert)
   (if (string-equal (tempo-lookup-named 'matching) "")
       () (list 'l "matching=\"" '(s matching) "\""))
   '(just-one-space)
   (p "<couple>: " couple 'noinsert)
   (if (string-equal (tempo-lookup-named 'couple) "")
       () (list 'l "couple=" '(s couple)))
   '(just-one-space)
   (p "<array (no quotes)>: " array 'noinsert)
   (if (string-equal (tempo-lookup-named 'array) "")
       () (list 'l "array=\"" '(s array) "\""))
   '(just-one-space)
   (p "<orientations (no quotes)>: " orientations 'noinsert)
   (if (string-equal (tempo-lookup-named 'orientations) "")
       () (list 'l "orientations=\"" '(s orientations) "\""))
   '(just-one-space)
   (p "<unit_value>: " unit_value 'noinsert)
   (if (string-equal (tempo-lookup-named 'unit_value) "")
       () (list 'l "unit_value=" '(s unit_value)))
   '(just-one-space)
   (p "<power>: " power 'noinsert)
   (if (string-equal (tempo-lookup-named 'power) "")
       () (list 'l "power=" '(s power)))
   '(just-one-space)
   (p "<min_ar>: " min_ar 'noinsert)
   (if (string-equal (tempo-lookup-named 'min_ar) "")
       () (list 'l "min_ar=" '(s min_ar)))
   '(just-one-space)
   (p "<max_ar>: " max_ar 'noinsert)
   (if (string-equal (tempo-lookup-named 'max_ar) "")
       () (list 'l "max_ar=" '(s max_ar)))
   '(just-one-space)
   (p "<routing_space>: " routing_space 'noinsert)
   (if (string-equal (tempo-lookup-named 'routing_space) "")
       () (list 'l "routing_space=" '(s routing_space)))
   '(just-one-space)
   )
 ;; "layla capacitor"
 "CL"
 "tempo template for layla capacitor"
 'spice-tempo-tags)


;; inductors
(tempo-define-template
 "spice-spice2g6-inductor"
 '("L"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "[val]: ") '(just-one-space)
   (p "<initial cond (current)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "")
       () (list 'l "ic=" '(s ic)))
   )
 ;; "spice2g6 inductor"
 "L"
 "tempo template for spice2g6 inductor"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-inductor"
 '("L"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "<mname | POLY>: " mname) '(just-one-space)
   (if (string-equal (tempo-lookup-named 'mname) "POLY")
       (list 'l '(p "[val and poly coefficients]: "))
     (list 'l '(p "[val]: ")))
   '(just-one-space)
   (p "<initial cond (current)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "")
       () (list 'l "ic=" '(s ic)))
   )
 ;; "eldo inductor"
 "LE"
 "tempo template for eldo inductor"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-expression-inductor"
 '("L"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   "VALUE={"
   (p "[val enter expression without {}]: ")
   "}"
   )
 ;; "eldo expression inductor"
 "LEE"
 "tempo template for eldo expression inductor"
 'spice-tempo-tags)


(tempo-define-template
 "spice-hspice-inductor"
 '("L"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "<POLY | NT>: " poly 'noinsert) '(just-one-space)
   (cond ((string-equal (tempo-lookup-named 'poly) "POLY")
	  (list 'l "POLY " '(p "[value and coefficients]: ")))
	 ((string-equal (tempo-lookup-named 'poly) "NT")
	  (list 'l "NT=" '(p "[number of turns]: " nt)))
	 (t
	  (list 'l "L=" '(p "[value or expression]: " val)))
	 )
   '(just-one-space)
   (if (and (tempo-lookup-named 'val)
	    (char-equal (string-to-char (tempo-lookup-named 'val))
			(string-to-char "'")))
       (list 'l '(p "<ltype>: " ltype 'noinsert)))
   (if (and (tempo-lookup-named 'ltype)
	    (not (string-equal (tempo-lookup-named 'ltype) "")))
       (list 'l "LTYPE=" '(s ltype)))
   '(just-one-space)
   (p "<temp coef 1>: " tc1 'noinsert)
   (if (string-equal (tempo-lookup-named 'tc1) "") ()
     (list 'l "TC1=" '(s tc1) '(p "<temp coef 2>: " tc2 'noinsert)))
   '(just-one-space)
   (if (and (tempo-lookup-named 'tc2)
	    (not (string-equal (tempo-lookup-named 'tc2) "")))
       (list 'l "TC2=" '(s tc2)))
   '(just-one-space)
   (p "<scale>: " scale 'noinsert)
   (if (string-equal (tempo-lookup-named 'scale) "")
       () (list 'l "SCALE=" '(s scale)))
   '(just-one-space)
   (p "<initial cond (current)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "")
       () (list 'l "IC=" '(s ic)))
   '(just-one-space)
   (p "<m>: " m 'noinsert)
   (if (string-equal (tempo-lookup-named 'm) "")
       () (list 'l "M=" '(s m)))
   '(just-one-space)
   (p "<diff temp>: " dtemp 'noinsert)
   (if (string-equal (tempo-lookup-named 'dtemp) "")
       () (list 'l "DTEMP=" '(s dtemp)))
   '(just-one-space)
   (p "<res>: " r 'noinsert)
   (if (string-equal (tempo-lookup-named 'r) "")
       () (list 'l "R=" '(s r)))
   '(just-one-space)
   )
 ;; "hspice inductor"
 "LH"
 "tempo template for hspice inductor"
 'spice-tempo-tags)


(tempo-define-template
 "spice-layla-inductor"
 '("L"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "<value>: ") '(just-one-space)
   (p "<outer radius>: " radius 'noinsert)
   (if (string-equal (tempo-lookup-named 'radius) "")
       () (list 'l "radius=" '(s radius)))
   '(just-one-space)
   (p "<width>: " w 'noinsert)
   (if (string-equal (tempo-lookup-named 'w) "")
       () (list 'l "w=" '(s w)))
   '(just-one-space)
   (p "<number of turns>: " n 'noinsert)
   (if (string-equal (tempo-lookup-named 'n) "")
       () (list 'l "n=" '(s n)))
   '(just-one-space)
   (p "<space>: " space 'noinsert)
   (if (string-equal (tempo-lookup-named 'space) "")
       () (list 'l "space=" '(s space)))
   '(just-one-space)
   (p "<end_fraction [0-1]>: " end_fraction 'noinsert)
   (if (string-equal (tempo-lookup-named 'end_fraction) "")
       () (list 'l "end_fraction=" '(s end_fraction)))
   '(just-one-space)
   "\n+ type=\""
   (p "[type (no quotes)]: ") "\""
   '(just-one-space)
   (p "<symmetry (no quotes)>: " symmetry 'noinsert)
   (if (string-equal (tempo-lookup-named 'symmetry) "")
       () (list 'l "symmetry=\"" '(s symmetry) "\""))
   '(just-one-space)
   (p "<matching (no quotes)>: " matching 'noinsert)
   (if (string-equal (tempo-lookup-named 'matching) "")
       () (list 'l "matching=\"" '(s matching) "\""))
   '(just-one-space)
   (p "<couple>: " couple 'noinsert)
   (if (string-equal (tempo-lookup-named 'couple) "")
       () (list 'l "couple=" '(s couple)))
   '(just-one-space)
   (p "<array (no quotes)>: " array 'noinsert)
   (if (string-equal (tempo-lookup-named 'array) "")
       () (list 'l "array=\"" '(s array) "\""))
   '(just-one-space)
   (p "<orientations (no quotes)>: " orientations 'noinsert)
   (if (string-equal (tempo-lookup-named 'orientations) "")
       () (list 'l "orientations=\"" '(s orientations) "\""))
   '(just-one-space)
   (p "<power>: " power 'noinsert)
   (if (string-equal (tempo-lookup-named 'power) "")
       () (list 'l "power=" '(s power)))
   '(just-one-space)
   (p "<min_ar>: " min_ar 'noinsert)
   (if (string-equal (tempo-lookup-named 'min_ar) "")
       () (list 'l "min_ar=" '(s min_ar)))
   '(just-one-space)
   (p "<max_ar>: " max_ar 'noinsert)
   (if (string-equal (tempo-lookup-named 'max_ar) "")
       () (list 'l "max_ar=" '(s max_ar)))
   '(just-one-space)
   (p "<routing_space>: " routing_space 'noinsert)
   (if (string-equal (tempo-lookup-named 'routing_space) "")
       () (list 'l "routing_space=" '(s routing_space)))
   '(just-one-space)
   )
 ;; "layla inductor"
 "LL"
 "tempo template for layla inductor"
 'spice-tempo-tags)


;; coupled inductors
(tempo-define-template
 "spice-spice2g6-coupled-inductors"
 '("K"
   (p "[name]: ") '(just-one-space)
   (p "[first inductor]: ") '(just-one-space)
   (p "[second inductor]: ") '(just-one-space)
   (p "[coupling coefficient]: ")
   )
 ;; "spice2g6 inductor coupling"
 "K"
 "tempo template for spice2g6 coupled inductors"
 'spice-tempo-tags)


;; lossless transmission lines
(tempo-define-template
 "spice-spice2g6-lossless-transmission"
 '("T"
   (p "[name]: ") '(just-one-space)
   (p "[out port1]: ") '(just-one-space)
   (p "[ref port1]: ") '(just-one-space)
   (p "[out port2]: ") '(just-one-space)
   (p "[ref port2]: ") '(just-one-space)
   "Z0=" (p "[char impedance]: ") '(just-one-space)
   (p "<time delay in seconds>: " td 'noinsert)
   (if (string-equal (tempo-lookup-named 'td) "")
       (list 'l '(p "<freq>: " freq 'noinsert)) (list 'l "td=" '(s td)))
   '(just-one-space)
   (if (and (tempo-lookup-named 'freq)
	    (not (string-equal (tempo-lookup-named 'freq) "")))
       (list 'l "f=" '(s freq) '(p "<normalized length [0.25]>: " nrmlen 'noinsert)))
   '(just-one-space)
   (if (and (tempo-lookup-named 'nrmlen)
	    (not (string-equal (tempo-lookup-named 'nrmlen) "")))
       (list 'l "nl=" '(s nrmlen)))
   '(just-one-space)
   (p "<initial cond (voltage port1, current port1, voltage port2, current port2)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "")
       () (list 'l "ic=" '(s ic)))
   )
 ;;"spice2g6 lossless transmission line"
 "T"
 "tempo template for spice2g6 lossless transmission line"
 'spice-tempo-tags)


(tempo-define-template
 "spice-hspice-lossless-transmission"
 '("T"
   (p "[name]: ") '(just-one-space)
   (p "[out port1]: ") '(just-one-space)
   (p "[ref port1]: ") '(just-one-space)
   (p "[out port2]: ") '(just-one-space)
   (p "[ref port2]: ") '(just-one-space)
   "Z0=" (p "[char impedance]: ") '(just-one-space)
   (p "<time delay in seconds per meter>: " td 'noinsert)
   (if (string-equal (tempo-lookup-named 'td) "")
       (list 'l '(p "<freq>: " freq 'noinsert))
     (list 'l "td=" '(s td) '(p "<length [1m]>: " l 'noinsert)))
   '(just-one-space)
   (if (and (tempo-lookup-named 'l)
	    (not (string-equal (tempo-lookup-named 'l) "")))
       (list 'l "l=" '(s l)))
   '(just-one-space)
   (if (and (tempo-lookup-named 'freq)
	    (not (string-equal (tempo-lookup-named 'freq) "")))
       (list 'l "f=" '(s freq) '(p "<normalized length [0.25]>: " nrmlen 'noinsert)))
   '(just-one-space)
   (if (and (tempo-lookup-named 'nrmlen)
	    (not (string-equal (tempo-lookup-named 'nrmlen) "")))
       (list 'l "nl=" '(s nrmlen)))
   '(just-one-space)
   (p "<initial cond (voltage port1, current port1, voltage port2, current port2)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "")
       () (list 'l "ic=" '(s ic)))
   )
 ;;"hspice lossless transmission line"
 "TH"
 "tempo template for hspice lossless transmission line"
 'spice-tempo-tags)


;; lossy transmission lines
(tempo-define-template
 "spice-spice2g6-lossy-transmission"
 '("O"
   (p "[name]: ") '(just-one-space)
   (p "[out port1]: ") '(just-one-space)
   (p "[ref port1]: ") '(just-one-space)
   (p "[out port2]: ") '(just-one-space)
   (p "[ref port2]: ") '(just-one-space)
   (p "[mname]: ") '(just-one-space)
   )
 ;;"spice2g6 lossy transmission line"
 "O"
 "tempo template for spice2g6 lossy transmission line"
 'spice-tempo-tags)


(tempo-define-template
 "spice-spice3-rcline"
 '("U"
   (p "[name]: ") '(just-one-space)
   (p "[in port]: ") '(just-one-space)
   (p "[out port]: ") '(just-one-space)
   (p "[ref port]: ") '(just-one-space)
   (p "[mname]: ") '(just-one-space)
   "L="
   (p "[length (m)]: ") '(just-one-space)
   (p "<lumps>: " lumps 'noinsert)
   (if (string-equal (tempo-lookup-named 'lumps) "")
       () (list 'l "N=" '(s lumps)))
   '(just-one-space)
   )
 ;;"spice3 lossy transmission line"
 "RCLS"
 "tempo template for spice2g6 lossy transmission line"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-rcline"
 '("R"
   (p "[name]: ") '(just-one-space)
   (p "[pos node]: ") '(just-one-space)
   (p "[neg node]: ") '(just-one-space)
   (p "[mname]: " mname) '(just-one-space)
   (p "<res>: " r 'noinsert)
   (if (string-equal (tempo-lookup-named 'r) "")
       () (list 'l "R=" '(s r)))
   '(just-one-space)
   (p "<temp coef 1>: " tc1 'noinsert)
   (if (string-equal (tempo-lookup-named 'tc1) "") ()
     (list 'l "TC1=" '(s tc1) '(p "<temp coef 2>: " tc2 'noinsert)))
   '(just-one-space)
   (if (and (tempo-lookup-named 'tc2)
	    (not (string-equal (tempo-lookup-named 'tc2) "")))
       (list 'l "TC2=" '(s tc2)))
   '(just-one-space)
   (p "<cap>: " c 'noinsert)
   (if (string-equal (tempo-lookup-named 'c) "")
       () (list 'l "C=" '(s c)))
   '(just-one-space)
   (p "<length>: " l 'noinsert)
   (if (string-equal (tempo-lookup-named 'l) "")
       () (list 'l "L=" '(s l)))
   '(just-one-space)
   (p "<width>: " w 'noinsert)
   (if (string-equal (tempo-lookup-named 'w) "")
       () (list 'l "W=" '(s w)))
   '(just-one-space)
   (p "<m>: " m 'noinsert)
   (if (string-equal (tempo-lookup-named 'm) "")
       () (list 'l "M=" '(s m)))
   '(just-one-space)
   (p "<diff temp>: " dtemp 'noinsert)
   (if (string-equal (tempo-lookup-named 'dtemp) "")
       () (list 'l "DTEMP=" '(s dtemp)))
   '(just-one-space)
   (p "<scale>: " scale 'noinsert)
   (if (string-equal (tempo-lookup-named 'scale) "")
       () (list 'l "SCALE=" '(s scale)))
   '(just-one-space)
   )
 ;;"eldo rcline"
 "RCLE"
 "tempo template for eldo rcline"
 'spice-tempo-tags)


;; active elements

;; diodes
(tempo-define-template
 "spice-spice2g6-diode"
 '("D"
   (p "[name]: ") '(just-one-space)
   (p "[positive node]: ") '(just-one-space)
   (p "[negative node]: ") '(just-one-space)
   (p "[mname]: ") '(just-one-space)
   (p "<area>: ") '(just-one-space)
   (p "<off [y/n]>: " off 'noinsert) '(just-one-space)
   (if (and (tempo-lookup-named 'off)
	    (string-equal (tempo-lookup-named 'off) "y"))
       (list 'l "OFF"))
   '(just-one-space)
   (p "<initial cond (diode voltage)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "")
       () (list 'l "IC=" '(s ic)))
   '(just-one-space)
   (p "<temp>: " temp 'noinsert)
   (if (string-equal (tempo-lookup-named 'temp) "")
       () (list 'l "TEMP=" '(s temp)))
   '(just-one-space)
   )
 ;;"spice2g6 diode"
 "D"
 "tempo template for spice2g6 diode"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-diode"
 '("D"
   (p "[name]: ") '(just-one-space)
   (p "[positive node]: ") '(just-one-space)
   (p "[negative node]: ") '(just-one-space)
   (p "[mname]: ") '(just-one-space)
   (p "<area>: " area 'noinsert) '(just-one-space)
   (if (string-equal (tempo-lookup-named 'area) "")
       () (list 'l "AREA=" '(s area)))
   '(just-one-space)
   (p "<perimeter>: " peri 'noinsert)
   (if (string-equal (tempo-lookup-named 'peri) "")
       () (list 'l "PERI=" '(s peri)))
   '(just-one-space)
   (p "<temp>: " temp 'noinsert)
   (if (string-equal (tempo-lookup-named 'temp) "")
       () (list 'l "TEMP=" '(s temp)))
   '(just-one-space)
   (p "<off [y/n]>: " off 'noinsert)
   (if (and (tempo-lookup-named 'off)
	    (string-equal (tempo-lookup-named 'off) "y"))
       (list 'l "OFF"))
   '(just-one-space)
   (p "<nonoise [y/n]>: " nonoise 'noinsert)
   (if (and (tempo-lookup-named 'nonoise)
	    (string-equal (tempo-lookup-named 'nonoise) "y"))
       (list 'l "NONOISE"))
   '(just-one-space)
   )
 ;;"eldo diode"
 "DE"
 "tempo template for eldo diode"
 'spice-tempo-tags)


(tempo-define-template
 "spice-hspice-diode"
 '("D"
   (p "[name]: ") '(just-one-space)
   (p "[positive node]: ") '(just-one-space)
   (p "[negative node]: ") '(just-one-space)
   (p "[mname]: ") '(just-one-space)
   (p "<area>: " area 'noinsert) '(just-one-space)
   (if (string-equal (tempo-lookup-named 'area) "")
       (list 'l '(p "<width>: " w 'noinsert))
     (list 'l "AREA=" '(s area) '(p "<perimeter>: " peri 'noinsert)))
   '(just-one-space)
   (if (and (tempo-lookup-named 'peri)
	    (not (string-equal (tempo-lookup-named 'peri) "")))
       (list 'l "PJ=" '(s peri)))
   '(just-one-space)
   (if (string-equal (tempo-lookup-named 'area) "")
       (list 'l '(p "<length>: " l 'noinsert)))
   '(just-one-space)
   (if (and (tempo-lookup-named 'l)
	    (not (string-equal (tempo-lookup-named 'l) "")))
       () (list 'l "L=" '(s l)))
   '(just-one-space)
   (p "<width poly cap>: " wp 'noinsert)
   (if (string-equal (tempo-lookup-named 'wp) "")
       () (list 'l "WP=" '(s wp)))
   '(just-one-space)
   (p "<length poly cap>: " lp 'noinsert)
   (if (string-equal (tempo-lookup-named 'lp) "")
       () (list 'l "LP=" '(s lp)))
   '(just-one-space)
   (p "<width metal cap>: " wm 'noinsert)
   (if (string-equal (tempo-lookup-named 'wm) "")
       () (list 'l "WM=" '(s wm)))
   '(just-one-space)
   (p "<length metal cap>: " lm 'noinsert)
   (if (string-equal (tempo-lookup-named 'lm) "")
       () (list 'l "LM=" '(s lm)))
   '(just-one-space)
   (p "<off [y/n]>: " off 'noinsert) '(just-one-space)
   (if (and (tempo-lookup-named 'off)
	    (string-equal (tempo-lookup-named 'off) "y"))
       (list 'l "OFF"))
   '(just-one-space)
   (p "<initial cond (voltage)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "")
       () (list 'l "IC=" '(s ic)))
   '(just-one-space)
   (p "<m>: " m 'noinsert)
   (if (string-equal (tempo-lookup-named 'm) "")
       () (list 'l "M=" '(s m)))
   '(just-one-space)
   (p "<diff temp>: " dtemp 'noinsert)
   (if (string-equal (tempo-lookup-named 'dtemp) "")
       () (list 'l "DTEMP=" '(s dtemp)))
   '(just-one-space)
   )
 ;;"hspice diode"
 "DH"
 "tempo template for hspice diode"
 'spice-tempo-tags)


(tempo-define-template
 "spice-layla-diode"
 '("D"
   (p "[name]: ") '(just-one-space)
   (p "[positive node]: ") '(just-one-space)
   (p "[negative node]: ") '(just-one-space)
                                        ;   (p "<mname>: ") '(just-one-space)
   (p "[area]: ") '(just-one-space)
   (p "<units_ver>: " units_ver 'noinsert)
   (if (string-equal (tempo-lookup-named 'units_ver) "")
       () (list 'l "units_ver=" '(s units_ver)))
   '(just-one-space)
   (p "<units_hor>: " units_hor 'noinsert)
   (if (string-equal (tempo-lookup-named 'units_hor) "")
       () (list 'l "units_hor=" '(s units_hor)))
   '(just-one-space)
   "\n+ type=\""
   (p "[type (no quotes)]: ") "\""
   '(just-one-space)
   (p "<symmetry (no quotes)>: " symmetry 'noinsert)
   (if (string-equal (tempo-lookup-named 'symmetry) "")
       () (list 'l "symmetry=\"" '(s symmetry) "\""))
   '(just-one-space)
   (p "<matching (no quotes)>: " matching 'noinsert)
   (if (string-equal (tempo-lookup-named 'matching) "")
       () (list 'l "matching=\"" '(s matching) "\""))
   '(just-one-space)
   (p "<couple>: " couple 'noinsert)
   (if (string-equal (tempo-lookup-named 'couple) "")
       () (list 'l "couple=" '(s couple)))
   '(just-one-space)
   (p "<array (no quotes)>: " array 'noinsert)
   (if (string-equal (tempo-lookup-named 'array) "")
       () (list 'l "array=\"" '(s array) "\""))
   '(just-one-space)
   (p "<orientations (no quotes)>: " orientations 'noinsert)
   (if (string-equal (tempo-lookup-named 'orientations) "")
       () (list 'l "orientations=\"" '(s orientations) "\""))
   '(just-one-space)
   (p "<unit_value>: " unit_value 'noinsert)
   (if (string-equal (tempo-lookup-named 'unit_value) "")
       () (list 'l "unit_value=" '(s unit_value)))
   '(just-one-space)
   (p "<current>: " current 'noinsert)
   (if (string-equal (tempo-lookup-named 'current) "")
       () (list 'l "current=" '(s current)))
   '(just-one-space)
   (p "<power>: " power 'noinsert)
   (if (string-equal (tempo-lookup-named 'power) "")
       () (list 'l "power=" '(s power)))
   '(just-one-space)
   (p "<min_ar>: " min_ar 'noinsert)
   (if (string-equal (tempo-lookup-named 'min_ar) "")
       () (list 'l "min_ar=" '(s min_ar)))
   '(just-one-space)
   (p "<max_ar>: " max_ar 'noinsert)
   (if (string-equal (tempo-lookup-named 'max_ar) "")
       () (list 'l "max_ar=" '(s max_ar)))
   '(just-one-space)
   (p "<routing_space>: " routing_space 'noinsert)
   (if (string-equal (tempo-lookup-named 'routing_space) "")
       () (list 'l "routing_space=" '(s routing_space)))
   '(just-one-space)
   )
 ;;"layla diode"
 "DL"
 "tempo template for layla diode"
 'spice-tempo-tags)


;; bipolars
(tempo-define-template
 "spice-spice2g6-bipolar"
 '("Q"
   (p "[name]: ") '(just-one-space)
   (p "[collector node]: ") '(just-one-space)
   (p "[base node]: ") '(just-one-space)
   (p "[emitter node]: ") '(just-one-space)
   (p "<substrate node>: ") '(just-one-space)
   (p "[mname]: ") '(just-one-space)
   (p "<area>: ") '(just-one-space)
   (p "<off [y/n]>: " off 'noinsert) '(just-one-space)
   (if (and (tempo-lookup-named 'off)
	    (string-equal (tempo-lookup-named 'off) "y"))
       (list 'l "OFF"))
   '(just-one-space)
   (p "<initial cond (vbe, vce)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "")
       () (list 'l "IC=" '(s ic)))
   '(just-one-space)
   (p "<temp>: " temp 'noinsert)
   (if (string-equal (tempo-lookup-named 'temp) "")
       () (list 'l "TEMP=" '(s temp)))
   '(just-one-space)
   )
 ;;"spice2g6 bipolar"
 "Q"
 "tempo template for spice2g6 bipolar"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-bipolar"
 '("Q"
   (p "[name]: ") '(just-one-space)
   (p "[collector node]: ") '(just-one-space)
   (p "[base node]: ") '(just-one-space)
   (p "[emitter node]: ") '(just-one-space)
   (p "<substrate node>: ") '(just-one-space)
   (p "[mname]: ") '(just-one-space)
   (p "<area>: " area 'noinsert) '(just-one-space)
   (if (string-equal (tempo-lookup-named 'area) "")
       () (list 'l "AREA=" '(s area)))
   '(just-one-space)
   (p "<rel base area>: " areab 'noinsert)
   (if (string-equal (tempo-lookup-named 'areab) "")
       () (list 'l "AREAB=" '(s areab)))
   '(just-one-space)
   (p "<rel collector area>: " areac 'noinsert)
   (if (string-equal (tempo-lookup-named 'areac) "")
       () (list 'l "AREAC=" '(s areac)))
   '(just-one-space)
   (p "<temp>: " temp 'noinsert)
   (if (string-equal (tempo-lookup-named 'temp) "")
       () (list 'l "T=" '(s temp)))
   '(just-one-space)
   (p "<m>: " m 'noinsert)
   (if (string-equal (tempo-lookup-named 'm) "")
       () (list 'l "M=" '(s m)))
   '(just-one-space)
   (p "<off [y/n]>: " off 'noinsert) '(just-one-space)
   (if (and (tempo-lookup-named 'off)
	    (string-equal (tempo-lookup-named 'off) "y"))
       (list 'l "OFF"))
   '(just-one-space)
   (p "<nonoise [y/n]>: " nonoise 'noinsert)
   (if (and (tempo-lookup-named 'nonoise)
	    (string-equal (tempo-lookup-named 'nonoise) "y"))
       (list 'l "NONOISE"))
   '(just-one-space)
   )
 ;;"eldo bipolar"
 "QE"
 "tempo template for eldo bipolar"
 'spice-tempo-tags)


(tempo-define-template
 "spice-hspice-bipolar"
 '("Q"
   (p "[name]: ") '(just-one-space)
   (p "[collector node]: ") '(just-one-space)
   (p "[base node]: ") '(just-one-space)
   (p "[emitter node]: ") '(just-one-space)
   (p "<substrate node>: ") '(just-one-space)
   (p "[mname]: ") '(just-one-space)
   (p "<area>: " area 'noinsert) '(just-one-space)
   (if (string-equal (tempo-lookup-named 'area) "")
       () (list 'l "AREA=" '(s area)))
   '(just-one-space)
   (p "<rel base area>: " areab 'noinsert)
   (if (string-equal (tempo-lookup-named 'areab) "")
       () (list 'l "AREAB=" '(s areab)))
   '(just-one-space)
   (p "<rel collector area>: " areac 'noinsert)
   (if (string-equal (tempo-lookup-named 'areac) "")
       () (list 'l "AREAC=" '(s areac)))
   '(just-one-space)
   (p "<off [y/n]>: " off 'noinsert) '(just-one-space)
   (if (and (tempo-lookup-named 'off)
	    (string-equal (tempo-lookup-named 'off) "y"))
       (list 'l "OFF"))
   '(just-one-space)
   (p "<init vbe>: " vbe 'noinsert)
   (if (string-equal (tempo-lookup-named 'vbe) "")
       () (list 'l "VBE=" '(s vbe)))
   '(just-one-space)
   (p "<init vce>: " vce 'noinsert)
   (if (string-equal (tempo-lookup-named 'vce) "")
       () (list 'l "VCE=" '(s vce)))
   '(just-one-space)
   (p "<m>: " m 'noinsert)
   (if (string-equal (tempo-lookup-named 'm) "")
       () (list 'l "M=" '(s m)))
   '(just-one-space)
   (p "<diff temp>: " dtemp 'noinsert)
   (if (string-equal (tempo-lookup-named 'dtemp) "")
       () (list 'l "DTEMP=" '(s dtemp)))
   '(just-one-space)
   )
 ;;"hspice bipolar"
 "QH"
 "tempo template for hspice bipolar"
 'spice-tempo-tags)


;; jfets
(tempo-define-template
 "spice-spice2g6-jfet"
 '("J"
   (p "[name]: ") '(just-one-space)
   (p "[drain node]: ") '(just-one-space)
   (p "[gate node]: ") '(just-one-space)
   (p "[source node]: ") '(just-one-space)
   (p "[mname]: ") '(just-one-space)
   (p "<area>: ") '(just-one-space)
   (p "<off [y/n]>: " off 'noinsert) '(just-one-space)
   (if (and (tempo-lookup-named 'off)
	    (string-equal (tempo-lookup-named 'off) "y"))
       (list 'l "OFF"))
   '(just-one-space)
   (p "<initial cond (vds, vgs)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "")
       () (list 'l "IC=" '(s ic)))
   '(just-one-space)
   (p "<temp>: " temp 'noinsert)
   (if (string-equal (tempo-lookup-named 'temp) "")
       () (list 'l "TEMP=" '(s temp)))
   '(just-one-space)
   )
 ;;"spice2g6 jfet"
 "J"
 "tempo template for spice2g6 jfet"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-jfet"
 '("J"
   (p "[name]: ") '(just-one-space)
   (p "[drain node]: ") '(just-one-space)
   (p "[gate node]: ") '(just-one-space)
   (p "[source node]: ") '(just-one-space)
   (p "[mname]: ") '(just-one-space)
   (p "<area>: " area 'noinsert) '(just-one-space)
   (if (string-equal (tempo-lookup-named 'area) "")
       () (list 'l "AREA=" '(s area)))
   '(just-one-space)
   (p "<length>: " l 'noinsert)
   (if (string-equal (tempo-lookup-named 'l) "")
       () (list 'l "L=" '(s l)))
   '(just-one-space)
   (p "<width>: " w 'noinsert)
   (if (string-equal (tempo-lookup-named 'w) "")
       () (list 'l "W=" '(s w)))
   '(just-one-space)
   (p "<temp>: " temp 'noinsert)
   (if (string-equal (tempo-lookup-named 'temp) "")
       () (list 'l "T=" '(s temp)))
   '(just-one-space)
   (p "<off [y/n]>: " off 'noinsert) '(just-one-space)
   (if (and (tempo-lookup-named 'off)
	    (string-equal (tempo-lookup-named 'off) "y"))
       (list 'l "OFF"))
   '(just-one-space)
   (p "<nonoise [y/n]>: " nonoise 'noinsert)
   (if (and (tempo-lookup-named 'nonoise)
	    (string-equal (tempo-lookup-named 'nonoise) "y"))
       (list 'l "NONOISE"))
   '(just-one-space)
   )
 ;;"eldo jfet"
 "JE"
 "tempo template for eldo jfet"
 'spice-tempo-tags)


(tempo-define-template
 "spice-hspice-jfet"
 '("J"
   (p "[name]: ") '(just-one-space)
   (p "[drain node]: ") '(just-one-space)
   (p "[gate node]: ") '(just-one-space)
   (p "[source node]: ") '(just-one-space)
   (p "[mname]: ") '(just-one-space)
   (p "<area>: " area 'noinsert)
   (if (string-equal (tempo-lookup-named 'area) "")
       (list 'l '(p "<length>: " l 'noinsert) '(p "<width>: " w 'noinsert))
     (list 'l "AREA=" '(s area)))
   '(just-one-space)
   (if (and (tempo-lookup-named 'l)
	    (not (string-equal (tempo-lookup-named 'l) "")))
       (list 'l "L=" '(s l)))
   '(just-one-space)
   (if (and (tempo-lookup-named 'w)
	    (not (string-equal (tempo-lookup-named 'w) "")))
       (list 'l "W=" '(s w)))
   '(just-one-space)
   (p "<off [y/n]>: " off 'noinsert) '(just-one-space)
   (if (and (tempo-lookup-named 'off)
	    (string-equal (tempo-lookup-named 'off) "y"))
       (list 'l "OFF"))
   '(just-one-space)
   (p "<initial cond (vds,vgs)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "")
       () (list 'l "IC=" '(s ic)))
   '(just-one-space)
   (p "<m>: " m 'noinsert)
   (if (string-equal (tempo-lookup-named 'm) "")
       () (list 'l "M=" '(s m)))
   '(just-one-space)
   (p "<diff temp>: " dtemp 'noinsert)
   (if (string-equal (tempo-lookup-named 'dtemp) "")
       () (list 'l "DTEMP=" '(s dtemp)))
   '(just-one-space)
   )
 ;;"hspice jfet"
 "JH"
 "tempo template for hspice jfet"
 'spice-tempo-tags)


;; mosfets
(tempo-define-template
 "spice-spice2g6-mosfet"
 '("M"
   (p "[name]: ") '(just-one-space)
   (p "[drain node]: ") '(just-one-space)
   (p "[gate node]: ") '(just-one-space)
   (p "[source node]: ") '(just-one-space)
   (p "[bulk node]: ") '(just-one-space)
   (p "[mname]: ") '(just-one-space)
   (p "<length>: " l 'noinsert)
   (if (string-equal (tempo-lookup-named 'l) "")
       () (list 'l "L=" '(s l)))
   '(just-one-space)
   (p "<width>: " w 'noinsert)
   (if (string-equal (tempo-lookup-named 'w) "")
       () (list 'l "W=" '(s w)))
   '(just-one-space)
   (p "<area drain>: " ad 'noinsert)
   (if (string-equal (tempo-lookup-named 'ad) "")
       () (list 'l "AD=" '(s ad)))
   '(just-one-space)
   (p "<area source>: " as 'noinsert)
   (if (string-equal (tempo-lookup-named 'as) "")
       () (list 'l "AS=" '(s as)))
   '(just-one-space)
   (p "<perimeter drain>: " pd 'noinsert)
   (if (string-equal (tempo-lookup-named 'pd) "")
       () (list 'l "PD=" '(s pd)))
   '(just-one-space)
   (p "<perimeter source>: " ps 'noinsert)
   (if (string-equal (tempo-lookup-named 'ps) "")
       () (list 'l "PS=" '(s ps)))
   '(just-one-space)
   (p "<number of drain contacts>: " nrd 'noinsert)
   (if (string-equal (tempo-lookup-named 'nrd) "")
       () (list 'l "NRD=" '(s nrd)))
   '(just-one-space)
   (p "<number of source contacts>: " nrs 'noinsert)
   (if (string-equal (tempo-lookup-named 'nrs) "")
       () (list 'l "NRS=" '(s nrs)))
   '(just-one-space)
   (p "<off [y/n]>: " off 'noinsert) '(just-one-space)
   (if (and (tempo-lookup-named 'off)
	    (string-equal (tempo-lookup-named 'off) "y"))
       (list 'l "OFF"))
   '(just-one-space)
   (p "<initial cond (vds, vgs, vgs)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "")
       () (list 'l "IC=" '(s ic)))
   '(just-one-space)
   )
 ;;"spice2g6 mosfet"
 "M"
 "tempo template for spice2g6 mosfet"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-mosfet"
 '("M"
   (p "[name]: ") '(just-one-space)
   (p "[drain node]: ") '(just-one-space)
   (p "[gate node]: ") '(just-one-space)
   (p "[source node]: ") '(just-one-space)
   (p "[bulk node]: ") '(just-one-space)
   (p "<optional nodes>: ") '(just-one-space)
   "MOD="
   (p "[mname]: ") '(just-one-space)
   (p "<length>: " l 'noinsert)
   (if (string-equal (tempo-lookup-named 'l) "")
       () (list 'l "L=" '(s l)))
   '(just-one-space)
   (p "<width>: " w 'noinsert)
   (if (string-equal (tempo-lookup-named 'w) "")
       () (list 'l "W=" '(s w)))
   '(just-one-space)
   (p "<area drain>: " ad 'noinsert)
   (if (string-equal (tempo-lookup-named 'ad) "")
       () (list 'l "AD=" '(s ad)))
   '(just-one-space)
   (p "<area source>: " as 'noinsert)
   (if (string-equal (tempo-lookup-named 'as) "")
       () (list 'l "AS=" '(s as)))
   '(just-one-space)
   (p "<perimeter drain>: " pd 'noinsert)
   (if (string-equal (tempo-lookup-named 'pd) "")
       () (list 'l "PD=" '(s pd)))
   '(just-one-space)
   (p "<perimeter source>: " ps 'noinsert)
   (if (string-equal (tempo-lookup-named 'ps) "")
       () (list 'l "PS=" '(s ps)))
   '(just-one-space)
   (p "<geometry model>: " geo 'noinsert)
   (if (string-equal (tempo-lookup-named 'geo) "")
       () (list 'l "GEO=" '(s geo)))
   '(just-one-space)
   (p "<number of drain contacts>: " nrd 'noinsert)
   (if (string-equal (tempo-lookup-named 'nrd) "")
       () (list 'l "NRD=" '(s nrd)))
   '(just-one-space)
   (p "<number of source contacts>: " nrs 'noinsert)
   (if (string-equal (tempo-lookup-named 'nrs) "")
       () (list 'l "NRS=" '(s nrs)))
   '(just-one-space)
   (p "<m>: " m 'noinsert)
   (if (string-equal (tempo-lookup-named 'm) "")
       () (list 'l "M=" '(s m)))
   '(just-one-space)
   (p "<extra drain contact resistance>: " rdc 'noinsert)
   (if (string-equal (tempo-lookup-named 'rdc) "")
       () (list 'l "RDC=" '(s rdc)))
   '(just-one-space)
   (p "<extra source contact resistance>: " rsc 'noinsert)
   (if (string-equal (tempo-lookup-named 'rsc) "")
       () (list 'l "RSC=" '(s rsc)))
   '(just-one-space)
   (p "<temp>: " temp 'noinsert)
   (if (string-equal (tempo-lookup-named 'temp) "")
       () (list 'l "T=" '(s temp)))
   '(just-one-space)
   (p "<off [y/n]>: " off 'noinsert) '(just-one-space)
   (if (and (tempo-lookup-named 'off)
	    (string-equal (tempo-lookup-named 'off) "y"))
       (list 'l "OFF"))
   '(just-one-space)
   (p "<nonoise [y/n]>: " nonoise 'noinsert)
   (if (and (tempo-lookup-named 'nonoise)
	    (string-equal (tempo-lookup-named 'nonoise) "y"))
       (list 'l "NONOISE"))
   '(just-one-space)
   )
 ;;"eldo mosfet"
 "ME"
 "tempo template for eldo mosfet"
 'spice-tempo-tags)


(tempo-define-template
 "spice-hspice-mosfet"
 '("M"
   (p "[name]: ") '(just-one-space)
   (p "[drain node]: ") '(just-one-space)
   (p "[gate node]: ") '(just-one-space)
   (p "[source node]: ") '(just-one-space)
   (p "[bulk node]: ") '(just-one-space)
   (p "[mname]: ") '(just-one-space)
   (p "<length>: " l 'noinsert)
   (if (string-equal (tempo-lookup-named 'l) "")
       () (list 'l "L=" '(s l)))
   '(just-one-space)
   (p "<width>: " w 'noinsert)
   (if (string-equal (tempo-lookup-named 'w) "")
       () (list 'l "W=" '(s w)))
   '(just-one-space)
   (p "<area drain>: " ad 'noinsert)
   (if (string-equal (tempo-lookup-named 'ad) "")
       () (list 'l "AD=" '(s ad)))
   '(just-one-space)
   (p "<area source>: " as 'noinsert)
   (if (string-equal (tempo-lookup-named 'as) "")
       () (list 'l "AS=" '(s as)))
   '(just-one-space)
   (p "<perimeter drain>: " pd 'noinsert)
   (if (string-equal (tempo-lookup-named 'pd) "")
       () (list 'l "PD=" '(s pd)))
   '(just-one-space)
   (p "<perimeter source>: " ps 'noinsert)
   (if (string-equal (tempo-lookup-named 'ps) "")
       () (list 'l "PS=" '(s ps)))
   '(just-one-space)
   (p "<number of drain contacts>: " nrd 'noinsert)
   (if (string-equal (tempo-lookup-named 'nrd) "")
       () (list 'l "NRD=" '(s nrd)))
   '(just-one-space)
   (p "<number of source contacts>: " nrs 'noinsert)
   (if (string-equal (tempo-lookup-named 'nrs) "")
       () (list 'l "NRS=" '(s nrs)))
   '(just-one-space)
   (p "<extra drain contact resistance>: " rdc 'noinsert)
   (if (string-equal (tempo-lookup-named 'rdc) "")
       () (list 'l "RDC=" '(s rdc)))
   '(just-one-space)
   (p "<extra source contact resistance>: " rsc 'noinsert)
   (if (string-equal (tempo-lookup-named 'rsc) "")
       () (list 'l "RSC=" '(s rsc)))
   '(just-one-space)
   (p "<off [y/n]>: " off 'noinsert) '(just-one-space)
   (if (and (tempo-lookup-named 'off)
	    (string-equal (tempo-lookup-named 'off) "y"))
       (list 'l "OFF"))
   '(just-one-space)
   (p "<init cond (vds,vgs,vbs)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "")
       () (list 'l "IC=" '(s ic)))
   '(just-one-space)
   (p "<diff temp>: " dtemp 'noinsert)
   (if (string-equal (tempo-lookup-named 'dtemp) "")
       () (list 'l "DTEMP=" '(s dtemp)))
   '(just-one-space)
   (p "<geometry model>: " geo 'noinsert)
   (if (string-equal (tempo-lookup-named 'geo) "")
       () (list 'l "GEO=" '(s geo)))
   '(just-one-space)
   (p "<m>: " m 'noinsert)
   (if (string-equal (tempo-lookup-named 'm) "")
       () (list 'l "M=" '(s m)))
   '(just-one-space)
   (p "<delvto>: " delvto 'noinsert)
   (if (string-equal (tempo-lookup-named 'delvto) "")
       () (list 'l "DELVTO=" '(s delvto)))
   '(just-one-space)
   )
 ;;"hspice mosfet"
 "MH"
 "tempo template for hspice mosfet"
 'spice-tempo-tags)


(tempo-define-template
 "spice-layla-mosfet"
 '("M"
   (p "[name]: ") '(just-one-space)
   (p "[drain node]: ") '(just-one-space)
   (p "[gate node]: ") '(just-one-space)
   (p "[source node]: ") '(just-one-space)
   (p "[bulk node]: ") '(just-one-space)
   (p "[mname]: ") '(just-one-space)
   (p "<length>: " l 'noinsert)
   (if (string-equal (tempo-lookup-named 'l) "")
       () (list 'l "L=" '(s l)))
   '(just-one-space)
   (p "<width>: " w 'noinsert)
   (if (string-equal (tempo-lookup-named 'w) "")
       () (list 'l "W=" '(s w)))
   '(just-one-space)
   "\n+ type=\""
   (p "[type (no quotes)]: ") "\""
   '(just-one-space)
   (p "<symmetry (no quotes)>: " symmetry 'noinsert)
   (if (string-equal (tempo-lookup-named 'symmetry) "")
       () (list 'l "symmetry=\"" '(s symmetry) "\""))
   '(just-one-space)
   (p "<matching (no quotes)>: " matching 'noinsert)
   (if (string-equal (tempo-lookup-named 'matching) "")
       () (list 'l "matching=\"" '(s matching) "\""))
   '(just-one-space)
   (p "<couple>: " couple 'noinsert)
   (if (string-equal (tempo-lookup-named 'couple) "")
       () (list 'l "couple=" '(s couple)))
   '(just-one-space)
   (p "<array (no quotes)>: " array 'noinsert)
   (if (string-equal (tempo-lookup-named 'array) "")
       () (list 'l "array=\"" '(s array) "\""))
   '(just-one-space)
   (p "<orientations (no quotes)>: " orientations 'noinsert)
   (if (string-equal (tempo-lookup-named 'orientations) "")
       () (list 'l "orientations=\"" '(s orientations) "\""))
   '(just-one-space)
   (p "<unit_value>: " unit_value 'noinsert)
   (if (string-equal (tempo-lookup-named 'unit_value) "")
       () (list 'l "unit_value=" '(s unit_value)))
   '(just-one-space)
   (p "<current>: " current 'noinsert)
   (if (string-equal (tempo-lookup-named 'current) "")
       () (list 'l "current=" '(s current)))
   '(just-one-space)
   (p "<power>: " power 'noinsert)
   (if (string-equal (tempo-lookup-named 'power) "")
       () (list 'l "power=" '(s power)))
   '(just-one-space)
   (p "<min_ar>: " min_ar 'noinsert)
   (if (string-equal (tempo-lookup-named 'min_ar) "")
       () (list 'l "min_ar=" '(s min_ar)))
   '(just-one-space)
   (p "<max_ar>: " max_ar 'noinsert)
   (if (string-equal (tempo-lookup-named 'max_ar) "")
       () (list 'l "max_ar=" '(s max_ar)))
   '(just-one-space)
   (p "<routing_space>: " routing_space 'noinsert)
   (if (string-equal (tempo-lookup-named 'routing_space) "")
       () (list 'l "routing_space=" '(s routing_space)))
   '(just-one-space)
   )
 ;;"layla mosfet"
 "ML"
 "tempo template for layla mosfet"
 'spice-tempo-tags)


;; mesfets
(tempo-define-template
 "spice-spice2g6-mesfet"
 '("Z"
   (p "[name]: ") '(just-one-space)
   (p "[drain node]: ") '(just-one-space)
   (p "[gate node]: ") '(just-one-space)
   (p "[source node]: ") '(just-one-space)
   (p "[mname]: ") '(just-one-space)
   (p "<area>: ") '(just-one-space)
   (p "<off [y/n]>: " off 'noinsert) '(just-one-space)
   (if (and (tempo-lookup-named 'off)
	    (string-equal (tempo-lookup-named 'off) "y"))
       (list 'l "OFF"))
   '(just-one-space)
   (p "<initial cond (vds, vgs)>: " ic 'noinsert)
   (if (string-equal (tempo-lookup-named 'ic) "")
       () (list 'l "IC=" '(s ic)))
   '(just-one-space)
   )
 ;;"spice2g6 mesfet"
 "Z"
 "tempo template for spice2g6 mesfet"
 'spice-tempo-tags)


;; subcircuit defs
(tempo-define-template
 "spice-spice2g6-subckt"
 '(".subckt "
   (p "[subckt name]: " lname) 'r 'n 'n
   ".ends " (s lname)  '>)
 "SUB"
 "template for inserting a subckt definition"
 'spice-tempo-tags)


;; Controlled sources

;; Voltage sources

(tempo-define-template
 "spice-spice2g6-vcvs"
 '("E"
   (p "[name]: ") '(just-one-space)
   (p "[positive node]: ") '(just-one-space)
   (p "[negative node]: ") '(just-one-space)
   (p "[positive controling node]: ") '(just-one-space)
   (p "[negative controling node]: ") '(just-one-space)
   (p "[gain]: ") '(just-one-space)
   )
 "VCVS"
 "template for inserting a voltage controled voltage source"
 'spice-tempo-tags)

(tempo-define-template
 "spice-spice2g6-ccvs"
 '("H"
   (p "[name]: ") '(just-one-space)
   (p "[positive node]: ") '(just-one-space)
   (p "[negative node]: ") '(just-one-space)
   (p "[voltage source]: ") '(just-one-space)
   (p "[gain]: ") '(just-one-space)
   )
 "CCVS"
 "template for inserting a current controled voltage source"
 'spice-tempo-tags)


;; Current sources

(tempo-define-template
 "spice-spice2g6-vccs"
 '("G"
   (p "[name]: ") '(just-one-space)
   (p "[positive node]: ") '(just-one-space)
   (p "[negative node]: ") '(just-one-space)
   (p "[positive controling node]: ") '(just-one-space)
   (p "[negative controling node]: ") '(just-one-space)
   (p "[transadmitance]: ") '(just-one-space)
   )
 "VCCS"
 "template for inserting a voltage controled current source"
 'spice-tempo-tags)

(tempo-define-template
 "spice-spice2g6-cccs"
 '("F"
   (p "[name]: ") '(just-one-space)
   (p "[positive node]: ") '(just-one-space)
   (p "[negative node]: ") '(just-one-space)
   (p "[voltage source]: ") '(just-one-space)
   (p "[gain]: ") '(just-one-space)
   )
 "CCCS"
 "template for inserting a current controled current source"
 'spice-tempo-tags)


;; Waveforms

(tempo-define-template
 "spice-pulse"
 '("pulse("
   (p "[start value]: ") " "
   (p "[pulsed value]: ") " "
   (p "[delay]: ") " "
   (p "[rise time]: ") " "
   (p "[fall time]: ") " "
   (p "[pulse duration]: ") " "
   (p "[period]: ")
   ")"'n)
 "PU"
 "template for inserting an Pulse waveform"
 'spice-tempo-tags)

(tempo-define-template
 "spice-sine"
 '("sin("
   (p "[Offset]: ") " "
   (p "[Amplitude]: ") " "
   (p "[Frequency]: ") " "
   (p "[Delay]: ") " "
   (p "[Damping factor]: ")
   (p "[Phase delay]: ") '(just-one-space)
   ")"'n)
 "sin"
 "template for inserting a Sine function"
 'spice-tempo-tags)

(tempo-define-template
 "spice-exp"
 '("exp("
   (p "[start value]: ") " "
   (p "[target value]: ") " "
   (p "[rise delay]: ") " "
   (p "[tau1]: ") " "
   (p "[fall delay]: ") " "
   (p "[tau2]: ")
   ")"'n)
 "exp"
 "template for inserting an EXP waveform"
 'spice-tempo-tags)

;;(tempo-define-template
;; "spice-pwl"
;; '("pwl("
;;     (p "[start time]: ") " "
;;     (p "[start value]: ") " "
;;     ")"'n)
;; "pwl"
;; "template for inserting an PWL waveform")

(load "skeleton")
(define-skeleton spice-pwl
  "Skeleton for Piece Wise Linear waveform"
  "Time/value doublet: "
  "pwl(" str
  ( "Next doublet: (%s) "
    " "str )
  resume:
  ")"
  'spice-tempo-tags)


(tempo-define-template
 "hspice-sffm"
 '("sffm("
   (p "[offset value]: ") " "
   (p "[amplitude value]: ") " "
   (p "[carrier frequency]: ") " "
   (p "[modulation index]: ") " "
   (p "[signal frequency]: ")
   ")"'n)
 "sffm"
 "template for inserting an HSPICE SFFM waveform"
 'spice-tempo-tags)

(tempo-define-template
 "hspice-am"
 '("am("
   (p "[signal amplitude]: ") " "
   (p "[offset constant]: ") " "
   (p "[modulation frequency]: ") " "
   (p "[carrier frequency]: ") " "
   (p "[delay time]: ")
   ")"'n)
 "am"
 "template for inserting an HSPICE AM waveform"
 'spice-tempo-tags)

(tempo-define-template
 "spice-ac"
 '("ac("
   (p "[magnitude]: ") " "
   (p "[phase]: ")
   ")"'n)
 "ac"
 "template for inserting an AC waveform"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-pattern"
 '("pattern "
   (p "[Vhi]: ") " "
   (p "[Vlo]: ") " "
   (p "[delay]: ") " "
   (p "[rise time]: ") " "
   (p "[fall time]: ") " "
   (p "[Bit duration]: ") " "
   (p "[Bits]: ")
   'n)
 "eldo-pattern"
 "template for inserting an ELDO Pattern function"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-noise"
 '("noise("
   (p "[White noise level]: ") " "
   (p "[Flicker noise level]: ") " "
   (p "[Alpha]: ") " "
   (p "[Cut-off freq]: ") " "
   (p "[Filter order]: ")
   ")"'n)
 "noise"
 "template for inserting an ELDO NOISE waveform"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldorf-fpulse"
 '("fpulse("
   (p "[initial value]: ") " "
   (p "[pulse value]: ") " "
   (p "[delay time]: ") " "
   (p "[rise time]: ") " "
   (p "[fall time]: ") " "
   (p "[pulse duration]: ") " "
   (p "[FUND1|FUND2|FUND3 (of .sst)]: ")
   ")"'n)
 "eldorf-fpulse"
 "template for inserting an Eldo-RF fpulse waveform"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldorf-four"
 '("four" '(just-one-space)
   (p "[FUND1]: ")
   (p "<FUND2>: " fund2 'noinsert)
   (if (string-equal (tempo-lookup-named 'fund2) "")
       ()
     (list 'l " " '(s fund2) '(p "<FUND3>: " fund3 'noinsert)))
   (if (string-equal (tempo-lookup-named 'fund3) "")
       ()
     (list 'l " " '(s fund3)))
   '(just-one-space)
   (p "[MA|RI|DB|PMA|PDB|PDBM]: ") '(just-one-space)
   "("
   (p "[int_val1]: ")
   (p "<int_val2>: " int_val2 'noinsert)
   (if (string-equal (tempo-lookup-named 'int_val2) "")
       ()
     (list 'l "," '(s int_val2) '(p "<int_val3>: " int_val3 'noinsert)))
   (if (string-equal (tempo-lookup-named 'int_val3) "")
       ()
     (list 'l "," '(s int_val3)))
   ")" '(just-one-space)
   (p "[real_val1]: ") '(just-one-space)
   (p "[real_val2]: ") '(just-one-space)
   'n)
 "eldorf-four"
 "template for inserting an Eldo-RF Fpulse waveform"
 'spice-tempo-tags)

;; Eldo Extracts

(tempo-define-template
 "spice-eldo-phmag"
 '(".EXTRACT AC label=\"Phase margin\" xycond(vp("
   (p "[Node]: " lname)
   "),vdb(" (s lname) ")<0.0)+180 "
   'n)
 "phmag"
 "template for extracting the phase margin"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-gmag"
 '(".EXTRACT AC label=\"Gain margin\" -xycond(vdb("
   (p "[Node]: " lname)
   "),vp(" (s lname) ")<-180) "
   'n)
 "gmag"
 "template for extracting the gain margin"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-fc"
 '(".EXTRACT AC label=\"Cut freq\" xdown(vdb("
   (p "[Node]: " lname)
   "),yval(vdb(" (s lname) "),1)-3) "
   'n)
 "fc"
 "template for extracting the cut frequency"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-ugfc"
 '(".EXTRACT AC label=\"Unity gain freq\" xdown(vdb("
   (p "[Node]: " lname)
   "),0) "
   'n)
 "ugfc"
 "template for extracting the unity gain frequency"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-period"
 '(".EXTRACT TRAN xdown(v("
   (p "[Node]: " lname)
   "),"
   (p "[threshold]: " vth)
   ","
   (p "[estimation time]: " t)
   ",end)"
   "-xdown(v(" (s lname) "),"(s vth) ","(s t) ",start) !period"
   'n)
 "period"
 "template for extracting the period of a signal"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-period-macro"
 '(".DEFMAC period(a,th,time)=xdown(a,th,time,end)"
   "-xdown(a,th,time,start)"
   'n)
 "period"
 "macro for extracting the period of signal a"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-duty-macro"
 '(".DEFMAC duty_cycle(a,th,time)=(xdown(a,th,time,end)"
   "-xup(a,th,time,end))/(xdown(a,th,time,end)-xdown(a,th,time,start))*100"
   'n)
 "duty"
 "macro for extracting the duty cycle of signal a"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-settling-macro"
 '(".DEFMAC settling(xaxis,a,ratio,Tstart,Tfinal)=xycond(xaxis,(a>(yval(a,Tfinal)*(1+ratio)))"
   " || (a<(yval(a,Tfinal)*(1-ratio))),Tfinal,Tstart) - Tstart"
   'n)
 "settling"
 "macro for extracting the settling cycle of signal A, within 켼atio of value of A at time Tfinal"
 'spice-tempo-tags)


;; Eldo Macromodels

(tempo-define-template
 "spice-eldo-comp"
 '("COMP"
   (p "[Instance name]: ") " "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vhi) "")
       (list 'l "VHI=5.0")	;; default value
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vlo) "")
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Input offset>: " voff 'noinsert)
   (if (string-equal (tempo-lookup-named 'voff) "")
       (list 'l "VOFF=0.0")	;; default value
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Hysteresis voltage>: " vdef 'noinsert)
   (if (string-equal (tempo-lookup-named 'vdef) "")
       (list 'l "VDEF=0.0")	;; default value
     (list 'l "VDEF=" '(s vdef)))
   '(just-one-space)
   (p "<Commutation time>: " tcom 'noinsert)
   (if (string-equal (tempo-lookup-named 'tcom) "")
       (list 'l "TCOM=1ns")	;; default value
     (list 'l "TCOM=" '(s tcom)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpd) "")
       (list 'l "TPD=0.0")	;; default value
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   'n)
 "comp"
 "template for inserting an ELDO Single output comparator"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-compd"
 '("COMPD"
   (p "[Instance name]: ") " "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vhi) "")
       (list 'l "VHI=5.0")	;; default value
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vlo) "")
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Input offset>: " voff 'noinsert)
   (if (string-equal (tempo-lookup-named 'voff) "")
       (list 'l "VOFF=0.0")	;; default value
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Hysteresis voltage>: " vdef 'noinsert)
   (if (string-equal (tempo-lookup-named 'vdef) "")
       (list 'l "VDEF=0.0")	;; default value
     (list 'l "VDEF=" '(s vdef)))
   '(just-one-space)
   (p "<Commutation time>: " tcom 'noinsert)
   (if (string-equal (tempo-lookup-named 'tcom) "")
       (list 'l "TCOM=1ns")	;; default value
     (list 'l "TCOM=" '(s tcom)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpd) "")
       (list 'l "TPD=0.0")	;; default value
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   'n)
 "compd"
 "template for inserting an ELDO Differential output comparator"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-linear-opa0"
 '("Y"
   (p "[Instance name]: ") " OPAMP0 "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Output]: ") " "
   (p "[Ground]: ") " param: "
   (p "<Gain>: " gain 'noinsert)
   (if (string-equal (tempo-lookup-named 'gain) "")
       (list 'l "GAIN=1e5")	;; default value
     (list 'l "GAIN=" '(s gain)))
   '(just-one-space)
   (p "<Input impedance>: " rin 'noinsert)
   (if (string-equal (tempo-lookup-named 'rin) "")
       (list 'l "RIN=1e7") 	;; default value
     (list 'l "RIN=" '(s rin)))
   '(just-one-space)
   'n)
 "opa0"
 "template for inserting an ELDO single output linear opamp"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-linear-opa0d"
 '("Y"
   (p "[Instance name]: ") " OPAMP0D "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " "
   (p "[Ground]: ") " param: "
   (p "<Gain>: " gain 'noinsert)
   (if (string-equal (tempo-lookup-named 'gain) "")
       (list 'l "GAIN=1e5")	;; default value
     (list 'l "GAIN=" '(s gain)))
   '(just-one-space)
   (p "<Input impedance>: " rin 'noinsert)
   (if (string-equal (tempo-lookup-named 'rin) "")
       (list 'l "RIN=1e7")	;; default value
     (list 'l "RIN=" '(s rin)))
   '(just-one-space)
   'n)
 "opa0d"
 "template for inserting an ELDO differential output linear opamp"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-linear-opa1"
 '("Y"
   (p "[Instance name]: ") " OPAMP1 "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Output]: ") " "
   (p "[Ground]: ") " param: "
   (p "<Gain>: " gain 'noinsert)
   (if (string-equal (tempo-lookup-named 'gain) "")
       (list 'l "GAIN=1e5")	;; default value
     (list 'l "GAIN=" '(s gain)))
   '(just-one-space)
   (p "<Input offset>: " voff 'noinsert)
   (if (string-equal (tempo-lookup-named 'voff) "")
       (list 'l "VOFF=0.0")	;; default value
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Dominant pole>: " p1 'noinsert)
   (if (string-equal (tempo-lookup-named 'p1) "")
       (list 'l "P1=100")	;; default value
     (list 'l "P1=" '(s p1)))
   '(just-one-space)
   (p "<Input impedance>: " rin 'noinsert)
   (if (string-equal (tempo-lookup-named 'rin) "")
       (list 'l "RIN=1e7")	;; default value
     (list 'l "RIN=" '(s rin)))
   '(just-one-space)
   'n)
 "opa1"
 "template for inserting an ELDO single output 1-pole linear opamp"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-linear-opa1d"
 '("Y"
   (p "[Instance name]: ") " OPAMP1D "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " "
   (p "[Ground]: ") " param: "
   (p "<Gain>: " gain 'noinsert)
   (if (string-equal (tempo-lookup-named 'gain) "")
       (list 'l "GAIN=1e5")	;; default value
     (list 'l "GAIN=" '(s gain)))
   '(just-one-space)
   (p "<Input offset>: " voff 'noinsert)
   (if (string-equal (tempo-lookup-named 'voff) "")
       (list 'l "VOFF=0.0")	;; default value
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Dominant pole>: " p1 'noinsert)
   (if (string-equal (tempo-lookup-named 'p1) "")
       (list 'l "P1=100")	;; default value
     (list 'l "P1=" '(s p1)))
   '(just-one-space)
   (p "<Input impedance>: " rin 'noinsert)
   (if (string-equal (tempo-lookup-named 'rin) "")
       (list 'l "RIN=1e7")	;; default value
     (list 'l "RIN=" '(s rin)))
   '(just-one-space)
   (p "<Common mode rejection ratio>: " cmrr 'noinsert)
   (if (string-equal (tempo-lookup-named 'cmrr) "")
       (list 'l "CMRR=0.0")	;; default value
     (list 'l "CMRR=" '(s cmrr)))
   '(just-one-space)
   'n)
 "opa1d"
 "template for inserting an ELDO differential output 1-pole linear opamp"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-linear-opa2"
 '("Y"
   (p "[Instance name]: ") " OPAMP2 "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Output]: ") " "
   (p "[Ground]: ") " param: "
   (p "<Gain>: " gain 'noinsert)
   (if (string-equal (tempo-lookup-named 'gain) "")
       (list 'l "GAIN=1e5")	;; default value
     (list 'l "GAIN=" '(s gain)))
   '(just-one-space)
   (p "<Input offset>: " voff 'noinsert)
   (if (string-equal (tempo-lookup-named 'voff) "")
       (list 'l "VOFF=0.0")	;; default value
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Dominant pole>: " p1 'noinsert)
   (if (string-equal (tempo-lookup-named 'p1) "")
       (list 'l "P1=100")	;; default value
     (list 'l "P1=" '(s p1)))
   '(just-one-space)
   (p "<Non-dominant pole>: " p2 'noinsert)
   (if (string-equal (tempo-lookup-named 'p2) "")
       (list 'l "P2=1e6")	;; default value
     (list 'l "P2=" '(s p2)))
   '(just-one-space)
   (p "<Input impedance>: " rin 'noinsert)
   (if (string-equal (tempo-lookup-named 'rin) "")
       (list 'l "RIN=1e7")	;; default value
     (list 'l "RIN=" '(s rin)))
   '(just-one-space)
   'n)
 "opa2"
 "template for inserting an ELDO single output 2-pole linear opamp"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-linear-opa2d"
 '("Y"
   (p "[Instance name]: ") " OPAMP2D "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " "
   (p "[Ground]: ") " param: "
   (p "<Gain>: " gain 'noinsert)
   (if (string-equal (tempo-lookup-named 'gain) "")
       (list 'l "GAIN=1e5")	;; default value
     (list 'l "GAIN=" '(s gain)))
   '(just-one-space)
   (p "<Input offset>: " voff 'noinsert)
   (if (string-equal (tempo-lookup-named 'voff) "")
       (list 'l "VOFF=0.0")	;; default value
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Dominant pole>: " p1 'noinsert)
   (if (string-equal (tempo-lookup-named 'p1) "")
       (list 'l "P1=100")	;; default value
     (list 'l "P1=" '(s p1)))
   '(just-one-space)
   (p "<Non-dominant pole>: " p2 'noinsert)
   (if (string-equal (tempo-lookup-named 'p2) "")
       (list 'l "P2=1e6")	;; default value
     (list 'l "P2=" '(s p2)))
   '(just-one-space)
   (p "<Input impedance>: " rin 'noinsert)
   (if (string-equal (tempo-lookup-named 'rin) "")
       (list 'l "RIN=1e7")	;; default value
     (list 'l "RIN=" '(s rin)))
   '(just-one-space)
   (p "<Common mode rejection ratio>: " cmrr 'noinsert)
   (if (string-equal (tempo-lookup-named 'cmrr) "")
       (list 'l "CMRR=0.0")	;; default value
     (list 'l "CMRR=" '(s cmrr)))
   '(just-one-space)
   'n)
 "opa2d"
 "template for inserting an ELDO differential output 2-pole linear opamp"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-delay"
 '("DEL"
   (p "[Instance name]: ") " "
   (p "[Input]: ") " "
   (p "[Output]: ") " "
   (p "[Delay value]: ") " "
   'n)
 "del"
 "template for inserting an ELDO delay"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-satr"
 '("Y"
   (p "[Instance name]: ") " SATR "
   (p "[Input]: ") " "
   (p "[Output]: ") " param: "
   (p "<Value of resistance>: " r 'noinsert)
   (if (string-equal (tempo-lookup-named 'r) "")
       (list 'l "R=1")	;; default value
     (list 'l "R=" '(s r)))
   '(just-one-space)
   (p "<Max current>: " imax 'noinsert)
   (if (string-equal (tempo-lookup-named 'imax) "")
       (list 'l "IMAX=1")	;; default value
     (list 'l "IMAX=" '(s imax)))
   '(just-one-space)
   (p "<Slew rate(V/탎)>: " sr 'noinsert)
   (if (string-equal (tempo-lookup-named 'sr) "")
       (list 'l "SR=0")	;; default value
     (list 'l "SR=" '(s sr)))
   '(just-one-space)
   (p "<Dominant pole>: " p1 'noinsert)
   (if (string-equal (tempo-lookup-named 'p1) "")
       (list 'l "P1=1e6")	;; default value
     (list 'l "P1=" '(s p1)))
   '(just-one-space)
   (p "<resistance of low-pass filter>: " r1 'noinsert)
   (if (string-equal (tempo-lookup-named 'r1) "")
       (list 'l "R1=30")	;; default value
     (list 'l "R1=" '(s r1)))
   'n)
 "satr"
 "template for inserting an ELDO saturating resistor"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-satv"
 '("Y"
   (p "[Instance name]: ") " SATV "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " param: "
   (p "<VMax>: " vmax 'noinsert)
   (if (string-equal (tempo-lookup-named 'vmax) "")
       (list 'l "VMAX=5.0")	;; default value
     (list 'l "VMAX=" '(s vmax)))
   '(just-one-space)
   (p "<VMin>: " vmin 'noinsert)
   (if (string-equal (tempo-lookup-named 'vmin) "")
       (list 'l "VMIN=-5.0")	;; default value
     (list 'l "VMIN=" '(s vmin)))
   '(just-one-space)
   (p "<Positive saturation voltage>: " vsatp 'noinsert)
   (if (string-equal (tempo-lookup-named 'vsatp) "")
       (list 'l "VSATP=4.75")	;; default value
     (list 'l "VSATP=" '(s vsatp)))
   '(just-one-space)
   (p "<Negative saturation voltage>: " vsatn 'noinsert)
   (if (string-equal (tempo-lookup-named 'vsatn) "")
       (list 'l "VSATN=-4.75")	;; default value
     (list 'l "VSATN=" '(s vsatn)))
   '(just-one-space)
   (p "<Slope at VSATP>: " pslope 'noinsert)
   (if (string-equal (tempo-lookup-named 'pslope) "")
       (list 'l "PSLOPE=0.25")	;; default value
     (list 'l "PSLOPE=" '(s pslope)))
   '(just-one-space)
   (p "<Slope at VSATN>: " nslope 'noinsert)
   (if (string-equal (tempo-lookup-named 'nslope) "")
       (list 'l "NSLOPE=0.25")	;; default value
     (list 'l "NSLOPE=" '(s nslope)))
   'n)
 "satv"
 "template for inserting an ELDO voltage limitor"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-vswitch"
 '("Y"
   (p "[Instance name]: ") " VSWITCH "
   (p "[Input]: ") " "
   (p "[Output]: ") " "
   (p "[Positive controlling node]: ") " "
   (p "[Negative controlling node]: ") " param: "
   (p "<Level (1/2)>: " level 'noinsert)
   (if (string-equal (tempo-lookup-named 'level) "")
       (list 'l "LEVEL=1")	;; default value
     (list 'l "LEVEL=2" ))
   '(just-one-space)
   (p "<Voltage for 'ON' state>: " von 'noinsert)
   (if (string-equal (tempo-lookup-named 'von) "")
       (list 'l "VON=0.95")	;; default value
     (list 'l "VON=" '(s von)))
   '(just-one-space)
   (p "<Voltage for 'OFF' state>: " voff 'noinsert)
   (if (string-equal (tempo-lookup-named 'voff) "")
       (list 'l "VOFF=0.05")	;; default value
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<RON resistance>: " ron 'noinsert)
   (if (string-equal (tempo-lookup-named 'ron) "")
       (list 'l "RON=1e-2")	;; default value
     (list 'l "RON=" '(s ron)))
   '(just-one-space)
   (p "<ROFF resistance>: " roff 'noinsert)
   (if (string-equal (tempo-lookup-named 'roff) "")
       (list 'l "ROFF=1e10")	;; default value
     (list 'l "ROFF=" '(s roff)))
   'n)
 "vswitch"
 "template for inserting an ELDO voltage controled switch"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-cswitch"
 '("Y"
   (p "[Instance name]: ") " CSWITCH "
   (p "[Input]: ") " "
   (p "[Output]: ") " IC: "
   (p "[Controlling current]: ") " param: "
   (p "<Level (1/2)>: " level 'noinsert)
   (if (string-equal (tempo-lookup-named 'level) "")
       (list 'l "LEVEL=1")	;; default value
     (list 'l "LEVEL=2" ))
   '(just-one-space)
   (p "<Current for 'ON' state>: " ion 'noinsert)
   (if (string-equal (tempo-lookup-named 'ion) "")
       (list 'l "ION=0.95")	;; default value
     (list 'l "ION=" '(s ion)))
   '(just-one-space)
   (p "<Current for 'OFF' state>: " ioff 'noinsert)
   (if (string-equal (tempo-lookup-named 'ioff) "")
       (list 'l "IOFF=0.05")	;; default value
     (list 'l "IOFF=" '(s ioff)))
   '(just-one-space)
   (p "<RON resistance>: " ron 'noinsert)
   (if (string-equal (tempo-lookup-named 'ron) "")
       (list 'l "RON=1e-2")	;; default value
     (list 'l "RON=" '(s ron)))
   '(just-one-space)
   (p "<ROFF resistance>: " roff 'noinsert)
   (if (string-equal (tempo-lookup-named 'roff) "")
       (list 'l "ROFF=1e10")	;; default value
     (list 'l "ROFF=" '(s roff)))
   'n)
 "cswitch"
 "template for inserting an ELDO current controled switch"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-tri2sin"
 '("Y"
   (p "[Instance name]: ") " TRI2SIN "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive output]: ") " "
   (p "[Negative output]: ") " param: "
   (p "<Level (1/2)>: " level 'noinsert)
   (if (string-equal (tempo-lookup-named 'level) "")
       (list 'l "LEVEL=1")	;; default value
     (list 'l "LEVEL=" '(s level)))
   '(just-one-space)
   (p "<Gain>: " gain 'noinsert)
   (if (string-equal (tempo-lookup-named 'gain) "")
       (list 'l "GAIN=1e5")	;; default value
     (list 'l "GAIN=" '(s gain)))
   '(just-one-space)
   (p "<Input offset>: " voff 'noinsert)
   (if (string-equal (tempo-lookup-named 'voff) "")
       (list 'l "VOFF=0.0")	;; default value
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Upper input voltage limit >: " vu 'noinsert)
   (if (string-equal (tempo-lookup-named 'vu) "")
       (list 'l "VU=1")		;; default value
     (list 'l "VU=" '(s vu)))
   '(just-one-space)
   (p "<Lower input voltage limit >: " vl 'noinsert)
   (if (string-equal (tempo-lookup-named 'vl) "")
       (list 'l "VL=1")		;; default value
     (list 'l "VL=" '(s vl)))
   'n)
 "tri2sin"
 "template for inserting an ELDO triangular to sine wave converter"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-stairgen"
 '("Y"
   (p "[Instance name]: ") " STAIRGEN "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " param: "
   (p "<Start voltage>: " vstart 'noinsert)
   (if (string-equal (tempo-lookup-named 'vstart) "")
       (list 'l "VSTART=0.0")	;; default value
     (list 'l "VSTART=" '(s vstart)))
   '(just-one-space)
   (p "<Step voltage>: " vdelta 'noinsert)
   (if (string-equal (tempo-lookup-named 'vdelta) "")
       (list 'l "VDELTA=0.1")	;; default value
     (list 'l "VDELTA=" '(s vdelta)))
   '(just-one-space)
   (p "<Number of steps>: " nstep 'noinsert)
   (if (string-equal (tempo-lookup-named 'nstep) "")
       (list 'l "NSTEP=10")	;; default value
     (list 'l "NSTEP=" '(s nstep)))
   '(just-one-space)
   (p "<Period>: " tdu 'noinsert)
   (if (string-equal (tempo-lookup-named 'tdu) "")
       (list 'l "TDU=1e-4")	;; default value
     (list 'l "TDU=" '(s tdu)))
   '(just-one-space)
   (p "<Slew rate (V/탎)>: " slr 'noinsert)
   (if (string-equal (tempo-lookup-named 'slr) "")
       (list 'l "SLR=1")	;; default value
     (list 'l "SLR=" '(s slr)))
   'n)
 "stairgen"
 "template for inserting an ELDO staircase waveform generator"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-sawgen"
 '("Y"
   (p "[Instance name]: ") " SAWGEN "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " param: "
   (p "<Start voltage>: " v0 'noinsert)
   (if (string-equal (tempo-lookup-named 'v0) "")
       (list 'l "V0=0.0")	;; default value
     (list 'l "V0=" '(s v0)))
   '(just-one-space)
   (p "<Voltage magnitude>: " v1 'noinsert)
   (if (string-equal (tempo-lookup-named 'v1) "")
       (list 'l "V1=5.0")	;; default value
     (list 'l "V1=" '(s v1)))
   '(just-one-space)
   (p "<Period>: " tdu 'noinsert)
   (if (string-equal (tempo-lookup-named 'tdu) "")
       (list 'l "TDU=1e-4")	;; default value
     (list 'l "TDU=" '(s tdu)))
   '(just-one-space)
   (p "<Delay>: " tdel 'noinsert)
   (if (string-equal (tempo-lookup-named 'tdel) "")
       (list 'l "TDEL=0.0")	;; default value
     (list 'l "TDEL=" '(s tdel)))
   'n)
 "sawgen"
 "template for inserting an ELDO sawtooth waveform generator"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-trigen"
 '("Y"
   (p "[Instance name]: ") " TRIGEN "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " param: "
   (p "<Start voltage>: " v0 'noinsert)
   (if (string-equal (tempo-lookup-named 'v0) "")
       (list 'l "V0=0.0")	;; default value
     (list 'l "V0=" '(s v0)))
   '(just-one-space)
   (p "<Voltage magnitude>: " v1 'noinsert)
   (if (string-equal (tempo-lookup-named 'v1) "")
       (list 'l "V1=5.0")	;; default value
     (list 'l "V1=" '(s v1)))
   '(just-one-space)
   (p "<First edge duration>: " rdu 'noinsert)
   (if (string-equal (tempo-lookup-named 'rdu) "")
       (list 'l "RDU=1e-4")	;; default value
     (list 'l "RDU=" '(s rdu)))
   '(just-one-space)
   (p "<Second edge duration>: " fdu 'noinsert)
   (if (string-equal (tempo-lookup-named 'fdu) "")
       (list 'l "FDU=1e-4")	;; default value
     (list 'l "FDU=" '(s fdu)))
   '(just-one-space)
   (p "<Delay>: " tdel 'noinsert)
   (if (string-equal (tempo-lookup-named 'tdel) "")
       (list 'l "TDEL=0.0")	;; default value
     (list 'l "TDEL=" '(s tdel)))
   'n)
 "trigen"
 "template for inserting an ELDO triangular waveform generator"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-amm"
 '("Y"
   (p "[Instance name]: ") " AMM "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " param: "
   (p "<Level (1/2)>: " level 'noinsert)
   (if (string-equal (tempo-lookup-named 'level) "")
       (list 'l "LEVEL=1")	;; default value
     (list 'l "LEVEL=" '(s level)))
   '(just-one-space)
   (p "<Slewrate (V/탎)>: " slr 'noinsert)
   (if (string-equal (tempo-lookup-named 'slr) "")
       (list 'l "SLR=10")	;; default value
     (list 'l "SLR=" '(s slr)))
   '(just-one-space)
   (p "<Offset voltage>: " voff 'noinsert)
   (if (string-equal (tempo-lookup-named 'voff) "")
       (list 'l "VOFF=0.0")	;; default value
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Carrier frequency>: " fc 'noinsert)
   (if (string-equal (tempo-lookup-named 'fc) "")
       (list 'l "FC=1e6")	;; default value
     (list 'l "FC=" '(s fc)))
   '(just-one-space)
   (p "<Minimal number of sampling points per period>: " nsam 'noinsert)
   (if (string-equal (tempo-lookup-named 'nsam) "")
       (list 'l "NSAM=10")	;; default value
     (list 'l "NSAM=" '(s nsam)))
   'n)
 "amm"
 "template for inserting an ELDO amplitude modulator"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-pam"
 '("Y"
   (p "[Instance name]: ") " PAM "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " param: "
   (p "<Level (1/2)>: " level 'noinsert)
   (if (string-equal (tempo-lookup-named 'level) "")
       (list 'l "LEVEL=1")	;; default value
     (list 'l "LEVEL=" '(s level)))
   '(just-one-space)
   (p "<Slewrate (V/탎)>: " slr 'noinsert)
   (if (string-equal (tempo-lookup-named 'slr) "")
       (list 'l "SLR=10")	;; default value
     (list 'l "SLR=" '(s slr)))
   '(just-one-space)
   (p "<Offset voltage>: " voff 'noinsert)
   (if (string-equal (tempo-lookup-named 'voff) "")
       (list 'l "VOFF=0.0")	;; default value
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Carrier frequency>: " fc 'noinsert)
   (if (string-equal (tempo-lookup-named 'fc) "")
       (list 'l "FC=1e6")	;; default value
     (list 'l "FC=" '(s fc)))
   '(just-one-space)
   (p "<Minimal number of sampling points per period>: " nsam 'noinsert)
   (if (string-equal (tempo-lookup-named 'nsam) "")
       (list 'l "NSAM=10")	;; default value
     (list 'l "NSAM=" '(s nsam)))
   'n)
 "pam"
 "template for inserting an ELDO pulse amplitude modulator"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-saho"
 '("Y"
   (p "[Instance name]: ") " SA_HO "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " param: "
   (p "<Sampling frequency>: " fs 'noinsert)
   (if (string-equal (tempo-lookup-named 'fs) "")
       (list 'l "FS=1e6")	;; default value
     (list 'l "FS=" '(s fs)))
   '(just-one-space)
   (p "<Acquisition time>: " tacq 'noinsert)
   (if (string-equal (tempo-lookup-named 'tacq) "")
       (list 'l "TACQ=1e-9")	;; default value
     (list 'l "TACQ=" '(s tacq)))
   '(just-one-space)
   (p "<Droop voltage>: " dv 'noinsert)
   (if (string-equal (tempo-lookup-named 'dv) "")
       (list 'l "DV=20mv")	;; default value
     (list 'l "DV=" '(s dv)))
   '(just-one-space)
   'n)
 "saho"
 "template for inserting an ELDO sample&hold"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-trho"
 '("Y"
   (p "[Instance name]: ") " TR_HO "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " "
   (p "[Controlling node]: ") " param: "
   (p "<Threshold voltage for CRT>: " vth 'noinsert)
   (if (string-equal (tempo-lookup-named 'vth) "")
       (list 'l "VTH=0.5")	;; default value
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Acquisition time>: " tacq 'noinsert)
   (if (string-equal (tempo-lookup-named 'tacq) "")
       (list 'l "TACQ=1e-9")	;; default value
     (list 'l "TACQ=" '(s tacq)))
   'n)
 "trho"
 "template for inserting an ELDO track&hold"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-peakd"
 '("Y"
   (p "[Instance name]: ") " PEAK_D "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " "
   (p "[Controlling node]: ") " param: "
   (p "<Level (1/2)>: " level 'noinsert)
   (if (string-equal (tempo-lookup-named 'level) "")
       (list 'l "LEVEL=1")	;; default value
     (list 'l "LEVEL=2" ))
   '(just-one-space)
   (p "<Threshold voltage for CRT>: " vth 'noinsert)
   (if (string-equal (tempo-lookup-named 'vth) "")
       (list 'l "VTH=0.5")	;; default value
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold voltage for reset on output>: " res 'noinsert)
   (if (string-equal (tempo-lookup-named 'res) "")
       (list 'l "RES=0.5")	;; default value
     (list 'l "RES=" '(s res)))
   '(just-one-space)
   (p "<Output slewrate (V/탎)>: " slr 'noinsert)
   (if (string-equal (tempo-lookup-named 'slr) "")
       (list 'l "SLR=1.0")	;; default value
     (list 'l "SLR=" '(s slr)))
   '(just-one-space)
   (p "<Output slewrate on reset>: " rslr 'noinsert)
   (if (string-equal (tempo-lookup-named 'rslr) "")
       (list 'l "RSLR=1.0")	;; default value
     (list 'l "RSLR=" '(s rslr)))
   'n)
 "peakd"
 "template for inserting an ELDO peak detector"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-levdso"
 '("Y"
   (p "[Instance name]: ") " LEV_D "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " param: "
   (p "<Rise time (탎)>: " tr 'noinsert)
   (if (string-equal (tempo-lookup-named 'tr) "")
       (list 'l "TR=1.0")	;; default value
     (list 'l "TR=" '(s tr)))
   '(just-one-space)
   (p "<Fall time (탎)>: " tf 'noinsert)
   (if (string-equal (tempo-lookup-named 'tf) "")
       (list 'l "TF=1.0")	;; default value
     (list 'l "TF=" '(s tf)))
   '(just-one-space)
   (p "<Transit time (s)>: " tpd 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpd) "")
       (list 'l "TPD=0.0")	;; default value
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Lower voltage level>: " v0 'noinsert)
   (if (string-equal (tempo-lookup-named 'v0) "")
       (list 'l "V0=0.0")	;; default value
     (list 'l "V0=" '(s v0)))
   '(just-one-space)
   (p "<Higher voltage level>: " v1 'noinsert)
   (if (string-equal (tempo-lookup-named 'v1) "")
       (list 'l "V1=1.0")	;; default value
     (list 'l "V1=" '(s v1)))
   '(just-one-space)
   (p "<Input offset voltage>: " voff 'noinsert)
   (if (string-equal (tempo-lookup-named 'voff) "")
       (list 'l "VOFF=0.0")	;; default value
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Lower reference voltage>: " vrl 'noinsert)
   (if (string-equal (tempo-lookup-named 'vrl) "")
       (list 'l "VRL=-0.1")	;; default value
     (list 'l "VRL=" '(s vrl)))
   '(just-one-space)
   (p "<Higher reference voltage>: " vru 'noinsert)
   (if (string-equal (tempo-lookup-named 'vru) "")
       (list 'l "VRU=0.1")	;; default value
     (list 'l "VRU=" '(s vru)))
   'n)
 "levdso"
 "template for inserting an ELDO single-output level detector"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-levddo"
 '("Y"
   (p "[Instance name]: ") " LEV_D "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " "
   (p "[Reference node]: ") " param: "
   (p "<Rise time (탎)>: " tr 'noinsert)
   (if (string-equal (tempo-lookup-named 'tr) "")
       (list 'l "TR=1.0")	;; default value
     (list 'l "TR=" '(s tr)))
   '(just-one-space)
   (p "<Fall time (탎)>: " tf 'noinsert)
   (if (string-equal (tempo-lookup-named 'tf) "")
       (list 'l "TF=1.0")	;; default value
     (list 'l "TF=" '(s tf)))
   '(just-one-space)
   (p "<Transit time (s)>: " tpd 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpd) "")
       (list 'l "TPD=0.0")	;; default value
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Lower voltage level>: " v0 'noinsert)
   (if (string-equal (tempo-lookup-named 'v0) "")
       (list 'l "V0=0.0")	;; default value
     (list 'l "V0=" '(s v0)))
   '(just-one-space)
   (p "<Higher voltage level>: " v1 'noinsert)
   (if (string-equal (tempo-lookup-named 'v1) "")
       (list 'l "V1=1.0")	;; default value
     (list 'l "V1=" '(s v1)))
   '(just-one-space)
   (p "<Input offset voltage>: " voff 'noinsert)
   (if (string-equal (tempo-lookup-named 'voff) "")
       (list 'l "VOFF=0.0")	;; default value
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Lower reference voltage>: " vrl 'noinsert)
   (if (string-equal (tempo-lookup-named 'vrl) "")
       (list 'l "VRL=-0.1")	;; default value
     (list 'l "VRL=" '(s vrl)))
   '(just-one-space)
   (p "<Higher reference voltage>: " vru 'noinsert)
   (if (string-equal (tempo-lookup-named 'vru) "")
       (list 'l "VRU=0.1")	;; default value
     (list 'l "VRU=" '(s vru)))
   'n)
 "levddo"
 "template for inserting an ELDO differential-output level detector"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-logamp"
 '("Y"
   (p "[Instance name]: ") " LOGAMP "
   (p "[Input]: ") " "
   (p "[Output]: ") " param: "
   (p "<Gain>: " gain 'noinsert)
   (if (string-equal (tempo-lookup-named 'gain) "")
       (list 'l "K=1.0")	;; default value
     (list 'l "K=" '(s gain)))
   '(just-one-space)
   (p "<Log function argument>: " e 'noinsert)
   (if (string-equal (tempo-lookup-named 'e) "")
       (list 'l "E=1")	;; default value
     (list 'l "E=" '(s vmin)))
   '(just-one-space)
   (p "<Vmax>: " vmax 'noinsert)
   (if (string-equal (tempo-lookup-named 'vmax) "")
       (list 'l "VMAX=5.0")	;; default value
     (list 'l "VMAX=" '(s vmax)))
   '(just-one-space)
   (p "<Vmin>: " vmin 'noinsert)
   (if (string-equal (tempo-lookup-named 'vmin) "")
       (list 'l "VMIN=-5.0")	;; default value
     (list 'l "VMIN=" '(s vmin)))
   '(just-one-space)
   (p "<Positive saturation voltage>: " vsatp 'noinsert)
   (if (string-equal (tempo-lookup-named 'vsatp) "")
       (list 'l "VSATP=4.75")	;; default value
     (list 'l "VSATP=" '(s vsatp)))
   '(just-one-space)
   (p "<Negative saturation voltage>: " vsatn 'noinsert)
   (if (string-equal (tempo-lookup-named 'vsatn) "")
       (list 'l "VSATN=-4.75")	;; default value
     (list 'l "VSATN=" '(s vsatn)))
   '(just-one-space)
   (p "<Slope at VSATP>: " pslope 'noinsert)
   (if (string-equal (tempo-lookup-named 'pslope) "")
       (list 'l "PSLOPE=0.25")	;; default value
     (list 'l "PSLOPE=" '(s pslope)))
   '(just-one-space)
   (p "<Slope at VSATN>: " nslope 'noinsert)
   (if (string-equal (tempo-lookup-named 'nslope) "")
       (list 'l "NSLOPE=0.25")	;; default value
     (list 'l "NSLOPE=" '(s nslope)))
   'n)
 "logamp"
 "template for inserting an ELDO logarithmic amplifier"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-antilog"
 '("Y"
   (p "[Instance name]: ") " EXPAMP "
   (p "[Input]: ") " "
   (p "[Output]: ") " param: "
   (p "<Gain>: " k 'noinsert)
   (if (string-equal (tempo-lookup-named 'k) "")
       (list 'l "K=1.0")	;; default value
     (list 'l "K=" '(s k)))
   '(just-one-space)
   (p "<Exp function argument>: " e 'noinsert)
   (if (string-equal (tempo-lookup-named 'e) "")
       (list 'l "E=1")		;; default value
     (list 'l "E=" '(s vmin)))
   '(just-one-space)
   (p "<Base of power function>: " base 'noinsert)
   (if (string-equal (tempo-lookup-named 'base) "")
       (list 'l "BASE={EXP(1)}")	;; default value
     (list 'l "BASE=" '(s base)))
   '(just-one-space)
   (p "<Vmax>: " vmax 'noinsert)
   (if (string-equal (tempo-lookup-named 'vmax) "")
       (list 'l "VMAX=5.0")	;; default value
     (list 'l "VMAX=" '(s vmax)))
   '(just-one-space)
   (p "<Vmin>: " vmin 'noinsert)
   (if (string-equal (tempo-lookup-named 'vmin) "")
       (list 'l "VMIN=-5.0")	;; default value
     (list 'l "VMIN=" '(s vmin)))
   '(just-one-space)
   (p "<Positive saturation voltage>: " vsatp 'noinsert)
   (if (string-equal (tempo-lookup-named 'vsatp) "")
       (list 'l "VSATP=4.75")	;; default value
     (list 'l "VSATP=" '(s vsatp)))
   '(just-one-space)
   (p "<Negative saturation voltage>: " vsatn 'noinsert)
   (if (string-equal (tempo-lookup-named 'vsatn) "")
       (list 'l "VSATN=-4.75")	;; default value
     (list 'l "VSATN=" '(s vsatn)))
   '(just-one-space)
   (p "<Slope at VSATP>: " pslope 'noinsert)
   (if (string-equal (tempo-lookup-named 'pslope) "")
       (list 'l "PSLOPE=0.25")	;; default value
     (list 'l "PSLOPE=" '(s pslope)))
   '(just-one-space)
   (p "<Slope at VSATN>: " nslope 'noinsert)
   (if (string-equal (tempo-lookup-named 'nslope) "")
       (list 'l "NSLOPE=0.25")	;; default value
     (list 'l "NSLOPE=" '(s nslope)))
   'n)
 "expamp"
 "template for inserting an ELDO anti-logarithmic amplifier"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-diff"
 '("Y"
   (p "[Instance name]: ") " DIFF "
   (p "[Input]: ") " "
   (p "[Output]: ") " param: "
   (p "<Time constant>: " k 'noinsert)
   (if (string-equal (tempo-lookup-named 'k) "")
       (list 'l "K=1")		;; default value
     (list 'l "K=" '(s k)))
   '(just-one-space)
   (p "<DC value>: " c0 'noinsert)
   (if (string-equal (tempo-lookup-named 'c0) "")
       (list 'l "C0=1")		;; default value
     (list 'l "C0=" '(s c0)))
   '(just-one-space)
   (p "<Slewrate (V/s)>: " slr 'noinsert)
   (if (string-equal (tempo-lookup-named 'slr) "")
       (list 'l "SLR=1e9")	;; default value
     (list 'l "SLR=" '(s slr)))
   'n)
 "diff"
 "template for inserting an ELDO differentiator"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-integ"
 '("Y"
   (p "[Instance name]: ") " INTEG "
   (p "[Input]: ") " "
   (p "[Output]: ") " param: "
   (p "<Time constant>: " k 'noinsert)
   (if (string-equal (tempo-lookup-named 'k) "")
       (list 'l "K=1")		;; default value
     (list 'l "K=" '(s k)))
   '(just-one-space)
   (p "<DC value>: " c0 'noinsert)
   (if (string-equal (tempo-lookup-named 'c0) "")
       (list 'l "C0=1")		;; default value
     (list 'l "C0=" '(s c0)))
   'n)
 "integ"
 "template for inserting an ELDO integrator"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-adder"
 '("Y"
   (p "[Instance name]: ")  " "
   (p "<ADD/SUB/MULT/DIV>: ") " "
   (p "[Input 2]: ") " "
   (p "[Output]: ") " param: "
   (p "<Vmax>: " vmax 'noinsert)
   (if (string-equal (tempo-lookup-named 'vmax) "")
       (list 'l "VMAX=5.0")	;; default value
     (list 'l "VMAX=" '(s vmax)))
   '(just-one-space)
   (p "<Vmin>: " vmin 'noinsert)
   (if (string-equal (tempo-lookup-named 'vmin) "")
       (list 'l "VMIN=-5.0")	;; default value
     (list 'l "VMIN=" '(s vmin)))
   '(just-one-space)
   (p "<Positive saturation voltage>: " vsatp 'noinsert)
   (if (string-equal (tempo-lookup-named 'vsatp) "")
       (list 'l "VSATP=4.75")	;; default value
     (list 'l "VSATP=" '(s vsatp)))
   '(just-one-space)
   (p "<Negative saturation voltage>: " vsatn 'noinsert)
   (if (string-equal (tempo-lookup-named 'vsatn) "")
       (list 'l "VSATN=-4.75")	;; default value
     (list 'l "VSATN=" '(s vsatn)))
   '(just-one-space)
   (p "<Slope at VSATP>: " pslope 'noinsert)
   (if (string-equal (tempo-lookup-named 'pslope) "")
       (list 'l "PSLOPE=0.25")	;; default value
     (list 'l "PSLOPE=" '(s pslope)))
   '(just-one-space)
   (p "<Slope at VSATN>: " nslope 'noinsert)
   (if (string-equal (tempo-lookup-named 'nslope) "")
       (list 'l "NSLOPE=0.25")	;; default value
     (list 'l "NSLOPE=" '(s nslope)))
   'n)
 "add"
 "template for inserting an ELDO adder/subtrator/multiplier/divider"
 'spice-tempo-tags)

;; -------------------
;; Digital Macromodels
;; -------------------

(tempo-define-template
 "spice-eldo-inv"
 '("INV"
   (p "[Instance name]: ") " "
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vhi) "")
       (list 'l "VHI=5.0")	;; default value
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vlo) "")
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert)
   (if (string-equal (tempo-lookup-named 'vth) "")
       (list 'l "VTH=2.5")	;; default value
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vthi) "")
       (list 'l "")	;; default value
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vtlo) "")
       (list 'l "")	;; default value
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpd) "")
       (list 'l "TPD=1.0ns")	;; default value
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdup) "")
       (list 'l "")	;; default value
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdown) "")
       (list 'l "")	;; default value
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert)
   (if (string-equal (tempo-lookup-named 'cin) "")
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)
 "inv"
 "template for inserting an ELDO INVERTER gate macromodel"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-xor"
 '("XOR"
   (p "[Instance name]: ") " "
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vhi) "")
       (list 'l "VHI=5.0")	;; default value
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vlo) "")
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert)
   (if (string-equal (tempo-lookup-named 'vth) "")
       (list 'l "VTH=2.5")	;; default value
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vthi) "")
       (list 'l "")	;; default value
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vtlo) "")
       (list 'l "")	;; default value
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpd) "")
       (list 'l "TPD=1.0ns")	;; default value
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdup) "")
       (list 'l "")	;; default value
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdown) "")
       (list 'l "")	;; default value
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert)
   (if (string-equal (tempo-lookup-named 'cin) "")
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)
 "xor"
 "template for inserting an ELDO Exclusive-OR gate macromodel"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-and2"
 '("AND"
   (p "[Instance name]: ") " "
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vhi) "")
       (list 'l "VHI=5.0")	;; default value
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vlo) "")
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert)
   (if (string-equal (tempo-lookup-named 'vth) "")
       (list 'l "VTH=2.5")	;; default value
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vthi) "")
       (list 'l "")	;; default value
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vtlo) "")
       (list 'l "")	;; default value
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpd) "")
       (list 'l "TPD=1.0ns")	;; default value
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdup) "")
       (list 'l "")	;; default value
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdown) "")
       (list 'l "")	;; default value
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert)
   (if (string-equal (tempo-lookup-named 'cin) "")
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)
 "and"
 "template for inserting an ELDO 2 input AND gate macromodel"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-nand2"
 '("NAND"
   (p "[Instance name]: ") " "
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vhi) "")
       (list 'l "VHI=5.0")	;; default value
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vlo) "")
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert)
   (if (string-equal (tempo-lookup-named 'vth) "")
       (list 'l "VTH=2.5")	;; default value
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vthi) "")
       (list 'l "")	;; default value
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vtlo) "")
       (list 'l "")	;; default value
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpd) "")
       (list 'l "TPD=1.0ns")	;; default value
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdup) "")
       (list 'l "")	;; default value
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdown) "")
       (list 'l "")	;; default value
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert)
   (if (string-equal (tempo-lookup-named 'cin) "")
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)
 "nand"
 "template for inserting an ELDO 2 input NAND gate macromodel"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-or2"
 '("OR"
   (p "[Instance name]: ") " "
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vhi) "")
       (list 'l "VHI=5.0")	;; default value
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vlo) "")
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert)
   (if (string-equal (tempo-lookup-named 'vth) "")
       (list 'l "VTH=2.5")	;; default value
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vthi) "")
       (list 'l "")	;; default value
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vtlo) "")
       (list 'l "")	;; default value
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpd) "")
       (list 'l "TPD=1.0ns")	;; default value
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdup) "")
       (list 'l "")	;; default value
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdown) "")
       (list 'l "")	;; default value
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert)
   (if (string-equal (tempo-lookup-named 'cin) "")
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)
 "or"
 "template for inserting an ELDO 2 input OR gate macromodel"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-nor2"
 '("NOR"
   (p "[Instance name]: ") " "
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vhi) "")
       (list 'l "VHI=5.0")	;; default value
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vlo) "")
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert)
   (if (string-equal (tempo-lookup-named 'vth) "")
       (list 'l "VTH=2.5")	;; default value
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vthi) "")
       (list 'l "")	;; default value
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vtlo) "")
       (list 'l "")	;; default value
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpd) "")
       (list 'l "TPD=1.0ns")	;; default value
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdup) "")
       (list 'l "")	;; default value
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdown) "")
       (list 'l "")	;; default value
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert)
   (if (string-equal (tempo-lookup-named 'cin) "")
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)
 "nor"
 "template for inserting an ELDO 2 input NOR gate macromodel"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-and3"
 '("AND3"
   (p "[Instance name]: ") " "
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vhi) "")
       (list 'l "VHI=5.0")	;; default value
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vlo) "")
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert)
   (if (string-equal (tempo-lookup-named 'vth) "")
       (list 'l "VTH=2.5")	;; default value
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vthi) "")
       (list 'l "")	;; default value
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vtlo) "")
       (list 'l "")	;; default value
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpd) "")
       (list 'l "TPD=1.0ns")	;; default value
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdup) "")
       (list 'l "")	;; default value
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdown) "")
       (list 'l "")	;; default value
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert)
   (if (string-equal (tempo-lookup-named 'cin) "")
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)
 "and3"
 "template for inserting an ELDO 3 input AND gate macromodel"
 'spice-tempo-tags)


(tempo-define-template
 "spice-eldo-nand3"
 '("NAND3"
   (p "[Instance name]: ") " "
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vhi) "")
       (list 'l "VHI=5.0")	;; default value
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vlo) "")
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert)
   (if (string-equal (tempo-lookup-named 'vth) "")
       (list 'l "VTH=2.5")	;; default value
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vthi) "")
       (list 'l "")	;; default value
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vtlo) "")
       (list 'l "")	;; default value
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpd) "")
       (list 'l "TPD=1.0ns")	;; default value
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdup) "")
       (list 'l "")	;; default value
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdown) "")
       (list 'l "")	;; default value
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert)
   (if (string-equal (tempo-lookup-named 'cin) "")
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)
 "nand3"
 "template for inserting an ELDO 3 input NAND gate macromodel"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-or3"
 '("OR3"
   (p "[Instance name]: ") " "
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vhi) "")
       (list 'l "VHI=5.0")	;; default value
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vlo) "")
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert)
   (if (string-equal (tempo-lookup-named 'vth) "")
       (list 'l "VTH=2.5")	;; default value
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vthi) "")
       (list 'l "")	;; default value
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vtlo) "")
       (list 'l "")	;; default value
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpd) "")
       (list 'l "TPD=1.0ns")	;; default value
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdup) "")
       (list 'l "")	;; default value
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdown) "")
       (list 'l "")	;; default value
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert)
   (if (string-equal (tempo-lookup-named 'cin) "")
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)
 "or3"
 "template for inserting an ELDO 3 input OR gate macromodel"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-nor3"
 '("NOR3"
   (p "[Instance name]: ") " "
   (p "[First input]: ") " "
   (p "[Second input]: ") " "
   (p "[Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Vhigh>: " vhi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vhi) "")
       (list 'l "VHI=5.0")	;; default value
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vlo) "")
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vth 'noinsert)
   (if (string-equal (tempo-lookup-named 'vth) "")
       (list 'l "VTH=2.5")	;; default value
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage rising edge>: " vthi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vthi) "")
       (list 'l "")	;; default value
     (list 'l "VTHI=" '(s vthi)))
   '(just-one-space)
   (p "<Threshold input voltage falling edge>: " vtlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vtlo) "")
       (list 'l "")	;; default value
     (list 'l "VTLO=" '(s vtlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpd) "")
       (list 'l "TPD=1.0ns")	;; default value
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Transit time for output to reach VTHI>: " tpdup 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdup) "")
       (list 'l "")	;; default value
     (list 'l "TPDUP=" '(s tpdup)))
   '(just-one-space)
   (p "<Transit time for output to reach VTLO>: " tpdown 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpdown) "")
       (list 'l "")	;; default value
     (list 'l "TPDOWN=" '(s tpdown)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert)
   (if (string-equal (tempo-lookup-named 'cin) "")
       (list 'l "CIN=0.0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   'n)
 "nor3"
 "template for inserting an ELDO 3 input NOR gate macromodel"
 'spice-tempo-tags)

;; -------------------------
;; Mixed signal Macromodels
;; -------------------------

(tempo-define-template
 "spice-eldo-adc"
 '("ADC"
   (p "[Instance name]: ") " "
   (p "[Clock]: ") " "
   (p "[Analog input]: ") " "
   (p "[Digital Outputs from MSB to LSB]: ") " "
   (p "<Edge (1/-1)>: " edge 'noinsert)
   (if (string-equal (tempo-lookup-named 'edge) "")
       (list 'l "EDGE=1")	;; default value
     (list 'l "EDGE=-1"))
   '(just-one-space)
   (p "<Threshold clock voltage>: " vth 'noinsert)
   (if (string-equal (tempo-lookup-named 'vth) "")
       (list 'l "VTH=2.5")	;; default value
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Vhigh>: " vhi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vhi) "")
       (list 'l "VHI=5.0")	;; default value
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Vlow>: " vlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vlo) "")
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Analog input lower voltage>: " vinf 'noinsert)
   (if (string-equal (tempo-lookup-named 'vinf) "")
       (list 'l "VINF=0.0")	;; default value
     (list 'l "VTHI=" '(s vinf)))
   '(just-one-space)
   (p "<Analog input higher voltage>: " vsup 'noinsert)
   (if (string-equal (tempo-lookup-named 'vsup) "")
       (list 'l "VSUP=5.0")	;; default value
     (list 'l "VSUP=" '(s vsup)))
   '(just-one-space)
   (p "<Output bits commutation time>: " tcom 'noinsert)
   (if (string-equal (tempo-lookup-named 'tcom) "")
       (list 'l "TCOM=1.0ns")	;; default value
     (list 'l "TCOM=" '(s tcom)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpd) "")
       (list 'l "TPD=10ns")	;; default value
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   'n)
 "adc"
 "template for inserting an ELDO Analog to Digital Converter  macromodel"
 'spice-tempo-tags)

(tempo-define-template
 "spice-eldo-dac"
 '("DAC"
   (p "[Instance name]: ") " "
   (p "[Clock]: ") " "
   (p "[Digital inputs from MSB to LSB]: ") " "
   (p "[Analog output]: ") " "
   (p "<Edge (1/-1)>: " edge 'noinsert)
   (if (string-equal (tempo-lookup-named 'edge) "")
       (list 'l "EDGE=1")	;; default value
     (list 'l "EDGE=-1"))
   '(just-one-space)
   (p "<Threshold clock voltage>: " vth 'noinsert)
   (if (string-equal (tempo-lookup-named 'vth) "")
       (list 'l "VTH=2.5")	;; default value
     (list 'l "VTH=" '(s vth)))
   '(just-one-space)
   (p "<Threshold input voltage>: " vtin 'noinsert)
   (if (string-equal (tempo-lookup-named 'vtin) "")
       (list 'l "VTIN=2.5")	;; default value
     (list 'l "VTIN=" '(s vtin)))
   '(just-one-space)
   (p "<Analog Output Vhigh>: " vhi 'noinsert)
   (if (string-equal (tempo-lookup-named 'vhi) "")
       (list 'l "VHI=5.0")	;; default value
     (list 'l "VHI=" '(s vhi)))
   '(just-one-space)
   (p "<Analog Output Vlow>: " vlo 'noinsert)
   (if (string-equal (tempo-lookup-named 'vlo) "")
       (list 'l "VLO=0.0")	;; default value
     (list 'l "VLO=" '(s vlo)))
   '(just-one-space)
   (p "<Transit time>: " tpd 'noinsert)
   (if (string-equal (tempo-lookup-named 'tpd) "")
       (list 'l "TPD=10ns")	;; default value
     (list 'l "TPD=" '(s tpd)))
   '(just-one-space)
   (p "<Output slope (V/s)>: " sl 'noinsert)
   (if (string-equal (tempo-lookup-named 'sl) "")
       (list 'l "SL=10e8")	;; default value
     (list 'l "SL=" '(s tcom)))
   '(just-one-space)
   'n)
 "DAC"
 "template for inserting an ELDO Digital to Analog Converter macromodel"
 'spice-tempo-tags)

;; -------------------------
;; Switched cap Macromodels
;; -------------------------

(tempo-define-template
 "spice-eldo-switchcap-opa"
 '("OPA"
   (p "[Instance name]: ") " "
   (p "[Positive input]: ") " "
   (p "[Negative input]: ") " "
   (p "[Positive Output]: ") " "
   (p "[Negative Output]: ") " "
   (p "[Model name]: ") " "
   (p "<Input offset>: " voff 'noinsert)
   (if (string-equal (tempo-lookup-named 'voff) "")
       (list 'l "VOFF=0.0")	;; default value
     (list 'l "VOFF=" '(s voff)))
   '(just-one-space)
   (p "<Slew rate (V/s)>: " sl 'noinsert)
   (if (string-equal (tempo-lookup-named 'sl) "")
       (list 'l "SL=1e6")	;; default value
     (list 'l "SL=" '(s voff)))
   '(just-one-space)
   (p "<Gain>: " gain 'noinsert)
   (if (string-equal (tempo-lookup-named 'gain) "")
       (list 'l "GAIN=1e5")	;; default value
     (list 'l "GAIN=" '(s gain)))
   '(just-one-space)
   (p "<Input capacitance>: " cin 'noinsert)
   (if (string-equal (tempo-lookup-named 'cin) "")
       (list 'l "CIN=0")	;; default value
     (list 'l "CIN=" '(s cin)))
   '(just-one-space)
   (p "<Output resistance>: " rs 'noinsert)
   (if (string-equal (tempo-lookup-named 'rs) "")
       (list 'l "RS=10e6")	;; default value
     (list 'l "RS=" '(s rs)))
   '(just-one-space)
   (p "<Symmetrical saturation voltage>: " vsat 'noinsert)
   (if (string-equal (tempo-lookup-named 'vsat) "")
       (list 'l "VSAT=5.0")	;; default value
     (list 'l "VSAT=" '(s vsat)))
   '(just-one-space)
   (p "<Asymmetrical saturation voltage>: " vsatm 'noinsert)
   (if (string-equal (tempo-lookup-named 'vsatm) "")
       (list 'l "VSATM=5.0")	;; default value
     (list 'l "VSATM=" '(s vsatm)))
   '(just-one-space)
   (p "<Cutoff frequency (double stage only)>: " fc 'noinsert)
   (if (string-equal (tempo-lookup-named 'fc) "")
       (list 'l "FC=1k")	;; default value
     (list 'l "FC=" '(s fc)))
   '(just-one-space)
   (p "<Non-dominant pole (single stage only)>: " fndp 'noinsert)
   (if (string-equal (tempo-lookup-named 'fndp) "")
       (list 'l "FNDP=1k")	;; default value
     (list 'l "FNDP=" '(s fndp)))
   '(just-one-space)
   (p "<Max current>: " imax 'noinsert)
   (if (string-equal (tempo-lookup-named 'imax) "")
       (list 'l "IMAX=100mA")	;; default value
     (list 'l "IMAX=" '(s imax)))
   '(just-one-space)
   (p "<Common mode rejection ratio>: " cmrr 'noinsert)
   (if (string-equal (tempo-lookup-named 'cmrr) "")
       (list 'l "CMRR=0.0")	;; default value
     (list 'l "CMRR=" '(s cmrr)))
   'n)
 "opa"
 "template for inserting an ELDO differential single or double stage opamp"
 'spice-tempo-tags
 )

(tempo-define-template
 "spice-eldo-switch"
 '("S"
   (p "[Instance name]: ") " "
   (p "[Controlling node]: ") " "
   (p "[Node 1]: ") " "
   (p "[Node 2]: ") " "
   (p "[Model name]: ") " "
   (p "<RON resistance>: " ron 'noinsert)
   (if (string-equal (tempo-lookup-named 'ron) "")
       (list 'l "RON=1k")	;; default value
     (list 'l "RON=" '(s ron)))
   '(just-one-space)
   (p "<Overlap capacitance>: " crec 'noinsert)
   (if (string-equal (tempo-lookup-named 'crec) "")
       (list 'l "CREC=0")	;; default value
     (list 'l "CREC=" '(s crec)))
   'n)
 "switch"
 "template for inserting an ELDO switch macromodel"
 'spice-tempo-tags
 )


;; Layla constructs

(tempo-define-template
 "layla-port"
 '(".port "
   (p "[Name of port]: ") " "
   (p "[Name of net]: ") " "
   'n)
 "layla-port"
 "template for inserting a Layla port"
 'spice-tempo-tags)

(tempo-define-template
 "layla-performance"
 '(".performance "
   (p "[Name of performance]: ") " "
   (p "[Nominal value]: ") " \n+"
   (p "[Weight (alfa)]: ") " "
                                        ;   (p "[Weight (alfa)]: ") " "
   'n)
 "layla-performance"
 "template for inserting a Layla performance"
 'spice-tempo-tags)

(tempo-define-template
 "layla-net"
 '(".net "
   (p "[Name of net]: ") " "
   'n)
 "layla-net"
 "template for inserting a Layla net"
 'spice-tempo-tags)

(tempo-define-template
 "layla-bus"
 '(".bus "
   (p "[Name of bus]: ") " "
   'n)
 "layla-bus"
 "template for inserting a Layla bus"
 'spice-tempo-tags)

(defmacro spice-layla-function-template (name type)
  "Create a layla tempo define for name and type"
  (` (let (p_prompt)
       (setq p_prompt (concat "[Name of " (, name) "]: "))
       (tempo-define-template
	(concat "layla-" (, name) "-" (, type))
	(list (concat "." (, name) "_" (, type) "_param(")
              (list 'p p_prompt) ", "
              '(p "[Name of parameter]: ") ", "
              '(p "[Value of parameter]: ") ")"
              'n)
	(concat "layla-" (, name) "-" (, type))
	(concat "template for inserting a " (, type) " parameter for a Layla "
		(, name))
	'spice-tempo-tags))))

(spice-layla-function-template "bus" "double")
(spice-layla-function-template "bus" "integer")
(spice-layla-function-template "bus" "string")
(spice-layla-function-template "device" "double")
(spice-layla-function-template "device" "integer")
(spice-layla-function-template "device" "string")
(spice-layla-function-template "net" "double")
(spice-layla-function-template "net" "integer")
(spice-layla-function-template "net" "string")
(spice-layla-function-template "placement" "double")
(spice-layla-function-template "placement" "integer")
(spice-layla-function-template "placement" "string")
(spice-layla-function-template "port" "double")
(spice-layla-function-template "port" "integer")
(spice-layla-function-template "port" "string")
(spice-layla-function-template "symmetry" "double")
(spice-layla-function-template "symmetry" "integer")
(spice-layla-function-template "symmetry" "string")


;;------------------------------------------------------------
;; Abbrev hook bindings (taken from eldo-mode)

(defvar spice-mode-abbrev-table nil
  "Abbrev table to use in `spice-mode' buffers.")

;; the table, global init inline here:
(if spice-mode-abbrev-table
    ()
  (let ((ac abbrevs-changed))
    (define-abbrev-table 'spice-mode-abbrev-table ())
    ;; passive elements:

    ;;  resistors
    (define-abbrev spice-mode-abbrev-table "r"     "" 'tempo-template-spice-spice2g6-resistor)
    (define-abbrev spice-mode-abbrev-table "rss"   "" 'tempo-template-spice-spice3-semiconductor-resistor)
    (define-abbrev spice-mode-abbrev-table "re"    "" 'tempo-template-spice-eldo-resistor)
    (define-abbrev spice-mode-abbrev-table "ree"   "" 'tempo-template-spice-eldo-expression-resistor)
    (define-abbrev spice-mode-abbrev-table "res"   "" 'tempo-template-spice-eldo-semiconductor-resistor)
    (define-abbrev spice-mode-abbrev-table "rh"    "" 'tempo-template-spice-hspice-resistor)
    (define-abbrev spice-mode-abbrev-table "rl"    "" 'tempo-template-spice-layla-resistor)

    ;;  capacitors
    (define-abbrev spice-mode-abbrev-table "c"     "" 'tempo-template-spice-spice2g6-capacitor)
    (define-abbrev spice-mode-abbrev-table "css"   "" 'tempo-template-spice-spice3-semiconductor-capacitor)
    (define-abbrev spice-mode-abbrev-table "ce"    "" 'tempo-template-spice-eldo-capacitor)
    (define-abbrev spice-mode-abbrev-table "cee"   "" 'tempo-template-spice-eldo-expression-capacitor)
    (define-abbrev spice-mode-abbrev-table "ch"    "" 'tempo-template-spice-hspice-capacitor)
    (define-abbrev spice-mode-abbrev-table "cl"    "" 'tempo-template-spice-layla-capacitor)

    ;;  inductors
    (define-abbrev spice-mode-abbrev-table "l"     "" 'tempo-template-spice-spice2g6-inductor)
    (define-abbrev spice-mode-abbrev-table "le"    "" 'tempo-template-spice-eldo-inductor)
    (define-abbrev spice-mode-abbrev-table "lee"   "" 'tempo-template-spice-eldo-expression-inductor)
    (define-abbrev spice-mode-abbrev-table "lh"    "" 'tempo-template-spice-hspice-inductor)
    (define-abbrev spice-mode-abbrev-table "ll"    "" 'tempo-template-spice-layla-inductor)

    ;;  coupled inductors
    (define-abbrev spice-mode-abbrev-table "k"     "" 'tempo-template-spice-spice2g6-coupled-inductors)

    ;;  lossless transmission lines
    (define-abbrev spice-mode-abbrev-table "t"     "" 'tempo-template-spice-spice2g6-lossless-transmission)
    (define-abbrev spice-mode-abbrev-table "te"     "" 'tempo-template-spice-spice2g6-lossless-transmission)
    (define-abbrev spice-mode-abbrev-table "th"     "" 'tempo-template-spice-hspice-lossless-transmission)

    ;;  lossy transmission lines
    (define-abbrev spice-mode-abbrev-table "o"     "" 'tempo-template-spice-spice2g6-lossy-transmission)
                                        ; spice3 rcline
    (define-abbrev spice-mode-abbrev-table "rcls"  "" 'tempo-template-spice-spice3-rcline)
                                        ; eldo rc-line
    (define-abbrev spice-mode-abbrev-table "rcle"  "" 'tempo-template-spice-eldo-rcline)
                                        ; eldo lossy transmission line
                                        ; not implemented

    ;; active elements:

    ;; diodes
    (define-abbrev spice-mode-abbrev-table "d"     "" 'tempo-template-spice-spice2g6-diode)
    (define-abbrev spice-mode-abbrev-table "de"    "" 'tempo-template-spice-eldo-diode)
    (define-abbrev spice-mode-abbrev-table "dh"    "" 'tempo-template-spice-hspice-diode)
    (define-abbrev spice-mode-abbrev-table "dl"    "" 'tempo-template-spice-layla-diode)

    ;; bipolars
    (define-abbrev spice-mode-abbrev-table "q"     "" 'tempo-template-spice-spice2g6-bipolar)
    (define-abbrev spice-mode-abbrev-table "qe"    "" 'tempo-template-spice-eldo-bipolar)
    (define-abbrev spice-mode-abbrev-table "qh"    "" 'tempo-template-spice-hspice-bipolar)


    ;; jfets
    (define-abbrev spice-mode-abbrev-table "j"     "" 'tempo-template-spice-spice2g6-jfet)
    (define-abbrev spice-mode-abbrev-table "je"    "" 'tempo-template-spice-eldo-jfet)
    (define-abbrev spice-mode-abbrev-table "jh"    "" 'tempo-template-spice-hspice-jfet)


    ;; mosfets
    (define-abbrev spice-mode-abbrev-table "m"     "" 'tempo-template-spice-spice2g6-mosfet)
    (define-abbrev spice-mode-abbrev-table "me"    "" 'tempo-template-spice-eldo-mosfet)
    (define-abbrev spice-mode-abbrev-table "mh"    "" 'tempo-template-spice-hspice-mosfet)
    (define-abbrev spice-mode-abbrev-table "ml"    "" 'tempo-template-spice-layla-mosfet)


    ;; mesfets
    (define-abbrev spice-mode-abbrev-table "z"     "" 'tempo-template-spice-spice2g6-mesfet)

    ;; subcircuits
    (define-abbrev spice-mode-abbrev-table "sub"   "" 'tempo-template-spice-spice2g6-subckt)

    ;; controlled sources
    (define-abbrev spice-mode-abbrev-table "vcvs"  "" 'tempo-template-spice-spice2g6-vcvs)
    (define-abbrev spice-mode-abbrev-table "e"     "" 'tempo-template-spice-spice2g6-vcvs)
    (define-abbrev spice-mode-abbrev-table "ccvs"  "" 'tempo-template-spice-spice2g6-ccvs)
    (define-abbrev spice-mode-abbrev-table "h"     "" 'tempo-template-spice-spice2g6-ccvs)
    (define-abbrev spice-mode-abbrev-table "vccs"  "" 'tempo-template-spice-spice2g6-vccs)
    (define-abbrev spice-mode-abbrev-table "g"     "" 'tempo-template-spice-spice2g6-vccs)
    (define-abbrev spice-mode-abbrev-table "cccs"  "" 'tempo-template-spice-spice2g6-cccs)
    (define-abbrev spice-mode-abbrev-table "f"     "" 'tempo-template-spice-spice2g6-cccs)

    ;; transient signals
    (define-abbrev spice-mode-abbrev-table "pwl"   "" '(spice-pwl))
    (define-abbrev spice-mode-abbrev-table "pu"    "" 'tempo-template-spice-pulse)
                                        ;(define-abbrev spice-mode-abbrev-table "'su"    "" 'tempo-template-spice-subckt)
                                        ;(define-abbrev spice-mode-abbrev-table "'ac"    "" 'tempo-template-spice-ac)
    (define-abbrev spice-mode-abbrev-table "sin"   "" 'tempo-template-spice-sine)
                                        ;(define-abbrev spice-mode-abbrev-table "'sffm"  "" 'tempo-template-spice-hspice-sffm)
    (define-abbrev spice-mode-abbrev-table "exp"   "" 'tempo-template-spice-exp)
    (define-abbrev spice-mode-abbrev-table "noi"   "" 'tempo-template-spice-eldo-noise)
    (define-abbrev spice-mode-abbrev-table "pat"   "" 'tempo-template-spice-eldo-pattern)

    ;; behavioral models
    ;;   eldo
    (define-abbrev spice-mode-abbrev-table "comp"  "" 'tempo-template-spice-eldo-comp)
    (define-abbrev spice-mode-abbrev-table "compd" "" 'tempo-template-spice-eldo-compd)
    (define-abbrev spice-mode-abbrev-table "opa0"   "" 'tempo-template-spice-eldo-linear-opa0)
    (define-abbrev spice-mode-abbrev-table "opa0d"  "" 'tempo-template-spice-eldo-linear-opa0d)
    (define-abbrev spice-mode-abbrev-table "opa1"   "" 'tempo-template-spice-eldo-linear-opa0)
    (define-abbrev spice-mode-abbrev-table "opa1d"  "" 'tempo-template-spice-eldo-linear-opa0d)
    (define-abbrev spice-mode-abbrev-table "opa2"   "" 'tempo-template-spice-eldo-linear-opa0)
    (define-abbrev spice-mode-abbrev-table "opa2d"  "" 'tempo-template-spice-eldo-linear-opa0d)

    ;; extracts
    (define-abbrev spice-mode-abbrev-table "phmag" "" 'tempo-template-spice-eldo-phmag)
    (define-abbrev spice-mode-abbrev-table "gmag"  "" 'tempo-template-spice-eldo-gmag)
    (define-abbrev spice-mode-abbrev-table "fc"    "" 'tempo-template-spice-eldo-fc)
    (define-abbrev spice-mode-abbrev-table "ugfc"  "" 'tempo-template-spice-eldo-ugfc)
    (define-abbrev spice-mode-abbrev-table "tf"    "" 'tempo-template-spice-eldo-period)
    (define-abbrev spice-mode-abbrev-table "tfm"   "" 'tempo-template-spice-eldo-period-macro)
    (define-abbrev spice-mode-abbrev-table "dc"    "" 'tempo-template-spice-eldo-duty-macro)
    (define-abbrev spice-mode-abbrev-table "he"    "" 'tempo-template-spice-eldo-circuit-header)
    (setq abbrevs-changed ac)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spice menu (using `easy-menu.el')

(defun spice-create-mode-menu ()
  "Create Spice Mode menu."
  (append
   (list
    "Spice"
    '("Edit"
      ["Comment Region"             comment-region (and (mark) (not buffer-read-only))]
      ["Uncomment Region"           spice-uncomment-region (and (mark) (not buffer-read-only))]
      ["Comment bar"		    (spice-comment-bar 't) (not buffer-read-only)]
      ["Fill Paragraph (Break Line)" fill-paragraph (not buffer-read-only)]
      ["Join line"                  spice-delete-indentation (not buffer-read-only)]
      "--"
      ["Fontify..."                 font-lock-fontify-buffer t]
      "--"
      ["(Re)activate File links..." spice-colorize-libraries-buffer t]
      ["Load include/lib files..."  spice-load-include-files (assoc spice-imenu-libraries-submenu-name imenu--index-alist)]
      ["Load output file"           spice-load-output-file (spice-output-file-p)]
      ["Unload all other spice files" spice-unload-other-decks t]
      "--"
      ["Replace with .guess stats"  spice-replace-with-guess-statements (and (mark) (not buffer-read-only))]
      ["Replace with .nodeset stats"  spice-replace-with-nodeset-statements (and (mark) (not buffer-read-only))]
      "--"
      ["Hide Comment Regions"       spice-hide-all-comments (spice-hide-all-comments-p)]
      ["Unhide Comment Regions"     spice-unhide-all-comments spice-some-comment-regions-are-hidden]
      "--"
      ["Search .subckt def"         spice-search-subckt t]
      "--"
      ["Add Changelog Entry"        spice-add-changelog-entry (not buffer-read-only)]
      )
    (list
     "Simulate"
     ;;     ["Simulate"               compile t]
     ["Simulate"               spice-compile (and buffer-file-name (not (spice-output-p)))]
     ["Stop Simulation"        kill-compilation (condition-case ()
						    (get-buffer-process
						     (compilation-find-buffer))
						  (error nil))]
     "--"
     ["Next Error..."          spice-next-error t]
     ["Previous Error..."      spice-previous-error t]
     "--"
     (append
      '("Simulator")
      ;; example code taken literally from vhdl-mode.el !
      ;; add menu entries for defined simulators
      (let ((simu-alist spice-simulator-alist) menu-alist name)
	(while simu-alist
	  (setq name (car (car simu-alist)))
	  (setq menu-alist (cons (vector name
					 (list 'spice-set-simulator name)
					 :style 'radio :selected
					;					(list 'equal 'spice-simulator name)
                                   (list 'equal '(spice-get-simulator) name)
                                   )
				 menu-alist))
	  (setq simu-alist (cdr simu-alist)))
	(setq menu-alist
	      (cons '["Add Simulator..."
		      (customize-variable 'spice-simulator-alist) t]
		    (cons "--" menu-alist)))
	(nreverse menu-alist))))
    (list
     "Waveform viewer"
     ["View"                   spice-run-waveform-viewer
      (and spice-waveform-viewer-alist-entry
	   buffer-file-name)]
     ["Kill Waveform Viewer"   spice-kill-waveform-viewer
      (condition-case ()
	  (get-buffer-process
	   (spice-waveform-buffer-name-function nil))
	(error nil))]
     "--"
     (append
      '("Waveform Viewer")
      ;; example code taken literally from vhdl-mode.el !
      ;; add menu entries for defined simulators
      (let ((wave-alist spice-waveform-viewer-alist) menu-alist name)
	(while wave-alist
	  (setq name (car (car wave-alist)))
	  (setq menu-alist (cons (vector name
					 (list 'spice-set-waveform-viewer name)
					 :style 'radio :selected
					;(list 'equal 'spice-simulator name)
                                   (list 'equal
                                         '(spice-get-waveform-viewer)
                                         name))
				 menu-alist))
	  (setq wave-alist (cdr wave-alist)))
	(setq menu-alist
	      (cons '["Add Waveform Viewer..."
		      (customize-variable 'spice-waveform-viewer-alist) t]
		    (cons "--" menu-alist)))
	(nreverse menu-alist))))
    "--"
    '("Passive Elements"
      ("Resistors"
       ["Spice2g6 Resistor"        tempo-template-spice-spice2g6-resistor t]
       ["Spice3 Silicon Resistor"  tempo-template-spice-spice3-semiconductor-resistor t]
       ["Eldo Resistor"            tempo-template-spice-eldo-resistor (spice-standard-p 'eldo)]
       ["Eldo Expression Resistor" tempo-template-spice-eldo-expression-resistor (spice-standard-p 'eldo)]
       ["Eldo Silicon Resistor"    tempo-template-spice-eldo-semiconductor-resistor (spice-standard-p 'eldo)]
       ["Hspice Resistor"          tempo-template-spice-hspice-resistor (spice-standard-p 'hspice)]
       ["Layla Resistor"           tempo-template-spice-layla-resistor (spice-standard-p 'layla)]
       )
      ("Capacitors"
       ["Spice2g6 Capacitor"        tempo-template-spice-spice2g6-capacitor t]
       ["Spice3 Silicon Capacitor"  tempo-template-spice-spice3-semiconductor-capacitor t]
       ["Eldo Capacitor"            tempo-template-spice-eldo-capacitor (spice-standard-p 'eldo)]
       ["Eldo Expression Capacitor" tempo-template-spice-eldo-expression-capacitor (spice-standard-p 'eldo)]
       ["Hspice Capacitor"          tempo-template-spice-hspice-capacitor (spice-standard-p 'hspice)]
       ["Layla Capacitor"           tempo-template-spice-layla-capacitor (spice-standard-p 'layla)]
       )
      ("Inductors"
       ["Spice2g6 Inductor"        tempo-template-spice-spice2g6-inductor t]
       ["Spice2g6 Coupled Inductors" tempo-template-spice-spice2g6-coupled-inductors t]
       ["Eldo Inductor"            tempo-template-spice-eldo-inductor (spice-standard-p 'eldo)]
       ["Eldo Expression Inductor" tempo-template-spice-eldo-expression-inductor (spice-standard-p 'eldo)]
       ["Hspice Inductor"          tempo-template-spice-hspice-inductor (spice-standard-p 'hspice)]
       ["Layla Inductor"           tempo-template-spice-layla-inductor (spice-standard-p 'layla)]
       )
      ("Transmission lines"
       ["Spice2g6 Lossless"        tempo-template-spice-spice2g6-lossless-transmission t]
       ["Eldo Lossless"            tempo-template-spice-eldo-lossless-transmission (spice-standard-p 'eldo)]
       ["Hspice Lossless"          tempo-template-spice-hspice-lossless-transmission (spice-standard-p 'hspice)]
       ["Spice2g6 Lossy"           tempo-template-spice-spice2g6-lossy-transmission t]
       ["Spice3 RC line"           tempo-template-spice-spice3-rcline t]
       ["Eldo RC line"             tempo-template-spice-eldo-rcline (spice-standard-p 'eldo)]
       )
      )
    '("Active Elements"
      ("Diodes"
       ["Spice2g6 Diode"           tempo-template-spice-spice2g6-diode t]
       ["Eldo Diode"               tempo-template-spice-eldo-diode (spice-standard-p 'eldo)]
       ["Hspice Diode"             tempo-template-spice-hspice-diode (spice-standard-p 'hspice)]
       ["Layla Diode"              tempo-template-spice-layla-diode (spice-standard-p 'layla)]
       )
      ("Bipolars"
       ["Spice2g6 Bipolar"           tempo-template-spice-spice2g6-bipolar t]
       ["Eldo Bipolar"               tempo-template-spice-eldo-bipolar (spice-standard-p 'eldo)]
       ["Hspice Bipolar"             tempo-template-spice-hspice-bipolar (spice-standard-p 'hspice)]
       )
      ("Jfets & Mesfets"
       ["Spice2g6 Jfet"           tempo-template-spice-spice2g6-jfet t]
       ["Spice2g6 Mesfet"         tempo-template-spice-spice2g6-mesfet t]
       ["Eldo Jfet"               tempo-template-spice-eldo-jfet (spice-standard-p 'eldo)]
       ["Hspice Jfet"             tempo-template-spice-hspice-jfet (spice-standard-p 'hspice)]
       )
      ("Mosfets"
       ["Spice2g6 Mosfet"           tempo-template-spice-spice2g6-mosfet t]
       ["Eldo Mosfet"               tempo-template-spice-eldo-mosfet (spice-standard-p 'eldo)]
       ["Hspice Mosfet"             tempo-template-spice-hspice-mosfet (spice-standard-p 'hspice)]
       ["Layla Mosfet"              tempo-template-spice-hspice-mosfet (spice-standard-p 'layla)]
       )
      )
    '("Controlled Sources"
      ["Spice2g6 VCVS"         tempo-template-spice-spice2g6-vcvs t]
      ["Spice2g6 CCVS"         tempo-template-spice-spice2g6-ccvs t]
      ["Spice2g6 VCCS"         tempo-template-spice-spice2g6-vccs t]
      ["Spice2g6 CCCS"         tempo-template-spice-spice2g6-ccvs t]
      )
    '("Waveforms"
      ["PWL"                    (spice-pwl) t]
      ["pulse"                  tempo-template-spice-pulse t]
					;     ["ac"                     tempo-template-eldo-ac t]
      ["sine"                   tempo-template-spice-sine t]
      ["exp"                    tempo-template-spice-exp t]
      ["hspice sffm"            tempo-template-spice-hspice-sffm (spice-standard-p 'hspice)]
      ["hspice am"              tempo-template-spice-hspice-am (spice-standard-p 'hspice)]
      ["eldo pattern"           tempo-template-spice-eldo-pattern (spice-standard-p 'eldo)]
      ["eldo noise"             tempo-template-spice-eldo-noise (spice-standard-p 'eldo)]
      ["eldoRF four"            tempo-template-spice-eldorf-four (spice-standard-p 'eldorf)]
      ["eldoRF fpulse"          tempo-template-spice-eldorf-fpulse (spice-standard-p 'eldorf)]
      )
    "--"
    '("Eldo Macromodels"
      ("Analog"
       ["SO Comparator"           tempo-template-spice-eldo-comp (spice-standard-p 'eldo)]
       ["DO Comparator"           tempo-template-spice-eldo-compd (spice-standard-p 'eldo)]
       ["SO Linear Opamp"         tempo-template-spice-eldo-linear-opa0 (spice-standard-p 'eldo)]
       ["DO Linear Opamp"         tempo-template-spice-eldo-linear-opa0d (spice-standard-p 'eldo)]
       ["SO Linear 1-pole Opamp"  tempo-template-spice-eldo-linear-opa1 (spice-standard-p 'eldo)]
       ["DO Linear 1-pole Opamp"  tempo-template-spice-eldo-linear-opa1d (spice-standard-p 'eldo)]
       ["SO Linear 2-pole Opamp"  tempo-template-spice-eldo-linear-opa2 (spice-standard-p 'eldo)]
       ["DO Linear 2-pole Opamp"  tempo-template-spice-eldo-linear-opa2d (spice-standard-p 'eldo)]
       ["Delay"                   tempo-template-spice-eldo-delay (spice-standard-p 'eldo)]
       ["Saturating Resistor"     tempo-template-spice-eldo-satr (spice-standard-p 'eldo)]
       ["Voltage Limiter"         tempo-template-spice-eldo-satv (spice-standard-p 'eldo)]
       ["Voltage cont. switch"    tempo-template-spice-eldo-vswitch (spice-standard-p 'eldo)]
       ["Current cont. switch"    tempo-template-spice-eldo-cswitch (spice-standard-p 'eldo)]
       ["Triangular to sine converter"	tempo-template-spice-eldo-tri2sin (spice-standard-p 'eldo)]
       ["Staircase generator"	tempo-template-spice-eldo-stairgen (spice-standard-p 'eldo)]
       ["Sawtooth generator"	tempo-template-spice-eldo-sawgen (spice-standard-p 'eldo)]
       ["Triangle generator"	tempo-template-spice-eldo-trigen (spice-standard-p 'eldo)]
       ["Amplitude modulator"	tempo-template-spice-eldo-amm (spice-standard-p 'eldo)]
       ["Pulse amplitude modulator"	tempo-template-spice-eldo-pam (spice-standard-p 'eldo)]
       ["Sample&Hold"             tempo-template-spice-eldo-saho (spice-standard-p 'eldo)]
       ["Track&Hold"              tempo-template-spice-eldo-trho (spice-standard-p 'eldo)]
       ["Peak Detector"           tempo-template-spice-eldo-peakd (spice-standard-p 'eldo)]
       ["SO Level Detector"       tempo-template-spice-eldo-levdso (spice-standard-p 'eldo)]
       ["DO Level Detector"       tempo-template-spice-eldo-levddo (spice-standard-p 'eldo)]
       ["Log Amplifier"		tempo-template-spice-eldo-logamp (spice-standard-p 'eldo)]
       ["Antilog Amplifier"	tempo-template-spice-eldo-antilog (spice-standard-p 'eldo)]
       ["Differentiator"	tempo-template-spice-eldo-diff (spice-standard-p 'eldo)]
       ["Integrator"		tempo-template-spice-eldo-integ (spice-standard-p 'eldo)]
       ["Add/Sub/Mult/Div"	tempo-template-spice-eldo-adder (spice-standard-p 'eldo)]
       )
      ("Digital"
       ["Delay"			tempo-template-spice-eldo-delay (spice-standard-p 'eldo)]
       ["Inverter"		tempo-template-spice-eldo-inv (spice-standard-p 'eldo)]
       ["XOR gate"		tempo-template-spice-eldo-xor (spice-standard-p 'eldo)]
       ["2 input AND gate"	tempo-template-spice-eldo-and2 (spice-standard-p 'eldo)]
       ["2 input NAND gate"	tempo-template-spice-eldo-nand2 (spice-standard-p 'eldo)]
       ["2 input OR gate"	tempo-template-spice-eldo-or2 (spice-standard-p 'eldo)]
       ["2 input NOR gate"	tempo-template-spice-eldo-nor2 (spice-standard-p 'eldo)]
       ["3 input AND gate"	tempo-template-spice-eldo-and3 (spice-standard-p 'eldo)]
       ["3 input NAND gate"	tempo-template-spice-eldo-nand3 (spice-standard-p 'eldo)]
       ["3 input OR gate"	tempo-template-spice-eldo-or3 (spice-standard-p 'eldo)]
       ["3 input NOR gate"	tempo-template-spice-eldo-nor3 (spice-standard-p 'eldo)]
       )
      ("Mixed Signal"
       ["AD Converter"		tempo-template-spice-eldo-adc (spice-standard-p 'eldo)]
       ["DA Converter"		tempo-template-spice-eldo-dac (spice-standard-p 'eldo)]
       )
      ("Switched Cap"
       ["Opamp"			tempo-template-spice-eldo-switchcap-opa (spice-standard-p 'eldo)]
       ["Switch"		tempo-template-spice-eldo-switch (spice-standard-p 'eldo)]
       )
      )
    "--"
    '("Layla Objects"
      ["Port"                   tempo-template-layla-port (spice-standard-p 'layla)]
      ["Performance"            tempo-template-layla-performance (spice-standard-p 'layla)]
      ["Net"                    tempo-template-layla-net (spice-standard-p 'layla)]
      ["Bus"                    tempo-template-layla-bus (spice-standard-p 'layla)]
					;     ["ac"                     tempo-template-eldo-ac t]
      )
    '("Layla Properties"
      ("Port"
       ["Port double"           tempo-template-layla-port-double (spice-standard-p 'layla)]
       ["Port integer"          tempo-template-layla-port-integer (spice-standard-p 'layla)]
       ["Port string"           tempo-template-layla-port-string (spice-standard-p 'layla)]
       )
      ("Placement"
       ["Placement double"           tempo-template-layla-placement-double (spice-standard-p 'layla)]
       ["Placement integer"          tempo-template-layla-placement-integer (spice-standard-p 'layla)]
       ["Placement string"           tempo-template-layla-placement-string (spice-standard-p 'layla)]
       )
      ("Symmetry"
       ["Symmetry double"           tempo-template-layla-symmetry-double (spice-standard-p 'layla)]
       ["Symmetry integer"          tempo-template-layla-symmetry-integer (spice-standard-p 'layla)]
       ["Symmetry string"           tempo-template-layla-symmetry-string (spice-standard-p 'layla)]
       )
      ("Net"
       ["Net double"           tempo-template-layla-net-double (spice-standard-p 'layla)]
       ["Net integer"          tempo-template-layla-net-integer (spice-standard-p 'layla)]
       ["Net string"           tempo-template-layla-net-string (spice-standard-p 'layla)]
       )
      ("Device"
       ["Device double"           tempo-template-layla-device-double (spice-standard-p 'layla)]
       ["Device integer"          tempo-template-layla-device-integer (spice-standard-p 'layla)]
       ["Device string"           tempo-template-layla-device-string (spice-standard-p 'layla)]
       )
      ("Bus"
       ["Bus double"           tempo-template-layla-bus-double (spice-standard-p 'layla)]
       ["Bus integer"          tempo-template-layla-bus-integer (spice-standard-p 'layla)]
       ["Bus string"           tempo-template-layla-bus-string (spice-standard-p 'layla)]
       )
      )
    "--"
    (append
     '("Goto Section")
     (let ((section-alist spice-section-alist) menu-alist name str)
       (setq menu-alist
	     (cons "--"
		   (cons '["Changelog"
			   (spice-goto-section "changelog")
					; (setq menu-sec (current-time))
			   (spice-cache-section-p "changelog")
			   ] menu-alist)))
       (while section-alist
	 (setq name (car (car section-alist)))
	 (setq str (downcase (car (cdr (car section-alist)))))
	 (setq menu-alist (cons (vector name
					(list 'spice-goto-section str)
					(list 'spice-section-p str)
					)
				menu-alist))
	 (setq section-alist (cdr section-alist)))
       (setq menu-alist
	     (cons '["Specify..."
		     spice-goto-section t]
		   (cons "--" menu-alist)))
       (nreverse menu-alist))
     )
    (append
     '("Add Section Header")
     (let ((section-alist spice-section-alist) menu-alist name str)
       (setq menu-alist
	     (cons "--"
		   (cons '["Changelog"
			   (spice-add-section "Changelog")
			   (not (spice-section-p "changelog"))] menu-alist)))
       (while section-alist
	 (setq name (car (car section-alist)))
	 (setq str (car (cdr (car section-alist))))
	 (setq menu-alist (cons (vector name
					(list 'spice-add-section str)
					(list 'not (list 'spice-section-p (downcase str)))
					)
				menu-alist))
	 (setq section-alist (cdr section-alist)))
       (setq menu-alist
	     (cons '["Specify..."
		     spice-add-section t]
		   (cons "--" menu-alist)))
       (nreverse menu-alist))
     )
    )
   (spice-common-menu-tail-entries))
  )


(defun spice-create-output-mode-menu ()
  "Create Spice-output Mode menu."
  (append
   (list
    "Spice-output"
    '("Edit"
      ["Fontify..."                 font-lock-fontify-buffer t]
      "--"
      ["Unload all other spice files" spice-unload-other-decks t]
      "--"
      ["Create <name>_guess.cir file" (spice-create-guess-nodeset-file "guess") (spice-output-p)]
      ["Create <name>_nodeset.cir file" (spice-create-guess-nodeset-file "nodeset") (spice-output-p)]
      )
    )
   (spice-common-menu-tail-entries))
  )


(defun spice-common-menu-tail-entries ()
  "Creat Common Spice and Spice-output Mode menu entries."
  (list
   "--"
   ;; customize sub menu
   '("Customize"
     ["Browse Spice Group"    (customize-browse 'spice) t]
     ["Spice Standard"	      (customize-variable 'spice-standard) t]
     ["Spice Faces"	      (customize-group 'spice-faces) t]
     ["Section Header list"   (customize-variable 'spice-section-alist) t]
     ;; "--"
     ;; ["Activate Customizations in Buffer" spice-activate-customizations t]
     ;; (not (equal spice-standard-local spice-standard))]
     )
   ;; settings sub menu
   (append '("Settings")
	   (when (fboundp 'speedbar)
	     (list '["Speedbar"               speedbar-frame-mode
		     :style toggle
		     :selected (and (boundp 'speedbar-frame)
				    (frame-live-p speedbar-frame)
				    (frame-visible-p speedbar-frame))]))
	   (list
	    '["Abbrevs"	      abbrev-mode :style toggle :selected abbrev-mode]
	    '["List abbrevs"  list-abbrevs abbrev-mode]
	    '["Auto fill"     auto-fill-mode :style toggle
	      :selected auto-fill-function]))
   "--"
   ["About Spice-Mode"         spice-about t]
   )
  )


(defvar spice-menu-list nil
  "Spice Mode menu.") ; global variable

(defvar spice-output-menu-list nil
  "Spice-output Mode menu.") ; global variable

(require 'easymenu)

(defun spice-menu-init ()
  "Initializes global vars for Spice menu's"
  (setq spice-menu-list (spice-create-mode-menu))
  (setq spice-output-menu-list (spice-create-output-mode-menu))
  )

(defun spice-update-mode-menu ()
  "Updates Spice mode menu for current buffer." ; assumes globals have
                                        ; been updated
  (interactive)
  (if (spice-output-p)
      (progn
	(easy-menu-define spice-output-menu spice-output-mode-map
	  "Menu keymap for Spice-output Mode." spice-output-menu-list))
    (easy-menu-define spice-menu spice-mode-map
      "Menu keymap for Spice Mode." spice-menu-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spice mode syntax table

(defvar spice-mode-syntax-table nil
  "Syntax table used in spice-mode buffers.")

(defun spice-mode-syntax-table-init ()
  "initialize syntax table from scratch."
  (setq spice-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?$  "w"  spice-mode-syntax-table)
  (modify-syntax-entry ?!  "w"  spice-mode-syntax-table)
  (modify-syntax-entry ?*  "w"  spice-mode-syntax-table) ; not < !!!!
  (if (spice-standard-p 'eldo)
      (progn (modify-syntax-entry ?#  "w 13" spice-mode-syntax-table)
	     (modify-syntax-entry ?c  "w 2"  spice-mode-syntax-table)
	     (modify-syntax-entry ?e  "w 4"  spice-mode-syntax-table)))
  ;; (modify-syntax-entry ?\n "> b" spice-mode-syntax-table) ;; no no, comments are handled by spice-mode, not by syntax-table !!!
  (modify-syntax-entry ?\n "." spice-mode-syntax-table) ;; make it punctuation ?
  (modify-syntax-entry ?\" "\"" spice-mode-syntax-table) ;; font-lock prob
  ;; (modify-syntax-entry ?\" "w"  spice-mode-syntax-table)
  (modify-syntax-entry ?.  "w"  spice-mode-syntax-table)
  (modify-syntax-entry ?,  "w"  spice-mode-syntax-table)
  (modify-syntax-entry ?_  "w"  spice-mode-syntax-table)
  (modify-syntax-entry ?@  "w"  spice-mode-syntax-table)
  (modify-syntax-entry ?/  "w"  spice-mode-syntax-table)
  ;; (modify-syntax-entry ?<  "(>" spice-mode-syntax-table) ; can be in symbols
  ;; (modify-syntax-entry ?>  ")<" spice-mode-syntax-table) ; can be in symbols
  ;; (modify-syntax-entry ?+ "w"   spice-mode-syntax-table) ; can be in expr
  ;; (modify-syntax-entry ?- "w"   spice-mode-syntax-table) ; can be in expr
  ;; (modify-syntax-entry ?= "."   spice-mode-syntax-table) ; can be in expr
  )


;;;
;;; speedbar stuff
;;;

(eval-and-compile
  (when (fboundp 'speedbar)

    (require 'speedbar)

    (defun spice-speedbar-init ()
      "Initialize speedbar."
      ;; general settings, depends on auto-mode-alist, so should be
      ;; called every time auto-mode-alist is modified (set
      ;; (make-local-variable 'speedbar-tag-hierarchy-method) nil)
      ;; SPICE file extensions (extracted from `auto-mode-alist')
      (let ((mode-alist auto-mode-alist))
	(while mode-alist
	  (when (eq (cdr (car mode-alist)) 'spice-mode)
	    (speedbar-add-supported-extension (car (car mode-alist))))
	  (setq mode-alist (cdr mode-alist)))))


    (defun spice-speedbar (&optional arg)
      "Open/close speedbar."
      (interactive)
      (if (not (fboundp 'speedbar))
	  (error "WARNING: Speedbar is only available in newer Emacs versions")
	(condition-case ()	; due to bug in `speedbar-el' v0.7.2a
	    (speedbar-frame-mode arg)
	  (error "WARNING:  Install included `speedbar.el' patch first"))))
    )
  )

;;; speedbar end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Imenu: S-mouse3 in emacs to find spice objects quickly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'imenu)

(defvar spice-imenu-generic-expression nil
  "Imenu generic expression for spice mode. See `imenu-generic-expression'.")

(defconst spice-imenu-end-submenu-name "*End*"
  "label of End submenu in imenu")

(defconst spice-imenu-libraries-submenu-name "*Libraries*"
  "label of Libraries submenu in imenu")

(defun spice-imenu-init ()
  "initialize imenu generic expression and pass to imenu"
  (setq spice-imenu-generic-expression
	(append
	 (list
	  (list spice-imenu-end-submenu-name
		(concat
		 "^"
		 "\\.\\(end\\)\\>"
		 ) 1))
	 (when (spice-standard-p 'layla)
	   (list
	    (list "*Nets*"  (concat
			     "^\\*?"
			     "[\.]\\(net\\)\\s-+"
			     "\\([a-z0-9]\\w*\\)\\>"
			     ) 2)
	    (list "*Ports*"
		  (concat
		   "^\\*?"
		   "\\.\\(port\\)\\s-+"
		   "\\([a-z0-9]\\w*\\)\\>"
		   ) 2)
	    (list "*Performances*"
		  (concat
		   "^\\*?"
		   "\\.\\(performance\\)\\s-+"
		   "\\([a-z]\\w*\\)\\>"
		   ) 2)) ; list
	   )
	 (list
	  (list
	   "*Misc*"
	   (concat
	    "^\\s-*\\.model\\s-+" spice-model-name spice-line-break
	    "\\s-+\\("
	    (regexp-opt spice-misc-model-type-names)
	    "\\)\\>" )
	   1)
	  (list
	   "*Diodes*"
	   (concat "^\\s-*\\.model\\s-+" spice-model-name
		   spice-line-break "\\s-+d\\>")
	   1)
	  (list
	   "*Bipolars*"
	   (concat "^\\s-*\\.model\\s-+" spice-model-name
		   spice-line-break "\\s-+\\(npn\\|pnp\\)\\>")
	   1)
	  (list
	   "*Mosfets*"
	   (concat "^\\s-*\\.model\\s-+" spice-model-name
		   spice-line-break "\\s-+\\(n\\|p\\)mos\\>")
	   1)
	  (list spice-imenu-libraries-submenu-name
		(concat spice-library-regexp-start
			spice-library-regexp-end)
		3)
	  (list
	   "*Analyses*"
	   (concat "^\\s-*\\.\\("
		   (regexp-opt spice-analyses)
		   "\\)\\>")
	   1)
	  (list "*Sections*"
		spice-section-headings-regexp 2)
	  (list nil
		(concat
		 "^\\.\\(subckt\\s-+"
		 (when (spice-standard-p 'eldo)
		   "\\(lib\\s-+[^ \t\n]+\\s-+\\)?")
		 "\\|macro\\s-+\\)"
		 "\\([a-z]\\w*\\)\\>"
		 ) (if (spice-standard-p 'eldo) 3 2))
	  )
	 )))


;; ======================================================================
;; Support for compilation (simulation) - doesn't work 100% currently
;; and probably never will ...
;; ======================================================================

(defun spice-simulation-buffer-name-function (arg)
  "Derives unique spice simulation buffer for simulator output"
  (concat "*Spice-simulation-" (buffer-name) "*"))


(defun spice-get-simulator ()
  "Make an educated guess on what simulator a user likely wants to use"
  (if (and spice-simulator
	   (assoc spice-simulator spice-simulator-alist))
      spice-simulator ;; is specified by user, take his choice
    (if (spice-standard-p 'eldo) "Eldo" ;; eldo
      (if (spice-standard-p 'hspice) "Hspice" ;; Hspice
	"Spice3")))) ;; fallback Spice3


(defun spice-set-simulator-command ()
  (interactive)
  (setq compile-command
	(let ((commands-alist spice-simulator-alist)
	      command)
	  (while commands-alist
	    (when (equal (spice-get-simulator) (nth 0 (car commands-alist)))
	      (setq command
		    (concat
		     (nth 1 (car commands-alist)) " "
		     spice-simulator-switches
		     (unless
			 (string-equal spice-simulator-switches "") " ")
		     (file-name-nondirectory (if buffer-file-name
						 buffer-file-name ""))
		     (unless
			 (string-equal (nth 2 (car commands-alist)) "") " ")
		     (nth 2 (car commands-alist))
		     )))
	    (setq commands-alist (cdr commands-alist)))
	  command)))

(defun spice-set-simulator (name)
  (setq spice-simulator name)
  (spice-set-simulator-command))

(require 'compile)

(defvar spice-compilation-error-regexp-alist nil)
(defvar spice-compilation-file-regexp-alist nil)

(defun spice-compile ()
  "spice wrapper function for compile."
  (interactive)
  (spice-set-simulator-command)
  (call-interactively 'compile nil))

(defun spice-compile-variables-init ()
  "build variable lists."
  (setq spice-compilation-error-regexp-alist
	(let ((commands-alist spice-simulator-alist)
	      regexp-alist sublist)
	  (while commands-alist
	    (setq sublist (nth 3 (car commands-alist)))
	    (unless (equal "" (car sublist))
	      (setq regexp-alist
		    (cons (append
			   (list (nth 0 sublist)
				 (if (= 0 (nth 1 sublist))
				     nil
				   (nth 1 sublist))
				 (if (numberp (nth 2 sublist))
				     (nth 2 sublist)
				   (nth 2 sublist)))
			   (list (nth 3 sublist))
			   (if (nth 4 sublist)
			       (if (stringp (nth 4 sublist))
				   (list (nth 4 sublist))
				 (list (eval (nth 4 sublist))))))
			  regexp-alist)))
	    (setq commands-alist (cdr commands-alist)))
	  regexp-alist))
                                        ; (message "setting compilation file regexps command")
  (setq spice-compilation-file-regexp-alist
	(let ((commands-alist spice-simulator-alist)
	      regexp-alist)
	  (while commands-alist
	    (unless (equal "" (car (nth 4 (car commands-alist))))
	      (setq regexp-alist
		    (append regexp-alist
			    (list (nth 4 (car commands-alist))))))
	    (setq commands-alist (cdr commands-alist)))
	  regexp-alist))
  )


(defun spice-compile-init ()
  "Initialize for simulation(/compilation)."

  ;;------------------------------------------------------------
  ;; use Eldo/Hspice as compiler, on current buffer
  (make-local-variable  'compile-command)
  (make-local-variable  'compilation-read-command)
  (make-local-variable  'compilation-buffer-name-function)
  (make-local-variable  'compilation-error-regexp-alist)
  (make-local-variable  'compilation-file-regexp-alist)

  (setq compilation-read-command 't)
  (setq compilation-buffer-name-function
	'spice-simulation-buffer-name-function)

                                        ; (message "setting compilation error regexps command")
  (setq compilation-error-regexp-alist spice-compilation-error-regexp-alist
	compilation-file-regexp-alist spice-compilation-file-regexp-alist)
  )


(defvar spice-column 1
  "global variable to do column hack. Why ? Compile.el requires that
every error is 'special', ie. different from the previous one. This is
either the file is different, the line number is different or the column
number is different. Prob: the file is always the same, the simulator
guys never output the file name of the file that is being read, so that
one doesn't change; the line number can not be found either; the column
number is same problem; Solution: always take line number one, then take
the column number 1 or 2 alternatingly, remember previous value here !

This is ugly, I know, but it's the only way I could think of to find
the errors in the simulation buffer with compile.el")


(defun spice-linenum (f c)
                                        ;(message (format "calling linenum fun '%s'" f))
  (save-excursion
    (set-buffer compilation-last-buffer)
                                        ;(message (format "buffer '%s'" (buffer-name)))
    )
  (list (point-marker) f 1 (if (= spice-column 1)
			       (setq spice-column 2)
			     (setq spice-column 1))))


(defun spice-next-error (n)
  "Move point to the next error in the compilation buffer.
Does NOT find the source line like \\[next-error] does, is defined
in spice-mode since many simulators don't output errors with source line
numbers included, so finding the error is still difficult."
  (interactive "p")
  (save-excursion
    (let (prev-buffer)
      (setq prev-buffer (buffer-name))
      (pop-to-buffer compilation-last-buffer)
      (compilation-next-error n)
      (recenter)
      (pop-to-buffer prev-buffer))))


(defun spice-previous-error (n)
  "Move point to the previous error in the compilation buffer.
Does NOT find the source line like \\[next-error] does, is defined
in spice-mode since many simulators don't output errors with source line
numbers included, so finding the error is still difficult."
  (interactive "p")
  (save-excursion
    (let (prev-buffer)
      (setq prev-buffer (buffer-name))
      (pop-to-buffer compilation-last-buffer)
      (compilation-previous-error n)
      (recenter)
      (pop-to-buffer prev-buffer))))


;;------------------------------------------------------------
;; Waveform viewer support
;;------------------------------------------------------------
;; This is work in progress; (user) interface might change

(defun spice-waveform-buffer-name-function (arg)
  "check running process"
  (let ((name (if arg arg (buffer-name))))
    (concat "*Spice-waveform-" name "*")))


(defun spice-set-waveform-viewer (name)
  (setq spice-waveform-viewer name)
  (spice-set-waveform-viewer-command))

(defun spice-get-waveform-viewer ()
  "Make an educated guess on what waveform viewer a user likely would want"
  (if (and spice-waveform-viewer
	   (assoc spice-waveform-viewer spice-waveform-viewer-alist))
      spice-waveform-viewer ;; is specified by user, take his choice
    (if (spice-standard-p 'eldo) "Xelga" ;; eldo's viewer is xelga
      (if (spice-standard-p 'hspice) "Awaves" ;; Hspice's viewer is awaves
	"Nutmeg")))) ;; fallback Spice3's viewer nutmeg ...


(defun spice-waveform-viewer-derive-filename (arg)
  "Derive from the buffer file name the name of a derived file. If ARG is
a string, it is a suffix to replace the buffer's suffix, if ARG is a list
, , if ARG is a function, the function is called without arguments and it
should return the derived filename, if ARG is nil, nil is returned, if ARG
is t, the filename itself is returned unmodified."
  (cond ((stringp arg)
	 (concat (file-name-sans-extension buffer-file-name) arg))
	((listp arg)
	 (let ((alist arg)
	       filename)
                                        ;(message alist)
	   (while alist
	     (message (concat (file-name-sans-extension buffer-file-name) (car alist)))
	     (setq
	      filename
	      (concat (file-name-sans-extension buffer-file-name) (car alist)))
	     (setq alist (cdr alist)))
	   filename))
	((functionp arg)
	 (funcall arg))
	(nil
	 nil) ;; nil if nil ...
	(t
	 buffer-file-name) ;; fallback if t ...
	)
  )

(defvar spice-waveform-viewer-command ""
  "variable containing buffer local waveform viewer command")

(defvar spice-waveform-viewer-filename nil
  "variable filename field of waveform viewer structure")

(defvar spice-waveform-viewer-alist-entry nil
  "variable holding selected entry of waveform viewer")

(defvar spice-waveform-viewer-read-command t
  "variable containing boolean indicating reading of buffer local waveform viewer command")

(defvar spice-after-start-process-function nil
  "variable containing after start process function")

(defun spice-set-waveform-viewer-command ()
  (interactive)
  (setq spice-waveform-viewer-alist-entry nil)
  (let ((commands-alist spice-waveform-viewer-alist))
    (while commands-alist
      (when (equal (spice-get-waveform-viewer)
		   (nth 0 (car commands-alist)))
	(setq spice-waveform-viewer-alist-entry (car commands-alist))
	)
      (setq commands-alist (cdr commands-alist)))))


(defun spice-waveform-viewer-init ()
  "Initialize for waveform viewer."

  ;;------------------------------------------------------------
  (make-local-variable 'spice-waveform-viewer-command)
  (make-local-variable 'spice-waveform-viewer-read-command)
  (make-local-variable 'spice-waveform-viewer-filename)
  (make-local-variable 'spice-waveform-viewer-alist-entry)
  (make-local-variable 'spice-after-start-process-function)

  (setq spice-waveform-viewer-read-command 't)

  (spice-set-waveform-viewer-command))


(defun spice-run-waveform-viewer ()
  "run the waveform viewer if it is not yet running."
  (interactive)
  (if (not buffer-file-name)
      (message "Can not run waveform viewer on unsaved file-less buffers.")
    (let (name command file)
      (setq name (nth 0 spice-waveform-viewer-alist-entry)
	    command (concat (nth 1 spice-waveform-viewer-alist-entry)
			    " "
			    (nth 2 spice-waveform-viewer-alist-entry)
			    " "
			    (spice-waveform-viewer-derive-filename
			     (nth 4 spice-waveform-viewer-alist-entry)))
	    file (buffer-name))
      (spice-process-check file)
      (message "Starting waveform viewer %s" name)
      (funcall (nth 3 spice-waveform-viewer-alist-entry) name command file))
    ) ; if (else part)
  )


(defun spice-master-directory ()
  "Directory of master file."
  (abbreviate-file-name
   (expand-file-name
    (file-name-directory (buffer-file-name)))))


(defun spice-run-silent (name command file)
  "Start process with (optional) second argument."
  (let ((dir (spice-master-directory)))
    (set-buffer (get-buffer-create "*spice silent*"))
    (erase-buffer)
    (if dir (cd dir))
                                        ;    (message "cd to %s" dir)
    (let ((process (start-process (concat name " silent")
				  (current-buffer)  ; can be nil
				  spice-shell
				  spice-shell-command-option
				  command)))
      (message "started %s" command)
      (if spice-after-start-process-function
	  (funcall spice-after-start-process-function process))
      (process-kill-without-query process))))

(defun spice-run-interactive (name command file)
  "Run waveform viewer interactively.
Run command in a buffer (in comint-shell-mode) so that it accepts user
interaction."
  (require 'comint)
  (let (; (default spice-command-default)
	(buffer (spice-waveform-buffer-name-function file))
	(process nil)
	(dir (spice-master-directory)))
    (spice-process-check file)		; Check that no process is running
                                        ; (setq spice-command-buffer (current-buffer))
    (with-output-to-temp-buffer buffer)
    (set-buffer buffer)
    (setq buffer-read-only nil)
    (if dir (cd dir))
    (insert "Running `" name "' on `" file "' with ``" command "''\n")
    (comint-exec buffer name spice-shell nil
		 (list spice-shell-command-option command))
    (comint-mode)
    (setq mode-name name)
                                        ; (setq spice-command-default default)
    (setq process (get-buffer-process buffer))
    (if spice-after-start-process-function
        (funcall spice-after-start-process-function process))
                                        ; (spice-command-mode-line process) ; mode line setting
                                        ; (set-process-sentinel process 'spice-command-sentinel)
    (set-marker (process-mark process) (point-max))
    ;;; (setq compilation-in-progress (cons process compilation-in-progress))
                                        ; (spice-parse-reset)
                                        ; (setq spice-parse-function 'spice-parse-spice)
                                        ; (setq spice-sentinel-function 'spice-LaTeX-sentinel)
    ))

(defun spice-command-mode-line (process)
  "Format the mode line for a buffer containing output from PROCESS."
  (setq mode-line-process (concat ": "
                                  (symbol-name (process-status process))))
  (set-buffer-modified-p (buffer-modified-p)))

(defun spice-process-check (name)
  "Check if a process for the spice deck NAME already exist.
If so, give the user the choice of aborting the process or the current
command."
  (let ((process (get-buffer-process
		  (spice-waveform-buffer-name-function name))))
                                        ;    (message "checking %s for running process" (spice-waveform-buffer-name-function name))
    (cond ((null process))
	  ((not (eq (process-status process) 'run)))
	  ((yes-or-no-p (concat "Process `"
				(process-name process)
				"' for deck `"
				name
				"' running, kill it? "))
	   (delete-process process))
	  (t
	   (error "Cannot have two processes for the spice deck")))))


;; ---- setting commands ?

(defun spice-set-command ()
  "Sets both simulator an waveform viewer commands for current buffer."
  (interactive)
  (spice-set-simulator-command)
  (spice-set-waveform-viewer-command)
  )


;;------------------------------------------------------------
;; File initialization support, code taken from eldo-mode and
;; modified/extended
;;------------------------------------------------------------

;; initialize empty file:
(defun spice-initialize-empty-file ()
  "Create a standard template for a new/empty file.

This is the default initialization function. If the user has specified
an initialization function, by setting
`spice-initialize-file-function', the user-specified function is
called instead. This function first checks for the template file
specified in `spice-initialization-file' which is inserted and a default
changelog entry is added. If this file isn't readable, a default
template is inserted depending on the submode (eldo, hspice or layla)
that has been selected."
  (interactive "*") ; read-only check
  (if (file-readable-p spice-initialize-template-file) ;; template file
      (insert-file-contents spice-initialize-template-file)
    ;; this has been taken from eldo-mode and thus only applies if
    ;; eldo is selected
    (if (spice-standard-p 'eldo)
	(progn
	  (insert
	   (concat "# " (buffer-name) " "
		   "\n.notrc\n.nocom"
		   "\n\n"
		   spice-default-header
		   "\n\n"))
	  (spice-add-section "LIBRARIES")
	  (insert "\n\n\n")
	  (spice-add-section "SIMULATION OPTIONS")
	  (insert
	   (concat "\n\n"
		   ".options STAT=1 SIMUDIV=10 !Status reports\n"
		   ".options noascii nomod    \n"
		   ".options eps=1e-7 itol=1e-6 gmin=1e-16 analog \n"
		   ".options nobound_phase"
		   ".width out=80 \n"
		   ".temp=27 \n"
		   "\n\n\n"))
	  (spice-add-section "SUPPLIES/REFERENCES")
	  (insert  "\n\n.END\n\n\n\n")
	  (spice-add-section "Changelog")
	  (insert "\n\n*** Local Variables:\n*** mode:spice\n*** End:\n")
	  )
      (if (spice-standard-p 'hspice) ;; hspice specific options
	  (progn
	    (insert
	     (concat "* " (buffer-name) " "
		     "\n\n"
		     spice-default-header
		     "\n\n"))
	    (spice-add-section "LIBRARIES")
	    (insert "\n\n\n")
	    (spice-add-section "SIMULATION OPTIONS")
	    (insert
	     (concat "\n\n"
		     ".options nomod nopage opts \n"
		     ".options itl1=5000 itl2=2500 itl3=20 itl4=20 itl5=0 \n"
		     ".options numdgt=10 $ print 10 digits in output \n"
		     ".width out=80 \n"
		     ".temp=27 \n"
		     "\n\n\n"))
	    (spice-add-section "SUPPLIES/REFERENCES")
	    (insert  "\n\n.end\n\n\n\n")
	    (spice-add-section "Changelog")
	    (insert "\n\n*** Local Variables:\n*** mode:spice\n*** End:\n")
	    )
	(if (spice-standard-p 'layla) ;; layla specific options
	    (progn
	      (insert
	       (concat "* " (buffer-name) " "
		       "\n"
		       spice-default-header
		       "\n\n"))
	      (spice-add-section "MAIN CIRCUIT")
	      (insert  "\n\n\n")
	      (spice-add-section "PORTS")
	      (insert  "\n\n.end\n\n\n\n")
	      (spice-add-section "Changelog")
	      (insert "\n\n*** Local Variables:\n*** mode:spice\n*** End:\n")
	      )
	  )
	)
      )
    )
  (spice-add-changelog-entry "File created") ; in any case
  )



;;------------------------------------------------------------
;; Hacks to implement the find function menu bar for spice-mode
;; subckts/models Fortunately spice-mode only provides one means of
;; abstraction so the parsing is very easy.  (only available in
;; XEmacs, remove this and spice-mode entry when compiling for emacs
;; to avoid warnings) This code has been taken from eldo-mode.el
;; (E. Rouat)
;;------------------------------------------------------------

(eval-and-compile
  (when (fboundp 'function-menu)
    (require 'func-menu)

    (defconst fume-function-name-regexp-spice
      "^\\.\\(subckt\\|model\\|macro\\)\\s-+\\([a-z]\\w*\\)"
      "Expression to parse Spice subcircuit and model names.")

    (defun fume-find-next-spice-function-name (buffer)
      "Searches for the next spice subcircuit name in BUFFER."
      (set-buffer buffer)
      (setq case-fold-search 't)		;;otherwise func-menu bombs....
      (if (re-search-forward fume-function-name-regexp nil t)
	  (let ((beg (match-beginning 2))
		(end (match-end 2)))
	    (cons (buffer-substring beg end) beg))))
    ) ; when
  )


(defun spice-func-menu-init ()
  "Initialize function menu." ; buffer local stuff
  ;; hook in the spice-mode mode regular expression above into the
  ;; association list of regexps used by the function menu generator
  (setq fume-function-name-regexp-alist
	(purecopy
	 (append
	  fume-function-name-regexp-alist
	  (list
	   '(spice-mode . fume-function-name-regexp-spice)))))

  ;; hook in the search method above into the association list used by the
  ;; function menu generating code
  (setq fume-find-function-name-method-alist
	(purecopy
	 (append
	  fume-find-function-name-method-alist
	  (list '(spice-mode . fume-find-next-spice-function-name)))))

  ;; Now activate func-menu - I hope that these settings don't
  ;; interfere with users settings
  (make-local-variable 'fume-menubar-menu-name)
  (make-local-variable 'fume-buffer-name)
  (make-local-variable 'fume-index-method)
  (setq fume-menubar-menu-name "Subckts"
	fume-buffer-name "*Subcircuits List*"
	fume-index-method 2)

  (add-hook 'find-file-hooks 'fume-add-menubar-entry)
  (define-key global-map '(shift button2) 'fume-mouse-function-goto)
  (fume-add-menubar-entry))


;; ======================================================================
;; Support for .subckt search !?
;; ======================================================================

;; What about searching from an included file, how to find the
;; top-level then ?  think I've cracked it: .end identifies top-level
;; spice files so remember the last one to start search from if the
;; search fails

;; BUG: doesn't handle nested .subckt defs ! Reports first found match...

(defun spice-search-included-files (subckt)
  (save-excursion
    (let ((mrk nil))
      (goto-char (point-min))
      (while (and
	      (search-forward-regexp spice-library-regexp-start
				     (point-max) t)
	      (not mrk))
	(beginning-of-line)
	(if (looking-at (concat spice-library-regexp-start
				spice-library-regexp-end))
	    (if (file-readable-p (substitute-in-file-name (match-string 3)))
		(setq mrk
		      (spice-search-file-for-subckt
		       (substitute-in-file-name (match-string 3)) subckt))
	      (message "File '%s' isn't readable" (match-string 3))) ; if
	  ) ; if
	(end-of-line)) ; while
      mrk) ; let
    ) ; save-
  )


(defvar spice-subckt-search-master-filename nil
  "latest top-level (identified by .end in file) .cir file used in
subcircuit searches.")

(defun spice-search-file-for-subckt (filename subckt)
  "Searches a file for a .subckt definition. Remembers
`spice-subckt-search-master-filename' for future subckt searches."
  (save-excursion
    (set-buffer (find-file-noselect filename))
    (condition-case nil
	(let ((index-alist (imenu--make-index-alist t))
	      (mrk nil))
	  (if (assoc spice-imenu-end-submenu-name index-alist)
	      (setq spice-subckt-search-master-filename buffer-file-name))
	  (setq mrk (assoc-ignore-case subckt index-alist))
	  (if mrk mrk
	    (spice-search-included-files subckt))
	  )
      (error nil))))


;; History of subckt searches.
(defvar spice-subckt-search-history nil
  "History of subcircuit searches.")

(defun spice-guess-subckt-name ()
  "guesses name of subckt from context, multiple lines"
  (let ((subckt "")) ; (current-word)
    (save-excursion
      (beginning-of-line)
      (while (and (looking-at "^+")
		  (not (forward-line -1))))
      (if (looking-at spice-xinstance-regexp)
	  (progn
	    ;; (message "Could it be '%s' ?" (match-string 3))
	    (setq subckt (match-string 5))
	    (remove-text-properties 0 (length subckt) '(face nil) subckt)
	    ))) ; save-
    subckt))


(defun spice-visit-subckt-def (mrk)
  "Helper function visiting buffer and mark specified."
  (if (eq (marker-buffer (cdr mrk))
	  (current-buffer))
      (if (not (and transient-mark-mode mark-active)) ; emacs, check if active region
	  (push-mark)))
  (pop-to-buffer (marker-buffer (cdr mrk)) t)
  (widen)
  (goto-char (cdr mrk)))


(defun spice-search-subckt (subckt-args)
  "Searches for the .subckt definition with name under cursor, or any other
name specified by user. Be CAREFUL using this command. Depending on the
structure of your spice decks this might find wrong definitions. To AVOID any
such problems always start searching from the TOP-LEVEL spice deck (ie. the
file that is supplied to the simulator). If you start searching from an
included file, potentially the definition is not found or it is found starting
from ANOTHER top-level file (which could result in a completely wrong
search result).
This search command places the mark if search result is in the same file,
return to the search start position by using C-u C-<SPC> or C-u C-@."
  (interactive
   (list (let* ((default-subckt (spice-guess-subckt-name))
		(input (read-from-minibuffer
			"Subcircuit name: "
			default-subckt nil nil
			spice-subckt-search-history)))
	   (if (string= input "")
	       (if (string= default-subckt "")
		   (error "No subckt args given")
		 default-subckt)
	     input))))
                                        ;(message (format "name of subckt is %s" subckt-args))
  (let (mrk)
    (setq mrk (spice-search-file-for-subckt buffer-file-name subckt-args))
                                        ; (message (format "mark is %s" (cdr mrk)))
    (if (and (cdr mrk) (markerp (cdr mrk)))
	(spice-visit-subckt-def mrk)
      (progn
	(message "Couldn't find subcircuit '%s', retrying search in top-level file"
		 subckt-args))) ; if
    (if (and (not mrk)
	     spice-subckt-search-master-filename)
	(progn
	  (setq mrk (spice-search-file-for-subckt
		     spice-subckt-search-master-filename subckt-args))
	  (if (and (cdr mrk) (markerp (cdr mrk)))
	      (progn
		(spice-visit-subckt-def mrk)
		(message "Used top-level file '%s' to find '%s'"
			 spice-subckt-search-master-filename subckt-args)
		)
	    (progn
	      (message
	       (format "Couldn't find subcircuit '%s', retry search in top-level file"
		       subckt-args))
	      ))))))


;; ======================================================================
;; loading of include files of current deck.

(defun spice-load-include-files (&optional non-recursive)
  "Loads all files that are included in this deck. Makes it more easy
to load a project. This loading occurs recursively. Files already
loaded are not reloaded or scanned for .includes. This function is
only guaranteed to work when all included files are not already loaded."
  (interactive)
  (let ((index-alist (imenu--make-index-alist t))
	l filename)
    (if (setq l (cdr (assoc spice-imenu-libraries-submenu-name index-alist))) ;; file contains include files/libraries
	(while l
	  (setq filename (expand-file-name
			  (substitute-in-file-name (car (car l)))))
	  ;;(message "Trying to load %s" filename)
	  (if (and (file-readable-p filename)
		   (not
		    (assoc filename ;; already loaded
			   (mapcar
			    (lambda (buffer)
			      (cons (buffer-file-name buffer) buffer))
			    (buffer-list)))))
	      (save-excursion
		;; (message "filename is %s" filename)
		(set-buffer (find-file-noselect filename))
		;; (spice-mode) ? ref. discussion Manu
		(unless (or non-recursive
			    (not (eq major-mode 'spice-mode)))
                  (spice-load-include-files))))
	  (setq l (cdr l))))))


;; ======================================================================
;; unloading of spice files except current deck.

(defun spice-unload-other-decks ()
  "Kills all other spice files except current one. Makes it easy to
unload a lot of spice files without restarting emacs."
  (interactive)
  (save-excursion
    (let ((current (current-buffer)))
      (mapcar
       (lambda (buffer)
	 (set-buffer buffer)
	 (if (and (eq major-mode 'spice-mode)
		  (not (eq current buffer)))
	     (progn
	       (message "Killing %s" buffer)
	       (kill-buffer buffer))))
       (buffer-list)))))


;; ======================================================================
;; folding for commented out regions ...
;;; taken from and adapted:
;;; Filename: foldingo.el
;;; Author: Christian Queinnec <Christian.Queinnec@lip6.fr>
;; This is work in progress; (user) interface might change

(defvar spice-some-comment-regions-are-hidden nil
  "Keeps track if some comment regions are hidden.")
(make-variable-buffer-local 'spice-some-comment-regions-are-hidden)

(defvar spice-last-hide-comment-regions-tick nil
  "Keeps track when last time comment hiding was called.")
(make-variable-buffer-local 'spice-last-hide-comment-regions-tick)

(defun spice-hide-init ()
  "Initialize buffer local variables to make hiding of spice regions
effective"
  ;; trick to make a comment-start/padding dependent regexp:
  (custom-initialize-reset 'spice-hide-line-prefix
			   (car (get 'spice-hide-line-prefix
				     'standard-value)))
  ;; make sure we have invisibility property working, local for a buffer
  (if (fboundp 'add-to-invisibility-spec)
      (add-to-invisibility-spec 'spice-mode))
  ;;  (remove-from-invisibility-spec '(spice-mode . t))
  ;; local for buffers:
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (if spice-auto-hide-comments
      (spice-hide-all-comments))
  )

(defun spice-hide (from to)
  "Not supported, don't use this."
  (interactive "*r")
  (spice-hide-region from to t))

(defun spice-unhide (from to)
  "Not supported, don't use this."
  (interactive "*r")
  (spice-hide-region from to nil))

(defun spice-hide-all-comments-p ()
  "Checks if there are comments that can be hidden...; assumes that if
the user edited the file, new comments might have been created and
thus hide all comments must be activated. This is not the best
criterion, but it is safe."
  ;;  (interactive)
  ;;  (message "buffer tick is %s, last hide comment is %s"
  ;;    (buffer-modified-tick) spice-last-hide-comment-regions-tick)
  (if (and
       spice-last-hide-comment-regions-tick ; can be nil
       (= spice-last-hide-comment-regions-tick (buffer-modified-tick)))
      nil t))

(defun spice-hide-all-comments ()
  "Hides all commented out regions in the current spice deck. Allows
to get a better overview of the deck if many lines are commented out.
It relies on having all regions being commented out using the
\"Comment Region\" menu entry that uses `comment-region'. The regexp
used to match comment lines, `spice-hide-line-prefix', might identify
standard spice comment lines used for documentation. To avoid
problems, make sure documentation is not part of comments. For
instance by using the doc starters available in the spice languages,
or by making sure they are different from the comment lines generated
with the menu entry, by placing two *'s when the default comment is
only one *. If you want to unhide all the hidden comment lines, use
`spice-unhide-all-comments'."
  (interactive)
  (spice-unhide-all-comments)
  (setq spice-last-hide-comment-regions-tick (buffer-modified-tick))
  (save-excursion
    (goto-char (point-min))
    ;;    (message "beginning of buffer reached %s" (point))
    (forward-line)
    (while (search-forward-regexp (concat "^" spice-hide-line-prefix)
				  (point-max) t)
      (beginning-of-line)
      ;;      (message "reached %s" (point))
      (let ((beg (point))
	    end
	    lines)
        ;;	(message "found start of regexp %s" (point))
	(setq lines (forward-line))
	;; (message "%s left" lines)
	;; the empty lines following commented lines are also hidden
	(while (and (looking-at (concat "^\\(" spice-hide-line-prefix "\\|[ ]*$\\)"))
		    (= (setq lines (forward-line)) 0))
	  ;; (message "%s left" lines)
	  )
	(setq end (point))
	(spice-hide-region beg end t))))
  (set-spice-name)
  )

(defun spice-unhide-all-comments ()
  "Unhides all hidden comment regions."
  (interactive)
  (setq spice-some-comment-regions-are-hidden nil)
  (setq spice-last-hide-comment-regions-tick nil)
  (spice-hide-region (point-min) (point-max) nil)
  (set-spice-name)) ;; update mode-line

(defun spice-hide-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.  If FLAG
is nil then the text is shown, while if FLAG is t the text is hidden."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char from)
                                        ;      (foldingo-discard-overlays (point) to 'invisible 'spice-comment)
      (spice-discard-overlays from to 'spice-comment)
      (if flag
	  (let ((overlay (make-overlay (point) to)))
	    (spice-make-overlay-hidden overlay))))))

(defun spice-make-overlay-hidden (overlay)
  ;; Make overlay hidden and intangible.
  ;;  (overlay-put overlay 'intangible t)
  (overlay-put overlay 'invisible 'spice-mode)
  (overlay-put overlay 'spice-comment t)
  (setq spice-some-comment-regions-are-hidden t)
  ;;  (overlay-put overlay 'intangible t)
  )

(defun spice-discard-overlays (from to prop)
  "discards overlays in region FROM to TO that have property PROP set."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char from)
      (while (< (point) to)
	(let ((overlays (overlays-at (point))))
	  (while overlays
	    (let ((o (car overlays)))
	      (if (overlay-get o prop)
		  (delete-overlay o)))
	    (setq overlays (cdr overlays))))
	(goto-char (next-overlay-change (point)))))))


;; ======================================================================
;; utility spice-mode-output functions

(defun spice-check-output-mode ()
  "Check if current buffer is output file and return symbol or NIL."
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "1\\*\\*\\*\\*\\*\\*\\*") ; eldo
        'eldo
      (if (or (looking-at "Using: ")
              (looking-at " \\*\\*\\*\\*\\*\\*  Star-HSPICE")) ; hspice
          'hspice
        nil))))

(defun spice-output-p ()
  "Check if current buffer is output file."
  (interactive)
  (if spice-output-local t nil))

(defconst spice-output-sections-regexp
  (concat
   "^"
   "\\(0\\*\\*\\*\\*\\|  \\*\\*\\*\\*\\*\\*\\) *"
   "\\( [a-z0-9]\\w*\\(\\s-+\\w+\\)* \\)\\s-+\\(t[a-z]*\\) ?=.*$")
  "Regexp identifying spice output headers (both hspice and eldo).")

(defvar spice-output-font-lock-keywords
  (list
   (list spice-output-sections-regexp
	 '(1 font-lock-comment-face)
	 '(2 spice-title-face)
	 (list 4 spice-constant-face))
   (list "^[0-1] ?\\*.*$" 0 font-lock-comment-face)
   (list "^ \\(\\*\\*\\*\\*\\*?\\) \\(.*\\)$"
	 '(1 font-lock-comment-face)
	 '(2 font-lock-type-face))
   (list "^\\(FATAL \\)?ERROR.+$" 0 'font-lock-warning-face) ; eldo errors
   (list "^\\s-*\\(..?error..?[: ]\\).+$" 0 'font-lock-warning-face) ; hspice errors
   )
  "List of regexps for font-lock in output mode."
  )

(defun spice-mode-output ()
  "Spice major mode start up function for spice output files. Experimental ;)"

  (setq buffer-read-only t) ; don't want to edit output files, do we
                                        ; common mistake in hspice output files!

  ;; use local keymap (for keys and menu)
  (use-local-map spice-output-mode-map)

  ;; set menu for local buffer
  (easy-menu-define spice-output-menu spice-output-mode-map
    "Menu keymap for Spice Output Mode." spice-output-menu-list)

  ;; font-lock local start-up
  (set (make-local-variable 'font-lock-defaults)
       (list 'spice-output-font-lock-keywords
	     nil t (list (cons ?\" "w")))) ; nil, t (do multiline
                                        ; comments)

  ;; imenu buffer local init
  (set (make-local-variable 'imenu-case-fold-search) t)
  (set (make-local-variable 'imenu-generic-expression)
       (list (list "*Errors*"
		   "^\\s-*\\(\\(..?error..?[: ]\\|\\(FATAL \\)?ERROR\\).+$\\)"
		   1)
	     (list nil ;; "*Sections*"
		   spice-output-sections-regexp
		   2)))
  )


(defun spice-output-filename ()
  "Determines output filename of current spice deck."
  (interactive)
  (let ((filename nil)
	(l spice-output-filename-alist))
    (while l
      (if (spice-standard-p (car (car l)))
	  (progn
	    (setq filename (eval (car (cdr (car l)))))
	    ;; (message "Checking filename %s" filename)
	    (if (file-readable-p filename)
		(setq l nil)
	      (setq filename nil))))
      (setq l (cdr l)))
    filename))

(defun spice-output-file-p ()
  "Checks if an output file is available for current spice deck."
  (interactive)
  (and (not spice-output-local)
       (buffer-file-name)
       (spice-output-filename)))

(defun spice-load-output-file-internal (filename)
  "Loads output file into emacs and sets major mode"
  (interactive)
  (if (and
       (stringp filename)
       (file-readable-p filename))
      (progn
	(find-file-other-window filename)
	(spice-mode))))

(defun spice-load-output-file ()
  "Loads output file into spice, calls `spice-load-output-file-internal'
to do the actual work, this is called from the menu or key binding"
  (interactive)
  (let (filename)
    (setq filename (spice-output-filename))
    (spice-load-output-file-internal filename)))


;; ======================================================================
;; .guess statement auxiliary functions (eldo !)

(defvar spice-guess-nodeset-statements nil
  "holds temp .guess/nodeset statements")

(defun spice-derive-guess-nodeset-statements (from to)
  "Derives .guess/nodeset statements from DC operating point
lines. This implements the hspice functionality of .ic0 files for
eldo..."
  (save-excursion
    (let ( ;; (count 0)
	  m1)
      (goto-char (max from to))
      (setq m1 (make-marker))
      (set-marker m1 (point))
      (goto-char (min from to))
      (while (re-search-forward "^\\s-*\\(\\S-+\\)\\s-+\\([-0-9\.E+]+\\)"
				m1 t)
	(progn
	  ;;(message "Inserting .guess...(%s)" (make-string (incf count) ?.))
	  ;;(beginning-of-line)
	  ;;(insert ".guess V(" (match-string 1) ") = " (match-string 2) "\n*")
	  (setq spice-guess-nodeset-statements
		(append spice-guess-nodeset-statements
			(list (concat " V("
				      (spice-match-string-no-properties 1)
				      ") = "
				      (spice-match-string-no-properties 2)))

                        ))
	  )))))

(defun spice-create-guess-nodeset-file (str)
  "Creates a <buffer-file-name>_guess/nodeset.cir file from node
voltage pairs in the current output file."
  (interactive)
  (setq spice-guess-nodeset-statements nil)
  (let (from to outbuf stats filename)
    (setq filename (file-name-nondirectory buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (setq from (re-search-forward "^\\s-+NODE\\s-+VOLTAGE\\s-*$" (point-max) t))
      (if from
	  (setq to (re-search-forward "^$" (point-max) t)))
      (if (and from to)
	  (spice-derive-guess-nodeset-statements from to)))
    (if (not spice-guess-nodeset-statements)
	(message "Didn't find 'node -- voltage' pairs in output file...")
      (setq outbuf
	    (find-file-noselect (concat
				 (file-name-sans-extension buffer-file-name)
				 "_" str ".cir")))
      (switch-to-buffer-other-window outbuf)
      (erase-buffer)
      (insert "* ." str " statements derived from " filename " ["
	      (format-time-string "%b %d %Y") " " (format-time-string "%T")
	      "]\n\n")
      (setq stats spice-guess-nodeset-statements)
      (while stats
	(insert ".")
	(insert str)
	(insert " ")
	(insert (car stats))
	(insert "\n")
	(setq stats (cdr stats)))
      (insert "\n\n* ." str " statements derived "
	      (format-time-string "%b %d %Y") " "
	      (format-time-string "%T") "\n\n")
      (save-buffer)
      (goto-char (point-min))
      ) ; when
    ) ; let
  )


(defun spice-replace-with-guess-statements (from to)
  "replace with .guess statements."
  (interactive "*r")
  (spice-replace-with-guess-nodeset-statements "guess" from to))


(defun spice-replace-with-nodeset-statements (from to)
  "replace with .nodeset statements."
  (interactive "*r")
  (spice-replace-with-guess-nodeset-statements "nodeset" from to))


(defun spice-replace-with-guess-nodeset-statements (str from to)
  "Replaces selected region with .guess/nodeset statements."
  (interactive)
  (setq spice-guess-nodeset-statements nil)
  (if (and from to)
      (spice-derive-guess-nodeset-statements from to))
  (if (not spice-guess-nodeset-statements)
      (message "Didn't find any 'node -- voltage' pairs in region...")
    (let (stats)
      (kill-region from to)
      (setq stats spice-guess-nodeset-statements)
      (while stats
	(insert ".")
	(insert str)
	(insert " ")
	(insert (car stats))
	(insert "\n")
	(setq stats (cdr stats))))
    ))


;; ======================================================================
;; fill-prefix related functions

(defvar spice-save-comment-line-break-function nil
  "*Mode specific variable to save previous line break function in.")

(defun spice-comment-indent-new-line (&optional soft)
  "Spice mode comment-indent-new-line function, used by `auto-fill'. Sets
`fill-prefix' depending on context: comment prefix if in comment with special
layla handling (!); \"!\" doc prefix if in hspice mode; \"$\" doc prefix
in eldo mode, otherwise uses \"+ \" prefix."
  (interactive)
  (let ((fpx fill-prefix) ; remember prefix
	(end (point)))    ; don't look beyond point for doc starters
    (if (save-excursion
	  (beginning-of-line)
	  (or
	   (looking-at (concat "\\([" comment-start
			       (when (spice-standard-p 'layla)
				 spice-continuation-prefix)
			       (when (spice-standard-p 'hspice)
				 "$")
			       (when (spice-standard-p 'eldo)
				 "!")
			       "]+\\)"))
	   (and
	    (spice-standard-p 'hspice)
	    (re-search-forward "\\s-\\([$]\\)" end t))
	   (and
	    (spice-standard-p 'eldo)
	    (re-search-forward "\\s-\\([!]\\)" end t))))
	(setq fill-prefix (concat (spice-match-string-no-properties 1) " "))
      (setq fill-prefix (concat spice-continuation-prefix " ")))
                                        ;(comment-indent-new-line) ;;call standard comment-indent-new-line function
    (when spice-save-comment-line-break-function
      (funcall spice-save-comment-line-break-function))
    (setq fill-prefix fpx)))


(defun spice-fill-context-prefix () ; ???
  "Calculates prefix from current position (move-to-left-margin), and
returns it. Non-comment paragraphs can also be filled correctly."
  ;;  (message "deriving fill prefix")
  (let ((result
	 (if (or (looking-at (concat "\\([" comment-start
                                     (when (spice-standard-p 'layla)
                                       spice-continuation-prefix)
                                     "]+\\)"))
		 (looking-at "\\([$!]+\\)"))
	     (concat (spice-match-string-no-properties 1) " ")
	   (concat spice-continuation-prefix " "))))
    ;;    (message (format "result is '%s'" result))
    result))

(defun spice-delete-indentation (&optional arg)
  "Wrapper for `delete-indentation', sets `fill-prefix' to adequate value."
  (interactive "*P")
  (let ((fillpfx fill-prefix))
    (beginning-of-line)
    (if arg (forward-line 1))
    (if (looking-at "\\([+$!*]+\\)") ;; don't check submodes here ?
	(setq fill-prefix (spice-match-string-no-properties 1)))
    (delete-indentation)
    (setq fill-prefix fillpfx)))


;; ======================================================================
;; msb fix (from cperl-mode.el)
(defvar spice-msb-fixed nil) ;; global variable keeping track of addition

(defun spice-msb-fix ()
  "Adds \"Spice Decks\" entry in msb menu, assumes that msb is already loaded"
  (setq spice-msb-fixed t)
  (let* ((l (length msb-menu-cond))
         (last (nth (1- l) msb-menu-cond))
         (precdr (nthcdr (- l 2) msb-menu-cond)) ; cdr of this is last
         (handle (1- (nth 1 last))))
    (setcdr precdr (list
                    (list
                     '(eq major-mode 'spice-mode)
                     handle
                     "Spice Decks (%d)")
                    last))))


;; ======================================================================
;; utility spice-mode functions

(defun spice-about ()
  (interactive)
  (sit-for 0)
  (message "spice-mode version %s, � %s" spice-version spice-developer))

(defun set-spice-name ()
  "Set mode line name of spice mode"
  (setq mode-name "Spice"))

;; (defun set-spice-name ()
;;   "Set mode line name of spice mode"
;;   (setq mode-name
;; 	(concat
;; 	 (when (not spice-output-local)
;; 	   (concat
;; 	    (when (spice-standard-p 'layla) "Layla")
;; 	    (when (spice-standard-p 'mondriaan) "-Mdrn")
;; 	    (when (spice-standard-p 'hspice)
;; 	      (when (spice-standard-p 'layla) "/"))
;; 	    (when (spice-standard-p 'hspice) "Hspice")
;; 	    (when (spice-standard-p 'eldo)
;; 	      (when (or (spice-standard-p 'hspice)
;; 			(spice-standard-p 'layla)) "/"))
;; 	    (when (spice-standard-p 'eldo) "Eldo")
;; 	    (when (spice-standard-p 'eldorf) "-RF")
;; 	    (when (spice-standard-p 'eldovloga) "-VlA")
;; 	    (when (spice-standard-p 'fasthenry)
;; 	      (when (or (spice-standard-p 'eldo)
;; 			(spice-standard-p 'hspice)
;; 			(spice-standard-p 'layla)) "/"))
;; 	    (when (spice-standard-p 'fasthenry) "FastHenry")
;; 	    (when (spice-standard-p 'draccdl)
;; 	      (when (or (spice-standard-p 'eldo)
;; 			(spice-standard-p 'hspice)
;; 			(spice-standard-p 'layla)
;; 			(spice-standard-p 'fasthenry)) "/"))
;; 	    (when (spice-standard-p 'draccdl) "DracCDL")
;; 	    (when (spice-standard-p 'spectre)
;; 	      (when (or (spice-standard-p 'eldo)
;; 			(spice-standard-p 'hspice)
;; 			(spice-standard-p 'layla)
;; 			(spice-standard-p 'fasthenry)
;; 			(spice-standard-p 'draccdl)) "/"))
;; 	    (when (spice-standard-p 'spectre) "Spectre")
;; 	    (when (or (spice-standard-p 'fasthenry)
;; 		      (spice-standard-p 'eldo)
;; 		      (spice-standard-p 'hspice)
;; 		      (spice-standard-p 'layla)
;; 		      (spice-standard-p 'draccdl)
;; 		      (spice-standard-p 'spectre)) "|")))
;; 	 "Spice"
;; 	 (when spice-output-local "-output")
;; 	 (when spice-some-comment-regions-are-hidden " H+"))))


(defun spice-check-spice-standard ()
  "checks if spice-standard is set to a correct value."
  ;; if mondriaan then also layla
  (when (spice-standard-p 'mondriaan)
    (unless (spice-standard-p 'layla)
      (error "Error: turn on Layla when you want to use Mondriaan")))
  ;; if eldoRf then also eldo
  (when (spice-standard-p 'eldorf)
    (unless (spice-standard-p 'eldo)
      (error "Error: turn on Eldo when you want to use Eldo RF")))
  ;; if eldo verilog-A then also eldo
  (when (spice-standard-p 'eldovloga)
    (unless (spice-standard-p 'eldo)
      (error "Error: turn on Eldo when you want to use Eldo Verilog-A")))
  )

;; xemacs - emacs compatibility wrapper
(defun spice-match-string-no-properties (num)
  "wrapper for no properties string matcher"
  (match-string-no-properties num))


(defun spice-update-existing-buffers ()
  "updates all spice-mode buffers with new customization"
  (save-excursion
    (let ((current (current-buffer)))
      (mapcar
       (lambda (buffer)
	 (set-buffer buffer)
	 (if (and (eq major-mode 'spice-mode)
		  (not (eq current buffer)))
	     (progn
	       (message "Activating customizations in %s" buffer)
	       (spice-activate-customizations-local))))
       (buffer-list))))
  )


(defun spice-activate-customizations-local ()
  "Activates customization (of global variables) in current buffer"
  (if (spice-output-p)
      (use-local-map spice-output-mode-map)
    (use-local-map spice-mode-map))
  (set-spice-name)
  (spice-update-mode-menu)
  (set-syntax-table spice-mode-syntax-table)
  (if spice-use-func-menu
      (if (fboundp 'function-menu)
	  (funcall 'spice-func-menu-init)))
  (if (not (spice-output-p))
      (setq imenu-generic-expression spice-imenu-generic-expression))
  (when spice-imenu-add-to-menubar
    (imenu-add-to-menubar "Index"))
  ;; rebuild menu:
  (setq imenu--index-alist nil)
  (imenu--make-index-alist t)
  (spice-compile-init)
  (spice-waveform-viewer-init)
  (if (fboundp 'font-lock-unset-defaults)
      (font-lock-unset-defaults))
  (font-lock-set-defaults)
  (font-lock-fontify-buffer))

(defun spice-activate-customizations-obsolete ()
  "Activate all customizations on local variables. Run this if you set
the spice-standard variable to modify spice-mode's behaviour in the local
buffer. It sets up the buffer local variables using the modified global
variables of the customization buffer."
  (interactive)
  (if (spice-output-p)
      (use-local-map spice-output-mode-map)
    (use-local-map spice-mode-map))
  ;; (setq spice-standard-local spice-standard)
  (set-spice-name)
  (spice-menu-init)
  (spice-update-mode-menu)
  (spice-mode-syntax-table-init)
  (set-syntax-table spice-mode-syntax-table)
  (spice-keywords-init)
  (spice-font-lock-init)
  (if spice-use-func-menu
      (if (fboundp 'function-menu)
	  (funcall 'spice-func-menu-init)))
  (spice-imenu-init)
  (if (not (spice-output-p))
      (setq imenu-generic-expression spice-imenu-generic-expression))
  ;; add imenu to menubar ?
  (when spice-imenu-add-to-menubar
    (imenu-add-to-menubar "Index"))
  (spice-compile-variables-init)
  (spice-compile-init)
  (spice-waveform-viewer-init)
  (if (fboundp 'font-lock-unset-defaults)
      (font-lock-unset-defaults))
  ;;  (setq font-lock-defaults
  ;;	(list 'spice-font-lock-keywords nil t (list (cons ?\" "w"))))
  (font-lock-set-defaults)
  (font-lock-fontify-buffer))


;; ======================================================================
;; spice-mode main entry point
;; ======================================================================
;;;###autoload
(defun spice-mode ()
  "Major mode for editing spice decks in (X)Emacs.

Entry to Spice mode calls the value of the variable `spice-mode-hook'
with no args, if that value is non-nil after initialization is finished.

Usage & Features:
-----------------

   - Comprehensive menu

   - Highlighting of (extended) SPICE syntax, with (limited) ERROR notification
     Please setup spice-mode to recognize the correct syntax through
     customization of the `spice-standard' variable. You can use the menu
     entry Spice->Customize->Spice Standard to do this interactively.

   - Template insertion (abbrev/electrification) for many spice constructs,
     two alternatives are available: Abbrev minor mode and `tempo-complete-tag'
       + Abbrevs can be turned on and off via the Settings submenu.
         To see the available abbrevs, use `M-x list-abbrevs' or use the menu
         after enabling abbrev minor mode. To find out what key sequence
         triggers an expand do 'C-h w expand-abbrev'.
       + `tempo-complete-tag' is bound to <tab> - for example, type 'M<tab>'
         at the beginning of a line and you will be prompted with a complete
         Mosfet template. Most tags are pretty straightforward i.e 'C' for a
         capacitor, 'L' for an inductance etc...
         You can type `C-h v tempo-tags'for a complete list of tags and
         associated templates. Note: to insert a real <TAB>, use <C-q TAB> or
         <shift TAB>.

   - Comment & documentation string handling
       + the '*' symbol is used to comment out a whole line - that symbol has
         to be at the beginning of a line
       + the '!' and '$' symbols are used to document your netlist in eldo
         and hspice/layla mode respectively
       + menu entry to comment out region/uncomment region
       + key bindings for commenting/uncommenting a region as in `auctex-mode'.

   - Comment hiding support
       + Can hide all commented out regions in a buffer to improve readability
       + prefix string is customizable: `spice-hide-line-prefix'.
       + custom variable can be set to automatically hide all commented regions
         at load time (`spice-auto-hide-comments')
       + requires use of doc strings, otherwise also documentation might be
         hidden
       + When parts of the deck are hidden the string \"H+\" appears in the
         modeline.

   - Imenu (Index menu and/or shift right click in emacs if configured)
       + shows subcircuit definitions
       + shows .end statements in submenu
       + shows device models in submenus
       + shows libraries (.lib/.inc) in submenu
       + shows analyses in submenu
       + shows sections in submenu
       + shows output file sections in menu
       + shows LAYLA objects in submenus if layla submode has been enabled
       + can be added to the menubar by setting `spice-imenu-add-to-menubar'
         (uses `imenu-add-to-menubar' to add an Imenu entry to the menubar).

   - File browser using Speedbar (`speedbar') and/or index/sources menu

   - .inc/.include/.lib/.libfas access
       + through mouse-2 click (`ffap-at-mouse' or `spice-load-file-at-mouse')
       + using \\r (ie. <return>), (`ffap')
       + through menu entry all include/lib files of a deck can be loaded at
         once (`spice-load-include-files'), recursively.

   - Searching for .subckt defs: `spice-search-subckt' or `C-c C-s'
       + extracts subcircuit name from context
       + search history
       + mark is set where search has been started if the definition is found
         in the same file. Return to mark with `C-u C-<SPC>' (or `C-u C-@')
         as with interactive searches (fi `isearch-forward')
       + be careful when starting the search from an included file,
         correctness can not be guaranteed. Starting a search from
         a top-level .cir file gives correct results. The latest used
         top-level file is stored (a top-level file contains a .end
         statement !), and also searched if the subckt def is not found in
         a first pass (for instance when starting from an included file).

   - Postscript printing with fontification (through `ps-print' package).

   - Addition of Spice Deck submenu in msb mode, see `msb', `spice-msb-fix'.

   - Section support (as in eldo-mode):
       + add section headers, goto section through menu entries or interactive
       + customizable through `spice-section-alist', you can add your
         own section headers, alter the list of predefined sections, ...
       + Changelog addition through `spice-add-changelog-entry', or
         use `C-c a c'.

   - Simulator support
       + use `spice-simulator' and `spice-simulator-switches' to
         select your simulator from `spice-simulator-alist'.
       + Support for error parsing of spice3 (batch-mode, -b),
         hspice, eldo and spectre. Add your own in spice-mode or through
         customization in `.emacs'.
       + use local file variables to customize per file as follows:
<<< test.cir >>>
.op
.end

* Local Variables:
* mode: spice
* spice-simulator: \"Hspice\"
* spice-simulator-switches: \"\"
* eval: (spice-set-command)
* End:
<<< test.cir ends here >>>

   - Waveform viewer support (beta)
       + use `spice-waveform-viewer' and `spice-waveform-viewer-switches' to
         select your waveform from `spice-waveform-viewer-alist'.
       + Support for interactive (command-line) waveform viewer such as
         nutmeg, or batch (GUI) type waveform viewer such as xelga or gsi.

   - Output file support
       + can load output file from menu (checks if file exists and is readable)
       + imenu in output files shows output file sections
       + for eldo .chi files: can create <file>_guess.cir files automatically
         for inclusion in <file>.cir file through
         `spice-create-guess-nodeset-file'
         or `spice-replace-with-guess-nodeset-statements'; this speeds up DC
         convergence for AC analyses.

   - (Fully) customizable
       + can select spice compatibility mode:
          * spice2g6/3 (default)
          * hspice (default)
          * eldo (default), RF and verilog-A extensions
          * fasthenry (default)
          * Dracula CDL
          * LAYLA, Mondriaan extensions
          * any combination of the above (there are conflicts however, so not
            all keyword fontification is correct in the latter case)
       + spice-mode font-lock faces
       + spice-mode default initialization of empty/new files determined by
         `spice-initialize-empty-file', `spice-initialize-template-file',
         `spice-default-header' and `spice-initialize-file-function'.
       + see the customization examples in the `spice-mode.el' file header
       + You can adapt `spice-simulator-alist' for your local site in
         `spice-mode.el' or in `.emacs' through customization.
       + You can adapt `spice-waveform-viewer-alist' for your local site in
         `spice-mode.el' or in `.emacs' through customization.
       + You can adapt `spice-section-alist' for your local site in
         `spice-mode.el' or in `.emacs' through customization.

   - Auto fill minor mode support
       + can be turned on from the Settings menu
       + auto fill works both for element where the continuation character
         is a '+', as well as in comment/doc mode where the continuation
         character is a '*', '$' or a '!'.
       + uses `fill-column' to determine where to wrap the line
       + doesn't use `fill-prefix'. A context dependent prefix is calculated
         through the `spice-comment-indent-new-line' function. This function
         is used as `comment-line-break-function' instead of the default line
         break function (in emacs: `comment-indent-new-line').

   - Paragraph support: [a-z] starts dev lines, '+' continues dev lines,
     [*!$] start paragraphs.

   - Works under GNU Emacs20.6/21.[123]

Do not use a -*- Mode -*- line in a spice deck as the first card in
the deck is defined to be the title card. Rather, autoload spice-mode
through your .emacs file:

 (autoload 'spice-mode \"spice-mode\" \"Spice/Layla Editing Mode\" t)
 (setq auto-mode-alist (append (list (cons \"\\\\.sp$\"  'spice-mode)
 				     (cons \"\\\\.cir$\" 'spice-mode)
 				     (cons \"\\\\.cdl$\" 'spice-mode)
 				     (cons \"\\\\.chi$\" 'spice-mode) ; output
 				     (cons \"\\\\.mod$\" 'spice-mode)); models
 			       auto-mode-alist))

Alternative methods are provided in the spice-mode.el file header.

Key bindings in highlighted include file lines:
-----------------------------------------------

\\{spice-mode-mouse-map}

Key bindings for other parts in the file:
-----------------------------------------

\\{spice-mode-map}
"
  ;;Key bindings for output files:
  ;;------------------------------
  ;;
  ;;\\{spice-output-mode-map}
  ;;"

  (interactive)
  (kill-all-local-variables) ; important !

  (setq major-mode 'spice-mode)
  (setq mode-name "Spice")

  ;; remember if this file is an output type
  (set (make-local-variable 'spice-output-local) (spice-check-output-mode))

  ;; set mode name
  (set-spice-name)

  ;; build global syntax table
  (unless spice-mode-syntax-table
    (spice-mode-syntax-table-init))
  ;; set it for local buffer
  (set-syntax-table spice-mode-syntax-table)

  ;; initialize global spice keywords, required for output mode !
  (unless spice-keywords
    (spice-keywords-init))

  ;; global font-lock start-up
  (unless spice-font-lock-keywords
    (spice-font-lock-init))

  ;; create global Spice mode menu's: mode-menu and output-mode-menu
  (unless (and spice-menu-list spice-output-menu-list)
    (spice-menu-init))

  ;; global spice-imenu init (not output mode !)
  (unless spice-imenu-generic-expression
    (spice-imenu-init))

  ;; global vars for compile init
  (unless spice-compilation-error-regexp-alist
    (spice-compile-variables-init))

  (set (make-local-variable 'parse-sexp-ignore-comments) nil)

  (set (make-local-variable 'tempo-interactive) t)
  (set (make-local-variable 'require-final-newline) t) ; hspice empty deck errors

  (set (make-local-variable 'comment-start) "*")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "\*")
  (set (make-local-variable 'comment-multi-line) nil)

  ;;------------------------------------------------------------
  ;; initialize hiding functions, mix of global and buffer local settings
  (spice-hide-init)

  (if (spice-output-p)
      (spice-mode-output)

    ;; use local keymap (for keys and menu)
    (use-local-map spice-mode-map)

    ;; set menu for local buffer
    (easy-menu-define spice-menu spice-mode-map
      "Menu keymap for Spice Mode." spice-menu-list)

    ;; abbreviations table: buffer local
    (setq local-abbrev-table spice-mode-abbrev-table)

    ;; buffer local init
    (set (make-local-variable 'fill-prefix) nil) ; automatically derived
    ;; remove auto-fill-inhibit and added spice-comment-indent... function
    ;; (set (make-local-variable 'auto-fill-inhibit-regexp) "^\*[^\.\+].*")

    ;; buffer local init for indentation of comment
    (set (make-local-variable 'spice-save-comment-line-break-function)
	 comment-line-break-function)
    (set (make-local-variable 'comment-line-break-function)
	 'spice-comment-indent-new-line)
    (set (make-local-variable 'fill-column) 78) ;; was 80

    ;; support for paragraphs (is it useful?)  ; this is really arbitrary
    ;; all buffer local
    ;; (set (make-local-variable 'paragraph-start) "^[!$*]-.*$")
    ;; (set (make-local-variable 'paragraph-separate) "^[!$*]-.*$")
    (set (make-local-variable 'paragraph-start)
	 "\\([a-z\\.].*\\([\n][+].*\\)*\\|[$]...+\\|[!]...+\\|[ \t\f]*$\\|\\*...+\\|.*\\s-[$!].*$\\)")
    ;; (set (make-local-variable 'paragraph-separate) "[a-z$!*]")
    (set (make-local-variable 'paragraph-separate) "\\([ \t\f]*\\|.*\\s-[$!].*\\)$")
    (set (make-local-variable 'adaptive-fill-regexp) nil)
    (set (make-local-variable 'adaptive-fill-function) 'spice-fill-context-prefix)
    (set (make-local-variable 'adaptive-fill-first-line-regexp) "[*$!+]\\s-+")

    ;; Tempo tags - using 'tempo-local-tags' doesn't work (why??)
    (set (make-local-variable 'tempo-tags)
	 (append spice-tempo-tags tempo-tags))

    ;; buffer local  font lock
    (set (make-local-variable 'font-lock-defaults)
	 (list 'spice-font-lock-keywords nil t (list (cons ?\" "w")))) ;; nil, t (do multiline comments)
    (set (make-local-variable 'font-lock-multiline) t)
    (font-lock-set-defaults)

    ;; imenu init:
    (set (make-local-variable 'imenu-case-fold-search) t)
    ;; buffer local imenu init
    (set (make-local-variable 'imenu-generic-expression)
	 spice-imenu-generic-expression)

    ;; add speedbar (global, can be moved ?)
    (spice-speedbar-init)

    ;; func-menu stuff, all buffer local
    (if spice-use-func-menu
	(if (fboundp 'function-menu)
	    (funcall 'spice-func-menu-init)))

    ;;------------------------------------------------------------
    ;; now hook in 'spice-colorize-libraries (eldo-mode.el)
    ;; all buffer local:
    ;;(make-local-hook 'font-lock-mode-hook)
    ;;(add-hook 'font-lock-mode-hook 'spice-colorize-libraries-buffer t t)
    (add-hook 'font-lock-after-fontify-buffer-hook 'spice-colorize-libraries-buffer t t) ; not in emacs 20

    (add-hook 'after-change-functions 'spice-colorize-libraries t t)
    (spice-colorize-libraries-buffer)

    ;;------------------------------------------------------------
    ;; compile buffer local init
    (spice-compile-init)

    ;;------------------------------------------------------------
    ;; buffer local init
    (spice-waveform-viewer-init)

    ;; if new file add a default template
    (if (and (= (buffer-size) 0)
	     (not buffer-read-only)
	     spice-initialize-empty-file
	     (functionp spice-initialize-file-function))
	(funcall spice-initialize-file-function))

    ) ;; matches big if (normal spice or spice output mode)

  ;; build imenu, buffer local
  (imenu--make-index-alist t)

  ;; add imenu to menubar, buffer local
  (when spice-imenu-add-to-menubar
    (imenu-add-to-menubar "Index"))

  ;; msb fix, run only once
  (and (featurep 'msb) ;; have we got this feature ?
       msb-mode ;; is it on ?
       (boundp 'msb-menu-cond) ;; still using msb-menu-cond ?
       (not spice-msb-fixed) ;; haven't yet added spice decks category ?
       (spice-msb-fix)) ;; add category

  ;; open describe window, hope this doesn't annoy people too much...
  (if spice-show-describe-mode
      (save-excursion
	(describe-mode) ;; aha...
	(setq spice-show-describe-mode nil)) ;; but only once in a session !!
    )
  (if spice-echo-intro
      (message "Spice mode %s.  Type C-h m for documentation." ;; always
               spice-version))

  ;; run spice-mode hooks
  (run-hooks 'spice-mode-hook)
  )


;; this is sometimes useful
(provide 'spice-mode)

;;; spice-mode.el ends here

;;; Local Variables:
;;; mode:Emacs-lisp
;;; End:
