Python-mode commands

====================

;;; Commentary:
---------------


;;; Code
--------


;;; Customization
-----------------


;;; Constants
-------------


;;; Macro definitions
---------------------


;;; Minor mode switches
-----------------------


toggle-py-jump-on-exception
---------------------------
If `py-jump-on-exception' should be on or off.

  Returns value of `py-jump-on-exception' switched to. 

py-jump-on-exception-on
-----------------------
Make sure, py-jump-on-exception' is on.

Returns value of `py-jump-on-exception'. 

py-jump-on-exception-off
------------------------
Make sure, `py-jump-on-exception' is off.

Returns value of `py-jump-on-exception'. 

toggle-python-mode-v5-behavior-p
--------------------------------
If `python-mode-v5-behavior-p' should be on or off.

  Returns value of `python-mode-v5-behavior-p' switched to. 

python-mode-v5-behavior-p-on
----------------------------
Make sure, `python-mode-v5-behavior-p' is on.

Returns value of `python-mode-v5-behavior-p'. 

python-mode-v5-behavior-p-off
-----------------------------
Make sure, `python-mode-v5-behavior-p' is off.

Returns value of `python-mode-v5-behavior-p'. 

py-toggle-shell-switch-buffers-on-execute
-----------------------------------------
If `py-switch-buffers-on-execute-p' should be on or off.

  Returns value of `py-switch-buffers-on-execute-p' switched to. 

py-shell-switch-buffers-on-execute-on
-------------------------------------
Make sure, `py-switch-buffers-on-execute-p' is on.

Returns value of `py-switch-buffers-on-execute-p'. 

py-shell-switch-buffers-on-execute-off
--------------------------------------
Make sure, `py-switch-buffers-on-execute-p' is off.

Returns value of `py-switch-buffers-on-execute-p'. 

py-toggle-split-windows-on-execute
----------------------------------
If `py-split-windows-on-execute-p' should be on or off.

  Returns value of `py-split-windows-on-execute-p' switched to. 

py-split-windows-on-execute-on
------------------------------
Make sure, `py-split-windows-on-execute-p' is on.

Returns value of `py-split-windows-on-execute-p'. 

py-split-windows-on-execute-off
-------------------------------
Make sure, `py-split-windows-on-execute-p' is off.

Returns value of `py-split-windows-on-execute-p'. 

py-toggle-highlight-indentation
-------------------------------
If `highlight-indentation-p' should be on or off. 

py-highlight-indentation-off
----------------------------
If `highlight-indentation-p' should be on or off. 

py-highlight-indentation-on
---------------------------
If `highlight-indentation-p' should be on or off. 

py-toggle-smart-indentation
---------------------------
If `py-smart-indentation' should be on or off.

Returns value of `py-smart-indentation' switched to. 

py-smart-indentation-on
-----------------------
Make sure, `py-smart-indentation' is on.

Returns value of `py-smart-indentation'. 

py-smart-indentation-off
------------------------
Make sure, `py-smart-indentation' is off.

Returns value of `py-smart-indentation'. 

py-toggle-smart-operator
------------------------
If `py-smart-operator-mode-p' should be on or off.

Returns value of `py-smart-operator-mode-p' switched to. 

py-smart-operator-mode-on
-------------------------
Make sure, `py-smart-operator-mode-p' is on.

Returns value of `py-smart-operator-mode-p'. 

py-smart-operator-mode-off
--------------------------
Make sure, `py-smart-operator-mode-p' is off.

Returns value of `py-smart-operator-mode-p'. 

toggle-py-use-current-dir-when-execute-p
----------------------------------------
If `py-use-current-dir-when-execute-p' should be on or off.

  Returns value of `py-use-current-dir-when-execute-p' switched to. 

py-use-current-dir-when-execute-p-on
------------------------------------
Make sure, py-use-current-dir-when-execute-p' is on.

Returns value of `py-use-current-dir-when-execute-p'. 

py-use-current-dir-when-execute-p-off
-------------------------------------
Make sure, `py-use-current-dir-when-execute-p' is off.

Returns value of `py-use-current-dir-when-execute-p'. 

py-toggle-autopair-mode
-----------------------
If `autopair-p' should be on or off. 

py-autopair-mode-on
-------------------
Make sure, autopair' is on. 

toggle-py-switch-buffers-on-execute-p
-------------------------------------
If `py-switch-buffers-on-execute-p' should be on or off.

  Returns value of `py-switch-buffers-on-execute-p' switched to. 

py-switch-buffers-on-execute-p-on
---------------------------------
Make sure, `py-py-switch-buffers-on-execute-p' is on.

Returns value of `py-switch-buffers-on-execute-p'. 

py-switch-buffers-on-execute-p-off
----------------------------------
Make sure, `py-switch-buffers-on-execute-p' is off.

Returns value of `py-switch-buffers-on-execute-p'. 

toggle-py-split-windows-on-execute-p
------------------------------------
If `py-split-windows-on-execute-p' should be on or off.

  Returns value of `py-split-windows-on-execute-p' switched to. 

py-split-windows-on-execute-p-on
--------------------------------
Make sure, `py-py-split-windows-on-execute-p' is on.

Returns value of `py-split-windows-on-execute-p'. 

py-split-windows-on-execute-p-off
---------------------------------
Make sure, `py-split-windows-on-execute-p' is off.

Returns value of `py-split-windows-on-execute-p'. 

py-toggle-sexp-function
-----------------------
Opens customization 

(defun p
--------


py-shell-get-process
--------------------
Get appropriate Python process for current buffer and return it.

py-shell-send-string
--------------------
Send STRING to inferior Python PROCESS.
When `py-verbose-p' and MSG is non-nil messages the first line of STRING.

py-shell-send-file
------------------
Send FILE-NAME to inferior Python PROCESS.
If TEMP-FILE-NAME is passed then that file is used for processing
instead, while internally the shell will continue to use
FILE-NAME.

py-switch-to-shell
------------------
Switch to inferior Python process buffer.

python-shell-completion-complete-at-point
-----------------------------------------
Perform completion at point in inferior Python process.

python-shell-completion-complete-or-indent
------------------------------------------
Complete or indent depending on the context.
If content before pointer is all whitespace indent.  If not try
to complete.

;;; Helper commands
-------------------


py-update-imports
-----------------
Returns `python-imports'.

Imports done are displayed in message buffer. 

py-guess-pdb-path
-----------------
If py-pdb-path isn't set, find location of pdb.py. 

(defun s
--------


py-forward-line
---------------
Goes to end of line after forward move.

Travels right-margin comments. 

py-go-to-beginning-of-comment
-----------------------------
Go to the beginning of current line's comment, if any.

From a programm use `py-beginning-of-comment' instead 

py-leave-comment-or-string-backward
-----------------------------------
If inside a comment or string, leave it backward. 

py-beginning-of-list-pps
------------------------
Go to the beginning of a list.
Optional ARG indicates a start-position for `parse-partial-sexp'.
Return beginning position, nil if not inside.

empty-line-p
------------
Returns t if cursor is at an line with nothing but whitespace-characters, nil otherwise.

py-count-lines
--------------
Count lines in accessible part until current line.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=7115

py-send-region
--------------
Send the region to the inferior Python process.

py-send-region-and-go
---------------------
Send the region to the inferior Python process.

Then switch to the process buffer.

python-send-string
------------------
Evaluate STRING in inferior Python process.

py-switch-to-python
-------------------
Switch to the Python process buffer, maybe starting new process.

With prefix arg, position cursor at end of buffer.

py-load-file
------------
Load a Python file FILE-NAME into the inferior Python process.

If the file has extension `.py' import or reload it as a module.
Treating it as a module keeps the global namespace clean, provides
function location information for debugging, and supports users of
module-qualified names.

py-proc
-------
Return the current Python process.

Start a new process if necessary. 

py-insert-default-shebang
-------------------------
Insert in buffer shebang of installed default Python. 

py-electric-comment
-------------------
Insert a comment. If starting a comment, indent accordingly.

If a numeric argument ARG is provided, that many "#" are inserted
non-electrically.
With C-u "#" electric behavior is inhibited inside a string or comment.

py-electric-colon
-----------------
Insert a colon and indent accordingly.

If a numeric argument ARG is provided, that many colons are inserted
non-electrically.

Electric behavior is inhibited inside a string or
comment or by universal prefix C-u.

Switched by `py-electric-colon-active-p', default is nil
See also `py-electric-colon-greedy-p' 

py-electric-backspace
---------------------
Delete preceding character or level of indentation.

With ARG do that ARG times.
Returns column reached. 

py-electric-delete
------------------
Delete following character or levels of whitespace.

With ARG do that ARG times. 

py-indent-line-outmost
----------------------
Indent the current line to the outmost reasonable indent.

With optional C-u an indent with length `py-indent-offset' is inserted unconditionally 

py-indent-line
--------------
Indent the current line according to Python rules.

When called interactivly with C-u, ignore dedenting rules for block closing statements
(e.g. return, raise, break, continue, pass)

An optional C-u followed by a numeric argument neither 1 nor 4 will switch off `py-smart-indentation' for this execution. This permits to correct allowed but unwanted indents.
Similar to `toggle-py-smart-indentation' resp. `py-smart-indentation-off' followed by TAB.

This function is normally used by `indent-line-function' resp.
TAB.
Returns current indentation 

py-newline-and-indent
---------------------
Add a newline and indent to outmost reasonable indent.
When indent is set back manually, this is honoured in following lines. 

py-newline-and-dedent
---------------------
Add a newline and indent to one level below current.
Returns column. 

toggle-force-local-shell
------------------------
If locally indicated Python shell should be taken and
enforced upon sessions execute commands.

Toggles boolean `py-force-local-shell-p' along with `py-force-py-shell-name-p'
Returns value of `toggle-force-local-shell' switched to.

When on, kind of an option 'follow', local shell sets `py-shell-name', enforces its use afterwards.

See also commands
`py-force-local-shell-on'
`py-force-local-shell-off'
 

py-force-local-shell-on
-----------------------
Make sure, `py-py-force-local-shell-p' is on.

Returns value of `py-force-local-shell-p'.

Kind of an option 'follow', local shell sets `py-shell-name', enforces its use afterwards 

py-force-local-shell-off
------------------------
Restore `py-shell-name' default value and `behaviour'. 

toggle-force-py-shell-name-p
----------------------------
If customized default `py-shell-name' should be enforced upon execution.

If `py-force-py-shell-name-p' should be on or off.
Returns value of `py-force-py-shell-name-p' switched to.

See also commands
force-py-shell-name-p-on
force-py-shell-name-p-off

Caveat: Completion might not work that way.


force-py-shell-name-p-on
------------------------
Switches `py-force-py-shell-name-p' on.

Customized default `py-shell-name' will be enforced upon execution.
Returns value of `py-force-py-shell-name-p'.

Caveat: Completion might not work that way.


force-py-shell-name-p-off
-------------------------
Make sure, `py-force-py-shell-name-p' is off.

Function to use by executes will be guessed from environment.
Returns value of `py-force-py-shell-name-p'. 

py-toggle-indent-tabs-mode
--------------------------
Toggle `indent-tabs-mode'.

Returns value of `indent-tabs-mode' switched to. 

py-indent-tabs-mode
-------------------
With positive ARG switch `indent-tabs-mode' on.

With negative ARG switch `indent-tabs-mode' off.
Returns value of `indent-tabs-mode' switched to. 

py-indent-tabs-mode-on
----------------------
Switch `indent-tabs-mode' on. 

py-indent-tabs-mode-off
-----------------------
Switch `indent-tabs-mode' on. 

py-guess-indent-offset
----------------------
Guess a value for, and change, `py-indent-offset'.

By default, make a buffer-local copy of `py-indent-offset' with the
new value.
With optional argument GLOBAL change the global value of `py-indent-offset'.

Returns `py-indent-offset'

py-narrow-to-defun
------------------
Make text outside current def or class invisible.

The defun visible is the one that contains point or follows point. 

;;; Shifting
------------


py-shift-left
-------------
Dedent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is dedented.
Returns indentation reached. 

py-shift-right
--------------
Indent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is indented.
Returns indentation reached. 

py-shift-paragraph-right
------------------------
Indent paragraph by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-paragraph-left
-----------------------
Dedent paragraph by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-block-right
--------------------
Indent block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-block-left
-------------------
Dedent block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-clause-right
---------------------
Indent clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-clause-left
--------------------
Dedent clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-block-or-clause-right
------------------------------
Indent block-or-clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-block-or-clause-left
-----------------------------
Dedent block-or-clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-def-right
------------------
Indent def by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-def-left
-----------------
Dedent def by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-class-right
--------------------
Indent class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-class-left
-------------------
Dedent class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-def-or-class-right
---------------------------
Indent def-or-class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-def-or-class-left
--------------------------
Dedent def-or-class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-line-right
-------------------
Indent line by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-line-left
------------------
Dedent line by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-statement-right
------------------------
Indent statement by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-shift-statement-left
-----------------------
Dedent statement by COUNT spaces.

COUNT defaults to `py-indent-offset',
use [universal-argument] to specify a different value.

Returns outmost indentation reached. 

py-indent-and-forward
---------------------
Indent current line according to mode, move one line forward. 

py-indent-region
----------------
Reindent a region of Python code.

With optional INDENT-OFFSET specify a different value than `py-indent-offset' at place.

Guesses the outmost reasonable indent
Returns and keeps relative position 

;;; Positions
-------------


py-def-or-class-beginning-position
----------------------------------
Returns beginning position of function or class definition. 

py-def-or-class-end-position
----------------------------
Returns end position of function or class definition. 

py-statement-beginning-position
-------------------------------
Returns beginning position of statement. 

py-statement-end-position
-------------------------
Returns end position of statement. 

py-current-indentation
----------------------
Returns beginning position of code in line. 

py-beginning-of-paragraph-position
----------------------------------
Returns beginning of paragraph position. 

py-end-of-paragraph-position
----------------------------
Returns end of paragraph position. 

py-beginning-of-block-position
------------------------------
Returns beginning of block position. 

py-end-of-block-position
------------------------
Returns end of block position. 

py-beginning-of-clause-position
-------------------------------
Returns beginning of clause position. 

py-end-of-clause-position
-------------------------
Returns end of clause position. 

py-beginning-of-block-or-clause-position
----------------------------------------
Returns beginning of block-or-clause position. 

py-end-of-block-or-clause-position
----------------------------------
Returns end of block-or-clause position. 

py-beginning-of-def-position
----------------------------
Returns beginning of def position. 

py-end-of-def-position
----------------------
Returns end of def position. 

py-beginning-of-class-position
------------------------------
Returns beginning of class position. 

py-end-of-class-position
------------------------
Returns end of class position. 

py-beginning-of-def-or-class-position
-------------------------------------
Returns beginning of def-or-class position. 

py-end-of-def-or-class-position
-------------------------------
Returns end of def-or-class position. 

py-beginning-of-line-position
-----------------------------
Returns beginning of line position. 

py-end-of-line-position
-----------------------
Returns end of line position. 

py-beginning-of-statement-position
----------------------------------
Returns beginning of statement position. 

py-end-of-statement-position
----------------------------
Returns end of statement position. 

py-beginning-of-expression-position
-----------------------------------
Returns beginning of expression position. 

py-end-of-expression-position
-----------------------------
Returns end of expression position. 

py-beginning-of-partial-expression-position
-------------------------------------------
Returns beginning of partial-expression position. 

py-end-of-partial-expression-position
-------------------------------------
Returns end of partial-expression position. 

py-list-beginning-position
--------------------------
Return lists beginning position, nil if not inside.

Optional ARG indicates a start-position for `parse-partial-sexp'.

py-end-of-list-position
-----------------------
Return end position, nil if not inside.

Optional ARG indicates a start-position for `parse-partial-sexp'.

py-in-triplequoted-string-p
---------------------------
Returns character address of start tqs-string, nil if not inside. 

py-in-string-p
--------------
Returns character address of start of string, nil if not inside. 

py-in-statement-p
-----------------
Returns list of beginning and end-position if inside.

Result is useful for booleans too: (when (py-in-statement-p)...)
will work.


;;; Bounds
----------


py-bounds-of-statement
----------------------
Returns bounds of statement at point.

With optional POSITION, a number, report bounds of statement at POSITION.
Returns a list, whose car is beg, cdr - end.

py-bounds-of-statements
-----------------------
Bounds of consecutive multitude of statements around point.

Indented same level, which don't open blocks. 

py-bounds-of-block
------------------
Returns bounds of block at point.

With optional POSITION, a number, report bounds of block at POSITION.
Returns a list, whose car is beg, cdr - end.

py-bounds-of-clause
-------------------
Returns bounds of clause at point.

With optional POSITION, a number, report bounds of clause at POSITION.
Returns a list, whose car is beg, cdr - end.

py-bounds-of-block-or-clause
----------------------------
Returns bounds of block-or-clause at point.

With optional POSITION, a number, report bounds of block-or-clause at POSITION.
Returns a list, whose car is beg, cdr - end.

py-bounds-of-def
----------------
Returns bounds of def at point.

With optional POSITION, a number, report bounds of def at POSITION.
Returns a list, whose car is beg, cdr - end.

py-bounds-of-class
------------------
Returns bounds of class at point.

With optional POSITION, a number, report bounds of class at POSITION.
Returns a list, whose car is beg, cdr - end.

py-bounds-of-region
-------------------
Returns bounds of region at point.

Returns a list, whose car is beg, cdr - end.

py-bounds-of-buffer
-------------------
Returns bounds of buffer at point.

With optional POSITION, a number, report bounds of buffer at POSITION.
Returns a list, whose car is beg, cdr - end.

py-bounds-of-expression
-----------------------
Returns bounds of expression at point.

With optional POSITION, a number, report bounds of expression at POSITION.
Returns a list, whose car is beg, cdr - end.

py-bounds-of-partial-expression
-------------------------------
Returns bounds of partial-expression at point.

With optional POSITION, a number, report bounds of partial-expression at POSITION.
Returns a list, whose car is beg, cdr - end.

py-bounds-of-declarations
-------------------------
Bounds of consecutive multitude of assigments resp. statements around point.

Indented same level, which don't open blocks.
Typically declarations resp. initialisations of variables following
a class or function definition.
See also py-bounds-of-statements 

;;; Comments, Filling
---------------------


py-comment-region
-----------------
Like `comment-region' but uses double hash (`#') comment starter.

py-fill-comment
---------------
Fill the comment paragraph at point

py-fill-paragraph
-----------------
`fill-paragraph-function'

commands py-fill-paragraph-SUFFIX
choose one of the following implemented styles:

DJANGO, ONETWO, PEP-257, PEP-257-NN, SYMMETRIC

Otherwise `py-fill-docstring-style' is used. Explanation:

DJANGO:

    """
    Process foo, return bar.
    """

    """
    Process foo, return bar.

    If processing fails throw ProcessingError.
    """

ONETWO:

    """Process foo, return bar."""

    """
    Process foo, return bar.

    If processing fails throw ProcessingError.

    """

PEP-257:

    """Process foo, return bar."""

    """Process foo, return bar.

    If processing fails throw ProcessingError.

    """

PEP-257-NN:

    """Process foo, return bar."""

    """Process foo, return bar.

    If processing fails throw ProcessingError.
    """

SYMMETRIC:

    """Process foo, return bar."""

    """
    Process foo, return bar.

    If processing fails throw ProcessingError.
    """


py-fill-labelled-string
-----------------------
Fill string or paragraph containing lines starting with label

See lp:1066489 

py-fill-string
--------------
String fill function for `py-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'.

py-fill-paren
-------------
Paren fill function for `py-fill-paragraph'.


py-fill-string-django
---------------------
Fill docstring according to Django's coding standards style.

    """
    Process foo, return bar.
    """

    """
    Process foo, return bar.

    If processing fails throw ProcessingError.
    """

See available styles at `py-fill-paragraph' or var `py-fill-docstring-style'


py-fill-string-onetwo
---------------------
One newline and start and Two at end style.

    """Process foo, return bar."""

    """
    Process foo, return bar.

    If processing fails throw ProcessingError.

    """

See available styles at `py-fill-paragraph' or var `py-fill-docstring-style'


py-fill-string-pep-257
----------------------
PEP-257 with 2 newlines at end of string.

    """Process foo, return bar."""

    """Process foo, return bar.

    If processing fails throw ProcessingError.

    """

See available styles at `py-fill-paragraph' or var `py-fill-docstring-style'


py-fill-string-pep-257-nn
-------------------------
PEP-257 with 1 newline at end of string.

    """Process foo, return bar."""

    """Process foo, return bar.

    If processing fails throw ProcessingError.
    """

See available styles at `py-fill-paragraph' or var `py-fill-docstring-style'


py-fill-string-symmetric
------------------------
Symmetric style.

    """Process foo, return bar."""

    """
    Process foo, return bar.

    If processing fails throw ProcessingError.
    """

See available styles at `py-fill-paragraph' or var `py-fill-docstring-style'


;;; Opens-p
-----------


py-statement-opens-block-p
--------------------------
Return position if the current statement opens a block
in stricter or wider sense.

For stricter sense specify regexp. 

py-statement-opens-clause-p
---------------------------
Return position if the current statement opens block or clause. 

py-statement-opens-block-or-clause-p
------------------------------------
Return position if the current statement opens block or clause. 

py-statement-opens-class-p
--------------------------
Return `t' if the statement opens a functions or class definition, nil otherwise. 

py-statement-opens-def-p
------------------------
Return `t' if the statement opens a functions or class definition, nil otherwise. 

py-statement-opens-def-or-class-p
---------------------------------
Return `t' if the statement opens a functions or class definition, nil otherwise. 

py-look-downward-for-clause
---------------------------
If beginning of other clause exists downward in current block.

If succesful return position. 

py-current-defun
----------------
Go to the outermost method or class definition in current scope.

Python value for `add-log-current-defun-function'.
This tells add-log.el how to find the current function/method/variable.
Returns name of class or methods definition, if found, nil otherwise.

See customizable variables `py-current-defun-show' and `py-current-defun-delay'.

py-sort-imports
---------------
Sort multiline imports.

Put point inside the parentheses of a multiline import and hit
M-x py-sort-imports to sort the imports lexicographically

py-which-function
-----------------
Return the name of the function or class, if curser is in, return nil otherwise. 

;;; Beginning/End
-----------------


py-beginning-of-statements
--------------------------
Got to the beginning of statements in current level which don't open blocks. 

py-end-of-statements
--------------------
Got to the end of statements in current level which don't open blocks. 

py-beginning-of-expression
--------------------------
Go to the beginning of a compound python expression.

With numeric ARG do it that many times.

A a compound python expression might be concatenated by "." operator, thus composed by minor python expressions.

If already at the beginning or before a expression, go to next expression in buffer upwards

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes.


py-end-of-expression
--------------------
Go to the end of a compound python expression.

With numeric ARG do it that many times.

A a compound python expression might be concatenated by "." operator, thus composed by minor python expressions.

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference

Operators however are left aside resp. limit py-expression designed for edit-purposes. 

py-beginning-of-partial-expression
----------------------------------
Go to the beginning of a minor python expression.

With numeric ARG do it that many times.

"." operators delimit a minor expression on their level.
Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes.

If already at the beginning or before a partial-expression, go to next partial-expression in buffer upwards 

py-end-of-partial-expression
----------------------------
Go to the end of a minor python expression.

With numeric ARG do it that many times.

"." operators delimit a minor expression on their level.
Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes. 

py-beginning-of-line
--------------------
Go to beginning-of-line, return position.

If already at beginning-of-line and not at BOB, go to beginning of previous line. 

py-end-of-line
--------------
Go to end-of-line, return position.

If already at end-of-line and not at EOB, go to end of next line. 

py-beginning-of-statement
-------------------------
Go to the initial line of a simple statement.

For beginning of compound statement use py-beginning-of-block.
For beginning of clause py-beginning-of-clause.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html


py-beginning-of-declarations
----------------------------
Got to the beginning of assigments resp. statements in current level which don't open blocks.


py-end-of-declarations
----------------------
Got to the end of assigments resp. statements in current level which don't open blocks. 

py-beginning-of-top-level
-------------------------
Go to beginning of block until level of indentation is null.

Returns beginning of block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-beginning-of-form-intern
---------------------------
Go to beginning of FORM.

With INDENT, go to beginning one level above.
Whit IACT, print result in message buffer.

Returns beginning of FORM if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-beginning
------------
Go to beginning of compound statement or definition at point.

With C-u, go to beginning one level above.
Returns position if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-end
------
Go to end of of compound statement or definition at point.

Returns position block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-up
-----
Go to beginning one level above of compound statement or definition at point.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-down
-------
Go to beginning one level below of compound statement or definition at point.

Returns position if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-beginning-of-block
---------------------
Go to beginning of block.

With C-u, go to beginning one level above.
Returns beginning of block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-end-of-block
---------------
Go to end of block.

Returns end of block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-beginning-of-clause
----------------------
Go to beginning of clause.

With C-u, go to beginning one level above.
Returns beginning of clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-end-of-clause
----------------
Go to end of clause.

Returns end of clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-beginning-of-block-or-clause
-------------------------------
Go to beginning of block-or-clause.

With C-u, go to beginning one level above.
Returns beginning of block-or-clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-end-of-block-or-clause
-------------------------
Go to end of block-or-clause.

Returns end of block-or-clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-beginning-of-def
-------------------
Go to beginning of def.

With C-u, go to beginning one level above.
Returns beginning of def if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-end-of-def
-------------
Go to end of def.

Returns end of def if successful, nil otherwise

With M-x universal argument or `py-mark-decorators' set to `t', decorators are marked too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-beginning-of-class
---------------------
Go to beginning of class.

With C-u, go to beginning one level above.
Returns beginning of class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-end-of-class
---------------
Go to end of class.

Returns end of class if successful, nil otherwise

With M-x universal argument or `py-mark-decorators' set to `t', decorators are marked too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-beginning-of-def-or-class
----------------------------
Go to beginning of def-or-class.

With C-u, go to beginning one level above.
Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-end-of-def-or-class
----------------------
Go to end of def-or-class.

Returns end of def-or-class if successful, nil otherwise

With M-x universal argument or `py-mark-decorators' set to `t', decorators are marked too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-beginning-of-if-block
------------------------
Go to beginning of if-block.

With C-u, go to beginning one level above.
Returns beginning of if-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-end-of-if-block
------------------
Go to end of if-block.

Returns end of if-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-beginning-of-try-block
-------------------------
Go to beginning of try-block.

With C-u, go to beginning one level above.
Returns beginning of try-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-end-of-try-block
-------------------
Go to end of try-block.

Returns end of try-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-beginning-of-minor-block
---------------------------
Go to beginning of minor-block.

With C-u, go to beginning one level above.
Returns beginning of minor-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

py-end-of-minor-block
---------------------
Go to end of minor-block.

Returns end of minor-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html

;;; Forms
---------


py-declarations
---------------
Copy and mark assigments resp. statements in current level which don't open blocks or start with a keyword.

See also `py-statements', which is more general, taking also simple statements starting with a keyword. 

py-statements
-------------
Copy and mark simple statements in current level which don't open blocks.

More general than py-declarations, which would stop at keywords like a print-statement. 

py-end-of-statement
-------------------
Go to the last char of current statement.

To go just beyond the final line of the current statement, use `py-down-statement-bol'. 

py-goto-statement-below
-----------------------
Goto beginning of next statement. 

;;; Mark forms
--------------


py-mark-paragraph
-----------------
Mark paragraph at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-block
-------------
Mark block at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-clause
--------------
Mark clause at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-block-or-clause
-----------------------
Mark block-or-clause at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-def
-----------
Mark def at point.

With M-x universal argument or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons. 

py-mark-class
-------------
Mark class at point.

With M-x universal argument or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons. 

py-mark-def-or-class
--------------------
Mark def-or-class at point.

With M-x universal argument or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons. 

py-mark-line
------------
Mark line at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-statement
-----------------
Mark statement at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-expression
------------------
Mark expression at point.

Returns beginning and end positions of marked area, a cons. 

py-mark-partial-expression
--------------------------
Mark partial-expression at point.

Returns beginning and end positions of marked area, a cons. 

py-beginning-of-decorator
-------------------------
Go to the beginning of a decorator.

Returns position if succesful 

py-end-of-decorator
-------------------
Go to the end of a decorator.

Returns position if succesful 

;;; Copyin
----------


py-copy-expression
------------------
Mark expression at point.

Returns beginning and end positions of marked area, a cons. 

py-copy-partial-expression
--------------------------
Mark partial-expression at point.

Returns beginning and end positions of marked area, a cons.

"." operators delimit a partial-expression expression on it's level, that's the difference to compound expressions.

Given the function below, `py-partial-expression'
called at pipe symbol would copy and return:

def usage():
    print """Usage: %s
    ....""" % (
        os.path.basename(sys.argv[0]))
------------|-------------------------
==> path

        os.path.basename(sys.argv[0]))
------------------|-------------------
==> basename(sys.argv[0]))

        os.path.basename(sys.argv[0]))
--------------------------|-----------
==> sys

        os.path.basename(sys.argv[0]))
------------------------------|-------
==> argv[0]

while `py-expression' would copy and return

(
        os.path.basename(sys.argv[0]))

;;

Also for existing commands a shorthand is defined:

(defalias 'py-statement 'py-copy-statement)

py-copy-statement
-----------------
Mark statement at point.

Returns beginning and end positions of marked area, a cons. 

py-copy-block
-------------
Mark block at point.

Returns beginning and end positions of marked area, a cons. 

py-copy-block-or-clause
-----------------------
Mark block-or-clause at point.

Returns beginning and end positions of marked area, a cons. 

py-copy-def
-----------
Mark def at point.

With universal argument or `py-mark-decorators' set to `t' decorators are copied too.
Returns beginning and end positions of marked area, a cons.

py-copy-def-or-class
--------------------
Mark def-or-class at point.

With universal argument or `py-mark-decorators' set to `t' decorators are copied too.
Returns beginning and end positions of marked area, a cons.

py-copy-class
-------------
Mark class at point.

With universal argument or `py-mark-decorators' set to `t' decorators are copied too.
Returns beginning and end positions of marked area, a cons.

py-copy-clause
--------------
Mark clause at point.
  Returns beginning and end positions of marked area, a cons. 

;;; Deleting
------------


py-kill-statements
------------------
Delete statements declared in current level.

Store deleted statements in kill-ring 

py-kill-declarations
--------------------
Delete variables declared in current level.

Store deleted variables in kill-ring 

py-kill-expression
------------------
Delete expression at point.
  Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-partial-expression
--------------------------
Delete partial-expression at point.
  Stores data in kill ring. Might be yanked back using `C-y'.

"." operators delimit a partial-expression expression on it's level, that's the difference to compound expressions.

py-kill-statement
-----------------
Delete statement at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-block
-------------
Delete block at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-block-or-clause
-----------------------
Delete block-or-clause at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-def-or-class
--------------------
Delete def-or-class at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-class
-------------
Delete class at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-def
-----------
Delete def at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-clause
--------------
Delete clause at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

;;; Beginning of line forms
---------------------------


py-beginning-of-block-bol-p
---------------------------
Returns position, if cursor is at the beginning of block, at beginning of line, nil otherwise. 

py-beginning-of-block-bol
-------------------------
Goto beginning of line where block starts.
  Returns position reached, if successful, nil otherwise.

See also `py-up-block': up from current definition to next beginning of block above. 

py-end-of-block-bol
-------------------
Goto beginning of line following end of block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-block': down from current definition to next beginning of block below. 

py-mark-block-bol
-----------------
Mark block, take beginning of line positions.

Returns beginning and end positions of region, a cons. 

py-copy-block-bol
-----------------
Delete block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-block-bol
-----------------
Delete block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-delete-block-bol
-------------------
Delete block bol at point.

Don't store data in kill ring. 

py-beginning-of-clause-bol-p
----------------------------
Returns position, if cursor is at the beginning of clause, at beginning of line, nil otherwise. 

py-beginning-of-clause-bol
--------------------------
Goto beginning of line where clause starts.
  Returns position reached, if successful, nil otherwise.

See also `py-up-clause': up from current definition to next beginning of clause above. 

py-end-of-clause-bol
--------------------
Goto beginning of line following end of clause.
  Returns position reached, if successful, nil otherwise.

See also `py-down-clause': down from current definition to next beginning of clause below. 

py-mark-clause-bol
------------------
Mark clause, take beginning of line positions.

Returns beginning and end positions of region, a cons. 

py-copy-clause-bol
------------------
Delete clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-clause-bol
------------------
Delete clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-delete-clause-bol
--------------------
Delete clause bol at point.

Don't store data in kill ring. 

py-beginning-of-block-or-clause-bol-p
-------------------------------------
Returns position, if cursor is at the beginning of block-or-clause, at beginning of line, nil otherwise. 

py-beginning-of-block-or-clause-bol
-----------------------------------
Goto beginning of line where block-or-clause starts.
  Returns position reached, if successful, nil otherwise.

See also `py-up-block-or-clause': up from current definition to next beginning of block-or-clause above. 

py-end-of-block-or-clause-bol
-----------------------------
Goto beginning of line following end of block-or-clause.
  Returns position reached, if successful, nil otherwise.

See also `py-down-block-or-clause': down from current definition to next beginning of block-or-clause below. 

py-mark-block-or-clause-bol
---------------------------
Mark block-or-clause, take beginning of line positions.

Returns beginning and end positions of region, a cons. 

py-copy-block-or-clause-bol
---------------------------
Delete block-or-clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-block-or-clause-bol
---------------------------
Delete block-or-clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-delete-block-or-clause-bol
-----------------------------
Delete block-or-clause bol at point.

Don't store data in kill ring. 

py-beginning-of-def-bol-p
-------------------------
Returns position, if cursor is at the beginning of def, at beginning of line, nil otherwise. 

py-beginning-of-def-bol
-----------------------
Goto beginning of line where def starts.
  Returns position reached, if successful, nil otherwise.

See also `py-up-def': up from current definition to next beginning of def above. 

py-end-of-def-bol
-----------------
Goto beginning of line following end of def.
  Returns position reached, if successful, nil otherwise.

See also `py-down-def': down from current definition to next beginning of def below. 

py-mark-def-bol
---------------
Mark def, take beginning of line positions.

With M-x universal argument or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of region, a cons. 

py-copy-def-bol
---------------
Delete def bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-def-bol
---------------
Delete def bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-delete-def-bol
-----------------
Delete def bol at point.

Don't store data in kill ring. 

py-beginning-of-class-bol-p
---------------------------
Returns position, if cursor is at the beginning of class, at beginning of line, nil otherwise. 

py-beginning-of-class-bol
-------------------------
Goto beginning of line where class starts.
  Returns position reached, if successful, nil otherwise.

See also `py-up-class': up from current definition to next beginning of class above. 

py-end-of-class-bol
-------------------
Goto beginning of line following end of class.
  Returns position reached, if successful, nil otherwise.

See also `py-down-class': down from current definition to next beginning of class below. 

py-mark-class-bol
-----------------
Mark class, take beginning of line positions.

With M-x universal argument or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of region, a cons. 

py-copy-class-bol
-----------------
Delete class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-class-bol
-----------------
Delete class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-delete-class-bol
-------------------
Delete class bol at point.

Don't store data in kill ring. 

py-beginning-of-def-or-class-bol-p
----------------------------------
Returns position, if cursor is at the beginning of def-or-class, at beginning of line, nil otherwise. 

py-beginning-of-def-or-class-bol
--------------------------------
Goto beginning of line where def-or-class starts.
  Returns position reached, if successful, nil otherwise.

See also `py-up-def-or-class': up from current definition to next beginning of def-or-class above. 

py-end-of-def-or-class-bol
--------------------------
Goto beginning of line following end of def-or-class.
  Returns position reached, if successful, nil otherwise.

See also `py-down-def-or-class': down from current definition to next beginning of def-or-class below. 

py-mark-def-or-class-bol
------------------------
Mark def-or-class, take beginning of line positions.

With M-x universal argument or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of region, a cons. 

py-copy-def-or-class-bol
------------------------
Delete def-or-class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-def-or-class-bol
------------------------
Delete def-or-class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-delete-def-or-class-bol
--------------------------
Delete def-or-class bol at point.

Don't store data in kill ring. 

py-beginning-of-statement-bol-p
-------------------------------
Returns position, if cursor is at the beginning of statement, at beginning of line, nil otherwise. 

py-beginning-of-statement-bol
-----------------------------
Goto beginning of line where statement starts.
  Returns position reached, if successful, nil otherwise.

See also `py-up-statement': up from current definition to next beginning of statement above. 

py-end-of-statement-bol
-----------------------
Goto beginning of line following end of statement.
  Returns position reached, if successful, nil otherwise.

See also `py-down-statement': down from current definition to next beginning of statement below. 

py-mark-statement-bol
---------------------
Mark statement, take beginning of line positions.

Returns beginning and end positions of region, a cons. 

py-copy-statement-bol
---------------------
Delete statement bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-kill-statement-bol
---------------------
Delete statement bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. 

py-delete-statement-bol
-----------------------
Delete statement bol at point.

Don't store data in kill ring. 

;;; Up/Down
-----------


py-up-statement
---------------
Go to the beginning of next statement upwards in buffer.

Return position if statement found, nil otherwise. 

py-down-statement
-----------------
Go to the beginning of next statement downwards in buffer.

Return position if statement found, nil otherwise. 

py-up-block
-----------
Go to the beginning of next block upwards in buffer.

Return position if block found, nil otherwise. 

py-up-minor-block
-----------------
Go to the beginning of next minor-block upwards in buffer.

Return position if minor-block found, nil otherwise. 

py-up-clause
------------
Go to the beginning of next clause upwards in buffer.

Return position if clause found, nil otherwise. 

py-up-block-or-clause
---------------------
Go to the beginning of next block-or-clause upwards in buffer.

Return position if block-or-clause found, nil otherwise. 

py-up-def
---------
Go to the beginning of next def upwards in buffer.

Return position if def found, nil otherwise. 

py-up-class
-----------
Go to the beginning of next class upwards in buffer.

Return position if class found, nil otherwise. 

py-up-def-or-class
------------------
Go to the beginning of next def-or-class upwards in buffer.

Return position if def-or-class found, nil otherwise. 

py-down-block
-------------
Go to the beginning of next block below in buffer.

Return position if block found, nil otherwise. 

py-down-minor-block
-------------------
Go to the beginning of next minor-block below in buffer.

Return position if minor-block found, nil otherwise. 

py-down-clause
--------------
Go to the beginning of next clause below in buffer.

Return position if clause found, nil otherwise. 

py-down-block-or-clause
-----------------------
Go to the beginning of next block-or-clause below in buffer.

Return position if block-or-clause found, nil otherwise. 

py-down-def
-----------
Go to the beginning of next def below in buffer.

Return position if def found, nil otherwise. 

py-down-class
-------------
Go to the beginning of next class below in buffer.

Return position if class found, nil otherwise. 

py-down-def-or-class
--------------------
Go to the beginning of next def-or-class below in buffer.

Return position if def-or-class found, nil otherwise. 

py-up-block-bol
---------------
Go to the beginning of next block upwards in buffer.

Go to beginning of line.
Return position if block found, nil otherwise. 

py-up-minor-block-bol
---------------------
Go to the beginning of next minor-block upwards in buffer.

Go to beginning of line.
Return position if minor-block found, nil otherwise. 

py-up-clause-bol
----------------
Go to the beginning of next clause upwards in buffer.

Go to beginning of line.
Return position if clause found, nil otherwise. 

py-up-block-or-clause-bol
-------------------------
Go to the beginning of next block-or-clause upwards in buffer.

Go to beginning of line.
Return position if block-or-clause found, nil otherwise. 

py-up-def-bol
-------------
Go to the beginning of next def upwards in buffer.

Go to beginning of line.
Return position if def found, nil otherwise. 

py-up-class-bol
---------------
Go to the beginning of next class upwards in buffer.

Go to beginning of line.
Return position if class found, nil otherwise. 

py-up-def-or-class-bol
----------------------
Go to the beginning of next def-or-class upwards in buffer.

Go to beginning of line.
Return position if def-or-class found, nil otherwise. 

py-down-block-bol
-----------------
Go to the beginning of next block below in buffer.

Go to beginning of line
Return position if block found, nil otherwise 

py-down-minor-block-bol
-----------------------
Go to the beginning of next minor-block below in buffer.

Go to beginning of line
Return position if minor-block found, nil otherwise 

py-down-clause-bol
------------------
Go to the beginning of next clause below in buffer.

Go to beginning of line
Return position if clause found, nil otherwise 

py-down-block-or-clause-bol
---------------------------
Go to the beginning of next block-or-clause below in buffer.

Go to beginning of line
Return position if block-or-clause found, nil otherwise 

py-down-def-bol
---------------
Go to the beginning of next def below in buffer.

Go to beginning of line
Return position if def found, nil otherwise 

py-down-class-bol
-----------------
Go to the beginning of next class below in buffer.

Go to beginning of line
Return position if class found, nil otherwise 

py-down-def-or-class-bol
------------------------
Go to the beginning of next def-or-class below in buffer.

Go to beginning of line
Return position if def-or-class found, nil otherwise 

py-forward-into-nomenclature
----------------------------
Move forward to end of a nomenclature section or word.

With C-u (programmatically, optional argument ARG), do it that many times.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores.

py-backward-into-nomenclature
-----------------------------
Move backward to beginning of a nomenclature section or word.

With optional ARG, move that many times.  If ARG is negative, move
forward.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores.

match-paren
-----------
Go to the matching brace, bracket or parenthesis if on its counterpart.

Otherwise insert the character, the key is assigned to, here `%'.
With universal arg  insert a `%'. 

;;; Named shells
----------------


python
------
Start an Python interpreter.

Optional C-u prompts for options to pass to the Python interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. 

ipython
-------
Start an IPython interpreter.

Optional C-u prompts for options to pass to the IPython interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. 

python3
-------
Start an Python3 interpreter.

Optional C-u prompts for options to pass to the Python3 interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. 

python2
-------
Start an Python2 interpreter.

Optional C-u prompts for options to pass to the Python2 interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. 

python2\.7
----------
Start an Python2.7 interpreter.

Optional C-u prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. 

jython
------
Start an Jython interpreter.

Optional C-u prompts for options to pass to the Jython interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. 

bpython
-------
Start an BPython interpreter.

Optional C-u prompts for options to pass to the Jython interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. 

python3\.2
----------
Start an Python3.2 interpreter.

Optional C-u prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. 

python3\.3
----------
Start an Python3.3 interpreter.

Optional C-u prompts for options to pass to the Python3.3 interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs.

Command expects Python3.3 installed at your system. 

python-dedicated
----------------
Start an unique Python interpreter in another window.

Optional C-u prompts for options to pass to the Python interpreter. See `py-python-command-args'.

ipython-dedicated
-----------------
Start an unique IPython interpreter in another window.

Optional C-u prompts for options to pass to the IPython interpreter. See `py-python-command-args'.

python3-dedicated
-----------------
Start an unique Python3 interpreter in another window.

Optional C-u prompts for options to pass to the Python3 interpreter. See `py-python-command-args'.

python2-dedicated
-----------------
Start an unique Python2 interpreter in another window.

Optional C-u prompts for options to pass to the Python2 interpreter. See `py-python-command-args'.

python2\.7-dedicated
--------------------
Start an unique Python2.7 interpreter in another window.

Optional C-u prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'.

jython-dedicated
----------------
Start an unique Jython interpreter in another window.

Optional C-u prompts for options to pass to the Jython interpreter. See `py-python-command-args'.

python3\.2-dedicated
--------------------
Start an unique Python3.2 interpreter in another window.

Optional C-u prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'.

python-switch
-------------
Switch to Python interpreter in another window.

Optional C-u prompts for options to pass to the Python interpreter. See `py-python-command-args'.

ipython-switch
--------------
Switch to IPython interpreter in another window.

Optional C-u prompts for options to pass to the IPython interpreter. See `py-python-command-args'.

python3-switch
--------------
Switch to Python3 interpreter in another window.

Optional C-u prompts for options to pass to the Python3 interpreter. See `py-python-command-args'.

python2-switch
--------------
Switch to Python2 interpreter in another window.

Optional C-u prompts for options to pass to the Python2 interpreter. See `py-python-command-args'.

python2\.7-switch
-----------------
Switch to Python2.7 interpreter in another window.

Optional C-u prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'.

jython-switch
-------------
Switch to Jython interpreter in another window.

Optional C-u prompts for options to pass to the Jython interpreter. See `py-python-command-args'.

python3\.2-switch
-----------------
Switch to Python3.2 interpreter in another window.

Optional C-u prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'.

python-no-switch
----------------
Open an Python interpreter in another window, but do not switch to it.

Optional C-u prompts for options to pass to the Python interpreter. See `py-python-command-args'.

ipython-no-switch
-----------------
Open an IPython interpreter in another window, but do not switch to it.

Optional C-u prompts for options to pass to the IPython interpreter. See `py-python-command-args'.

python3-no-switch
-----------------
Open an Python3 interpreter in another window, but do not switch to it.

Optional C-u prompts for options to pass to the Python3 interpreter. See `py-python-command-args'.

python2-no-switch
-----------------
Open an Python2 interpreter in another window, but do not switch to it.

Optional C-u prompts for options to pass to the Python2 interpreter. See `py-python-command-args'.

python2\.7-no-switch
--------------------
Open an Python2.7 interpreter in another window, but do not switch to it.

Optional C-u prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'.

jython-no-switch
----------------
Open an Jython interpreter in another window, but do not switch to it.

Optional C-u prompts for options to pass to the Jython interpreter. See `py-python-command-args'.

python3\.2-no-switch
--------------------
Open an Python3.2 interpreter in another window, but do not switch to it.

Optional C-u prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'.

python-switch-dedicated
-----------------------
Switch to an unique Python interpreter in another window.

Optional C-u prompts for options to pass to the Python interpreter. See `py-python-command-args'.

ipython-switch-dedicated
------------------------
Switch to an unique IPython interpreter in another window.

Optional C-u prompts for options to pass to the IPython interpreter. See `py-python-command-args'.

python3-switch-dedicated
------------------------
Switch to an unique Python3 interpreter in another window.

Optional C-u prompts for options to pass to the Python3 interpreter. See `py-python-command-args'.

python2-switch-dedicated
------------------------
Switch to an unique Python2 interpreter in another window.

Optional C-u prompts for options to pass to the Python2 interpreter. See `py-python-command-args'.

python2\.7-switch-dedicated
---------------------------
Switch to an unique Python2.7 interpreter in another window.

Optional C-u prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'.

jython-switch-dedicated
-----------------------
Switch to an unique Jython interpreter in another window.

Optional C-u prompts for options to pass to the Jython interpreter. See `py-python-command-args'.

python3\.2-switch-dedicated
---------------------------
Switch to an unique Python3.2 interpreter in another window.

Optional C-u prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'.

;;; Code execution
------------------


py-which-execute-file-command
-----------------------------
Return the command appropriate to Python version.

Per default it's "(format "execfile(r'%s') # PYTHON-MODE\n" filename)" for Python 2 series.

py-execute-region-no-switch
---------------------------
Send the region to a Python interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', buffer with region stays current.
 

py-execute-region-switch
------------------------
Send the region to a Python interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to.


py-execute-region
-----------------
Send the region to a Python interpreter.

When called with C-u, execution through `default-value' of `py-shell-name' is forced.
When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)


py-execute-region-default
-------------------------
Send the region to the systems default Python interpreter.
See also `py-execute-region'. 

py-execute-region-dedicated
---------------------------
Get the region processed by an unique Python interpreter.

When called with C-u, execution through `default-value' of `py-shell-name' is forced.
When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument. 

py-execute-region-default-dedicated
-----------------------------------
Send the region to an unique shell of systems default Python. 

py-execute-string
-----------------
Send the argument STRING to a Python interpreter.

See also `py-execute-region'. 

py-execute-string-dedicated
---------------------------
Send the argument STRING to an unique Python interpreter.

See also `py-execute-region'. 

py-fetch-py-master-file
-----------------------
Lookup if a `py-master-file' is specified.

See also doku of variable `py-master-file' 

py-execute-import-or-reload
---------------------------
Import the current buffer's file in a Python interpreter.

If the file has already been imported, then do reload instead to get
the latest version.

If the file's name does not end in ".py", then do execfile instead.

If the current buffer is not visiting a file, do `py-execute-buffer'
instead.

If the file local variable `py-master-file' is non-nil, import or
reload the named file instead of the buffer's file.  The file may be
saved based on the value of `py-execute-import-or-reload-save-p'.

See also `M-x py-execute-region'.

This may be preferable to `M-x py-execute-buffer' because:

 - Definitions stay in their module rather than appearing at top
   level, where they would clutter the global namespace and not affect
   uses of qualified names (MODULE.NAME).

 - The Python debugger gets line number information about the functions.

py-execute-buffer-dedicated
---------------------------
Send the contents of the buffer to a unique Python interpreter.

If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.

If a clipping restriction is in effect, only the accessible portion of the buffer is sent. A trailing newline will be supplied if needed.

With C-u user is prompted to specify another then default shell.
See also `M-x py-execute-region'. 

py-execute-buffer-switch
------------------------
Send the contents of the buffer to a Python interpreter and switches to output.

If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.
If there is a *Python* process buffer, it is used.
If a clipping restriction is in effect, only the accessible portion of the buffer is sent. A trailing newline will be supplied if needed.

With C-u user is prompted to specify another then default shell.
See also `M-x py-execute-region'. 

py-execute-buffer-dedicated-switch
----------------------------------
Send the contents of the buffer to an unique Python interpreter.

Ignores setting of `py-switch-buffers-on-execute-p'.
If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.

If a clipping restriction is in effect, only the accessible portion of the buffer is sent. A trailing newline will be supplied if needed.

With C-u user is prompted to specify another then default shell.
See also `M-x py-execute-region'. 

py-execute-buffer
-----------------
Send the contents of the buffer to a Python interpreter.

When called with C-u, execution through `default-value' of `py-shell-name' is forced.
When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch) 

py-execute-buffer-no-switch
---------------------------
Send the contents of the buffer to a Python interpreter but don't switch to output.

If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.
If there is a *Python* process buffer, it is used.
If a clipping restriction is in effect, only the accessible portion of the buffer is sent. A trailing newline will be supplied if needed.

With C-u user is prompted to specify another then default shell.
See also `M-x py-execute-region'. 

py-execute-defun
----------------
Send the current defun (class or method) to the inferior Python process.

py-process-file
---------------
Process "python filename".

Optional OUTPUT-BUFFER and ERROR-BUFFER might be given. 

py-exec-execfile-region
-----------------------
Execute the region in a Python interpreter. 

py-exec-execfile
----------------
Process "python filename",
Optional OUTPUT-BUFFER and ERROR-BUFFER might be given.')


py-execute-line
---------------
Send current line from beginning of indent to Python interpreter. 

py-execute-file
---------------
When called interactively, user is prompted for filename. 

;;; Pdb
-------


py-pdbtrack-toggle-stack-tracking
---------------------------------
Set variable `py-pdbtrack-do-tracking-p'. 

turn-on-pdbtrack
----------------


turn-off-pdbtrack
-----------------


;;; Documentation
-----------------


py-documentation
----------------
Launch PyDOC on the Word at Point

py-fetch-docu
-------------
Lookup in current buffer for the doku for the symbol at point.

Useful for newly defined symbol, not known to python yet. 

py-find-imports
---------------
Find top-level imports, updating `py-imports'.

Returns py-imports

py-eldoc-function
-----------------
Print help on symbol at point. 

py-describe-symbol
------------------
Print help on symbol at point.

If symbol is defined in current buffer, jump to it's definition
Optional C-u used for debugging, will prevent deletion of temp file. 

py-describe-mode
----------------
Dump long form of `python-mode' docs.

py-find-definition
------------------
Find source of definition of function NAME.

Interactively, prompt for name.

Search in current buffer first. 

;;; Miscellanus
---------------


py-insert-super
---------------
Insert a function "super()" from current environment.

As example given in Python v3.1 documentation » The Python Standard Library »

class C(B):
    def method(self, arg):
        super().method(arg) # This does the same thing as:
                               # super(C, self).method(arg)

Returns the string inserted. 

py-nesting-level
----------------
Accepts the output of `parse-partial-sexp'. 

py-compute-indentation
----------------------
Compute Python indentation.

When HONOR-BLOCK-CLOSE-P is non-nil, statements such as `return',
`raise', `break', `continue', and `pass' force one level of dedenting.

py-continuation-offset
----------------------
With numeric ARG different from 1 py-continuation-offset is set to that value; returns py-continuation-offset. 

py-indentation-of-statement
---------------------------
Returns the indenation of the statement at point. 

py-guess-default-python
-----------------------
Defaults to "python", if guessing didn't succeed. 

py-set-ipython-completion-command-string
----------------------------------------
Set and return `ipython-completion-command-string'. 

py-shell-dedicated
------------------
Start an interactive Python interpreter in another window.

With optional C-u user is prompted by
`py-choose-shell' for command and options to pass to the Python
interpreter.


py-shell
--------
Start an interactive Python interpreter in another window.
Interactively, C-u 4 prompts for a buffer.
C-u 2 prompts for `py-python-command-args'.
If `default-directory' is a remote file name, it is also prompted
to change if called with a prefix arg.

Returns py-shell's buffer-name.
Optional string PYSHELLNAME overrides default `py-shell-name'.
Optional symbol SWITCH ('switch/'noswitch) precedes `py-switch-buffers-on-execute-p'
When SEPCHAR is given, `py-shell' must not detect the file-separator.
BUFFER allows specifying a name, the Python process is connected to
When DONE is `t', `py-shell-manage-windows' is omitted
Optional symbol SPLIT ('split/'nosplit) precedes `py-split-buffers-on-execute-p'


py-indent-forward-line
----------------------
Indent and move one line forward to next indentation.
Returns column of line reached.

If `py-kill-empty-line' is non-nil, delete an empty line.
When closing a form, use py-close-block et al, which will move and indent likewise.
With M-x universal argument just indent.


py-dedent-forward-line
----------------------
Dedent line and move one line forward. 

py-dedent
---------
Dedent line according to `py-indent-offset'.

With arg, do it that many times.
If point is between indent levels, dedent to next level.
Return indentation reached, if dedent done, nil otherwise.

Affected by `py-dedent-keep-relative-column'. 

py-close-def
------------
Set indent level to that of beginning of function definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. 

py-close-class
--------------
Set indent level to that of beginning of class definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. 

py-close-clause
---------------
Set indent level to that of beginning of clause definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. 

py-close-block
--------------
Set indent level to that of beginning of block definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. 

py-class-at-point
-----------------
Return class definition as string.

With interactive call, send it to the message buffer too. 

py-line-at-point
----------------
Return line as string.
  With interactive call, send it to the message buffer too. 

py-looking-at-keywords-p
------------------------
If looking at a python keyword. Returns t or nil. 

py-match-paren-mode
-------------------
py-match-paren-mode nil oder t

py-match-paren
--------------
Goto to the opening or closing of block before or after point.

With arg, do it that many times.
 Closes unclosed block if jumping from beginning. 

py-printform-insert
-------------------
Inserts a print statement out of current `(car kill-ring)' by default, inserts ARG instead if delivered. 

eva
---
Put "eval(...)" forms around strings at point. 

pst-here
--------
Kill previous "pdb.set_trace()" and insert it at point. 

py-line-to-printform-python2
----------------------------
Transforms the item on current in a print statement. 

;;; Imenu
---------


py-switch-imenu-index-function
------------------------------
Switch between series 5. index machine `py-imenu-create-index' and `py-imenu-create-index-new', which also lists modules variables 

py-choose-shell-by-path
-----------------------
Select Python executable according to version desplayed in path, current buffer-file is selected from.

Returns versioned string, nil if nothing appropriate found 

py-choose-shell-by-shebang
--------------------------
Choose shell by looking at #! on the first line.

Returns the specified Python resp. Jython shell command name. 

py-which-python
---------------
Returns version of Python of current environment, a number. 

py-python-current-environment
-----------------------------
Returns path of current Python installation. 

py-switch-shell
---------------
Toggles between the interpreter customized in `py-shell-toggle-1' resp. `py-shell-toggle-2'. Was hard-coded CPython and Jython in earlier versions, now starts with Python2 and Python3 by default.

ARG might be a python-version string to set to.

C-u `py-toggle-shell' prompts to specify a reachable Python command.
C-u followed by numerical arg 2 or 3, `py-toggle-shell' opens a respective Python shell.
C-u followed by numerical arg 5 opens a Jython shell.

Should you need more shells to select, extend this command by adding inside the first cond:

                    ((eq NUMBER (prefix-numeric-value arg))
                     "MY-PATH-TO-SHELL")


py-choose-shell
---------------
Return an appropriate executable as a string.

Returns nil, if no executable found.

This does the following:
 - look for an interpreter with `py-choose-shell-by-shebang'
 - examine imports using `py-choose-shell-by-import'
 - look if Path/To/File indicates a Python version
 - if not successful, return default value of `py-shell-name'

When interactivly called, messages the shell name, Emacs would in the given circtumstances.

With C-u 4 is called `py-switch-shell' see docu there.


py-install-directory-check
--------------------------
Do some sanity check for `py-install-directory'.

Returns `t' if successful. 

py-guess-py-install-directory
-----------------------------
Takes value of user directory aka $HOME
if `(locate-library "python-mode")' is not succesful.

Used only, if `py-install-directory' is empty. 

py-set-load-path
----------------
Include needed subdirs of python-mode directory. 

;;; Abbrevs
-----------


py-edit-abbrevs
---------------
Jumps to `python-mode-abbrev-table' in a buffer containing lists of abbrev definitions.
You can edit them and type C-c C-c to redefine abbrevs
according to your editing.
Buffer contains a header line for each abbrev table,
 which is the abbrev table name in parentheses.
This is followed by one line per abbrev in that table:
NAME   USECOUNT   EXPANSION   HOOK
where NAME and EXPANSION are strings with quotes,
USECOUNT is an integer, and HOOK is any valid function
or may be omitted (it is usually omitted).  

py-add-abbrev
-------------
Defines python-mode specific abbrev for last expressions before point.
Argument is how many `py-partial-expression's form the expansion; or zero means the region is the expansion.

Reads the abbreviation in the minibuffer; with numeric arg it displays a proposal for an abbrev.
Proposal is composed from the initial character(s) of the
expansion.

Don't use this function in a Lisp program; use `define-abbrev' instead.

py-python-version
-----------------
Returns versions number of a Python EXECUTABLE, string.

If no EXECUTABLE given, `py-shell-name' is used.
Interactively output of `--version' is displayed. 

py-version
----------
Echo the current version of `python-mode' in the minibuffer.

py-install-search-local
-----------------------


py-install-local-shells
-----------------------
Builds Python-shell commands from executable found in LOCAL.

If LOCAL is empty, shell-command `find' searches beneath current directory.
Eval resulting buffer to install it, see customizable `py-extensions'. 

;;; Completion
--------------


py-completion-at-point
----------------------
An alternative completion, similar the way python.el does it. 

py-script-complete
------------------


py-python-script-complete
-------------------------
Complete word before point, if any.

When `py-no-completion-calls-dabbrev-expand-p' is non-nil, try dabbrev-expand. Otherwise, when `py-indent-no-completion-p' is non-nil, call `tab-to-tab-stop'. 

py-python2-shell-complete
-------------------------


py-python3-shell-complete
-------------------------
Complete word before point, if any. Otherwise insert TAB. 

py-shell-complete
-----------------
Complete word before point, if any. Otherwise insert TAB. 

ipython-complete
----------------
Complete the python symbol before point.

If no completion available, insert a TAB.
Returns the completed symbol, a string, if successful, nil otherwise. 

ipython-complete-py-shell-name
------------------------------
Complete the python symbol before point.

If no completion available, insert a TAB.
Returns the completed symbol, a string, if successful, nil otherwise.

Bug: if no IPython-shell is running, fails first time due to header returned, which messes up the result. Please repeat once then. 

;;; Checker
-----------


clear-flymake-allowed-file-name-masks
-------------------------------------
Remove entries with SUFFIX from `flymake-allowed-file-name-masks'.

Default is "\.py\'" 

pylint-flymake-mode
-------------------
Toggle `pylint' `flymake-mode'. 

pyflakes-flymake-mode
---------------------
Toggle `pyflakes' `flymake-mode'. 

pychecker-flymake-mode
----------------------
Toggle `pychecker' `flymake-mode'. 

pep8-flymake-mode
-----------------
Toggle `pep8' `flymake-mode'. 

pyflakespep8-flymake-mode
-------------------------
Toggle `pyflakespep8' `flymake-mode'.

Joint call to pyflakes and pep8 as proposed by

Keegan Carruthers-Smith



py-pep8-run
-----------
*Run pep8, check formatting (default on the file currently visited).


py-pep8-help
------------
Display pep8 command line help messages. 

py-pylint-run
-------------
*Run pylint (default on the file currently visited).

For help see M-x pylint-help resp. M-x pylint-long-help.
Home-page: http://www.logilab.org/project/pylint 

py-pylint-help
--------------
Display Pylint command line help messages.

Let's have this until more Emacs-like help is prepared 

py-pylint-doku
--------------
Display Pylint Documentation.

Calls `pylint --full-documentation'

py-pyflakes-run
---------------
*Run pyflakes (default on the file currently visited).

For help see M-x pyflakes-help resp. M-x pyflakes-long-help.
Home-page: http://www.logilab.org/project/pyflakes 

py-pyflakes-help
----------------
Display Pyflakes command line help messages.

Let's have this until more Emacs-like help is prepared 

py-pyflakespep8-run
-------------------
*Run pyflakespep8, check formatting (default on the file currently visited).


py-pyflakespep8-help
--------------------
Display pyflakespep8 command line help messages. 

py-pychecker-run
----------------
*Run pychecker (default on the file currently visited).

;;; Skeletons
-------------


;;; Virtualenv
--------------


(defun v
--------


virtualenv-current
------------------
barfs the current activated virtualenv

virtualenv-activate
-------------------
Activate the virtualenv located in DIR

virtualenv-deactivate
---------------------
Deactivate the current virtual enviroment

virtualenv-workon
-----------------
Issue a virtualenvwrapper-like virtualenv-workon command

py-toggle-local-default-use
---------------------------


;;; Execute
-----------


py-execute-statement
--------------------
Send statement at point to a Python interpreter.

When called with C-u, execution through `default-value' of `py-shell-name' is forced.
See also `py-force-py-shell-name-p'.

When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)

py-execute-block
----------------
Send block at point to a Python interpreter.

When called with C-u, execution through `default-value' of `py-shell-name' is forced.
See also `py-force-py-shell-name-p'.

When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)

py-execute-block-or-clause
--------------------------
Send block-or-clause at point to a Python interpreter.

When called with C-u, execution through `default-value' of `py-shell-name' is forced.
See also `py-force-py-shell-name-p'.

When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)

py-execute-def
--------------
Send def at point to a Python interpreter.

When called with C-u, execution through `default-value' of `py-shell-name' is forced.
See also `py-force-py-shell-name-p'.

When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)

py-execute-class
----------------
Send class at point to a Python interpreter.

When called with C-u, execution through `default-value' of `py-shell-name' is forced.
See also `py-force-py-shell-name-p'.

When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)

py-execute-def-or-class
-----------------------
Send def-or-class at point to a Python interpreter.

When called with C-u, execution through `default-value' of `py-shell-name' is forced.
See also `py-force-py-shell-name-p'.

When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)

py-execute-expression
---------------------
Send expression at point to a Python interpreter.

When called with C-u, execution through `default-value' of `py-shell-name' is forced.
See also `py-force-py-shell-name-p'.

When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)

py-execute-partial-expression
-----------------------------
Send partial-expression at point to a Python interpreter.

When called with C-u, execution through `default-value' of `py-shell-name' is forced.
See also `py-force-py-shell-name-p'.

When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)

;;; Extended executes
---------------------


py-execute-statement-python
---------------------------
Send statement at point to Python interpreter. 

py-execute-statement-python-switch
----------------------------------
Send statement at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-statement-python-noswitch
------------------------------------
Send statement at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-statement-python-dedicated
-------------------------------------
Send statement at point to Python unique interpreter. 

py-execute-statement-python-dedicated-switch
--------------------------------------------
Send statement at point to Python unique interpreter and switch to result. 

py-execute-statement-ipython
----------------------------
Send statement at point to IPython interpreter. 

py-execute-statement-ipython-switch
-----------------------------------
Send statement at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-statement-ipython-noswitch
-------------------------------------
Send statement at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-statement-ipython-dedicated
--------------------------------------
Send statement at point to IPython unique interpreter. 

py-execute-statement-ipython-dedicated-switch
---------------------------------------------
Send statement at point to IPython unique interpreter and switch to result. 

py-execute-statement-python3
----------------------------
Send statement at point to Python3 interpreter. 

py-execute-statement-python3-switch
-----------------------------------
Send statement at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-statement-python3-noswitch
-------------------------------------
Send statement at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-statement-python3-dedicated
--------------------------------------
Send statement at point to Python3 unique interpreter. 

py-execute-statement-python3-dedicated-switch
---------------------------------------------
Send statement at point to Python3 unique interpreter and switch to result. 

py-execute-statement-python2
----------------------------
Send statement at point to Python2 interpreter. 

py-execute-statement-python2-switch
-----------------------------------
Send statement at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-statement-python2-noswitch
-------------------------------------
Send statement at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-statement-python2-dedicated
--------------------------------------
Send statement at point to Python2 unique interpreter. 

py-execute-statement-python2-dedicated-switch
---------------------------------------------
Send statement at point to Python2 unique interpreter and switch to result. 

py-execute-statement-python2\.7
-------------------------------
Send statement at point to Python2.7 interpreter. 

py-execute-statement-python2\.7-switch
--------------------------------------
Send statement at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-statement-python2\.7-noswitch
----------------------------------------
Send statement at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-statement-python2\.7-dedicated
-----------------------------------------
Send statement at point to Python2.7 unique interpreter. 

py-execute-statement-python2\.7-dedicated-switch
------------------------------------------------
Send statement at point to Python2.7 unique interpreter and switch to result. 

py-execute-statement-jython
---------------------------
Send statement at point to Jython interpreter. 

py-execute-statement-jython-switch
----------------------------------
Send statement at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-statement-jython-noswitch
------------------------------------
Send statement at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-statement-jython-dedicated
-------------------------------------
Send statement at point to Jython unique interpreter. 

py-execute-statement-jython-dedicated-switch
--------------------------------------------
Send statement at point to Jython unique interpreter and switch to result. 

py-execute-statement-python3\.2
-------------------------------
Send statement at point to Python3.2 interpreter. 

py-execute-statement-python3\.2-switch
--------------------------------------
Send statement at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-statement-python3\.2-noswitch
----------------------------------------
Send statement at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-statement-python3\.2-dedicated
-----------------------------------------
Send statement at point to Python3.2 unique interpreter. 

py-execute-statement-python3\.2-dedicated-switch
------------------------------------------------
Send statement at point to Python3.2 unique interpreter and switch to result. 

py-execute-block-python
-----------------------
Send block at point to Python interpreter. 

py-execute-block-python-switch
------------------------------
Send block at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-python-noswitch
--------------------------------
Send block at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-python-dedicated
---------------------------------
Send block at point to Python unique interpreter. 

py-execute-block-python-dedicated-switch
----------------------------------------
Send block at point to Python unique interpreter and switch to result. 

py-execute-block-ipython
------------------------
Send block at point to IPython interpreter. 

py-execute-block-ipython-switch
-------------------------------
Send block at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-ipython-noswitch
---------------------------------
Send block at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-ipython-dedicated
----------------------------------
Send block at point to IPython unique interpreter. 

py-execute-block-ipython-dedicated-switch
-----------------------------------------
Send block at point to IPython unique interpreter and switch to result. 

py-execute-block-python3
------------------------
Send block at point to Python3 interpreter. 

py-execute-block-python3-switch
-------------------------------
Send block at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-python3-noswitch
---------------------------------
Send block at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-python3-dedicated
----------------------------------
Send block at point to Python3 unique interpreter. 

py-execute-block-python3-dedicated-switch
-----------------------------------------
Send block at point to Python3 unique interpreter and switch to result. 

py-execute-block-python2
------------------------
Send block at point to Python2 interpreter. 

py-execute-block-python2-switch
-------------------------------
Send block at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-python2-noswitch
---------------------------------
Send block at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-python2-dedicated
----------------------------------
Send block at point to Python2 unique interpreter. 

py-execute-block-python2-dedicated-switch
-----------------------------------------
Send block at point to Python2 unique interpreter and switch to result. 

py-execute-block-python2\.7
---------------------------
Send block at point to Python2.7 interpreter. 

py-execute-block-python2\.7-switch
----------------------------------
Send block at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-python2\.7-noswitch
------------------------------------
Send block at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-python2\.7-dedicated
-------------------------------------
Send block at point to Python2.7 unique interpreter. 

py-execute-block-python2\.7-dedicated-switch
--------------------------------------------
Send block at point to Python2.7 unique interpreter and switch to result. 

py-execute-block-jython
-----------------------
Send block at point to Jython interpreter. 

py-execute-block-jython-switch
------------------------------
Send block at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-jython-noswitch
--------------------------------
Send block at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-jython-dedicated
---------------------------------
Send block at point to Jython unique interpreter. 

py-execute-block-jython-dedicated-switch
----------------------------------------
Send block at point to Jython unique interpreter and switch to result. 

py-execute-block-python3\.2
---------------------------
Send block at point to Python3.2 interpreter. 

py-execute-block-python3\.2-switch
----------------------------------
Send block at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-python3\.2-noswitch
------------------------------------
Send block at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-python3\.2-dedicated
-------------------------------------
Send block at point to Python3.2 unique interpreter. 

py-execute-block-python3\.2-dedicated-switch
--------------------------------------------
Send block at point to Python3.2 unique interpreter and switch to result. 

py-execute-clause-python
------------------------
Send clause at point to Python interpreter. 

py-execute-clause-python-switch
-------------------------------
Send clause at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-clause-python-noswitch
---------------------------------
Send clause at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-clause-python-dedicated
----------------------------------
Send clause at point to Python unique interpreter. 

py-execute-clause-python-dedicated-switch
-----------------------------------------
Send clause at point to Python unique interpreter and switch to result. 

py-execute-clause-ipython
-------------------------
Send clause at point to IPython interpreter. 

py-execute-clause-ipython-switch
--------------------------------
Send clause at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-clause-ipython-noswitch
----------------------------------
Send clause at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-clause-ipython-dedicated
-----------------------------------
Send clause at point to IPython unique interpreter. 

py-execute-clause-ipython-dedicated-switch
------------------------------------------
Send clause at point to IPython unique interpreter and switch to result. 

py-execute-clause-python3
-------------------------
Send clause at point to Python3 interpreter. 

py-execute-clause-python3-switch
--------------------------------
Send clause at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-clause-python3-noswitch
----------------------------------
Send clause at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-clause-python3-dedicated
-----------------------------------
Send clause at point to Python3 unique interpreter. 

py-execute-clause-python3-dedicated-switch
------------------------------------------
Send clause at point to Python3 unique interpreter and switch to result. 

py-execute-clause-python2
-------------------------
Send clause at point to Python2 interpreter. 

py-execute-clause-python2-switch
--------------------------------
Send clause at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-clause-python2-noswitch
----------------------------------
Send clause at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-clause-python2-dedicated
-----------------------------------
Send clause at point to Python2 unique interpreter. 

py-execute-clause-python2-dedicated-switch
------------------------------------------
Send clause at point to Python2 unique interpreter and switch to result. 

py-execute-clause-python2\.7
----------------------------
Send clause at point to Python2.7 interpreter. 

py-execute-clause-python2\.7-switch
-----------------------------------
Send clause at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-clause-python2\.7-noswitch
-------------------------------------
Send clause at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-clause-python2\.7-dedicated
--------------------------------------
Send clause at point to Python2.7 unique interpreter. 

py-execute-clause-python2\.7-dedicated-switch
---------------------------------------------
Send clause at point to Python2.7 unique interpreter and switch to result. 

py-execute-clause-jython
------------------------
Send clause at point to Jython interpreter. 

py-execute-clause-jython-switch
-------------------------------
Send clause at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-clause-jython-noswitch
---------------------------------
Send clause at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-clause-jython-dedicated
----------------------------------
Send clause at point to Jython unique interpreter. 

py-execute-clause-jython-dedicated-switch
-----------------------------------------
Send clause at point to Jython unique interpreter and switch to result. 

py-execute-clause-python3\.2
----------------------------
Send clause at point to Python3.2 interpreter. 

py-execute-clause-python3\.2-switch
-----------------------------------
Send clause at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-clause-python3\.2-noswitch
-------------------------------------
Send clause at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-clause-python3\.2-dedicated
--------------------------------------
Send clause at point to Python3.2 unique interpreter. 

py-execute-clause-python3\.2-dedicated-switch
---------------------------------------------
Send clause at point to Python3.2 unique interpreter and switch to result. 

py-execute-block-or-clause-python
---------------------------------
Send block-or-clause at point to Python interpreter. 

py-execute-block-or-clause-python-switch
----------------------------------------
Send block-or-clause at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-or-clause-python-noswitch
------------------------------------------
Send block-or-clause at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-or-clause-python-dedicated
-------------------------------------------
Send block-or-clause at point to Python unique interpreter. 

py-execute-block-or-clause-python-dedicated-switch
--------------------------------------------------
Send block-or-clause at point to Python unique interpreter and switch to result. 

py-execute-block-or-clause-ipython
----------------------------------
Send block-or-clause at point to IPython interpreter. 

py-execute-block-or-clause-ipython-switch
-----------------------------------------
Send block-or-clause at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-or-clause-ipython-noswitch
-------------------------------------------
Send block-or-clause at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-or-clause-ipython-dedicated
--------------------------------------------
Send block-or-clause at point to IPython unique interpreter. 

py-execute-block-or-clause-ipython-dedicated-switch
---------------------------------------------------
Send block-or-clause at point to IPython unique interpreter and switch to result. 

py-execute-block-or-clause-python3
----------------------------------
Send block-or-clause at point to Python3 interpreter. 

py-execute-block-or-clause-python3-switch
-----------------------------------------
Send block-or-clause at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-or-clause-python3-noswitch
-------------------------------------------
Send block-or-clause at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-or-clause-python3-dedicated
--------------------------------------------
Send block-or-clause at point to Python3 unique interpreter. 

py-execute-block-or-clause-python3-dedicated-switch
---------------------------------------------------
Send block-or-clause at point to Python3 unique interpreter and switch to result. 

py-execute-block-or-clause-python2
----------------------------------
Send block-or-clause at point to Python2 interpreter. 

py-execute-block-or-clause-python2-switch
-----------------------------------------
Send block-or-clause at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-or-clause-python2-noswitch
-------------------------------------------
Send block-or-clause at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-or-clause-python2-dedicated
--------------------------------------------
Send block-or-clause at point to Python2 unique interpreter. 

py-execute-block-or-clause-python2-dedicated-switch
---------------------------------------------------
Send block-or-clause at point to Python2 unique interpreter and switch to result. 

py-execute-block-or-clause-python2\.7
-------------------------------------
Send block-or-clause at point to Python2.7 interpreter. 

py-execute-block-or-clause-python2\.7-switch
--------------------------------------------
Send block-or-clause at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-or-clause-python2\.7-noswitch
----------------------------------------------
Send block-or-clause at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-or-clause-python2\.7-dedicated
-----------------------------------------------
Send block-or-clause at point to Python2.7 unique interpreter. 

py-execute-block-or-clause-python2\.7-dedicated-switch
------------------------------------------------------
Send block-or-clause at point to Python2.7 unique interpreter and switch to result. 

py-execute-block-or-clause-jython
---------------------------------
Send block-or-clause at point to Jython interpreter. 

py-execute-block-or-clause-jython-switch
----------------------------------------
Send block-or-clause at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-or-clause-jython-noswitch
------------------------------------------
Send block-or-clause at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-or-clause-jython-dedicated
-------------------------------------------
Send block-or-clause at point to Jython unique interpreter. 

py-execute-block-or-clause-jython-dedicated-switch
--------------------------------------------------
Send block-or-clause at point to Jython unique interpreter and switch to result. 

py-execute-block-or-clause-python3\.2
-------------------------------------
Send block-or-clause at point to Python3.2 interpreter. 

py-execute-block-or-clause-python3\.2-switch
--------------------------------------------
Send block-or-clause at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-block-or-clause-python3\.2-noswitch
----------------------------------------------
Send block-or-clause at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-block-or-clause-python3\.2-dedicated
-----------------------------------------------
Send block-or-clause at point to Python3.2 unique interpreter. 

py-execute-block-or-clause-python3\.2-dedicated-switch
------------------------------------------------------
Send block-or-clause at point to Python3.2 unique interpreter and switch to result. 

py-execute-def-python
---------------------
Send def at point to Python interpreter. 

py-execute-def-python-switch
----------------------------
Send def at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-def-python-noswitch
------------------------------
Send def at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-def-python-dedicated
-------------------------------
Send def at point to Python unique interpreter. 

py-execute-def-python-dedicated-switch
--------------------------------------
Send def at point to Python unique interpreter and switch to result. 

py-execute-def-ipython
----------------------
Send def at point to IPython interpreter. 

py-execute-def-ipython-switch
-----------------------------
Send def at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-def-ipython-noswitch
-------------------------------
Send def at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-def-ipython-dedicated
--------------------------------
Send def at point to IPython unique interpreter. 

py-execute-def-ipython-dedicated-switch
---------------------------------------
Send def at point to IPython unique interpreter and switch to result. 

py-execute-def-python3
----------------------
Send def at point to Python3 interpreter. 

py-execute-def-python3-switch
-----------------------------
Send def at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-def-python3-noswitch
-------------------------------
Send def at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-def-python3-dedicated
--------------------------------
Send def at point to Python3 unique interpreter. 

py-execute-def-python3-dedicated-switch
---------------------------------------
Send def at point to Python3 unique interpreter and switch to result. 

py-execute-def-python2
----------------------
Send def at point to Python2 interpreter. 

py-execute-def-python2-switch
-----------------------------
Send def at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-def-python2-noswitch
-------------------------------
Send def at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-def-python2-dedicated
--------------------------------
Send def at point to Python2 unique interpreter. 

py-execute-def-python2-dedicated-switch
---------------------------------------
Send def at point to Python2 unique interpreter and switch to result. 

py-execute-def-python2\.7
-------------------------
Send def at point to Python2.7 interpreter. 

py-execute-def-python2\.7-switch
--------------------------------
Send def at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-def-python2\.7-noswitch
----------------------------------
Send def at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-def-python2\.7-dedicated
-----------------------------------
Send def at point to Python2.7 unique interpreter. 

py-execute-def-python2\.7-dedicated-switch
------------------------------------------
Send def at point to Python2.7 unique interpreter and switch to result. 

py-execute-def-jython
---------------------
Send def at point to Jython interpreter. 

py-execute-def-jython-switch
----------------------------
Send def at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-def-jython-noswitch
------------------------------
Send def at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-def-jython-dedicated
-------------------------------
Send def at point to Jython unique interpreter. 

py-execute-def-jython-dedicated-switch
--------------------------------------
Send def at point to Jython unique interpreter and switch to result. 

py-execute-def-python3\.2
-------------------------
Send def at point to Python3.2 interpreter. 

py-execute-def-python3\.2-switch
--------------------------------
Send def at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-def-python3\.2-noswitch
----------------------------------
Send def at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-def-python3\.2-dedicated
-----------------------------------
Send def at point to Python3.2 unique interpreter. 

py-execute-def-python3\.2-dedicated-switch
------------------------------------------
Send def at point to Python3.2 unique interpreter and switch to result. 

py-execute-class-python
-----------------------
Send class at point to Python interpreter. 

py-execute-class-python-switch
------------------------------
Send class at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-class-python-noswitch
--------------------------------
Send class at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-class-python-dedicated
---------------------------------
Send class at point to Python unique interpreter. 

py-execute-class-python-dedicated-switch
----------------------------------------
Send class at point to Python unique interpreter and switch to result. 

py-execute-class-ipython
------------------------
Send class at point to IPython interpreter. 

py-execute-class-ipython-switch
-------------------------------
Send class at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-class-ipython-noswitch
---------------------------------
Send class at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-class-ipython-dedicated
----------------------------------
Send class at point to IPython unique interpreter. 

py-execute-class-ipython-dedicated-switch
-----------------------------------------
Send class at point to IPython unique interpreter and switch to result. 

py-execute-class-python3
------------------------
Send class at point to Python3 interpreter. 

py-execute-class-python3-switch
-------------------------------
Send class at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-class-python3-noswitch
---------------------------------
Send class at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-class-python3-dedicated
----------------------------------
Send class at point to Python3 unique interpreter. 

py-execute-class-python3-dedicated-switch
-----------------------------------------
Send class at point to Python3 unique interpreter and switch to result. 

py-execute-class-python2
------------------------
Send class at point to Python2 interpreter. 

py-execute-class-python2-switch
-------------------------------
Send class at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-class-python2-noswitch
---------------------------------
Send class at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-class-python2-dedicated
----------------------------------
Send class at point to Python2 unique interpreter. 

py-execute-class-python2-dedicated-switch
-----------------------------------------
Send class at point to Python2 unique interpreter and switch to result. 

py-execute-class-python2\.7
---------------------------
Send class at point to Python2.7 interpreter. 

py-execute-class-python2\.7-switch
----------------------------------
Send class at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-class-python2\.7-noswitch
------------------------------------
Send class at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-class-python2\.7-dedicated
-------------------------------------
Send class at point to Python2.7 unique interpreter. 

py-execute-class-python2\.7-dedicated-switch
--------------------------------------------
Send class at point to Python2.7 unique interpreter and switch to result. 

py-execute-class-jython
-----------------------
Send class at point to Jython interpreter. 

py-execute-class-jython-switch
------------------------------
Send class at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-class-jython-noswitch
--------------------------------
Send class at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-class-jython-dedicated
---------------------------------
Send class at point to Jython unique interpreter. 

py-execute-class-jython-dedicated-switch
----------------------------------------
Send class at point to Jython unique interpreter and switch to result. 

py-execute-class-python3\.2
---------------------------
Send class at point to Python3.2 interpreter. 

py-execute-class-python3\.2-switch
----------------------------------
Send class at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-class-python3\.2-noswitch
------------------------------------
Send class at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-class-python3\.2-dedicated
-------------------------------------
Send class at point to Python3.2 unique interpreter. 

py-execute-class-python3\.2-dedicated-switch
--------------------------------------------
Send class at point to Python3.2 unique interpreter and switch to result. 

py-execute-region-python
------------------------
Send region at point to Python interpreter. 

py-execute-region-python-switch
-------------------------------
Send region at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-region-python-noswitch
---------------------------------
Send region at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-region-python-dedicated
----------------------------------
Send region at point to Python unique interpreter. 

py-execute-region-python-dedicated-switch
-----------------------------------------
Send region at point to Python unique interpreter and switch to result. 

py-execute-region-ipython
-------------------------
Send region at point to IPython interpreter. 

py-execute-region-ipython-switch
--------------------------------
Send region at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-region-ipython-noswitch
----------------------------------
Send region at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-region-ipython-dedicated
-----------------------------------
Send region at point to IPython unique interpreter. 

py-execute-region-ipython-dedicated-switch
------------------------------------------
Send region at point to IPython unique interpreter and switch to result. 

py-execute-region-python3
-------------------------
Send region at point to Python3 interpreter. 

py-execute-region-python3-switch
--------------------------------
Send region at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-region-python3-noswitch
----------------------------------
Send region at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-region-python3-dedicated
-----------------------------------
Send region at point to Python3 unique interpreter. 

py-execute-region-python3-dedicated-switch
------------------------------------------
Send region at point to Python3 unique interpreter and switch to result. 

py-execute-region-python2
-------------------------
Send region at point to Python2 interpreter. 

py-execute-region-python2-switch
--------------------------------
Send region at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-region-python2-noswitch
----------------------------------
Send region at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-region-python2-dedicated
-----------------------------------
Send region at point to Python2 unique interpreter. 

py-execute-region-python2-dedicated-switch
------------------------------------------
Send region at point to Python2 unique interpreter and switch to result. 

py-execute-region-python2\.7
----------------------------
Send region at point to Python2.7 interpreter. 

py-execute-region-python2\.7-switch
-----------------------------------
Send region at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-region-python2\.7-noswitch
-------------------------------------
Send region at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-region-python2\.7-dedicated
--------------------------------------
Send region at point to Python2.7 unique interpreter. 

py-execute-region-python2\.7-dedicated-switch
---------------------------------------------
Send region at point to Python2.7 unique interpreter and switch to result. 

py-execute-region-jython
------------------------
Send region at point to Jython interpreter. 

py-execute-region-jython-switch
-------------------------------
Send region at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-region-jython-noswitch
---------------------------------
Send region at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-region-jython-dedicated
----------------------------------
Send region at point to Jython unique interpreter. 

py-execute-region-jython-dedicated-switch
-----------------------------------------
Send region at point to Jython unique interpreter and switch to result. 

py-execute-region-python3\.2
----------------------------
Send region at point to Python3.2 interpreter. 

py-execute-region-python3\.2-switch
-----------------------------------
Send region at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-region-python3\.2-noswitch
-------------------------------------
Send region at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-region-python3\.2-dedicated
--------------------------------------
Send region at point to Python3.2 unique interpreter. 

py-execute-region-python3\.2-dedicated-switch
---------------------------------------------
Send region at point to Python3.2 unique interpreter and switch to result. 

py-execute-buffer-python
------------------------
Send buffer at point to Python interpreter. 

py-execute-buffer-python-switch
-------------------------------
Send buffer at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-buffer-python-noswitch
---------------------------------
Send buffer at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-buffer-python-dedicated
----------------------------------
Send buffer at point to Python unique interpreter. 

py-execute-buffer-python-dedicated-switch
-----------------------------------------
Send buffer at point to Python unique interpreter and switch to result. 

py-execute-buffer-ipython
-------------------------
Send buffer at point to IPython interpreter. 

py-execute-buffer-ipython-switch
--------------------------------
Send buffer at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-buffer-ipython-noswitch
----------------------------------
Send buffer at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-buffer-ipython-dedicated
-----------------------------------
Send buffer at point to IPython unique interpreter. 

py-execute-buffer-ipython-dedicated-switch
------------------------------------------
Send buffer at point to IPython unique interpreter and switch to result. 

py-execute-buffer-python3
-------------------------
Send buffer at point to Python3 interpreter. 

py-execute-buffer-python3-switch
--------------------------------
Send buffer at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-buffer-python3-noswitch
----------------------------------
Send buffer at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-buffer-python3-dedicated
-----------------------------------
Send buffer at point to Python3 unique interpreter. 

py-execute-buffer-python3-dedicated-switch
------------------------------------------
Send buffer at point to Python3 unique interpreter and switch to result. 

py-execute-buffer-python2
-------------------------
Send buffer at point to Python2 interpreter. 

py-execute-buffer-python2-switch
--------------------------------
Send buffer at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-buffer-python2-noswitch
----------------------------------
Send buffer at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-buffer-python2-dedicated
-----------------------------------
Send buffer at point to Python2 unique interpreter. 

py-execute-buffer-python2-dedicated-switch
------------------------------------------
Send buffer at point to Python2 unique interpreter and switch to result. 

py-execute-buffer-python2\.7
----------------------------
Send buffer at point to Python2.7 interpreter. 

py-execute-buffer-python2\.7-switch
-----------------------------------
Send buffer at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-buffer-python2\.7-noswitch
-------------------------------------
Send buffer at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-buffer-python2\.7-dedicated
--------------------------------------
Send buffer at point to Python2.7 unique interpreter. 

py-execute-buffer-python2\.7-dedicated-switch
---------------------------------------------
Send buffer at point to Python2.7 unique interpreter and switch to result. 

py-execute-buffer-jython
------------------------
Send buffer at point to Jython interpreter. 

py-execute-buffer-jython-switch
-------------------------------
Send buffer at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-buffer-jython-noswitch
---------------------------------
Send buffer at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-buffer-jython-dedicated
----------------------------------
Send buffer at point to Jython unique interpreter. 

py-execute-buffer-jython-dedicated-switch
-----------------------------------------
Send buffer at point to Jython unique interpreter and switch to result. 

py-execute-buffer-python3\.2
----------------------------
Send buffer at point to Python3.2 interpreter. 

py-execute-buffer-python3\.2-switch
-----------------------------------
Send buffer at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-buffer-python3\.2-noswitch
-------------------------------------
Send buffer at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-buffer-python3\.2-dedicated
--------------------------------------
Send buffer at point to Python3.2 unique interpreter. 

py-execute-buffer-python3\.2-dedicated-switch
---------------------------------------------
Send buffer at point to Python3.2 unique interpreter and switch to result. 

py-execute-expression-python
----------------------------
Send expression at point to Python interpreter. 

py-execute-expression-python-switch
-----------------------------------
Send expression at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-expression-python-noswitch
-------------------------------------
Send expression at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-expression-python-dedicated
--------------------------------------
Send expression at point to Python unique interpreter. 

py-execute-expression-python-dedicated-switch
---------------------------------------------
Send expression at point to Python unique interpreter and switch to result. 

py-execute-expression-ipython
-----------------------------
Send expression at point to IPython interpreter. 

py-execute-expression-ipython-switch
------------------------------------
Send expression at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-expression-ipython-noswitch
--------------------------------------
Send expression at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-expression-ipython-dedicated
---------------------------------------
Send expression at point to IPython unique interpreter. 

py-execute-expression-ipython-dedicated-switch
----------------------------------------------
Send expression at point to IPython unique interpreter and switch to result. 

py-execute-expression-python3
-----------------------------
Send expression at point to Python3 interpreter. 

py-execute-expression-python3-switch
------------------------------------
Send expression at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-expression-python3-noswitch
--------------------------------------
Send expression at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-expression-python3-dedicated
---------------------------------------
Send expression at point to Python3 unique interpreter. 

py-execute-expression-python3-dedicated-switch
----------------------------------------------
Send expression at point to Python3 unique interpreter and switch to result. 

py-execute-expression-python2
-----------------------------
Send expression at point to Python2 interpreter. 

py-execute-expression-python2-switch
------------------------------------
Send expression at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-expression-python2-noswitch
--------------------------------------
Send expression at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-expression-python2-dedicated
---------------------------------------
Send expression at point to Python2 unique interpreter. 

py-execute-expression-python2-dedicated-switch
----------------------------------------------
Send expression at point to Python2 unique interpreter and switch to result. 

py-execute-expression-python2\.7
--------------------------------
Send expression at point to Python2.7 interpreter. 

py-execute-expression-python2\.7-switch
---------------------------------------
Send expression at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-expression-python2\.7-noswitch
-----------------------------------------
Send expression at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-expression-python2\.7-dedicated
------------------------------------------
Send expression at point to Python2.7 unique interpreter. 

py-execute-expression-python2\.7-dedicated-switch
-------------------------------------------------
Send expression at point to Python2.7 unique interpreter and switch to result. 

py-execute-expression-jython
----------------------------
Send expression at point to Jython interpreter. 

py-execute-expression-jython-switch
-----------------------------------
Send expression at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-expression-jython-noswitch
-------------------------------------
Send expression at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-expression-jython-dedicated
--------------------------------------
Send expression at point to Jython unique interpreter. 

py-execute-expression-jython-dedicated-switch
---------------------------------------------
Send expression at point to Jython unique interpreter and switch to result. 

py-execute-expression-python3\.2
--------------------------------
Send expression at point to Python3.2 interpreter. 

py-execute-expression-python3\.2-switch
---------------------------------------
Send expression at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-expression-python3\.2-noswitch
-----------------------------------------
Send expression at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-expression-python3\.2-dedicated
------------------------------------------
Send expression at point to Python3.2 unique interpreter. 

py-execute-expression-python3\.2-dedicated-switch
-------------------------------------------------
Send expression at point to Python3.2 unique interpreter and switch to result. 

py-execute-partial-expression-python
------------------------------------
Send partial-expression at point to Python interpreter. 

py-execute-partial-expression-python-switch
-------------------------------------------
Send partial-expression at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-partial-expression-python-noswitch
---------------------------------------------
Send partial-expression at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-partial-expression-python-dedicated
----------------------------------------------
Send partial-expression at point to Python unique interpreter. 

py-execute-partial-expression-python-dedicated-switch
-----------------------------------------------------
Send partial-expression at point to Python unique interpreter and switch to result. 

py-execute-partial-expression-ipython
-------------------------------------
Send partial-expression at point to IPython interpreter. 

py-execute-partial-expression-ipython-switch
--------------------------------------------
Send partial-expression at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-partial-expression-ipython-noswitch
----------------------------------------------
Send partial-expression at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-partial-expression-ipython-dedicated
-----------------------------------------------
Send partial-expression at point to IPython unique interpreter. 

py-execute-partial-expression-ipython-dedicated-switch
------------------------------------------------------
Send partial-expression at point to IPython unique interpreter and switch to result. 

py-execute-partial-expression-python3
-------------------------------------
Send partial-expression at point to Python3 interpreter. 

py-execute-partial-expression-python3-switch
--------------------------------------------
Send partial-expression at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-partial-expression-python3-noswitch
----------------------------------------------
Send partial-expression at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-partial-expression-python3-dedicated
-----------------------------------------------
Send partial-expression at point to Python3 unique interpreter. 

py-execute-partial-expression-python3-dedicated-switch
------------------------------------------------------
Send partial-expression at point to Python3 unique interpreter and switch to result. 

py-execute-partial-expression-python2
-------------------------------------
Send partial-expression at point to Python2 interpreter. 

py-execute-partial-expression-python2-switch
--------------------------------------------
Send partial-expression at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-partial-expression-python2-noswitch
----------------------------------------------
Send partial-expression at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-partial-expression-python2-dedicated
-----------------------------------------------
Send partial-expression at point to Python2 unique interpreter. 

py-execute-partial-expression-python2-dedicated-switch
------------------------------------------------------
Send partial-expression at point to Python2 unique interpreter and switch to result. 

py-execute-partial-expression-python2\.7
----------------------------------------
Send partial-expression at point to Python2.7 interpreter. 

py-execute-partial-expression-python2\.7-switch
-----------------------------------------------
Send partial-expression at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-partial-expression-python2\.7-noswitch
-------------------------------------------------
Send partial-expression at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-partial-expression-python2\.7-dedicated
--------------------------------------------------
Send partial-expression at point to Python2.7 unique interpreter. 

py-execute-partial-expression-python2\.7-dedicated-switch
---------------------------------------------------------
Send partial-expression at point to Python2.7 unique interpreter and switch to result. 

py-execute-partial-expression-jython
------------------------------------
Send partial-expression at point to Jython interpreter. 

py-execute-partial-expression-jython-switch
-------------------------------------------
Send partial-expression at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-partial-expression-jython-noswitch
---------------------------------------------
Send partial-expression at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-partial-expression-jython-dedicated
----------------------------------------------
Send partial-expression at point to Jython unique interpreter. 

py-execute-partial-expression-jython-dedicated-switch
-----------------------------------------------------
Send partial-expression at point to Jython unique interpreter and switch to result. 

py-execute-partial-expression-python3\.2
----------------------------------------
Send partial-expression at point to Python3.2 interpreter. 

py-execute-partial-expression-python3\.2-switch
-----------------------------------------------
Send partial-expression at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-partial-expression-python3\.2-noswitch
-------------------------------------------------
Send partial-expression at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-partial-expression-python3\.2-dedicated
--------------------------------------------------
Send partial-expression at point to Python3.2 unique interpreter. 

py-execute-partial-expression-python3\.2-dedicated-switch
---------------------------------------------------------
Send partial-expression at point to Python3.2 unique interpreter and switch to result. 

py-execute-line-python
----------------------
Send line at point to Python interpreter. 

py-execute-line-python-switch
-----------------------------
Send line at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-line-python-noswitch
-------------------------------
Send line at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-line-python-dedicated
--------------------------------
Send line at point to Python unique interpreter. 

py-execute-line-python-dedicated-switch
---------------------------------------
Send line at point to Python unique interpreter and switch to result. 

py-execute-line-ipython
-----------------------
Send line at point to IPython interpreter. 

py-execute-line-ipython-switch
------------------------------
Send line at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-line-ipython-noswitch
--------------------------------
Send line at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-line-ipython-dedicated
---------------------------------
Send line at point to IPython unique interpreter. 

py-execute-line-ipython-dedicated-switch
----------------------------------------
Send line at point to IPython unique interpreter and switch to result. 

py-execute-line-python3
-----------------------
Send line at point to Python3 interpreter. 

py-execute-line-python3-switch
------------------------------
Send line at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-line-python3-noswitch
--------------------------------
Send line at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-line-python3-dedicated
---------------------------------
Send line at point to Python3 unique interpreter. 

py-execute-line-python3-dedicated-switch
----------------------------------------
Send line at point to Python3 unique interpreter and switch to result. 

py-execute-line-python2
-----------------------
Send line at point to Python2 interpreter. 

py-execute-line-python2-switch
------------------------------
Send line at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-line-python2-noswitch
--------------------------------
Send line at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-line-python2-dedicated
---------------------------------
Send line at point to Python2 unique interpreter. 

py-execute-line-python2-dedicated-switch
----------------------------------------
Send line at point to Python2 unique interpreter and switch to result. 

py-execute-line-python2\.7
--------------------------
Send line at point to Python2.7 interpreter. 

py-execute-line-python2\.7-switch
---------------------------------
Send line at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-line-python2\.7-noswitch
-----------------------------------
Send line at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-line-python2\.7-dedicated
------------------------------------
Send line at point to Python2.7 unique interpreter. 

py-execute-line-python2\.7-dedicated-switch
-------------------------------------------
Send line at point to Python2.7 unique interpreter and switch to result. 

py-execute-line-jython
----------------------
Send line at point to Jython interpreter. 

py-execute-line-jython-switch
-----------------------------
Send line at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-line-jython-noswitch
-------------------------------
Send line at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-line-jython-dedicated
--------------------------------
Send line at point to Jython unique interpreter. 

py-execute-line-jython-dedicated-switch
---------------------------------------
Send line at point to Jython unique interpreter and switch to result. 

py-execute-line-python3\.2
--------------------------
Send line at point to Python3.2 interpreter. 

py-execute-line-python3\.2-switch
---------------------------------
Send line at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

py-execute-line-python3\.2-noswitch
-----------------------------------
Send line at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

py-execute-line-python3\.2-dedicated
------------------------------------
Send line at point to Python3.2 unique interpreter. 

py-execute-line-python3\.2-dedicated-switch
-------------------------------------------
Send line at point to Python3.2 unique interpreter and switch to result. 

py-execute-file-python
----------------------
Send file to a Python interpreter.

py-execute-file-python-switch
-----------------------------
Send file to a Python interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python-noswitch
-------------------------------
Send file to a Python interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil"

py-execute-file-python-dedicated
--------------------------------
Send file to a Python interpreter.

Uses a dedicated shell.

py-execute-file-python-dedicated-switch
---------------------------------------
Send file to a Python interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-ipython
-----------------------
Send file to a Ipython interpreter.

py-execute-file-ipython-switch
------------------------------
Send file to a Ipython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-ipython-noswitch
--------------------------------
Send file to a Ipython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil"

py-execute-file-ipython-dedicated
---------------------------------
Send file to a Ipython interpreter.

Uses a dedicated shell.

py-execute-file-ipython-dedicated-switch
----------------------------------------
Send file to a Ipython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python3
-----------------------
Send file to a Python3 interpreter.

py-execute-file-python3-switch
------------------------------
Send file to a Python3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python3-noswitch
--------------------------------
Send file to a Python3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil"

py-execute-file-python3-dedicated
---------------------------------
Send file to a Python3 interpreter.

Uses a dedicated shell.

py-execute-file-python3-dedicated-switch
----------------------------------------
Send file to a Python3 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python2
-----------------------
Send file to a Python2 interpreter.

py-execute-file-python2-switch
------------------------------
Send file to a Python2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python2-noswitch
--------------------------------
Send file to a Python2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil"

py-execute-file-python2-dedicated
---------------------------------
Send file to a Python2 interpreter.

Uses a dedicated shell.

py-execute-file-python2-dedicated-switch
----------------------------------------
Send file to a Python2 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python2\.7
--------------------------
Send file to a Python2.7 interpreter.

py-execute-file-python2\.7-switch
---------------------------------
Send file to a Python2.7 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python2\.7-noswitch
-----------------------------------
Send file to a Python2.7 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil"

py-execute-file-python2\.7-dedicated
------------------------------------
Send file to a Python2.7 interpreter.

Uses a dedicated shell.

py-execute-file-python2\.7-dedicated-switch
-------------------------------------------
Send file to a Python2.7 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-jython
----------------------
Send file to a Jython interpreter.

py-execute-file-jython-switch
-----------------------------
Send file to a Jython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-jython-noswitch
-------------------------------
Send file to a Jython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil"

py-execute-file-jython-dedicated
--------------------------------
Send file to a Jython interpreter.

Uses a dedicated shell.

py-execute-file-jython-dedicated-switch
---------------------------------------
Send file to a Jython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python3\.2
--------------------------
Send file to a Python3.2 interpreter.

py-execute-file-python3\.2-switch
---------------------------------
Send file to a Python3.2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python3\.2-noswitch
-----------------------------------
Send file to a Python3.2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil"

py-execute-file-python3\.2-dedicated
------------------------------------
Send file to a Python3.2 interpreter.

Uses a dedicated shell.

py-execute-file-python3\.2-dedicated-switch
-------------------------------------------
Send file to a Python3.2 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python3\.3
--------------------------
Send file to a Python3.3 interpreter.

py-execute-file-python3\.3-switch
---------------------------------
Send file to a Python3.3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-python3\.3-noswitch
-----------------------------------
Send file to a Python3.3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil"

py-execute-file-python3\.3-dedicated
------------------------------------
Send file to a Python3.3 interpreter.

Uses a dedicated shell.

py-execute-file-python3\.3-dedicated-switch
-------------------------------------------
Send file to a Python3.3 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-bpython
-----------------------
Send file to a Bpython interpreter.

py-execute-file-bpython-switch
------------------------------
Send file to a Bpython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-execute-file-bpython-noswitch
--------------------------------
Send file to a Bpython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil"

py-execute-file-bpython-dedicated
---------------------------------
Send file to a Bpython interpreter.

Uses a dedicated shell.

py-execute-file-bpython-dedicated-switch
----------------------------------------
Send file to a Bpython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil"

py-down-exception
-----------------
Go to the next line down in the traceback.

With C-u (programmatically, optional argument
BOTTOM), jump to the bottom (innermost) exception in the exception
stack.

py-up-exception
---------------
Go to the previous line up in the traceback.

With C-u (programmatically, optional argument TOP)
jump to the top (outermost) exception in the exception stack.

py-output-buffer-filter
-----------------------
Clear output buffer from py-shell-input prompt etc. 

py-send-string
--------------
Evaluate STRING in inferior Python process.

py-load-pycomplete
------------------
Load Pymacs based pycomplete.

