;;; python-extended-executes-test.el --- extended-executes test
;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:


(require 'py-bug-numbered-tests)

(defun py-execute-statement-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python-base arg teststring)))

(defun py-execute-statement-python-base ()
  (assert (markerp (py-execute-statement-python)) nil "py-execute-statement-python-test failed"))

(defun py-execute-statement-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python-switch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python-switch-base arg teststring)))

(defun py-execute-statement-python-switch-base ()
  (assert (markerp (py-execute-statement-python-switch)) nil "py-execute-statement-python-switch-test failed"))

(defun py-execute-statement-python-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python-noswitch-base arg teststring)))

(defun py-execute-statement-python-noswitch-base ()
  (assert (markerp (py-execute-statement-python-noswitch)) nil "py-execute-statement-python-noswitch-test failed"))

(defun py-execute-statement-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python-dedicated-base arg teststring)))

(defun py-execute-statement-python-dedicated-base ()
  (assert (markerp (py-execute-statement-python-dedicated)) nil "py-execute-statement-python-dedicated-test failed"))

(defun py-execute-statement-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python-dedicated-switch-base arg teststring)))

(defun py-execute-statement-python-dedicated-switch-base ()
  (assert (markerp (py-execute-statement-python-dedicated-switch)) nil "py-execute-statement-python-dedicated-switch-test failed"))

(defun py-execute-statement-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-ipython-test\")"))
    (py-bug-tests-intern 'py-execute-statement-ipython-base arg teststring)))

(defun py-execute-statement-ipython-base ()
  (assert (markerp (py-execute-statement-ipython)) nil "py-execute-statement-ipython-test failed"))

(defun py-execute-statement-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-ipython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-ipython-switch-base arg teststring)))

(defun py-execute-statement-ipython-switch-base ()
  (assert (markerp (py-execute-statement-ipython-switch)) nil "py-execute-statement-ipython-switch-test failed"))

(defun py-execute-statement-ipython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-ipython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-ipython-noswitch-base arg teststring)))

(defun py-execute-statement-ipython-noswitch-base ()
  (assert (markerp (py-execute-statement-ipython-noswitch)) nil "py-execute-statement-ipython-noswitch-test failed"))

(defun py-execute-statement-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-ipython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-statement-ipython-dedicated-base arg teststring)))

(defun py-execute-statement-ipython-dedicated-base ()
  (assert (markerp (py-execute-statement-ipython-dedicated)) nil "py-execute-statement-ipython-dedicated-test failed"))

(defun py-execute-statement-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-ipython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-statement-ipython-dedicated-switch-base ()
  (assert (markerp (py-execute-statement-ipython-dedicated-switch)) nil "py-execute-statement-ipython-dedicated-switch-test failed"))

(defun py-execute-statement-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python3-base arg teststring)))

(defun py-execute-statement-python3-base ()
  (assert (markerp (py-execute-statement-python3)) nil "py-execute-statement-python3-test failed"))

(defun py-execute-statement-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3-switch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python3-switch-base arg teststring)))

(defun py-execute-statement-python3-switch-base ()
  (assert (markerp (py-execute-statement-python3-switch)) nil "py-execute-statement-python3-switch-test failed"))

(defun py-execute-statement-python3-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python3-noswitch-base arg teststring)))

(defun py-execute-statement-python3-noswitch-base ()
  (assert (markerp (py-execute-statement-python3-noswitch)) nil "py-execute-statement-python3-noswitch-test failed"))

(defun py-execute-statement-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python3-dedicated-base arg teststring)))

(defun py-execute-statement-python3-dedicated-base ()
  (assert (markerp (py-execute-statement-python3-dedicated)) nil "py-execute-statement-python3-dedicated-test failed"))

(defun py-execute-statement-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python3-dedicated-switch-base arg teststring)))

(defun py-execute-statement-python3-dedicated-switch-base ()
  (assert (markerp (py-execute-statement-python3-dedicated-switch)) nil "py-execute-statement-python3-dedicated-switch-test failed"))

(defun py-execute-statement-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python2-base arg teststring)))

(defun py-execute-statement-python2-base ()
  (assert (markerp (py-execute-statement-python2)) nil "py-execute-statement-python2-test failed"))

(defun py-execute-statement-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python2-switch-base arg teststring)))

(defun py-execute-statement-python2-switch-base ()
  (assert (markerp (py-execute-statement-python2-switch)) nil "py-execute-statement-python2-switch-test failed"))

(defun py-execute-statement-python2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python2-noswitch-base arg teststring)))

(defun py-execute-statement-python2-noswitch-base ()
  (assert (markerp (py-execute-statement-python2-noswitch)) nil "py-execute-statement-python2-noswitch-test failed"))

(defun py-execute-statement-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python2-dedicated-base arg teststring)))

(defun py-execute-statement-python2-dedicated-base ()
  (assert (markerp (py-execute-statement-python2-dedicated)) nil "py-execute-statement-python2-dedicated-test failed"))

(defun py-execute-statement-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python2-dedicated-switch-base arg teststring)))

(defun py-execute-statement-python2-dedicated-switch-base ()
  (assert (markerp (py-execute-statement-python2-dedicated-switch)) nil "py-execute-statement-python2-dedicated-switch-test failed"))

(defun py-execute-statement-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2.7-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python2.7-base arg teststring)))

(defun py-execute-statement-python2.7-base ()
  (assert (markerp (py-execute-statement-python2.7)) nil "py-execute-statement-python2.7-test failed"))

(defun py-execute-statement-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2.7-switch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python2.7-switch-base arg teststring)))

(defun py-execute-statement-python2.7-switch-base ()
  (assert (markerp (py-execute-statement-python2.7-switch)) nil "py-execute-statement-python2.7-switch-test failed"))

(defun py-execute-statement-python2.7-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2.7-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python2.7-noswitch-base arg teststring)))

(defun py-execute-statement-python2.7-noswitch-base ()
  (assert (markerp (py-execute-statement-python2.7-noswitch)) nil "py-execute-statement-python2.7-noswitch-test failed"))

(defun py-execute-statement-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2.7-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python2.7-dedicated-base arg teststring)))

(defun py-execute-statement-python2.7-dedicated-base ()
  (assert (markerp (py-execute-statement-python2.7-dedicated)) nil "py-execute-statement-python2.7-dedicated-test failed"))

(defun py-execute-statement-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python2.7-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-statement-python2.7-dedicated-switch-base ()
  (assert (markerp (py-execute-statement-python2.7-dedicated-switch)) nil "py-execute-statement-python2.7-dedicated-switch-test failed"))

(defun py-execute-statement-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-jython-test\")"))
    (py-bug-tests-intern 'py-execute-statement-jython-base arg teststring)))

(defun py-execute-statement-jython-base ()
  (assert (markerp (py-execute-statement-jython)) nil "py-execute-statement-jython-test failed"))

(defun py-execute-statement-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-jython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-jython-switch-base arg teststring)))

(defun py-execute-statement-jython-switch-base ()
  (assert (markerp (py-execute-statement-jython-switch)) nil "py-execute-statement-jython-switch-test failed"))

(defun py-execute-statement-jython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-jython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-jython-noswitch-base arg teststring)))

(defun py-execute-statement-jython-noswitch-base ()
  (assert (markerp (py-execute-statement-jython-noswitch)) nil "py-execute-statement-jython-noswitch-test failed"))

(defun py-execute-statement-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-jython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-statement-jython-dedicated-base arg teststring)))

(defun py-execute-statement-jython-dedicated-base ()
  (assert (markerp (py-execute-statement-jython-dedicated)) nil "py-execute-statement-jython-dedicated-test failed"))

(defun py-execute-statement-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-jython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-jython-dedicated-switch-base arg teststring)))

(defun py-execute-statement-jython-dedicated-switch-base ()
  (assert (markerp (py-execute-statement-jython-dedicated-switch)) nil "py-execute-statement-jython-dedicated-switch-test failed"))

(defun py-execute-statement-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3.2-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python3.2-base arg teststring)))

(defun py-execute-statement-python3.2-base ()
  (assert (markerp (py-execute-statement-python3.2)) nil "py-execute-statement-python3.2-test failed"))

(defun py-execute-statement-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3.2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python3.2-switch-base arg teststring)))

(defun py-execute-statement-python3.2-switch-base ()
  (assert (markerp (py-execute-statement-python3.2-switch)) nil "py-execute-statement-python3.2-switch-test failed"))

(defun py-execute-statement-python3.2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3.2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python3.2-noswitch-base arg teststring)))

(defun py-execute-statement-python3.2-noswitch-base ()
  (assert (markerp (py-execute-statement-python3.2-noswitch)) nil "py-execute-statement-python3.2-noswitch-test failed"))

(defun py-execute-statement-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3.2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python3.2-dedicated-base arg teststring)))

(defun py-execute-statement-python3.2-dedicated-base ()
  (assert (markerp (py-execute-statement-python3.2-dedicated)) nil "py-execute-statement-python3.2-dedicated-test failed"))

(defun py-execute-statement-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-python3.2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-statement-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-statement-python3.2-dedicated-switch-base ()
  (assert (markerp (py-execute-statement-python3.2-dedicated-switch)) nil "py-execute-statement-python3.2-dedicated-switch-test failed"))

(defun py-execute-block-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python-test\")"))
    (py-bug-tests-intern 'py-execute-block-python-base arg teststring)))

(defun py-execute-block-python-base ()
  (sit-for 1)
  (assert (markerp (py-execute-block-python)) nil "py-execute-block-python-test failed"))

(defun py-execute-block-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-python-switch-base arg teststring)))

(defun py-execute-block-python-switch-base ()
  (assert (markerp (py-execute-block-python-switch)) nil "py-execute-block-python-switch-test failed"))

(defun py-execute-block-python-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-block-python-noswitch-base arg teststring)))

(defun py-execute-block-python-noswitch-base ()
  (assert (markerp (py-execute-block-python-noswitch)) nil "py-execute-block-python-noswitch-test failed"))

(defun py-execute-block-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-block-python-dedicated-base arg teststring)))

(defun py-execute-block-python-dedicated-base ()
  (assert (markerp (py-execute-block-python-dedicated)) nil "py-execute-block-python-dedicated-test failed"))

(defun py-execute-block-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-python-dedicated-switch-base arg teststring)))

(defun py-execute-block-python-dedicated-switch-base ()
  (assert (markerp (py-execute-block-python-dedicated-switch)) nil "py-execute-block-python-dedicated-switch-test failed"))

(defun py-execute-block-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-ipython-test\")"))
    (py-bug-tests-intern 'py-execute-block-ipython-base arg teststring)))

(defun py-execute-block-ipython-base ()
  (assert (markerp (py-execute-block-ipython)) nil "py-execute-block-ipython-test failed"))

(defun py-execute-block-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-ipython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-ipython-switch-base arg teststring)))

(defun py-execute-block-ipython-switch-base ()
  (assert (markerp (py-execute-block-ipython-switch)) nil "py-execute-block-ipython-switch-test failed"))

(defun py-execute-block-ipython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-ipython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-block-ipython-noswitch-base arg teststring)))

(defun py-execute-block-ipython-noswitch-base ()
  (assert (markerp (py-execute-block-ipython-noswitch)) nil "py-execute-block-ipython-noswitch-test failed"))

(defun py-execute-block-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-ipython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-block-ipython-dedicated-base arg teststring)))

(defun py-execute-block-ipython-dedicated-base ()
  (assert (markerp (py-execute-block-ipython-dedicated)) nil "py-execute-block-ipython-dedicated-test failed"))

(defun py-execute-block-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-ipython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-block-ipython-dedicated-switch-base ()
  (assert (markerp (py-execute-block-ipython-dedicated-switch)) nil "py-execute-block-ipython-dedicated-switch-test failed"))

(defun py-execute-block-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3-test\")"))
    (py-bug-tests-intern 'py-execute-block-python3-base arg teststring)))

(defun py-execute-block-python3-base ()
  (assert (markerp (py-execute-block-python3)) nil "py-execute-block-python3-test failed"))

(defun py-execute-block-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-python3-switch-base arg teststring)))

(defun py-execute-block-python3-switch-base ()
  (assert (markerp (py-execute-block-python3-switch)) nil "py-execute-block-python3-switch-test failed"))

(defun py-execute-block-python3-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-block-python3-noswitch-base arg teststring)))

(defun py-execute-block-python3-noswitch-base ()
  (assert (markerp (py-execute-block-python3-noswitch)) nil "py-execute-block-python3-noswitch-test failed"))

(defun py-execute-block-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-block-python3-dedicated-base arg teststring)))

(defun py-execute-block-python3-dedicated-base ()
  (assert (markerp (py-execute-block-python3-dedicated)) nil "py-execute-block-python3-dedicated-test failed"))

(defun py-execute-block-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-python3-dedicated-switch-base arg teststring)))

(defun py-execute-block-python3-dedicated-switch-base ()
  (assert (markerp (py-execute-block-python3-dedicated-switch)) nil "py-execute-block-python3-dedicated-switch-test failed"))

(defun py-execute-block-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2-test\")"))
    (py-bug-tests-intern 'py-execute-block-python2-base arg teststring)))

(defun py-execute-block-python2-base ()
  (assert (markerp (py-execute-block-python2)) nil "py-execute-block-python2-test failed"))

(defun py-execute-block-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-python2-switch-base arg teststring)))

(defun py-execute-block-python2-switch-base ()
  (assert (markerp (py-execute-block-python2-switch)) nil "py-execute-block-python2-switch-test failed"))

(defun py-execute-block-python2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-block-python2-noswitch-base arg teststring)))

(defun py-execute-block-python2-noswitch-base ()
  (assert (markerp (py-execute-block-python2-noswitch)) nil "py-execute-block-python2-noswitch-test failed"))

(defun py-execute-block-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-block-python2-dedicated-base arg teststring)))

(defun py-execute-block-python2-dedicated-base ()
  (assert (markerp (py-execute-block-python2-dedicated)) nil "py-execute-block-python2-dedicated-test failed"))

(defun py-execute-block-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-python2-dedicated-switch-base arg teststring)))

(defun py-execute-block-python2-dedicated-switch-base ()
  (assert (markerp (py-execute-block-python2-dedicated-switch)) nil "py-execute-block-python2-dedicated-switch-test failed"))

(defun py-execute-block-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2.7-test\")"))
    (py-bug-tests-intern 'py-execute-block-python2.7-base arg teststring)))

(defun py-execute-block-python2.7-base ()
  (assert (markerp (py-execute-block-python2.7)) nil "py-execute-block-python2.7-test failed"))

(defun py-execute-block-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2.7-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-python2.7-switch-base arg teststring)))

(defun py-execute-block-python2.7-switch-base ()
  (assert (markerp (py-execute-block-python2.7-switch)) nil "py-execute-block-python2.7-switch-test failed"))

(defun py-execute-block-python2.7-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2.7-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-block-python2.7-noswitch-base arg teststring)))

(defun py-execute-block-python2.7-noswitch-base ()
  (assert (markerp (py-execute-block-python2.7-noswitch)) nil "py-execute-block-python2.7-noswitch-test failed"))

(defun py-execute-block-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2.7-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-block-python2.7-dedicated-base arg teststring)))

(defun py-execute-block-python2.7-dedicated-base ()
  (assert (markerp (py-execute-block-python2.7-dedicated)) nil "py-execute-block-python2.7-dedicated-test failed"))

(defun py-execute-block-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python2.7-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-block-python2.7-dedicated-switch-base ()
  (assert (markerp (py-execute-block-python2.7-dedicated-switch)) nil "py-execute-block-python2.7-dedicated-switch-test failed"))

(defun py-execute-block-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-jython-test\")"))
    (py-bug-tests-intern 'py-execute-block-jython-base arg teststring)))

(defun py-execute-block-jython-base ()
  (assert (markerp (py-execute-block-jython)) nil "py-execute-block-jython-test failed"))

(defun py-execute-block-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-jython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-jython-switch-base arg teststring)))

(defun py-execute-block-jython-switch-base ()
  (assert (markerp (py-execute-block-jython-switch)) nil "py-execute-block-jython-switch-test failed"))

(defun py-execute-block-jython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-jython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-block-jython-noswitch-base arg teststring)))

(defun py-execute-block-jython-noswitch-base ()
  (assert (markerp (py-execute-block-jython-noswitch)) nil "py-execute-block-jython-noswitch-test failed"))

(defun py-execute-block-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-jython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-block-jython-dedicated-base arg teststring)))

(defun py-execute-block-jython-dedicated-base ()
  (assert (markerp (py-execute-block-jython-dedicated)) nil "py-execute-block-jython-dedicated-test failed"))

(defun py-execute-block-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-jython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-jython-dedicated-switch-base arg teststring)))

(defun py-execute-block-jython-dedicated-switch-base ()
  (assert (markerp (py-execute-block-jython-dedicated-switch)) nil "py-execute-block-jython-dedicated-switch-test failed"))

(defun py-execute-block-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3.2-test\")"))
    (py-bug-tests-intern 'py-execute-block-python3.2-base arg teststring)))

(defun py-execute-block-python3.2-base ()
  (assert (markerp (py-execute-block-python3.2)) nil "py-execute-block-python3.2-test failed"))

(defun py-execute-block-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3.2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-python3.2-switch-base arg teststring)))

(defun py-execute-block-python3.2-switch-base ()
  (assert (markerp (py-execute-block-python3.2-switch)) nil "py-execute-block-python3.2-switch-test failed"))

(defun py-execute-block-python3.2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3.2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-block-python3.2-noswitch-base arg teststring)))

(defun py-execute-block-python3.2-noswitch-base ()
  (assert (markerp (py-execute-block-python3.2-noswitch)) nil "py-execute-block-python3.2-noswitch-test failed"))

(defun py-execute-block-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3.2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-block-python3.2-dedicated-base arg teststring)))

(defun py-execute-block-python3.2-dedicated-base ()
  (assert (markerp (py-execute-block-python3.2-dedicated)) nil "py-execute-block-python3.2-dedicated-test failed"))

(defun py-execute-block-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-python3.2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-block-python3.2-dedicated-switch-base ()
  (assert (markerp (py-execute-block-python3.2-dedicated-switch)) nil "py-execute-block-python3.2-dedicated-switch-test failed"))

(defun py-execute-block-or-clause-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python-base arg teststring)))

(defun py-execute-block-or-clause-python-base ()
  (assert (markerp (py-execute-block-or-clause-python)) nil "py-execute-block-or-clause-python-test failed"))

(defun py-execute-block-or-clause-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python-switch-base arg teststring)))

(defun py-execute-block-or-clause-python-switch-base ()
  (assert (markerp (py-execute-block-or-clause-python-switch)) nil "py-execute-block-or-clause-python-switch-test failed"))

(defun py-execute-block-or-clause-python-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python-noswitch-base arg teststring)))

(defun py-execute-block-or-clause-python-noswitch-base ()
  (assert (markerp (py-execute-block-or-clause-python-noswitch)) nil "py-execute-block-or-clause-python-noswitch-test failed"))

(defun py-execute-block-or-clause-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python-dedicated-base arg teststring)))

(defun py-execute-block-or-clause-python-dedicated-base ()
  (assert (markerp (py-execute-block-or-clause-python-dedicated)) nil "py-execute-block-or-clause-python-dedicated-test failed"))

(defun py-execute-block-or-clause-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python-dedicated-switch-base arg teststring)))

(defun py-execute-block-or-clause-python-dedicated-switch-base ()
  (assert (markerp (py-execute-block-or-clause-python-dedicated-switch)) nil "py-execute-block-or-clause-python-dedicated-switch-test failed"))

(defun py-execute-block-or-clause-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-ipython-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-ipython-base arg teststring)))

(defun py-execute-block-or-clause-ipython-base ()
  (assert (markerp (py-execute-block-or-clause-ipython)) nil "py-execute-block-or-clause-ipython-test failed"))

(defun py-execute-block-or-clause-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-ipython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-ipython-switch-base arg teststring)))

(defun py-execute-block-or-clause-ipython-switch-base ()
  (assert (markerp (py-execute-block-or-clause-ipython-switch)) nil "py-execute-block-or-clause-ipython-switch-test failed"))

(defun py-execute-block-or-clause-ipython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-ipython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-ipython-noswitch-base arg teststring)))

(defun py-execute-block-or-clause-ipython-noswitch-base ()
  (assert (markerp (py-execute-block-or-clause-ipython-noswitch)) nil "py-execute-block-or-clause-ipython-noswitch-test failed"))

(defun py-execute-block-or-clause-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-ipython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-ipython-dedicated-base arg teststring)))

(defun py-execute-block-or-clause-ipython-dedicated-base ()
  (assert (markerp (py-execute-block-or-clause-ipython-dedicated)) nil "py-execute-block-or-clause-ipython-dedicated-test failed"))

(defun py-execute-block-or-clause-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-ipython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-block-or-clause-ipython-dedicated-switch-base ()
  (assert (markerp (py-execute-block-or-clause-ipython-dedicated-switch)) nil "py-execute-block-or-clause-ipython-dedicated-switch-test failed"))

(defun py-execute-block-or-clause-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3-base arg teststring)))

(defun py-execute-block-or-clause-python3-base ()
  (assert (markerp (py-execute-block-or-clause-python3)) nil "py-execute-block-or-clause-python3-test failed"))

(defun py-execute-block-or-clause-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3-switch-base arg teststring)))

(defun py-execute-block-or-clause-python3-switch-base ()
  (assert (markerp (py-execute-block-or-clause-python3-switch)) nil "py-execute-block-or-clause-python3-switch-test failed"))

(defun py-execute-block-or-clause-python3-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3-noswitch-base arg teststring)))

(defun py-execute-block-or-clause-python3-noswitch-base ()
  (assert (markerp (py-execute-block-or-clause-python3-noswitch)) nil "py-execute-block-or-clause-python3-noswitch-test failed"))

(defun py-execute-block-or-clause-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3-dedicated-base arg teststring)))

(defun py-execute-block-or-clause-python3-dedicated-base ()
  (assert (markerp (py-execute-block-or-clause-python3-dedicated)) nil "py-execute-block-or-clause-python3-dedicated-test failed"))

(defun py-execute-block-or-clause-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3-dedicated-switch-base arg teststring)))

(defun py-execute-block-or-clause-python3-dedicated-switch-base ()
  (assert (markerp (py-execute-block-or-clause-python3-dedicated-switch)) nil "py-execute-block-or-clause-python3-dedicated-switch-test failed"))

(defun py-execute-block-or-clause-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2-base arg teststring)))

(defun py-execute-block-or-clause-python2-base ()
  (assert (markerp (py-execute-block-or-clause-python2)) nil "py-execute-block-or-clause-python2-test failed"))

(defun py-execute-block-or-clause-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2-switch-base arg teststring)))

(defun py-execute-block-or-clause-python2-switch-base ()
  (assert (markerp (py-execute-block-or-clause-python2-switch)) nil "py-execute-block-or-clause-python2-switch-test failed"))

(defun py-execute-block-or-clause-python2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2-noswitch-base arg teststring)))

(defun py-execute-block-or-clause-python2-noswitch-base ()
  (assert (markerp (py-execute-block-or-clause-python2-noswitch)) nil "py-execute-block-or-clause-python2-noswitch-test failed"))

(defun py-execute-block-or-clause-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2-dedicated-base arg teststring)))

(defun py-execute-block-or-clause-python2-dedicated-base ()
  (assert (markerp (py-execute-block-or-clause-python2-dedicated)) nil "py-execute-block-or-clause-python2-dedicated-test failed"))

(defun py-execute-block-or-clause-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2-dedicated-switch-base arg teststring)))

(defun py-execute-block-or-clause-python2-dedicated-switch-base ()
  (assert (markerp (py-execute-block-or-clause-python2-dedicated-switch)) nil "py-execute-block-or-clause-python2-dedicated-switch-test failed"))

(defun py-execute-block-or-clause-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2.7-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2.7-base arg teststring)))

(defun py-execute-block-or-clause-python2.7-base ()
  (assert (markerp (py-execute-block-or-clause-python2.7)) nil "py-execute-block-or-clause-python2.7-test failed"))

(defun py-execute-block-or-clause-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2.7-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2.7-switch-base arg teststring)))

(defun py-execute-block-or-clause-python2.7-switch-base ()
  (assert (markerp (py-execute-block-or-clause-python2.7-switch)) nil "py-execute-block-or-clause-python2.7-switch-test failed"))

(defun py-execute-block-or-clause-python2.7-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2.7-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2.7-noswitch-base arg teststring)))

(defun py-execute-block-or-clause-python2.7-noswitch-base ()
  (assert (markerp (py-execute-block-or-clause-python2.7-noswitch)) nil "py-execute-block-or-clause-python2.7-noswitch-test failed"))

(defun py-execute-block-or-clause-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2.7-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2.7-dedicated-base arg teststring)))

(defun py-execute-block-or-clause-python2.7-dedicated-base ()
  (assert (markerp (py-execute-block-or-clause-python2.7-dedicated)) nil "py-execute-block-or-clause-python2.7-dedicated-test failed"))

(defun py-execute-block-or-clause-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python2.7-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-block-or-clause-python2.7-dedicated-switch-base ()
  (assert (markerp (py-execute-block-or-clause-python2.7-dedicated-switch)) nil "py-execute-block-or-clause-python2.7-dedicated-switch-test failed"))

(defun py-execute-block-or-clause-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-jython-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-jython-base arg teststring)))

(defun py-execute-block-or-clause-jython-base ()
  (assert (markerp (py-execute-block-or-clause-jython)) nil "py-execute-block-or-clause-jython-test failed"))

(defun py-execute-block-or-clause-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-jython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-jython-switch-base arg teststring)))

(defun py-execute-block-or-clause-jython-switch-base ()
  (assert (markerp (py-execute-block-or-clause-jython-switch)) nil "py-execute-block-or-clause-jython-switch-test failed"))

(defun py-execute-block-or-clause-jython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-jython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-jython-noswitch-base arg teststring)))

(defun py-execute-block-or-clause-jython-noswitch-base ()
  (assert (markerp (py-execute-block-or-clause-jython-noswitch)) nil "py-execute-block-or-clause-jython-noswitch-test failed"))

(defun py-execute-block-or-clause-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-jython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-jython-dedicated-base arg teststring)))

(defun py-execute-block-or-clause-jython-dedicated-base ()
  (assert (markerp (py-execute-block-or-clause-jython-dedicated)) nil "py-execute-block-or-clause-jython-dedicated-test failed"))

(defun py-execute-block-or-clause-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-jython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-jython-dedicated-switch-base arg teststring)))

(defun py-execute-block-or-clause-jython-dedicated-switch-base ()
  (assert (markerp (py-execute-block-or-clause-jython-dedicated-switch)) nil "py-execute-block-or-clause-jython-dedicated-switch-test failed"))

(defun py-execute-block-or-clause-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3.2-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3.2-base arg teststring)))

(defun py-execute-block-or-clause-python3.2-base ()
  (assert (markerp (py-execute-block-or-clause-python3.2)) nil "py-execute-block-or-clause-python3.2-test failed"))

(defun py-execute-block-or-clause-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3.2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3.2-switch-base arg teststring)))

(defun py-execute-block-or-clause-python3.2-switch-base ()
  (assert (markerp (py-execute-block-or-clause-python3.2-switch)) nil "py-execute-block-or-clause-python3.2-switch-test failed"))

(defun py-execute-block-or-clause-python3.2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3.2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3.2-noswitch-base arg teststring)))

(defun py-execute-block-or-clause-python3.2-noswitch-base ()
  (assert (markerp (py-execute-block-or-clause-python3.2-noswitch)) nil "py-execute-block-or-clause-python3.2-noswitch-test failed"))

(defun py-execute-block-or-clause-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3.2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3.2-dedicated-base arg teststring)))

(defun py-execute-block-or-clause-python3.2-dedicated-base ()
  (assert (markerp (py-execute-block-or-clause-python3.2-dedicated)) nil "py-execute-block-or-clause-python3.2-dedicated-test failed"))

(defun py-execute-block-or-clause-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-python3.2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-block-or-clause-python3.2-dedicated-switch-base ()
  (assert (markerp (py-execute-block-or-clause-python3.2-dedicated-switch)) nil "py-execute-block-or-clause-python3.2-dedicated-switch-test failed"))

(defun py-execute-def-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python-test\")"))
    (py-bug-tests-intern 'py-execute-def-python-base arg teststring)))

(defun py-execute-def-python-base ()
  (assert (markerp (py-execute-def-python)) nil "py-execute-def-python-test failed"))

(defun py-execute-def-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python-switch-test\")"))
    (py-bug-tests-intern 'py-execute-def-python-switch-base arg teststring)))

(defun py-execute-def-python-switch-base ()
  (assert (markerp (py-execute-def-python-switch)) nil "py-execute-def-python-switch-test failed"))

(defun py-execute-def-python-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-def-python-noswitch-base arg teststring)))

(defun py-execute-def-python-noswitch-base ()
  (assert (markerp (py-execute-def-python-noswitch)) nil "py-execute-def-python-noswitch-test failed"))

(defun py-execute-def-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-def-python-dedicated-base arg teststring)))

(defun py-execute-def-python-dedicated-base ()
  (assert (markerp (py-execute-def-python-dedicated)) nil "py-execute-def-python-dedicated-test failed"))

(defun py-execute-def-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-def-python-dedicated-switch-base arg teststring)))

(defun py-execute-def-python-dedicated-switch-base ()
  (assert (markerp (py-execute-def-python-dedicated-switch)) nil "py-execute-def-python-dedicated-switch-test failed"))

(defun py-execute-def-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-ipython-test\")"))
    (py-bug-tests-intern 'py-execute-def-ipython-base arg teststring)))

(defun py-execute-def-ipython-base ()
  (assert (markerp (py-execute-def-ipython)) nil "py-execute-def-ipython-test failed"))

(defun py-execute-def-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-ipython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-def-ipython-switch-base arg teststring)))

(defun py-execute-def-ipython-switch-base ()
  (assert (markerp (py-execute-def-ipython-switch)) nil "py-execute-def-ipython-switch-test failed"))

(defun py-execute-def-ipython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-ipython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-def-ipython-noswitch-base arg teststring)))

(defun py-execute-def-ipython-noswitch-base ()
  (assert (markerp (py-execute-def-ipython-noswitch)) nil "py-execute-def-ipython-noswitch-test failed"))

(defun py-execute-def-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-ipython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-def-ipython-dedicated-base arg teststring)))

(defun py-execute-def-ipython-dedicated-base ()
  (assert (markerp (py-execute-def-ipython-dedicated)) nil "py-execute-def-ipython-dedicated-test failed"))

(defun py-execute-def-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-ipython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-def-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-def-ipython-dedicated-switch-base ()
  (assert (markerp (py-execute-def-ipython-dedicated-switch)) nil "py-execute-def-ipython-dedicated-switch-test failed"))

(defun py-execute-def-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3-test\")"))
    (py-bug-tests-intern 'py-execute-def-python3-base arg teststring)))

(defun py-execute-def-python3-base ()
  (assert (markerp (py-execute-def-python3)) nil "py-execute-def-python3-test failed"))

(defun py-execute-def-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3-switch-test\")"))
    (py-bug-tests-intern 'py-execute-def-python3-switch-base arg teststring)))

(defun py-execute-def-python3-switch-base ()
  (assert (markerp (py-execute-def-python3-switch)) nil "py-execute-def-python3-switch-test failed"))

(defun py-execute-def-python3-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-def-python3-noswitch-base arg teststring)))

(defun py-execute-def-python3-noswitch-base ()
  (assert (markerp (py-execute-def-python3-noswitch)) nil "py-execute-def-python3-noswitch-test failed"))

(defun py-execute-def-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-def-python3-dedicated-base arg teststring)))

(defun py-execute-def-python3-dedicated-base ()
  (assert (markerp (py-execute-def-python3-dedicated)) nil "py-execute-def-python3-dedicated-test failed"))

(defun py-execute-def-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-def-python3-dedicated-switch-base arg teststring)))

(defun py-execute-def-python3-dedicated-switch-base ()
  (assert (markerp (py-execute-def-python3-dedicated-switch)) nil "py-execute-def-python3-dedicated-switch-test failed"))

(defun py-execute-def-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2-test\")"))
    (py-bug-tests-intern 'py-execute-def-python2-base arg teststring)))

(defun py-execute-def-python2-base ()
  (assert (markerp (py-execute-def-python2)) nil "py-execute-def-python2-test failed"))

(defun py-execute-def-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-def-python2-switch-base arg teststring)))

(defun py-execute-def-python2-switch-base ()
  (assert (markerp (py-execute-def-python2-switch)) nil "py-execute-def-python2-switch-test failed"))

(defun py-execute-def-python2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-def-python2-noswitch-base arg teststring)))

(defun py-execute-def-python2-noswitch-base ()
  (assert (markerp (py-execute-def-python2-noswitch)) nil "py-execute-def-python2-noswitch-test failed"))

(defun py-execute-def-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-def-python2-dedicated-base arg teststring)))

(defun py-execute-def-python2-dedicated-base ()
  (assert (markerp (py-execute-def-python2-dedicated)) nil "py-execute-def-python2-dedicated-test failed"))

(defun py-execute-def-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-def-python2-dedicated-switch-base arg teststring)))

(defun py-execute-def-python2-dedicated-switch-base ()
  (assert (markerp (py-execute-def-python2-dedicated-switch)) nil "py-execute-def-python2-dedicated-switch-test failed"))

(defun py-execute-def-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2.7-test\")"))
    (py-bug-tests-intern 'py-execute-def-python2.7-base arg teststring)))

(defun py-execute-def-python2.7-base ()
  (assert (markerp (py-execute-def-python2.7)) nil "py-execute-def-python2.7-test failed"))

(defun py-execute-def-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2.7-switch-test\")"))
    (py-bug-tests-intern 'py-execute-def-python2.7-switch-base arg teststring)))

(defun py-execute-def-python2.7-switch-base ()
  (assert (markerp (py-execute-def-python2.7-switch)) nil "py-execute-def-python2.7-switch-test failed"))

(defun py-execute-def-python2.7-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2.7-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-def-python2.7-noswitch-base arg teststring)))

(defun py-execute-def-python2.7-noswitch-base ()
  (assert (markerp (py-execute-def-python2.7-noswitch)) nil "py-execute-def-python2.7-noswitch-test failed"))

(defun py-execute-def-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2.7-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-def-python2.7-dedicated-base arg teststring)))

(defun py-execute-def-python2.7-dedicated-base ()
  (assert (markerp (py-execute-def-python2.7-dedicated)) nil "py-execute-def-python2.7-dedicated-test failed"))

(defun py-execute-def-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python2.7-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-def-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-def-python2.7-dedicated-switch-base ()
  (assert (markerp (py-execute-def-python2.7-dedicated-switch)) nil "py-execute-def-python2.7-dedicated-switch-test failed"))

(defun py-execute-def-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-jython-test\")"))
    (py-bug-tests-intern 'py-execute-def-jython-base arg teststring)))

(defun py-execute-def-jython-base ()
  (assert (markerp (py-execute-def-jython)) nil "py-execute-def-jython-test failed"))

(defun py-execute-def-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-jython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-def-jython-switch-base arg teststring)))

(defun py-execute-def-jython-switch-base ()
  (assert (markerp (py-execute-def-jython-switch)) nil "py-execute-def-jython-switch-test failed"))

(defun py-execute-def-jython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-jython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-def-jython-noswitch-base arg teststring)))

(defun py-execute-def-jython-noswitch-base ()
  (assert (markerp (py-execute-def-jython-noswitch)) nil "py-execute-def-jython-noswitch-test failed"))

(defun py-execute-def-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-jython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-def-jython-dedicated-base arg teststring)))

(defun py-execute-def-jython-dedicated-base ()
  (assert (markerp (py-execute-def-jython-dedicated)) nil "py-execute-def-jython-dedicated-test failed"))

(defun py-execute-def-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-jython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-def-jython-dedicated-switch-base arg teststring)))

(defun py-execute-def-jython-dedicated-switch-base ()
  (assert (markerp (py-execute-def-jython-dedicated-switch)) nil "py-execute-def-jython-dedicated-switch-test failed"))

(defun py-execute-def-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3.2-test\")"))
    (py-bug-tests-intern 'py-execute-def-python3.2-base arg teststring)))

(defun py-execute-def-python3.2-base ()
  (assert (markerp (py-execute-def-python3.2)) nil "py-execute-def-python3.2-test failed"))

(defun py-execute-def-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3.2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-def-python3.2-switch-base arg teststring)))

(defun py-execute-def-python3.2-switch-base ()
  (assert (markerp (py-execute-def-python3.2-switch)) nil "py-execute-def-python3.2-switch-test failed"))

(defun py-execute-def-python3.2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3.2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-def-python3.2-noswitch-base arg teststring)))

(defun py-execute-def-python3.2-noswitch-base ()
  (assert (markerp (py-execute-def-python3.2-noswitch)) nil "py-execute-def-python3.2-noswitch-test failed"))

(defun py-execute-def-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3.2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-def-python3.2-dedicated-base arg teststring)))

(defun py-execute-def-python3.2-dedicated-base ()
  (assert (markerp (py-execute-def-python3.2-dedicated)) nil "py-execute-def-python3.2-dedicated-test failed"))

(defun py-execute-def-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-python3.2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-def-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-def-python3.2-dedicated-switch-base ()
  (assert (markerp (py-execute-def-python3.2-dedicated-switch)) nil "py-execute-def-python3.2-dedicated-switch-test failed"))

(defun py-execute-class-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python-test\")"))
    (py-bug-tests-intern 'py-execute-class-python-base arg teststring)))

(defun py-execute-class-python-base ()
  (assert (markerp (py-execute-class-python)) nil "py-execute-class-python-test failed"))

(defun py-execute-class-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python-switch-test\")"))
    (py-bug-tests-intern 'py-execute-class-python-switch-base arg teststring)))

(defun py-execute-class-python-switch-base ()
  (assert (markerp (py-execute-class-python-switch)) nil "py-execute-class-python-switch-test failed"))

(defun py-execute-class-python-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-class-python-noswitch-base arg teststring)))

(defun py-execute-class-python-noswitch-base ()
  (assert (markerp (py-execute-class-python-noswitch)) nil "py-execute-class-python-noswitch-test failed"))

(defun py-execute-class-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-class-python-dedicated-base arg teststring)))

(defun py-execute-class-python-dedicated-base ()
  (assert (markerp (py-execute-class-python-dedicated)) nil "py-execute-class-python-dedicated-test failed"))

(defun py-execute-class-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-class-python-dedicated-switch-base arg teststring)))

(defun py-execute-class-python-dedicated-switch-base ()
  (assert (markerp (py-execute-class-python-dedicated-switch)) nil "py-execute-class-python-dedicated-switch-test failed"))

(defun py-execute-class-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-ipython-test\")"))
    (py-bug-tests-intern 'py-execute-class-ipython-base arg teststring)))

(defun py-execute-class-ipython-base ()
  (assert (markerp (py-execute-class-ipython)) nil "py-execute-class-ipython-test failed"))

(defun py-execute-class-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-ipython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-class-ipython-switch-base arg teststring)))

(defun py-execute-class-ipython-switch-base ()
  (assert (markerp (py-execute-class-ipython-switch)) nil "py-execute-class-ipython-switch-test failed"))

(defun py-execute-class-ipython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-ipython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-class-ipython-noswitch-base arg teststring)))

(defun py-execute-class-ipython-noswitch-base ()
  (assert (markerp (py-execute-class-ipython-noswitch)) nil "py-execute-class-ipython-noswitch-test failed"))

(defun py-execute-class-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-ipython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-class-ipython-dedicated-base arg teststring)))

(defun py-execute-class-ipython-dedicated-base ()
  (assert (markerp (py-execute-class-ipython-dedicated)) nil "py-execute-class-ipython-dedicated-test failed"))

(defun py-execute-class-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-ipython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-class-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-class-ipython-dedicated-switch-base ()
  (assert (markerp (py-execute-class-ipython-dedicated-switch)) nil "py-execute-class-ipython-dedicated-switch-test failed"))

(defun py-execute-class-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3-test\")"))
    (py-bug-tests-intern 'py-execute-class-python3-base arg teststring)))

(defun py-execute-class-python3-base ()
  (assert (markerp (py-execute-class-python3)) nil "py-execute-class-python3-test failed"))

(defun py-execute-class-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3-switch-test\")"))
    (py-bug-tests-intern 'py-execute-class-python3-switch-base arg teststring)))

(defun py-execute-class-python3-switch-base ()
  (assert (markerp (py-execute-class-python3-switch)) nil "py-execute-class-python3-switch-test failed"))

(defun py-execute-class-python3-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-class-python3-noswitch-base arg teststring)))

(defun py-execute-class-python3-noswitch-base ()
  (assert (markerp (py-execute-class-python3-noswitch)) nil "py-execute-class-python3-noswitch-test failed"))

(defun py-execute-class-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-class-python3-dedicated-base arg teststring)))

(defun py-execute-class-python3-dedicated-base ()
  (assert (markerp (py-execute-class-python3-dedicated)) nil "py-execute-class-python3-dedicated-test failed"))

(defun py-execute-class-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-class-python3-dedicated-switch-base arg teststring)))

(defun py-execute-class-python3-dedicated-switch-base ()
  (assert (markerp (py-execute-class-python3-dedicated-switch)) nil "py-execute-class-python3-dedicated-switch-test failed"))

(defun py-execute-class-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2-test\")"))
    (py-bug-tests-intern 'py-execute-class-python2-base arg teststring)))

(defun py-execute-class-python2-base ()
  (assert (markerp (py-execute-class-python2)) nil "py-execute-class-python2-test failed"))

(defun py-execute-class-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-class-python2-switch-base arg teststring)))

(defun py-execute-class-python2-switch-base ()
  (assert (markerp (py-execute-class-python2-switch)) nil "py-execute-class-python2-switch-test failed"))

(defun py-execute-class-python2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-class-python2-noswitch-base arg teststring)))

(defun py-execute-class-python2-noswitch-base ()
  (assert (markerp (py-execute-class-python2-noswitch)) nil "py-execute-class-python2-noswitch-test failed"))

(defun py-execute-class-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-class-python2-dedicated-base arg teststring)))

(defun py-execute-class-python2-dedicated-base ()
  (assert (markerp (py-execute-class-python2-dedicated)) nil "py-execute-class-python2-dedicated-test failed"))

(defun py-execute-class-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-class-python2-dedicated-switch-base arg teststring)))

(defun py-execute-class-python2-dedicated-switch-base ()
  (assert (markerp (py-execute-class-python2-dedicated-switch)) nil "py-execute-class-python2-dedicated-switch-test failed"))

(defun py-execute-class-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2.7-test\")"))
    (py-bug-tests-intern 'py-execute-class-python2.7-base arg teststring)))

(defun py-execute-class-python2.7-base ()
  (assert (markerp (py-execute-class-python2.7)) nil "py-execute-class-python2.7-test failed"))

(defun py-execute-class-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2.7-switch-test\")"))
    (py-bug-tests-intern 'py-execute-class-python2.7-switch-base arg teststring)))

(defun py-execute-class-python2.7-switch-base ()
  (assert (markerp (py-execute-class-python2.7-switch)) nil "py-execute-class-python2.7-switch-test failed"))

(defun py-execute-class-python2.7-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2.7-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-class-python2.7-noswitch-base arg teststring)))

(defun py-execute-class-python2.7-noswitch-base ()
  (assert (markerp (py-execute-class-python2.7-noswitch)) nil "py-execute-class-python2.7-noswitch-test failed"))

(defun py-execute-class-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2.7-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-class-python2.7-dedicated-base arg teststring)))

(defun py-execute-class-python2.7-dedicated-base ()
  (assert (markerp (py-execute-class-python2.7-dedicated)) nil "py-execute-class-python2.7-dedicated-test failed"))

(defun py-execute-class-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python2.7-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-class-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-class-python2.7-dedicated-switch-base ()
  (assert (markerp (py-execute-class-python2.7-dedicated-switch)) nil "py-execute-class-python2.7-dedicated-switch-test failed"))

(defun py-execute-class-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-jython-test\")"))
    (py-bug-tests-intern 'py-execute-class-jython-base arg teststring)))

(defun py-execute-class-jython-base ()
  (assert (markerp (py-execute-class-jython)) nil "py-execute-class-jython-test failed"))

(defun py-execute-class-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-jython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-class-jython-switch-base arg teststring)))

(defun py-execute-class-jython-switch-base ()
  (assert (markerp (py-execute-class-jython-switch)) nil "py-execute-class-jython-switch-test failed"))

(defun py-execute-class-jython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-jython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-class-jython-noswitch-base arg teststring)))

(defun py-execute-class-jython-noswitch-base ()
  (assert (markerp (py-execute-class-jython-noswitch)) nil "py-execute-class-jython-noswitch-test failed"))

(defun py-execute-class-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-jython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-class-jython-dedicated-base arg teststring)))

(defun py-execute-class-jython-dedicated-base ()
  (assert (markerp (py-execute-class-jython-dedicated)) nil "py-execute-class-jython-dedicated-test failed"))

(defun py-execute-class-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-jython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-class-jython-dedicated-switch-base arg teststring)))

(defun py-execute-class-jython-dedicated-switch-base ()
  (assert (markerp (py-execute-class-jython-dedicated-switch)) nil "py-execute-class-jython-dedicated-switch-test failed"))

(defun py-execute-class-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3.2-test\")"))
    (py-bug-tests-intern 'py-execute-class-python3.2-base arg teststring)))

(defun py-execute-class-python3.2-base ()
  (assert (markerp (py-execute-class-python3.2)) nil "py-execute-class-python3.2-test failed"))

(defun py-execute-class-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3.2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-class-python3.2-switch-base arg teststring)))

(defun py-execute-class-python3.2-switch-base ()
  (assert (markerp (py-execute-class-python3.2-switch)) nil "py-execute-class-python3.2-switch-test failed"))

(defun py-execute-class-python3.2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3.2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-class-python3.2-noswitch-base arg teststring)))

(defun py-execute-class-python3.2-noswitch-base ()
  (assert (markerp (py-execute-class-python3.2-noswitch)) nil "py-execute-class-python3.2-noswitch-test failed"))

(defun py-execute-class-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3.2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-class-python3.2-dedicated-base arg teststring)))

(defun py-execute-class-python3.2-dedicated-base ()
  (assert (markerp (py-execute-class-python3.2-dedicated)) nil "py-execute-class-python3.2-dedicated-test failed"))

(defun py-execute-class-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-python3.2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-class-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-class-python3.2-dedicated-switch-base ()
  (assert (markerp (py-execute-class-python3.2-dedicated-switch)) nil "py-execute-class-python3.2-dedicated-switch-test failed"))

(defun py-execute-region-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python-test\")"))
    (py-bug-tests-intern 'py-execute-region-python-base arg teststring)))

(defun py-execute-region-python-base ()
  (assert (markerp (py-execute-region-python (line-beginning-position) (line-end-position))) nil "py-execute-region-python-test failed"))

(defun py-execute-region-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python-switch-test\")"))
    (py-bug-tests-intern 'py-execute-region-python-switch-base arg teststring)))

(defun py-execute-region-python-switch-base ()
  (assert (markerp (py-execute-region-python-switch (line-beginning-position) (line-end-position))) nil "py-execute-region-python-switch-test failed"))

(defun py-execute-region-python-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-region-python-noswitch-base arg teststring)))

(defun py-execute-region-python-noswitch-base ()
  (assert (markerp (py-execute-region-python-noswitch (line-beginning-position) (line-end-position))) nil "py-execute-region-python-noswitch-test failed"))

(defun py-execute-region-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-region-python-dedicated-base arg teststring)))

(defun py-execute-region-python-dedicated-base ()
  (assert (markerp (py-execute-region-python-dedicated (line-beginning-position) (line-end-position))) nil "py-execute-region-python-dedicated-test failed"))

(defun py-execute-region-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-region-python-dedicated-switch-base arg teststring)))

(defun py-execute-region-python-dedicated-switch-base ()
  (assert (markerp (py-execute-region-python-dedicated-switch (line-beginning-position) (line-end-position))) nil "py-execute-region-python-dedicated-switch-test failed"))

(defun py-execute-region-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-ipython-test\")"))
    (py-bug-tests-intern 'py-execute-region-ipython-base arg teststring)))

(defun py-execute-region-ipython-base ()
  (assert (markerp (py-execute-region-ipython (line-beginning-position) (line-end-position))) nil "py-execute-region-ipython-test failed"))

(defun py-execute-region-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-ipython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-region-ipython-switch-base arg teststring)))

(defun py-execute-region-ipython-switch-base ()
  (assert (markerp (py-execute-region-ipython-switch (line-beginning-position) (line-end-position))) nil "py-execute-region-ipython-switch-test failed"))

(defun py-execute-region-ipython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-ipython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-region-ipython-noswitch-base arg teststring)))

(defun py-execute-region-ipython-noswitch-base ()
  (assert (markerp (py-execute-region-ipython-noswitch (line-beginning-position) (line-end-position))) nil "py-execute-region-ipython-noswitch-test failed"))

(defun py-execute-region-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-ipython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-region-ipython-dedicated-base arg teststring)))

(defun py-execute-region-ipython-dedicated-base ()
  (assert (markerp (py-execute-region-ipython-dedicated (line-beginning-position) (line-end-position))) nil "py-execute-region-ipython-dedicated-test failed"))

(defun py-execute-region-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-ipython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-region-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-region-ipython-dedicated-switch-base ()
  (assert (markerp (py-execute-region-ipython-dedicated-switch (line-beginning-position) (line-end-position))) nil "py-execute-region-ipython-dedicated-switch-test failed"))

(defun py-execute-region-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3-test\")"))
    (py-bug-tests-intern 'py-execute-region-python3-base arg teststring)))

(defun py-execute-region-python3-base ()
  (assert (markerp (py-execute-region-python3 (line-beginning-position) (line-end-position))) nil "py-execute-region-python3-test failed"))

(defun py-execute-region-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3-switch-test\")"))
    (py-bug-tests-intern 'py-execute-region-python3-switch-base arg teststring)))

(defun py-execute-region-python3-switch-base ()
  (assert (markerp (py-execute-region-python3-switch (line-beginning-position) (line-end-position))) nil "py-execute-region-python3-switch-test failed"))

(defun py-execute-region-python3-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-region-python3-noswitch-base arg teststring)))

(defun py-execute-region-python3-noswitch-base ()
  (assert (markerp (py-execute-region-python3-noswitch (line-beginning-position) (line-end-position))) nil "py-execute-region-python3-noswitch-test failed"))

(defun py-execute-region-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-region-python3-dedicated-base arg teststring)))

(defun py-execute-region-python3-dedicated-base ()
  (assert (markerp (py-execute-region-python3-dedicated (line-beginning-position) (line-end-position))) nil "py-execute-region-python3-dedicated-test failed"))

(defun py-execute-region-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-region-python3-dedicated-switch-base arg teststring)))

(defun py-execute-region-python3-dedicated-switch-base ()
  (assert (markerp (py-execute-region-python3-dedicated-switch (line-beginning-position) (line-end-position))) nil "py-execute-region-python3-dedicated-switch-test failed"))

(defun py-execute-region-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2-test\")"))
    (py-bug-tests-intern 'py-execute-region-python2-base arg teststring)))

(defun py-execute-region-python2-base ()
  (assert (markerp (py-execute-region-python2 (line-beginning-position) (line-end-position))) nil "py-execute-region-python2-test failed"))

(defun py-execute-region-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-region-python2-switch-base arg teststring)))

(defun py-execute-region-python2-switch-base ()
  (assert (markerp (py-execute-region-python2-switch (line-beginning-position) (line-end-position))) nil "py-execute-region-python2-switch-test failed"))

(defun py-execute-region-python2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-region-python2-noswitch-base arg teststring)))

(defun py-execute-region-python2-noswitch-base ()
  (assert (markerp (py-execute-region-python2-noswitch (line-beginning-position) (line-end-position))) nil "py-execute-region-python2-noswitch-test failed"))

(defun py-execute-region-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-region-python2-dedicated-base arg teststring)))

(defun py-execute-region-python2-dedicated-base ()
  (assert (markerp (py-execute-region-python2-dedicated (line-beginning-position) (line-end-position))) nil "py-execute-region-python2-dedicated-test failed"))

(defun py-execute-region-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-region-python2-dedicated-switch-base arg teststring)))

(defun py-execute-region-python2-dedicated-switch-base ()
  (assert (markerp (py-execute-region-python2-dedicated-switch (line-beginning-position) (line-end-position))) nil "py-execute-region-python2-dedicated-switch-test failed"))

(defun py-execute-region-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2.7-test\")"))
    (py-bug-tests-intern 'py-execute-region-python2.7-base arg teststring)))

(defun py-execute-region-python2.7-base ()
  (assert (markerp (py-execute-region-python2.7 (line-beginning-position) (line-end-position))) nil "py-execute-region-python2.7-test failed"))

(defun py-execute-region-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2.7-switch-test\")"))
    (py-bug-tests-intern 'py-execute-region-python2.7-switch-base arg teststring)))

(defun py-execute-region-python2.7-switch-base ()
  (assert (markerp (py-execute-region-python2.7-switch (line-beginning-position) (line-end-position))) nil "py-execute-region-python2.7-switch-test failed"))

(defun py-execute-region-python2.7-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2.7-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-region-python2.7-noswitch-base arg teststring)))

(defun py-execute-region-python2.7-noswitch-base ()
  (assert (markerp (py-execute-region-python2.7-noswitch (line-beginning-position) (line-end-position))) nil "py-execute-region-python2.7-noswitch-test failed"))

(defun py-execute-region-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2.7-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-region-python2.7-dedicated-base arg teststring)))

(defun py-execute-region-python2.7-dedicated-base ()
  (assert (markerp (py-execute-region-python2.7-dedicated (line-beginning-position) (line-end-position))) nil "py-execute-region-python2.7-dedicated-test failed"))

(defun py-execute-region-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python2.7-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-region-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-region-python2.7-dedicated-switch-base ()
  (assert (markerp (py-execute-region-python2.7-dedicated-switch (line-beginning-position) (line-end-position))) nil "py-execute-region-python2.7-dedicated-switch-test failed"))

(defun py-execute-region-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-jython-test\")"))
    (py-bug-tests-intern 'py-execute-region-jython-base arg teststring)))

(defun py-execute-region-jython-base ()
  (assert (markerp (py-execute-region-jython (line-beginning-position) (line-end-position))) nil "py-execute-region-jython-test failed"))

(defun py-execute-region-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-jython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-region-jython-switch-base arg teststring)))

(defun py-execute-region-jython-switch-base ()
  (assert (markerp (py-execute-region-jython-switch (line-beginning-position) (line-end-position))) nil "py-execute-region-jython-switch-test failed"))

(defun py-execute-region-jython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-jython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-region-jython-noswitch-base arg teststring)))

(defun py-execute-region-jython-noswitch-base ()
  (assert (markerp (py-execute-region-jython-noswitch (line-beginning-position) (line-end-position))) nil "py-execute-region-jython-noswitch-test failed"))

(defun py-execute-region-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-jython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-region-jython-dedicated-base arg teststring)))

(defun py-execute-region-jython-dedicated-base ()
  (assert (markerp (py-execute-region-jython-dedicated (line-beginning-position) (line-end-position))) nil "py-execute-region-jython-dedicated-test failed"))

(defun py-execute-region-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-jython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-region-jython-dedicated-switch-base arg teststring)))

(defun py-execute-region-jython-dedicated-switch-base ()
  (assert (markerp (py-execute-region-jython-dedicated-switch (line-beginning-position) (line-end-position))) nil "py-execute-region-jython-dedicated-switch-test failed"))

(defun py-execute-region-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3.2-test\")"))
    (py-bug-tests-intern 'py-execute-region-python3.2-base arg teststring)))

(defun py-execute-region-python3.2-base ()
  (assert (markerp (py-execute-region-python3.2 (line-beginning-position) (line-end-position))) nil "py-execute-region-python3.2-test failed"))

(defun py-execute-region-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3.2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-region-python3.2-switch-base arg teststring)))

(defun py-execute-region-python3.2-switch-base ()
  (assert (markerp (py-execute-region-python3.2-switch (line-beginning-position) (line-end-position))) nil "py-execute-region-python3.2-switch-test failed"))

(defun py-execute-region-python3.2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3.2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-region-python3.2-noswitch-base arg teststring)))

(defun py-execute-region-python3.2-noswitch-base ()
  (assert (markerp (py-execute-region-python3.2-noswitch (line-beginning-position) (line-end-position))) nil "py-execute-region-python3.2-noswitch-test failed"))

(defun py-execute-region-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3.2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-region-python3.2-dedicated-base arg teststring)))

(defun py-execute-region-python3.2-dedicated-base ()
  (assert (markerp (py-execute-region-python3.2-dedicated (line-beginning-position) (line-end-position))) nil "py-execute-region-python3.2-dedicated-test failed"))

(defun py-execute-region-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-python3.2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-region-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-region-python3.2-dedicated-switch-base ()
  (assert (markerp (py-execute-region-python3.2-dedicated-switch (line-beginning-position) (line-end-position))) nil "py-execute-region-python3.2-dedicated-switch-test failed"))

(defun py-execute-buffer-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python-base arg teststring)))

(defun py-execute-buffer-python-base ()
  (assert (markerp (py-execute-buffer-python)) nil "py-execute-buffer-python-test failed"))

(defun py-execute-buffer-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python-switch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python-switch-base arg teststring)))

(defun py-execute-buffer-python-switch-base ()
  (assert (markerp (py-execute-buffer-python-switch)) nil "py-execute-buffer-python-switch-test failed"))

(defun py-execute-buffer-python-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python-noswitch-base arg teststring)))

(defun py-execute-buffer-python-noswitch-base ()
  (assert (markerp (py-execute-buffer-python-noswitch)) nil "py-execute-buffer-python-noswitch-test failed"))

(defun py-execute-buffer-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python-dedicated-base arg teststring)))

(defun py-execute-buffer-python-dedicated-base ()
  (assert (markerp (py-execute-buffer-python-dedicated)) nil "py-execute-buffer-python-dedicated-test failed"))

(defun py-execute-buffer-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python-dedicated-switch-base arg teststring)))

(defun py-execute-buffer-python-dedicated-switch-base ()
  (assert (markerp (py-execute-buffer-python-dedicated-switch)) nil "py-execute-buffer-python-dedicated-switch-test failed"))

(defun py-execute-buffer-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-ipython-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-ipython-base arg teststring)))

(defun py-execute-buffer-ipython-base ()
  (assert (markerp (py-execute-buffer-ipython)) nil "py-execute-buffer-ipython-test failed"))

(defun py-execute-buffer-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-ipython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-ipython-switch-base arg teststring)))

(defun py-execute-buffer-ipython-switch-base ()
  (assert (markerp (py-execute-buffer-ipython-switch)) nil "py-execute-buffer-ipython-switch-test failed"))

(defun py-execute-buffer-ipython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-ipython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-ipython-noswitch-base arg teststring)))

(defun py-execute-buffer-ipython-noswitch-base ()
  (assert (markerp (py-execute-buffer-ipython-noswitch)) nil "py-execute-buffer-ipython-noswitch-test failed"))

(defun py-execute-buffer-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-ipython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-ipython-dedicated-base arg teststring)))

(defun py-execute-buffer-ipython-dedicated-base ()
  (assert (markerp (py-execute-buffer-ipython-dedicated)) nil "py-execute-buffer-ipython-dedicated-test failed"))

(defun py-execute-buffer-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-ipython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-buffer-ipython-dedicated-switch-base ()
  (assert (markerp (py-execute-buffer-ipython-dedicated-switch)) nil "py-execute-buffer-ipython-dedicated-switch-test failed"))

(defun py-execute-buffer-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python3-base arg teststring)))

(defun py-execute-buffer-python3-base ()
  (assert (markerp (py-execute-buffer-python3)) nil "py-execute-buffer-python3-test failed"))

(defun py-execute-buffer-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3-switch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python3-switch-base arg teststring)))

(defun py-execute-buffer-python3-switch-base ()
  (assert (markerp (py-execute-buffer-python3-switch)) nil "py-execute-buffer-python3-switch-test failed"))

(defun py-execute-buffer-python3-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python3-noswitch-base arg teststring)))

(defun py-execute-buffer-python3-noswitch-base ()
  (assert (markerp (py-execute-buffer-python3-noswitch)) nil "py-execute-buffer-python3-noswitch-test failed"))

(defun py-execute-buffer-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python3-dedicated-base arg teststring)))

(defun py-execute-buffer-python3-dedicated-base ()
  (assert (markerp (py-execute-buffer-python3-dedicated)) nil "py-execute-buffer-python3-dedicated-test failed"))

(defun py-execute-buffer-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python3-dedicated-switch-base arg teststring)))

(defun py-execute-buffer-python3-dedicated-switch-base ()
  (assert (markerp (py-execute-buffer-python3-dedicated-switch)) nil "py-execute-buffer-python3-dedicated-switch-test failed"))

(defun py-execute-buffer-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python2-base arg teststring)))

(defun py-execute-buffer-python2-base ()
  (assert (markerp (py-execute-buffer-python2)) nil "py-execute-buffer-python2-test failed"))

(defun py-execute-buffer-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python2-switch-base arg teststring)))

(defun py-execute-buffer-python2-switch-base ()
  (assert (markerp (py-execute-buffer-python2-switch)) nil "py-execute-buffer-python2-switch-test failed"))

(defun py-execute-buffer-python2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python2-noswitch-base arg teststring)))

(defun py-execute-buffer-python2-noswitch-base ()
  (assert (markerp (py-execute-buffer-python2-noswitch)) nil "py-execute-buffer-python2-noswitch-test failed"))

(defun py-execute-buffer-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python2-dedicated-base arg teststring)))

(defun py-execute-buffer-python2-dedicated-base ()
  (assert (markerp (py-execute-buffer-python2-dedicated)) nil "py-execute-buffer-python2-dedicated-test failed"))

(defun py-execute-buffer-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python2-dedicated-switch-base arg teststring)))

(defun py-execute-buffer-python2-dedicated-switch-base ()
  (assert (markerp (py-execute-buffer-python2-dedicated-switch)) nil "py-execute-buffer-python2-dedicated-switch-test failed"))

(defun py-execute-buffer-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2.7-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python2.7-base arg teststring)))

(defun py-execute-buffer-python2.7-base ()
  (assert (markerp (py-execute-buffer-python2.7)) nil "py-execute-buffer-python2.7-test failed"))

(defun py-execute-buffer-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2.7-switch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python2.7-switch-base arg teststring)))

(defun py-execute-buffer-python2.7-switch-base ()
  (assert (markerp (py-execute-buffer-python2.7-switch)) nil "py-execute-buffer-python2.7-switch-test failed"))

(defun py-execute-buffer-python2.7-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2.7-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python2.7-noswitch-base arg teststring)))

(defun py-execute-buffer-python2.7-noswitch-base ()
  (assert (markerp (py-execute-buffer-python2.7-noswitch)) nil "py-execute-buffer-python2.7-noswitch-test failed"))

(defun py-execute-buffer-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2.7-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python2.7-dedicated-base arg teststring)))

(defun py-execute-buffer-python2.7-dedicated-base ()
  (assert (markerp (py-execute-buffer-python2.7-dedicated)) nil "py-execute-buffer-python2.7-dedicated-test failed"))

(defun py-execute-buffer-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python2.7-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-buffer-python2.7-dedicated-switch-base ()
  (assert (markerp (py-execute-buffer-python2.7-dedicated-switch)) nil "py-execute-buffer-python2.7-dedicated-switch-test failed"))

(defun py-execute-buffer-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-jython-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-jython-base arg teststring)))

(defun py-execute-buffer-jython-base ()
  (assert (markerp (py-execute-buffer-jython)) nil "py-execute-buffer-jython-test failed"))

(defun py-execute-buffer-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-jython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-jython-switch-base arg teststring)))

(defun py-execute-buffer-jython-switch-base ()
  (assert (markerp (py-execute-buffer-jython-switch)) nil "py-execute-buffer-jython-switch-test failed"))

(defun py-execute-buffer-jython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-jython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-jython-noswitch-base arg teststring)))

(defun py-execute-buffer-jython-noswitch-base ()
  (assert (markerp (py-execute-buffer-jython-noswitch)) nil "py-execute-buffer-jython-noswitch-test failed"))

(defun py-execute-buffer-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-jython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-jython-dedicated-base arg teststring)))

(defun py-execute-buffer-jython-dedicated-base ()
  (assert (markerp (py-execute-buffer-jython-dedicated)) nil "py-execute-buffer-jython-dedicated-test failed"))

(defun py-execute-buffer-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-jython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-jython-dedicated-switch-base arg teststring)))

(defun py-execute-buffer-jython-dedicated-switch-base ()
  (assert (markerp (py-execute-buffer-jython-dedicated-switch)) nil "py-execute-buffer-jython-dedicated-switch-test failed"))

(defun py-execute-buffer-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3.2-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python3.2-base arg teststring)))

(defun py-execute-buffer-python3.2-base ()
  (assert (markerp (py-execute-buffer-python3.2)) nil "py-execute-buffer-python3.2-test failed"))

(defun py-execute-buffer-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3.2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python3.2-switch-base arg teststring)))

(defun py-execute-buffer-python3.2-switch-base ()
  (assert (markerp (py-execute-buffer-python3.2-switch)) nil "py-execute-buffer-python3.2-switch-test failed"))

(defun py-execute-buffer-python3.2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3.2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python3.2-noswitch-base arg teststring)))

(defun py-execute-buffer-python3.2-noswitch-base ()
  (assert (markerp (py-execute-buffer-python3.2-noswitch)) nil "py-execute-buffer-python3.2-noswitch-test failed"))

(defun py-execute-buffer-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3.2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python3.2-dedicated-base arg teststring)))

(defun py-execute-buffer-python3.2-dedicated-base ()
  (assert (markerp (py-execute-buffer-python3.2-dedicated)) nil "py-execute-buffer-python3.2-dedicated-test failed"))

(defun py-execute-buffer-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-python3.2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-buffer-python3.2-dedicated-switch-base ()
  (assert (markerp (py-execute-buffer-python3.2-dedicated-switch)) nil "py-execute-buffer-python3.2-dedicated-switch-test failed"))

(defun py-execute-expression-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python-base arg teststring)))

(defun py-execute-expression-python-base ()
  (assert (markerp (py-execute-expression-python)) nil "py-execute-expression-python-test failed"))

(defun py-execute-expression-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python-switch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python-switch-base arg teststring)))

(defun py-execute-expression-python-switch-base ()
  (assert (markerp (py-execute-expression-python-switch)) nil "py-execute-expression-python-switch-test failed"))

(defun py-execute-expression-python-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python-noswitch-base arg teststring)))

(defun py-execute-expression-python-noswitch-base ()
  (assert (markerp (py-execute-expression-python-noswitch)) nil "py-execute-expression-python-noswitch-test failed"))

(defun py-execute-expression-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python-dedicated-base arg teststring)))

(defun py-execute-expression-python-dedicated-base ()
  (assert (markerp (py-execute-expression-python-dedicated)) nil "py-execute-expression-python-dedicated-test failed"))

(defun py-execute-expression-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python-dedicated-switch-base arg teststring)))

(defun py-execute-expression-python-dedicated-switch-base ()
  (assert (markerp (py-execute-expression-python-dedicated-switch)) nil "py-execute-expression-python-dedicated-switch-test failed"))

(defun py-execute-expression-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-ipython-test\")"))
    (py-bug-tests-intern 'py-execute-expression-ipython-base arg teststring)))

(defun py-execute-expression-ipython-base ()
  (assert (markerp (py-execute-expression-ipython)) nil "py-execute-expression-ipython-test failed"))

(defun py-execute-expression-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-ipython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-ipython-switch-base arg teststring)))

(defun py-execute-expression-ipython-switch-base ()
  (assert (markerp (py-execute-expression-ipython-switch)) nil "py-execute-expression-ipython-switch-test failed"))

(defun py-execute-expression-ipython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-ipython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-ipython-noswitch-base arg teststring)))

(defun py-execute-expression-ipython-noswitch-base ()
  (assert (markerp (py-execute-expression-ipython-noswitch)) nil "py-execute-expression-ipython-noswitch-test failed"))

(defun py-execute-expression-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-ipython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-expression-ipython-dedicated-base arg teststring)))

(defun py-execute-expression-ipython-dedicated-base ()
  (assert (markerp (py-execute-expression-ipython-dedicated)) nil "py-execute-expression-ipython-dedicated-test failed"))

(defun py-execute-expression-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-ipython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-expression-ipython-dedicated-switch-base ()
  (assert (markerp (py-execute-expression-ipython-dedicated-switch)) nil "py-execute-expression-ipython-dedicated-switch-test failed"))

(defun py-execute-expression-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python3-base arg teststring)))

(defun py-execute-expression-python3-base ()
  (assert (markerp (py-execute-expression-python3)) nil "py-execute-expression-python3-test failed"))

(defun py-execute-expression-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3-switch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python3-switch-base arg teststring)))

(defun py-execute-expression-python3-switch-base ()
  (assert (markerp (py-execute-expression-python3-switch)) nil "py-execute-expression-python3-switch-test failed"))

(defun py-execute-expression-python3-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python3-noswitch-base arg teststring)))

(defun py-execute-expression-python3-noswitch-base ()
  (assert (markerp (py-execute-expression-python3-noswitch)) nil "py-execute-expression-python3-noswitch-test failed"))

(defun py-execute-expression-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python3-dedicated-base arg teststring)))

(defun py-execute-expression-python3-dedicated-base ()
  (assert (markerp (py-execute-expression-python3-dedicated)) nil "py-execute-expression-python3-dedicated-test failed"))

(defun py-execute-expression-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python3-dedicated-switch-base arg teststring)))

(defun py-execute-expression-python3-dedicated-switch-base ()
  (assert (markerp (py-execute-expression-python3-dedicated-switch)) nil "py-execute-expression-python3-dedicated-switch-test failed"))

(defun py-execute-expression-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python2-base arg teststring)))

(defun py-execute-expression-python2-base ()
  (assert (markerp (py-execute-expression-python2)) nil "py-execute-expression-python2-test failed"))

(defun py-execute-expression-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python2-switch-base arg teststring)))

(defun py-execute-expression-python2-switch-base ()
  (assert (markerp (py-execute-expression-python2-switch)) nil "py-execute-expression-python2-switch-test failed"))

(defun py-execute-expression-python2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python2-noswitch-base arg teststring)))

(defun py-execute-expression-python2-noswitch-base ()
  (assert (markerp (py-execute-expression-python2-noswitch)) nil "py-execute-expression-python2-noswitch-test failed"))

(defun py-execute-expression-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python2-dedicated-base arg teststring)))

(defun py-execute-expression-python2-dedicated-base ()
  (assert (markerp (py-execute-expression-python2-dedicated)) nil "py-execute-expression-python2-dedicated-test failed"))

(defun py-execute-expression-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python2-dedicated-switch-base arg teststring)))

(defun py-execute-expression-python2-dedicated-switch-base ()
  (assert (markerp (py-execute-expression-python2-dedicated-switch)) nil "py-execute-expression-python2-dedicated-switch-test failed"))

(defun py-execute-expression-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2.7-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python2.7-base arg teststring)))

(defun py-execute-expression-python2.7-base ()
  (assert (markerp (py-execute-expression-python2.7)) nil "py-execute-expression-python2.7-test failed"))

(defun py-execute-expression-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2.7-switch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python2.7-switch-base arg teststring)))

(defun py-execute-expression-python2.7-switch-base ()
  (assert (markerp (py-execute-expression-python2.7-switch)) nil "py-execute-expression-python2.7-switch-test failed"))

(defun py-execute-expression-python2.7-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2.7-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python2.7-noswitch-base arg teststring)))

(defun py-execute-expression-python2.7-noswitch-base ()
  (assert (markerp (py-execute-expression-python2.7-noswitch)) nil "py-execute-expression-python2.7-noswitch-test failed"))

(defun py-execute-expression-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2.7-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python2.7-dedicated-base arg teststring)))

(defun py-execute-expression-python2.7-dedicated-base ()
  (assert (markerp (py-execute-expression-python2.7-dedicated)) nil "py-execute-expression-python2.7-dedicated-test failed"))

(defun py-execute-expression-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python2.7-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-expression-python2.7-dedicated-switch-base ()
  (assert (markerp (py-execute-expression-python2.7-dedicated-switch)) nil "py-execute-expression-python2.7-dedicated-switch-test failed"))

(defun py-execute-expression-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-jython-test\")"))
    (py-bug-tests-intern 'py-execute-expression-jython-base arg teststring)))

(defun py-execute-expression-jython-base ()
  (assert (markerp (py-execute-expression-jython)) nil "py-execute-expression-jython-test failed"))

(defun py-execute-expression-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-jython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-jython-switch-base arg teststring)))

(defun py-execute-expression-jython-switch-base ()
  (assert (markerp (py-execute-expression-jython-switch)) nil "py-execute-expression-jython-switch-test failed"))

(defun py-execute-expression-jython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-jython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-jython-noswitch-base arg teststring)))

(defun py-execute-expression-jython-noswitch-base ()
  (assert (markerp (py-execute-expression-jython-noswitch)) nil "py-execute-expression-jython-noswitch-test failed"))

(defun py-execute-expression-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-jython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-expression-jython-dedicated-base arg teststring)))

(defun py-execute-expression-jython-dedicated-base ()
  (assert (markerp (py-execute-expression-jython-dedicated)) nil "py-execute-expression-jython-dedicated-test failed"))

(defun py-execute-expression-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-jython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-jython-dedicated-switch-base arg teststring)))

(defun py-execute-expression-jython-dedicated-switch-base ()
  (assert (markerp (py-execute-expression-jython-dedicated-switch)) nil "py-execute-expression-jython-dedicated-switch-test failed"))

(defun py-execute-expression-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3.2-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python3.2-base arg teststring)))

(defun py-execute-expression-python3.2-base ()
  (assert (markerp (py-execute-expression-python3.2)) nil "py-execute-expression-python3.2-test failed"))

(defun py-execute-expression-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3.2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python3.2-switch-base arg teststring)))

(defun py-execute-expression-python3.2-switch-base ()
  (assert (markerp (py-execute-expression-python3.2-switch)) nil "py-execute-expression-python3.2-switch-test failed"))

(defun py-execute-expression-python3.2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3.2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python3.2-noswitch-base arg teststring)))

(defun py-execute-expression-python3.2-noswitch-base ()
  (assert (markerp (py-execute-expression-python3.2-noswitch)) nil "py-execute-expression-python3.2-noswitch-test failed"))

(defun py-execute-expression-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3.2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python3.2-dedicated-base arg teststring)))

(defun py-execute-expression-python3.2-dedicated-base ()
  (assert (markerp (py-execute-expression-python3.2-dedicated)) nil "py-execute-expression-python3.2-dedicated-test failed"))

(defun py-execute-expression-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-python3.2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-expression-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-expression-python3.2-dedicated-switch-base ()
  (assert (markerp (py-execute-expression-python3.2-dedicated-switch)) nil "py-execute-expression-python3.2-dedicated-switch-test failed"))

(defun py-execute-partial-expression-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python-base arg teststring)))

(defun py-execute-partial-expression-python-base ()
  (assert (markerp (py-execute-partial-expression-python)) nil "py-execute-partial-expression-python-test failed"))

(defun py-execute-partial-expression-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python-switch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python-switch-base arg teststring)))

(defun py-execute-partial-expression-python-switch-base ()
  (assert (markerp (py-execute-partial-expression-python-switch)) nil "py-execute-partial-expression-python-switch-test failed"))

(defun py-execute-partial-expression-python-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python-noswitch-base arg teststring)))

(defun py-execute-partial-expression-python-noswitch-base ()
  (assert (markerp (py-execute-partial-expression-python-noswitch)) nil "py-execute-partial-expression-python-noswitch-test failed"))

(defun py-execute-partial-expression-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python-dedicated-base arg teststring)))

(defun py-execute-partial-expression-python-dedicated-base ()
  (assert (markerp (py-execute-partial-expression-python-dedicated)) nil "py-execute-partial-expression-python-dedicated-test failed"))

(defun py-execute-partial-expression-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python-dedicated-switch-base arg teststring)))

(defun py-execute-partial-expression-python-dedicated-switch-base ()
  (assert (markerp (py-execute-partial-expression-python-dedicated-switch)) nil "py-execute-partial-expression-python-dedicated-switch-test failed"))

(defun py-execute-partial-expression-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-ipython-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-ipython-base arg teststring)))

(defun py-execute-partial-expression-ipython-base ()
  (assert (markerp (py-execute-partial-expression-ipython)) nil "py-execute-partial-expression-ipython-test failed"))

(defun py-execute-partial-expression-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-ipython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-ipython-switch-base arg teststring)))

(defun py-execute-partial-expression-ipython-switch-base ()
  (assert (markerp (py-execute-partial-expression-ipython-switch)) nil "py-execute-partial-expression-ipython-switch-test failed"))

(defun py-execute-partial-expression-ipython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-ipython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-ipython-noswitch-base arg teststring)))

(defun py-execute-partial-expression-ipython-noswitch-base ()
  (assert (markerp (py-execute-partial-expression-ipython-noswitch)) nil "py-execute-partial-expression-ipython-noswitch-test failed"))

(defun py-execute-partial-expression-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-ipython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-ipython-dedicated-base arg teststring)))

(defun py-execute-partial-expression-ipython-dedicated-base ()
  (assert (markerp (py-execute-partial-expression-ipython-dedicated)) nil "py-execute-partial-expression-ipython-dedicated-test failed"))

(defun py-execute-partial-expression-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-ipython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-partial-expression-ipython-dedicated-switch-base ()
  (assert (markerp (py-execute-partial-expression-ipython-dedicated-switch)) nil "py-execute-partial-expression-ipython-dedicated-switch-test failed"))

(defun py-execute-partial-expression-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python3-base arg teststring)))

(defun py-execute-partial-expression-python3-base ()
  (assert (markerp (py-execute-partial-expression-python3)) nil "py-execute-partial-expression-python3-test failed"))

(defun py-execute-partial-expression-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3-switch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python3-switch-base arg teststring)))

(defun py-execute-partial-expression-python3-switch-base ()
  (assert (markerp (py-execute-partial-expression-python3-switch)) nil "py-execute-partial-expression-python3-switch-test failed"))

(defun py-execute-partial-expression-python3-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python3-noswitch-base arg teststring)))

(defun py-execute-partial-expression-python3-noswitch-base ()
  (assert (markerp (py-execute-partial-expression-python3-noswitch)) nil "py-execute-partial-expression-python3-noswitch-test failed"))

(defun py-execute-partial-expression-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python3-dedicated-base arg teststring)))

(defun py-execute-partial-expression-python3-dedicated-base ()
  (assert (markerp (py-execute-partial-expression-python3-dedicated)) nil "py-execute-partial-expression-python3-dedicated-test failed"))

(defun py-execute-partial-expression-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python3-dedicated-switch-base arg teststring)))

(defun py-execute-partial-expression-python3-dedicated-switch-base ()
  (assert (markerp (py-execute-partial-expression-python3-dedicated-switch)) nil "py-execute-partial-expression-python3-dedicated-switch-test failed"))

(defun py-execute-partial-expression-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python2-base arg teststring)))

(defun py-execute-partial-expression-python2-base ()
  (assert (markerp (py-execute-partial-expression-python2)) nil "py-execute-partial-expression-python2-test failed"))

(defun py-execute-partial-expression-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python2-switch-base arg teststring)))

(defun py-execute-partial-expression-python2-switch-base ()
  (assert (markerp (py-execute-partial-expression-python2-switch)) nil "py-execute-partial-expression-python2-switch-test failed"))

(defun py-execute-partial-expression-python2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python2-noswitch-base arg teststring)))

(defun py-execute-partial-expression-python2-noswitch-base ()
  (assert (markerp (py-execute-partial-expression-python2-noswitch)) nil "py-execute-partial-expression-python2-noswitch-test failed"))

(defun py-execute-partial-expression-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python2-dedicated-base arg teststring)))

(defun py-execute-partial-expression-python2-dedicated-base ()
  (assert (markerp (py-execute-partial-expression-python2-dedicated)) nil "py-execute-partial-expression-python2-dedicated-test failed"))

(defun py-execute-partial-expression-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python2-dedicated-switch-base arg teststring)))

(defun py-execute-partial-expression-python2-dedicated-switch-base ()
  (assert (markerp (py-execute-partial-expression-python2-dedicated-switch)) nil "py-execute-partial-expression-python2-dedicated-switch-test failed"))

(defun py-execute-partial-expression-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2.7-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python2.7-base arg teststring)))

(defun py-execute-partial-expression-python2.7-base ()
  (assert (markerp (py-execute-partial-expression-python2.7)) nil "py-execute-partial-expression-python2.7-test failed"))

(defun py-execute-partial-expression-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2.7-switch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python2.7-switch-base arg teststring)))

(defun py-execute-partial-expression-python2.7-switch-base ()
  (assert (markerp (py-execute-partial-expression-python2.7-switch)) nil "py-execute-partial-expression-python2.7-switch-test failed"))

(defun py-execute-partial-expression-python2.7-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2.7-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python2.7-noswitch-base arg teststring)))

(defun py-execute-partial-expression-python2.7-noswitch-base ()
  (assert (markerp (py-execute-partial-expression-python2.7-noswitch)) nil "py-execute-partial-expression-python2.7-noswitch-test failed"))

(defun py-execute-partial-expression-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2.7-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python2.7-dedicated-base arg teststring)))

(defun py-execute-partial-expression-python2.7-dedicated-base ()
  (assert (markerp (py-execute-partial-expression-python2.7-dedicated)) nil "py-execute-partial-expression-python2.7-dedicated-test failed"))

(defun py-execute-partial-expression-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python2.7-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-partial-expression-python2.7-dedicated-switch-base ()
  (assert (markerp (py-execute-partial-expression-python2.7-dedicated-switch)) nil "py-execute-partial-expression-python2.7-dedicated-switch-test failed"))

(defun py-execute-partial-expression-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-jython-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-jython-base arg teststring)))

(defun py-execute-partial-expression-jython-base ()
  (assert (markerp (py-execute-partial-expression-jython)) nil "py-execute-partial-expression-jython-test failed"))

(defun py-execute-partial-expression-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-jython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-jython-switch-base arg teststring)))

(defun py-execute-partial-expression-jython-switch-base ()
  (assert (markerp (py-execute-partial-expression-jython-switch)) nil "py-execute-partial-expression-jython-switch-test failed"))

(defun py-execute-partial-expression-jython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-jython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-jython-noswitch-base arg teststring)))

(defun py-execute-partial-expression-jython-noswitch-base ()
  (assert (markerp (py-execute-partial-expression-jython-noswitch)) nil "py-execute-partial-expression-jython-noswitch-test failed"))

(defun py-execute-partial-expression-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-jython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-jython-dedicated-base arg teststring)))

(defun py-execute-partial-expression-jython-dedicated-base ()
  (assert (markerp (py-execute-partial-expression-jython-dedicated)) nil "py-execute-partial-expression-jython-dedicated-test failed"))

(defun py-execute-partial-expression-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-jython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-jython-dedicated-switch-base arg teststring)))

(defun py-execute-partial-expression-jython-dedicated-switch-base ()
  (assert (markerp (py-execute-partial-expression-jython-dedicated-switch)) nil "py-execute-partial-expression-jython-dedicated-switch-test failed"))

(defun py-execute-partial-expression-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3.2-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python3.2-base arg teststring)))

(defun py-execute-partial-expression-python3.2-base ()
  (assert (markerp (py-execute-partial-expression-python3.2)) nil "py-execute-partial-expression-python3.2-test failed"))

(defun py-execute-partial-expression-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3.2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python3.2-switch-base arg teststring)))

(defun py-execute-partial-expression-python3.2-switch-base ()
  (assert (markerp (py-execute-partial-expression-python3.2-switch)) nil "py-execute-partial-expression-python3.2-switch-test failed"))

(defun py-execute-partial-expression-python3.2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3.2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python3.2-noswitch-base arg teststring)))

(defun py-execute-partial-expression-python3.2-noswitch-base ()
  (assert (markerp (py-execute-partial-expression-python3.2-noswitch)) nil "py-execute-partial-expression-python3.2-noswitch-test failed"))

(defun py-execute-partial-expression-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3.2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python3.2-dedicated-base arg teststring)))

(defun py-execute-partial-expression-python3.2-dedicated-base ()
  (assert (markerp (py-execute-partial-expression-python3.2-dedicated)) nil "py-execute-partial-expression-python3.2-dedicated-test failed"))

(defun py-execute-partial-expression-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-python3.2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-partial-expression-python3.2-dedicated-switch-base ()
  (assert (markerp (py-execute-partial-expression-python3.2-dedicated-switch)) nil "py-execute-partial-expression-python3.2-dedicated-switch-test failed"))

(defun py-execute-line-python-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python-test\")"))
    (py-bug-tests-intern 'py-execute-line-python-base arg teststring)))

(defun py-execute-line-python-base ()
  (assert (markerp (py-execute-line-python)) nil "py-execute-line-python-test failed"))

(defun py-execute-line-python-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python-switch-test\")"))
    (py-bug-tests-intern 'py-execute-line-python-switch-base arg teststring)))

(defun py-execute-line-python-switch-base ()
  (assert (markerp (py-execute-line-python-switch)) nil "py-execute-line-python-switch-test failed"))

(defun py-execute-line-python-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-line-python-noswitch-base arg teststring)))

(defun py-execute-line-python-noswitch-base ()
  (assert (markerp (py-execute-line-python-noswitch)) nil "py-execute-line-python-noswitch-test failed"))

(defun py-execute-line-python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-line-python-dedicated-base arg teststring)))

(defun py-execute-line-python-dedicated-base ()
  (assert (markerp (py-execute-line-python-dedicated)) nil "py-execute-line-python-dedicated-test failed"))

(defun py-execute-line-python-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-line-python-dedicated-switch-base arg teststring)))

(defun py-execute-line-python-dedicated-switch-base ()
  (assert (markerp (py-execute-line-python-dedicated-switch)) nil "py-execute-line-python-dedicated-switch-test failed"))

(defun py-execute-line-ipython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-ipython-test\")"))
    (py-bug-tests-intern 'py-execute-line-ipython-base arg teststring)))

(defun py-execute-line-ipython-base ()
  (assert (markerp (py-execute-line-ipython)) nil "py-execute-line-ipython-test failed"))

(defun py-execute-line-ipython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-ipython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-line-ipython-switch-base arg teststring)))

(defun py-execute-line-ipython-switch-base ()
  (assert (markerp (py-execute-line-ipython-switch)) nil "py-execute-line-ipython-switch-test failed"))

(defun py-execute-line-ipython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-ipython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-line-ipython-noswitch-base arg teststring)))

(defun py-execute-line-ipython-noswitch-base ()
  (assert (markerp (py-execute-line-ipython-noswitch)) nil "py-execute-line-ipython-noswitch-test failed"))

(defun py-execute-line-ipython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-ipython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-line-ipython-dedicated-base arg teststring)))

(defun py-execute-line-ipython-dedicated-base ()
  (assert (markerp (py-execute-line-ipython-dedicated)) nil "py-execute-line-ipython-dedicated-test failed"))

(defun py-execute-line-ipython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-ipython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-line-ipython-dedicated-switch-base arg teststring)))

(defun py-execute-line-ipython-dedicated-switch-base ()
  (assert (markerp (py-execute-line-ipython-dedicated-switch)) nil "py-execute-line-ipython-dedicated-switch-test failed"))

(defun py-execute-line-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3-test\")"))
    (py-bug-tests-intern 'py-execute-line-python3-base arg teststring)))

(defun py-execute-line-python3-base ()
  (assert (markerp (py-execute-line-python3)) nil "py-execute-line-python3-test failed"))

(defun py-execute-line-python3-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3-switch-test\")"))
    (py-bug-tests-intern 'py-execute-line-python3-switch-base arg teststring)))

(defun py-execute-line-python3-switch-base ()
  (assert (markerp (py-execute-line-python3-switch)) nil "py-execute-line-python3-switch-test failed"))

(defun py-execute-line-python3-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-line-python3-noswitch-base arg teststring)))

(defun py-execute-line-python3-noswitch-base ()
  (assert (markerp (py-execute-line-python3-noswitch)) nil "py-execute-line-python3-noswitch-test failed"))

(defun py-execute-line-python3-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-line-python3-dedicated-base arg teststring)))

(defun py-execute-line-python3-dedicated-base ()
  (assert (markerp (py-execute-line-python3-dedicated)) nil "py-execute-line-python3-dedicated-test failed"))

(defun py-execute-line-python3-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-line-python3-dedicated-switch-base arg teststring)))

(defun py-execute-line-python3-dedicated-switch-base ()
  (assert (markerp (py-execute-line-python3-dedicated-switch)) nil "py-execute-line-python3-dedicated-switch-test failed"))

(defun py-execute-line-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2-test\")"))
    (py-bug-tests-intern 'py-execute-line-python2-base arg teststring)))

(defun py-execute-line-python2-base ()
  (assert (markerp (py-execute-line-python2)) nil "py-execute-line-python2-test failed"))

(defun py-execute-line-python2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-line-python2-switch-base arg teststring)))

(defun py-execute-line-python2-switch-base ()
  (assert (markerp (py-execute-line-python2-switch)) nil "py-execute-line-python2-switch-test failed"))

(defun py-execute-line-python2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-line-python2-noswitch-base arg teststring)))

(defun py-execute-line-python2-noswitch-base ()
  (assert (markerp (py-execute-line-python2-noswitch)) nil "py-execute-line-python2-noswitch-test failed"))

(defun py-execute-line-python2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-line-python2-dedicated-base arg teststring)))

(defun py-execute-line-python2-dedicated-base ()
  (assert (markerp (py-execute-line-python2-dedicated)) nil "py-execute-line-python2-dedicated-test failed"))

(defun py-execute-line-python2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-line-python2-dedicated-switch-base arg teststring)))

(defun py-execute-line-python2-dedicated-switch-base ()
  (assert (markerp (py-execute-line-python2-dedicated-switch)) nil "py-execute-line-python2-dedicated-switch-test failed"))

(defun py-execute-line-python2.7-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2.7-test\")"))
    (py-bug-tests-intern 'py-execute-line-python2.7-base arg teststring)))

(defun py-execute-line-python2.7-base ()
  (assert (markerp (py-execute-line-python2.7)) nil "py-execute-line-python2.7-test failed"))

(defun py-execute-line-python2.7-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2.7-switch-test\")"))
    (py-bug-tests-intern 'py-execute-line-python2.7-switch-base arg teststring)))

(defun py-execute-line-python2.7-switch-base ()
  (assert (markerp (py-execute-line-python2.7-switch)) nil "py-execute-line-python2.7-switch-test failed"))

(defun py-execute-line-python2.7-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2.7-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-line-python2.7-noswitch-base arg teststring)))

(defun py-execute-line-python2.7-noswitch-base ()
  (assert (markerp (py-execute-line-python2.7-noswitch)) nil "py-execute-line-python2.7-noswitch-test failed"))

(defun py-execute-line-python2.7-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2.7-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-line-python2.7-dedicated-base arg teststring)))

(defun py-execute-line-python2.7-dedicated-base ()
  (assert (markerp (py-execute-line-python2.7-dedicated)) nil "py-execute-line-python2.7-dedicated-test failed"))

(defun py-execute-line-python2.7-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python2.7-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-line-python2.7-dedicated-switch-base arg teststring)))

(defun py-execute-line-python2.7-dedicated-switch-base ()
  (assert (markerp (py-execute-line-python2.7-dedicated-switch)) nil "py-execute-line-python2.7-dedicated-switch-test failed"))

(defun py-execute-line-jython-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-jython-test\")"))
    (py-bug-tests-intern 'py-execute-line-jython-base arg teststring)))

(defun py-execute-line-jython-base ()
  (assert (markerp (py-execute-line-jython)) nil "py-execute-line-jython-test failed"))

(defun py-execute-line-jython-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-jython-switch-test\")"))
    (py-bug-tests-intern 'py-execute-line-jython-switch-base arg teststring)))

(defun py-execute-line-jython-switch-base ()
  (assert (markerp (py-execute-line-jython-switch)) nil "py-execute-line-jython-switch-test failed"))

(defun py-execute-line-jython-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-jython-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-line-jython-noswitch-base arg teststring)))

(defun py-execute-line-jython-noswitch-base ()
  (assert (markerp (py-execute-line-jython-noswitch)) nil "py-execute-line-jython-noswitch-test failed"))

(defun py-execute-line-jython-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-jython-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-line-jython-dedicated-base arg teststring)))

(defun py-execute-line-jython-dedicated-base ()
  (assert (markerp (py-execute-line-jython-dedicated)) nil "py-execute-line-jython-dedicated-test failed"))

(defun py-execute-line-jython-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-jython-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-line-jython-dedicated-switch-base arg teststring)))

(defun py-execute-line-jython-dedicated-switch-base ()
  (assert (markerp (py-execute-line-jython-dedicated-switch)) nil "py-execute-line-jython-dedicated-switch-test failed"))

(defun py-execute-line-python3.2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3.2-test\")"))
    (py-bug-tests-intern 'py-execute-line-python3.2-base arg teststring)))

(defun py-execute-line-python3.2-base ()
  (assert (markerp (py-execute-line-python3.2)) nil "py-execute-line-python3.2-test failed"))

(defun py-execute-line-python3.2-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3.2-switch-test\")"))
    (py-bug-tests-intern 'py-execute-line-python3.2-switch-base arg teststring)))

(defun py-execute-line-python3.2-switch-base ()
  (assert (markerp (py-execute-line-python3.2-switch)) nil "py-execute-line-python3.2-switch-test failed"))

(defun py-execute-line-python3.2-noswitch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3.2-noswitch-test\")"))
    (py-bug-tests-intern 'py-execute-line-python3.2-noswitch-base arg teststring)))

(defun py-execute-line-python3.2-noswitch-base ()
  (assert (markerp (py-execute-line-python3.2-noswitch)) nil "py-execute-line-python3.2-noswitch-test failed"))

(defun py-execute-line-python3.2-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3.2-dedicated-test\")"))
    (py-bug-tests-intern 'py-execute-line-python3.2-dedicated-base arg teststring)))

(defun py-execute-line-python3.2-dedicated-base ()
  (assert (markerp (py-execute-line-python3.2-dedicated)) nil "py-execute-line-python3.2-dedicated-test failed"))

(defun py-execute-line-python3.2-dedicated-switch-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-python3.2-dedicated-switch-test\")"))
    (py-bug-tests-intern 'py-execute-line-python3.2-dedicated-switch-base arg teststring)))

(defun py-execute-line-python3.2-dedicated-switch-base ()
  (assert (markerp (py-execute-line-python3.2-dedicated-switch)) nil "py-execute-line-python3.2-dedicated-switch-test failed"))

(provide 'python-extended-executes-test)
;;; python-extended-executes-test.el ends here

