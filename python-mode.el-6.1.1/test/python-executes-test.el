;;; python-executes-test.el --- executes test
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

(defun py-execute-statement-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-statement-test\")"))
    (py-bug-tests-intern 'py-execute-statement-base arg teststring)))

(defun py-execute-statement-base ()
  (assert (markerp (py-execute-statement)) nil "py-execute-statement-test failed"))

(defun py-execute-block-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-test\")"))
    (py-bug-tests-intern 'py-execute-block-base arg teststring)))

(defun py-execute-block-base ()
  (assert (markerp (py-execute-block)) nil "py-execute-block-test failed"))

(defun py-execute-block-or-clause-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True: print(\"I'm the py-execute-block-or-clause-test\")"))
    (py-bug-tests-intern 'py-execute-block-or-clause-base arg teststring)))

(defun py-execute-block-or-clause-base ()
  (assert (markerp (py-execute-block-or-clause)) nil "py-execute-block-or-clause-test failed"))

(defun py-execute-def-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo (): print(\"I'm the py-execute-def-test\")"))
    (py-bug-tests-intern 'py-execute-def-base arg teststring)))

(defun py-execute-def-base ()
  (assert (markerp (py-execute-def)) nil "py-execute-def-test failed"))

(defun py-execute-class-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class foo (): print(\"I'm the py-execute-class-test\")"))
    (py-bug-tests-intern 'py-execute-class-base arg teststring)))

(defun py-execute-class-base ()
  (assert (markerp (py-execute-class)) nil "py-execute-class-test failed"))

(defun py-execute-region-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-region-test\")"))
    (py-bug-tests-intern 'py-execute-region-base arg teststring)))

(defun py-execute-region-base ()
  (assert (markerp (py-execute-region (line-beginning-position) (line-end-position))) nil "py-execute-region-test failed"))

(defun py-execute-buffer-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-buffer-test\")"))
    (py-bug-tests-intern 'py-execute-buffer-test-base arg teststring)))

(defun py-execute-buffer-test-base ()
  (assert (markerp (py-execute-buffer)) nil "py-execute-buffer-test failed"))

(defun py-execute-expression-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-expression-test\")"))
    (py-bug-tests-intern 'py-execute-expression-base arg teststring)))

(defun py-execute-expression-base ()
  (assert (markerp (py-execute-expression)) nil "py-execute-expression-test failed"))

(defun py-execute-partial-expression-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-partial-expression-test\")"))
    (py-bug-tests-intern 'py-execute-partial-expression-base arg teststring)))

(defun py-execute-partial-expression-base ()
  (assert (markerp (py-execute-partial-expression)) nil "py-execute-partial-expression-test failed"))

(defun py-execute-line-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print(\"I'm the py-execute-line-test\")"))
    (py-bug-tests-intern 'py-execute-line-base arg teststring)))

(defun py-execute-line-base ()
  (assert (markerp (py-execute-line)) nil "py-execute-line-test failed"))

(provide 'python-extended-executes-test)
;;; python-extended-executes-test.el ends here
