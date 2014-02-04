;;; py-shell-completion-tests.el --- Test completion for available Python shell

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

;;; Commentary: Edit `py-test-pyshellname-list' before
;; running this test-builder or give a list of shells as
;; arguments

;;; Code:

(setq python-mode-shell-complete-tests
      (list

       'python-shell-complete-test
       'usr-bin-python-shell-complete-test
       'usr-bin-python2.7-shell-complete-test
       ;; 'arbeit-python-epdfree-epd_free-7.2-2-rh5-x86-bin-python2.7-shell-complete-test
       'usr-bin-python3-shell-complete-test
       ;; 'usr-bin-python3.2-shell-complete-test
       ;; 'ipython-shell-complete-test
       ;; 'usr-bin-ipython-shell-complete-test
       ;; 'arbeit-python-epd_free-7.1-2-rh5-x86-bin-ipython-shell-complete-test
       )
)

(defun py-run-shell-complete-tests ()
  (interactive)
  (dolist (ele python-mode-shell-complete-tests)
    (funcall ele)
    ;; (sit-for 1)
))

(defun python-shell-complete-test ()
  (interactive)
  (let (py-shell-switch-buffers-on-execute-p
        py-split-windows-on-execute-p)
    (set-buffer (py-shell nil t "python" nil "/"))
    (when (interactive-p) (switch-to-buffer (current-buffer)))
    (sit-for 0.2 t)
    (goto-char (point-max))
    (save-excursion
      (insert "pri")
      (py-shell-complete))
    (assert (looking-at "print") nil "python-shell-complete-test failed")
    (message "%s" "python-shell-complete-test passed")))


(defun usr-bin-python-shell-complete-test ()
  (interactive)
  (let (py-shell-switch-buffers-on-execute-p
        py-split-windows-on-execute-p)
    (set-buffer (py-shell nil t "/usr/bin/python" nil "/"))
    (switch-to-buffer (current-buffer))
    (sit-for 0.1)
    (goto-char (point-max))
    (insert "pri")
    (py-shell-complete)
    (forward-word -1)
    (assert (looking-at "print") nil "usr-bin-python-shell-complete-test failed")
    (when py-verbose-p (message "%s" "usr-bin-python-shell-complete-test passed"))))


(defun usr-bin-python2.7-shell-complete-test ()
  (interactive)
  (let (py-shell-switch-buffers-on-execute-p
        py-split-windows-on-execute-p)
    (set-buffer (py-shell nil t "/usr/bin/python2.7" nil "/"))
    (when (interactive-p) (switch-to-buffer (current-buffer)))
    (sit-for 0.1)
    (goto-char (point-max))
    (insert "pri")
    (py-shell-complete)
    (forward-word -1)
    (assert (looking-at "print") nil "usr-bin-python2.7-shell-complete-test failed")
    (message "%s" "usr-bin-python2.7-shell-complete-test passed")))


(defun arbeit-python-epdfree-epd_free-7.2-2-rh5-x86-bin-python2.7-shell-complete-test ()
  (interactive)
  (let (py-shell-switch-buffers-on-execute-p
        py-split-windows-on-execute-p)
    (set-buffer (py-shell nil t "~/arbeit/python/epdfree/epd_free-7.2-2-rh5-x86/bin/python2.7" nil "/"))
    (sit-for 0.2 t)
    (goto-char (point-max))
    (insert "pri")
    (py-shell-complete)
    (sit-for 0.1)
    (forward-word -1)
    (assert (looking-at "print") nil "arbeit-python-epdfree-epd_free-7.2-2-rh5-x86-bin-python2.7-shell-complete-test failed")
    (when py-verbose-p (message "%s" "arbeit-python-epdfree-epd_free-7.2-2-rh5-x86-bin-python2.7-shell-complete-test passed"))))

(defun usr-bin-python3-shell-complete-test ()
  (interactive)
  (let (py-shell-switch-buffers-on-execute-p
        py-split-windows-on-execute-p)
    (set-buffer (py-shell nil t "/usr/local/bin/python3" nil "/"))
    (when (interactive-p) (switch-to-buffer (current-buffer)))
    (goto-char (point-max))
    (insert "pri")
    (py-shell-complete)
    (forward-word -1)
    (sit-for 0.1)
    (assert (looking-at "print") nil "usr-bin-python3-shell-complete-test failed")
    (message "%s" "usr-bin-python3-shell-complete-test passed")))

(defun ipython-shell-complete-test ()
  (interactive)
  (let (py-shell-switch-buffers-on-execute-p
        py-split-windows-on-execute-p)
    (set-buffer (py-shell nil t "ipython" nil "/"))
    (sit-for 0.1)
    (goto-char (point-max))
    (insert "pri")
    (py-shell-complete)
    (sit-for 0.1)
    (forward-word -1)
    (assert (looking-at "print") nil "ipython-shell-complete-test failed")
    (message "%s" "ipython-shell-complete-test passed")))


(defun usr-bin-ipython-shell-complete-test ()
  (interactive)
  (let (py-shell-switch-buffers-on-execute-p
        py-split-windows-on-execute-p)
    (set-buffer (py-shell nil t "/usr/bin/ipython" nil "/"))
    (sit-for 0.1)
    (goto-char (point-max))
    (insert "pri")
    (py-shell-complete)
    (sit-for 1 t)
    (forward-word -1)
    (assert (looking-at "print") nil "usr-bin-ipython-shell-complete-test failed")
    (message "%s" "usr-bin-ipython-shell-complete-test passed")))


(defun arbeit-python-epd_free-7.1-2-rh5-x86-bin-ipython-shell-complete-test ()
  (interactive)
  (let (py-shell-switch-buffers-on-execute-p
        py-split-windows-on-execute-p)
    (set-buffer (py-shell nil t "~/arbeit/python/epd_free-7.1-2-rh5-x86/bin/ipython" nil "/"))
    (sit-for 0.1)
    (switch-to-buffer (current-buffer))
    (goto-char (point-max))
    (insert "pri")
    (py-shell-complete)
    (sit-for 1 t)
    (forward-word -1)
    (assert (looking-at "print") nil "arbeit-python-epd_free-7.1-2-rh5-x86-bin-ipython-shell-complete-test failed")
    (message "%s" "arbeit-python-epd_free-7.1-2-rh5-x86-bin-ipython-shell-complete-test passed")))



(provide 'py-shell-completion-tests)
;;; py-shell-completion-tests ends here

