;; Time-stamp: <2014-09-30 16:29:36 kmodi>

;; Temporary minor mode
;; Main use is to enable it only in specific buffers to achieve the goal of
;; buffer-specific keymap

(defvar temp-mode-map (make-sparse-keymap)
  "Keymap while temp-mode is active.")

;;;###autoload
(define-minor-mode temp-mode
  "A temporary minor mode to be activated only specific to a buffer."
  nil
  :lighter " Temp"
  temp-mode-map)

;;;###autoload
(defun turn-on-temp-mode ()
  "Turns on temp-mode."
  (interactive)

  (temp-mode t))

;;;###autoload
(defun turn-off-temp-mode ()
  "Turns off temp-mode."
  (interactive)
  (temp-mode -1))

(provide 'temp-mode)
