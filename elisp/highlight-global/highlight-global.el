;;; highlight-global.el --- package for highlighting multi symbols accross ALL buffers

;; Copyright 2013-2014 Glen Dai
;; Author: Glen Dai <gafglen@gmail.com>
;; Keywords: highlight
;; URL: https://github.com/glen-dai/highlight-global
;; Version: 0.01

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

;;; Commentary:

;; When reading source code with EMACS, couples of related files will
;; be opened simultaneously. A function/variable defined in one file
;; will be referenced by another function in another file. A
;; highlight of the corresponding function/variable across these
;; buffers will make code reading much more friendly.

;; EMACS support multi highlight symbols in one buffer but not
;; accross multi buffers. This package provide highlighting of
;; symbols accross all buffer.

;; When new highlight is being added, only the windows of current
;; frame are updated. Whenever frame configration change(say new
;; window is added to current frame), window's highlight will be
;; updated to make windows currently showing always has the updated
;; highlights. This way is much more effecient than iterating the
;; overall buffer-list to update highlights for every buffer.
;;
;; Multi symbols can be highlighted simultaneously. Different
;; highlights have different face. You could add your highlight face
;; to `highlight-global-hl-faces'.

;;; How to use?

;; Put the package into load-path, and load the packaget
;;     (require 'highlight-global)

;; Toggle highlight of current region (or symbol under cursor if
;; region is not active) and bind it like this:
;;     (global-set-key (kbd "M-H") 'highlight-global-hl-frame-toggle)

;; Clear all highlight of current frame, and bind it like this:
;;     (global-set-key (kbd "M-C") 'highlight-global-clear-hl-frame)


(require 'hi-lock)

(defvar highlight-global-hl-faces
  '(('hi-yellow . 0)
    ('hi-pink   . 0)
    ('hi-green  . 0)
    ('hi-blue   . 0))
  "Default faces for hi-lock interactive functions, you could add your own.")

;; List to store what had been highlighted
(defvar highlight-global-hl-list nil
  "Global highlight list, always store the updated highlight
  regexp list, and every item is stored like this
  ((hilight-str1 . hilight-face1)
   (hilight-str2 . hilight-face2) ...)")

(defvar highlight-global-hl-list-update-timestamp 0.0
  "Store the timestamp when `highlight-global-hl-list' was updated.")

(defvar highlight-global-new-unhighlight nil
  "Stores thing to be unhighlight.")

(defvar highlight-global-new-highlight nil
  "Stores thing to be highlight.")

(defvar highlight-global--buffer-highlight-list nil
  "Stores the regexp highlighted by `highlight-global-hl-window' function.")
(make-variable-buffer-local 'highlight-global--buffer-highlight-list)
(put 'highlight-global--buffer-highlight-list 'permanent-local t)

;; set it to -0.5 to make sure first time it will update
(defvar highlight-global--buffer-highlight-list-update-timestamp -0.5
  "Stores the recently timestamp when `highlight-global--buffer-highlight-list'
was updated.")
(make-variable-buffer-local 'highlight-global--buffer-highlight-list-update-timestamp)
(put 'highlight-global--buffer-highlight-list-update-timestamp 'permanent-local t)

(defun highlight-global--clear-all-faces ()
  "Reset all face's usage count to zero."
  (dolist (item highlight-global-hl-faces)
    (setcdr item 0)))

(defun highlight-global--find-and-use-face ()
  "Find the least used face and increase it, the face will be returned to caller."
  (let ((least-used-one (nth 0 highlight-global-hl-faces)))
    (progn
      (dolist (face highlight-global-hl-faces)
        (when (< (cdr face) (cdr least-used-one))
          (setq least-used-one face)))
      (setcdr least-used-one (+ 1 (cdr least-used-one)))
      (car least-used-one))))

(defun highlight-global--release-face (face-to-release)
  "Release the use of a face by decreasing the count."
  (dolist (face highlight-global-hl-faces)
    (when (equal (car face) face-to-release)
      (setcdr face (- (cdr face) 1)))))

(defun highlight-global-clear-highlight-window (win)
  "Clear all highlight of current buffer, called by `unhighlit-windows-all'
when iterating all windows. When a buffer is being buried, this function also
will be called to clear all highlight."
  (select-window win)
  (setq highlight-global--buffer-highlight-list-update-timestamp (float-time))
  (dolist (item highlight-global-hl-list)
    (font-lock-remove-keywords
     nil
     `((,(car item) 0 ,(cdr item) prepend)))
    (font-lock-fontify-buffer)))

(defun highlight-global-unhl-window (win)
  "Highligt a buffer, should update of buffer-local
highlight-list and timestamp, used by `walk-windows'"
  (select-window win)
  (setq highlight-global--buffer-highlight-list-update-timestamp (float-time))
  (setq highlight-global--buffer-highlight-list
        (delete highlight-global-new-unhighlight
                highlight-global--buffer-highlight-list))
  ;; add new highlight to current buffer's keyword list
  (font-lock-remove-keywords
   nil
   `((,(car highlight-global-new-unhighlight)
      0
      ,(cdr highlight-global-new-unhighlight) prepend)))
  (font-lock-fontify-buffer))

(defun highlight-global-hl-window (win)
  "Highligt a buffer, should update buffer-local highlight-list and timestamp."
  (select-window win)
  (setq highlight-global--buffer-highlight-list-update-timestamp (float-time))
  (push highlight-global-new-highlight highlight-global--buffer-highlight-list)
  (font-lock-add-keywords
   nil
   `((,(car highlight-global-new-highlight) 0 ,(cdr highlight-global-new-highlight) prepend)) 'append)
  (font-lock-fontify-buffer))

(defun highlight-global-get-thing-to-highlight ()
  "Get thing to highlight.
If active region, get region, else get symbol under cursor."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (if (thing-at-point 'symbol)
        (buffer-substring-no-properties
         (car (bounds-of-thing-at-point 'symbol))
         (cdr (bounds-of-thing-at-point 'symbol))))))

(defun highlight-global-check-whether-highlighted (hi)
  "Check if HI is already highlighted by checking `highlight-global-hl-list'."
  (let ((the-found-one nil))
    (progn
      (dolist (item highlight-global-hl-list)
        (when (equal hi (car item))
          (progn
            (setq the-found-one item))))
      the-found-one)))

(defun highlight-global-update-current-buffer-hl ()
  "Update a buffer's highlight to be consistent with global highlight."
  (if (<= highlight-global--buffer-highlight-list-update-timestamp
          highlight-global-hl-list-update-timestamp)
      (if (null highlight-global-hl-list) ; clear buffer
          ;; 1) global null
          (progn
            (dolist (item highlight-global--buffer-highlight-list)
              (font-lock-remove-keywords
               nil
               `((,(car item) 0 ,(cdr item) prepend)))
              (font-lock-fontify-buffer))
            (setq highlight-global--buffer-highlight-list-update-timestamp (float-time))
            (setq highlight-global--buffer-highlight-list nil))
        ;; 2) global is not null, now update local to global
        (progn
          ;; (message "Updating buffer : %s" (current-buffer))
          ;; 2.1) iterate buffer-list to delete
          (dolist (item highlight-global--buffer-highlight-list)
            (if (not (member item highlight-global-hl-list))
                (progn
                  (font-lock-remove-keywords
                   nil
                   `((,(car item) 0 ,(cdr item) prepend)))
                  (font-lock-fontify-buffer)
                  (setq highlight-global--buffer-highlight-list
                        (delete item highlight-global--buffer-highlight-list)))))
          ;; 2.2) iterate global-list to add
          (dolist (item highlight-global-hl-list)
            (if (not (member item highlight-global--buffer-highlight-list))
                (progn
                  (font-lock-add-keywords
                   nil
                   `((,(car item) 0 ,(cdr item) prepend)) 'append)
                  (font-lock-fontify-buffer)
                  (push item highlight-global--buffer-highlight-list))))
          (setq highlight-global--buffer-highlight-list-update-timestamp (float-time)))
        ;; (message "no need to update : %s" (current-buffer))
        )))

(defun highlight-global-update-hl-fixup (frame)
  "Automatically update new buffer's highlights when any windows on current
frame changed. This will make buffers that to be showned because of window
splitting alway has highlights updated to date."
  (highlight-global-force-hl-frame))

;; register the on-the-fly highlight-list updating strategy to
;; window-size-change-functions hook
(if (null window-size-change-functions)
    (setq window-size-change-functions '(highlight-global-update-hl-fixup))
  (add-to-list 'window-size-change-functions 'highlight-global-update-hl-fixup))

;;;###autoload
(defun highlight-global-hl-frame-toggle ()
  (interactive)
  (let* ((thing-to-highlight (highlight-global-get-thing-to-highlight))
         (hi nil)
         (face nil))
    (if (stringp thing-to-highlight)
        (progn
          (setq hi (highlight-global-check-whether-highlighted thing-to-highlight))
          ;; toogle highlight, 2 cases
          ;; 1) thing already unlighlight and stored in list, unhighight it
          ;; 2) new highlight, highlight it and add it to list
          (if hi
              ;; 1) toogle off
              ;;    1. delete item from global-list && update timestamp
              ;;    2. set highlight-global-new-unhighlight and unhighight each window
              (progn
                (setq highlight-global-new-unhighlight hi)
                (highlight-global--release-face (cdr highlight-global-new-unhighlight))
                (setq highlight-global-hl-list
                      (delete highlight-global-new-unhighlight highlight-global-hl-list))
                (setq highlight-global-hl-list-update-timestamp (float-time))
                (walk-windows 'highlight-global-unhl-window))
            ;; 2) new highlight
            (progn
              (setq highlight-global-new-highlight
                    (cons thing-to-highlight (highlight-global--find-and-use-face)))
              (setq highlight-global-hl-list
                    (cons highlight-global-new-highlight highlight-global-hl-list))
              (setq highlight-global-hl-list-update-timestamp (float-time))
              (walk-windows 'highlight-global-hl-window))))
      (message "No valid region, or no valid symbol under cursor!"))))

;;;###autoload
(defun highlight-global-clear-hl-frame ()
  "Clear all highlights in all windows."
  (interactive)
  (walk-windows 'highlight-global-clear-highlight-window)
  (setq highlight-global-hl-list nil)
  (highlight-global--clear-all-faces)
  (setq highlight-global-hl-list-update-timestamp (float-time)))

(defun highlight-global-force-hl-frame ()
  "Force to update highlights in every window in the current frame."
  (interactive)
  (save-excursion
    (walk-windows #'(lambda (win)
                      (select-window win)
                      (highlight-global-update-current-buffer-hl)))))


(provide 'highlight-global)

;;; highlight-global.el ends here
