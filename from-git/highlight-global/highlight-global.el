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
;; highlight of the corresponding function/vairalbe accross these
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
;; to highlight-faces.

;;; How to use?

;; Put the package into load-path, and load the packaget
;;     (require 'highlight-global)

;; Toggle highlight of current region (or symbol under cursor if
;; region is not active) and bind it like this:
;;     (global-set-key (kbd "M-H") 'highlight-frame-toggle)

;; Clear all highlight of current frame, and bind it like this:
;;     (global-set-key (kbd "M-C") 'clear-highlight-frame)


(require 'hi-lock)
(setq hi-lock-file-patterns-policy 'never)

(defvar highlight-faces
  '(('hi-yellow . 0)
    ('hi-pink . 0)
    ('hi-green . 0)
    ('hi-blue . 0))
  "Default faces for hi-lock interactive functions, you could add your own.")

(defun clear-all-faces ()
  "Reset all face's usage count to zero."
  (dolist (item highlight-faces)
    (setcdr item 0)))

(defun find-and-use-face ()
  "Find the least used face and increase it, the face will be returned to caller"
  (let ((least-used-one (nth 0 highlight-faces)))
    (progn
      (dolist (face highlight-faces)
        (when (< (cdr face) (cdr least-used-one))
          (setq least-used-one face)))
      (setcdr least-used-one (+ 1 (cdr least-used-one)))
      (car least-used-one))))

(defun release-face (face-to-release)
  "Release the use of a face by decreasing the counting"
  (dolist (face highlight-faces)
    (when (equal (car face) face-to-release)
      (setcdr face (- (cdr face) 1)))))

;; list to store what had been highlighted
(defvar global-highlight-list nil
  "Global highlight list, always store the updated highlight
  regexp list, and every item is stored like this ((hilight-str1
  . hilight-face1) (hilight-str2 . hilight-face2) ...)")

(defvar global-highlight-list-update-timestamp 0.0
  "Store the timestamp when `global-highlight-list' was updated")

(defvar new-unhighlight nil "Store's thing to be unhighlight")
(defvar new-highlight nil "Store's thing to be highlight")

(defvar buffer-highlight-list nil
  "Stores the regexp highlighed by `highlight-windows' package of
  current buffer")
(make-variable-buffer-local 'buffer-highlight-list)
(put 'buffer-highlight-list 'permanent-local t)

;; set it to -0.5 to make sure first time it will update
(defvar buffer-highlight-list-update-timestamp -0.5
  "Stores the recently timestamp when `buffer-highlight-list' was
  updated")
(make-variable-buffer-local 'buffer-highlight-list-update-timestamp)
(put 'buffer-highlight-list-update-timestamp 'permanent-local t)

(defun clear-highlight-window (win)
  "Clear all highlight of current buffer, called by
  `unhighlit-windows-all' when iterating all windows. When a
  buffer is being burry, this funciton also will be called to
  clear all highlight"
  (select-window win)
  (setq buffer-highlight-list-update-timestamp (float-time))
  (dolist (item global-highlight-list)
    (font-lock-remove-keywords
     nil
     `((,(car item) 0 ,(cdr item) prepend)))
    (font-lock-fontify-buffer)))

(defun clear-highlight-frame ()
  "Clear all highlights of all windows "
  (interactive)
  (walk-windows 'clear-highlight-window)
  (setq global-highlight-list nil)
  (clear-all-faces)
  (setq global-highlight-list-update-timestamp (float-time)))

(defun unhighlight-window (win)
  "Highligt a buffer, should update of buffer-local
highlight-list and timestamp, used by `walk-windows'"
  (select-window win)
  (setq buffer-highlight-list-update-timestamp (float-time))
  (setq buffer-highlight-list
        (delete new-unhighlight buffer-highlight-list))
  ;; add new highlight to current buffer's keyword list
  (font-lock-remove-keywords
   nil
   `((,(car new-unhighlight) 0 ,(cdr new-unhighlight) prepend)))
  (font-lock-fontify-buffer))

(defun highlight-window (win)
  "Highligt a buffer, should update buffer-local highlight-list
and timestamp"
  (select-window win)
  (setq buffer-highlight-list-update-timestamp (float-time))
  (push new-highlight buffer-highlight-list)
  (font-lock-add-keywords
   nil
   `((,(car new-highlight) 0 ,(cdr new-highlight) prepend)) 'append)
  (font-lock-fontify-buffer))

(defun get-thing-to-highlight ()
  "Get thing to highlight. If active region, get reigon, else get
symbol under cursor"
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (if (thing-at-point 'symbol)
        (buffer-substring-no-properties
         (car (bounds-of-thing-at-point 'symbol))
         (cdr (bounds-of-thing-at-point 'symbol))))))

(defun check-whether-highlighted (hi)
  "Check if HI is already highlighted by checking
global-highlight-list"
  (let ((the-found-one nil))
    (progn
      (dolist (item global-highlight-list)
        (when (equal hi (car item))
          (progn
            (setq the-found-one item))))
      the-found-one)))

(defun highlight-frame-toggle ()
  (interactive)
  (let* ((thing-to-highlight (get-thing-to-highlight))
         (hi nil)
         (face nil))
    (if (stringp thing-to-highlight)
        (progn
          (setq hi (check-whether-highlighted thing-to-highlight))
          ;; toogle highlight, 2 cases
          ;; 1) thing already unlighlight and stored in list, unhighight it
          ;; 2) new highlight, highlight it and add it to list
          (if hi
              ;; 1) toogle off
              ;;    1. delete item from global-list && update timestamp
              ;;    2. set new-unhighlight and unhighight each window
              (progn
                (setq new-unhighlight hi)
                (release-face (cdr new-unhighlight))
                (setq global-highlight-list
                      (delete new-unhighlight global-highlight-list))
                (setq global-highlight-list-update-timestamp (float-time))
                (walk-windows 'unhighlight-window))
            ;; 2) new highlight
            (progn
              (setq new-highlight (cons thing-to-highlight (find-and-use-face)))
              (setq global-highlight-list (cons new-highlight global-highlight-list))
              (setq global-highlight-list-update-timestamp (float-time))
              (walk-windows 'highlight-window))))
      (message "No vaidate region, or no validate symbol under cursor!"))))

(defun highlight-update-current-buffer ()
  "Update a buffer's highlight to be consistent with global
highlight"
  (if (<= buffer-highlight-list-update-timestamp
          global-highlight-list-update-timestamp)
      (if (null global-highlight-list)      ; clear buffer
          ;; 1) global null
          (progn
            (dolist (item buffer-highlight-list)
              (font-lock-remove-keywords
               nil
               `((,(car item) 0 ,(cdr item) prepend)))
              (font-lock-fontify-buffer))
            (setq buffer-highlight-list-update-timestamp (float-time))
            (setq buffer-highlight-list nil))
        ;; 2) globla is not null, now update local to global
        (progn
          ;; (message "Updating buffer : %s" (current-buffer))
          ;; 2.1) iterate buffer-list to delete
          (dolist (item buffer-highlight-list)
            (if (not (member item global-highlight-list))
                (progn
                  ;; `((,(car new-highlight) 0 ,(cdr new-highlight) prepend))
                  ;; (list (car item))
                  (font-lock-remove-keywords
                   nil
                   `((,(car item) 0 ,(cdr item) prepend)))
                  (font-lock-fontify-buffer)
                  (setq buffer-highlight-list
                        (delete item buffer-highlight-list)))))
          ;; 2.2) iterate global-list to add
          (dolist (item global-highlight-list)
            (if (not (member item buffer-highlight-list))
                (progn
                  (font-lock-add-keywords
                   nil
                   `((,(car item) 0 ,(cdr item) prepend)) 'append)
                  (font-lock-fontify-buffer)
                  (push item buffer-highlight-list))))
          (setq buffer-highlight-list-update-timestamp (float-time)))
        ;; (message "no need to update : %s" (current-buffer))
        )))

;; Force to update highlights on current frame, call this function
;; will update highlights in every window within current frame
(defun force-highlight-frame ()
  (interactive)
  (save-excursion
    (walk-windows #'(lambda (win)
                      (select-window win)
                      (highlight-update-current-buffer)))))

;; Automatically update new buffer's highlights when any windows on
;; current frame changed. This will make buffers that to be showned
;; because of window splitting alway has highlights updated to date.
(defun update-highlight-fixup (frame)
  (force-highlight-frame))

;; register the on-the-fly highlight-list updating strategy to
;; window-size-change-functions hook
(if (null window-size-change-functions)
    (setq window-size-change-functions '(update-highlight-fixup))
  (add-to-list 'window-size-change-functions 'update-highlight-fixup))

(provide 'highlight-global)

;;; highlight-global.el ends here
