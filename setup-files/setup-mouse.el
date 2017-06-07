;; Time-stamp: <2017-06-06 16:46:16 kmodi>

;; Mouse

;; Contents:
;;
;;  Mouse
;;  Mouse wheel
;;  Scrolling other window using mouse
;;  Mouse and Scroll All Mode
;;  Swap buffers using mouse
;;  Mouse Copy
;;  Misc
;;  Mode-line mouse bindings

;;; Mouse
(use-package mouse
  :config
  (progn
    (>=e "26.0"
        ;; Drag-drop a region to cut/paste it; works across windows too!
        ;; Drag, press Control, and then drop to copy/paste instead.
        (setq mouse-drag-and-drop-region 'control))))

;;; Mouse wheel
;; `mouse-wheel-mode' is auto-enabled under X (GUI)
;; Binds by default <mouse-4> to scroll-down, and <mouse-5> to scroll-up
(use-package mwheel
  :config
  (progn
    ;; http://lists.gnu.org/archive/html/emacs-devel/2017-03/msg00797.html
    (setq mouse-wheel-scroll-amount '(1 ;regular scroll
                                      ((shift) . 5) ;Shift + scroll
                                      ((control) . nil))) ;Ctrl + scroll (don't scroll)
    ;; Do not make mouse wheel accelerate its action (example: scrolling)
    (setq mouse-wheel-progressive-speed nil)))

;;; Scrolling other window using mouse
(with-eval-after-load 'setup-windows-buffers
  (bind-keys
   :map modi-mode-map
   ("<M-mouse-4>" . modi/scroll-other-window-down) ; Alt + wheel up
   ("<M-mouse-5>" . modi/scroll-other-window-up))) ; Alt + wheel down

;;; Mouse and Scroll All Mode
;; Allow scrolling of all buffers using mouse-wheel in `scroll-all-mode'.
;; By default, `scroll-all-mode' works only with C-v/M-v.
(defun modi/advice-mwhell-scroll-all (orig-fun &rest args)
  "Execute ORIG-FUN in all the windows."
  (let (ret)
    (if scroll-all-mode
        (save-selected-window (walk-windows (lambda (win)
                                              (select-window win)
                                              (condition-case nil
                                                  (setq ret (apply orig-fun args))
                                                (error nil)))))
      (setq ret (apply orig-fun args)))
    ret))
(advice-add 'scroll-up :around #'modi/advice-mwhell-scroll-all)
(advice-add 'scroll-down :around #'modi/advice-mwhell-scroll-all)

;;; Swap buffers using mouse
;; https://tsdh.wordpress.com/2015/03/03/swapping-emacs-windows-using-dragndrop/
(defun th/swap-window-buffers-by-dnd (drag-event)
  "Swaps the buffers displayed in the DRAG-EVENT's start and end window."
  (interactive "e")
  (let ((start-win (car (event-start drag-event)))
        (end-win (car (event-end drag-event))))
    (when (and (windowp start-win)
               (windowp end-win)
               (not (eq start-win end-win))
               (not (memq (minibuffer-window) (list start-win end-win))))
      (let ((start-buf (window-buffer start-win))
            (end-buf (window-buffer end-win)))
        (unless (eq start-buf end-buf)
          (set-window-buffer start-win end-buf)
          (set-window-buffer end-win start-buf))))))
(bind-key "<C-S-drag-mouse-1>" #'th/swap-window-buffers-by-dnd modi-mode-map)

;;; Mouse Copy
(use-package mouse-copy
  :bind (:map modi-mode-map
         ;; Mouse drag secondary pasting
         ;; Put the point at one place, then click-drag using the below binding,
         ;; and the selected region will be COPIED at the point location.
         ("<s-down-mouse-1>" . mouse-drag-secondary-pasting)
         ;; Mouse drag secondary moving
         ;; Put the point at one place, then click-drag using the below binding,
         ;; and the selected region will be MOVED to the point location.
         ("<S-s-down-mouse-1>" . mouse-drag-secondary-moving)))

;;; Misc
;; Below function is inspired from `mouse-event-p'
(defun modi/mouse-scroll-p (object)
  "Return non-nil if OBJECT is a mouse scroll event."
  (memq (event-basic-type object) '(mouse-4 mouse-5)))

;;; Mode-line mouse bindings
;; http://stackoverflow.com/a/26629984/1219634
(with-eval-after-load 'setup-windows-buffers
  ;; Middle clicking a buffer name in mode line
  (bind-key "<mode-line> <mouse-2>" #'modi/copy-buffer-file-name
            mode-line-buffer-identification-keymap)
  ;; Shift + Middle clicking a buffer name in mode line
  (bind-key "<mode-line> <S-mouse-2>" (lambda ()
                                        (interactive)
                                        (modi/copy-buffer-file-name 4))
            mode-line-buffer-identification-keymap))


(provide 'setup-mouse)
