;; Time-stamp: <2014-02-28 10:03:55 kmodi>

;; w3m - web browser

(require 'w3m)

(setq browse-url-browser-function 'w3m-browse-url)

(defun wicked/w3m-open-current-page-in-firefox ()
  "Open the current URL in Mozilla Firefox."
  (interactive)
  (browse-url-firefox w3m-current-url)) ;; (1)

(defun wicked/w3m-open-link-or-image-in-firefox ()
  "Open the current link or image in Firefox."
  (interactive)
  (browse-url-firefox (or (w3m-anchor) ;; (2)
                          (w3m-image)))) ;; (3)

(defun wicked/toggle-w3m ()
  "Switch to a w3m buffer or return to the previous buffer."
  (interactive)
  (if (derived-mode-p 'w3m-mode)
      ;; Currently in a w3m buffer
      ;; Bury buffers until you reach a non-w3m one
      (while (derived-mode-p 'w3m-mode)
	(bury-buffer))
    ;; Not in w3m
    ;; Find the first w3m buffer
    (let ((list (buffer-list)))
      (while list
	(if (with-current-buffer (car list)
	      (derived-mode-p 'w3m-mode))
	    (progn
	      (switch-to-buffer (car list))
	      (setq list nil))
	  (setq list (cdr list))))
      (unless (derived-mode-p 'w3m-mode)
	(call-interactively 'w3m)))))

(defun w3m-interactive-search ()
  "Give user an option to pick the search engine (google, yahoo, amazon, etc)
before doing the search"
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively 'w3m-search)))

;; w3m key-binding
;; http://www.emacswiki.org/emacs/WThreeMKeymap
(let ((map (make-keymap)))
  (suppress-keymap map)
  (define-key map [(button2)]         'w3m-mouse-view-this-url)
  (define-key map [(shift button2)]   'w3m-mouse-view-this-url-new-session)
  (define-key map (kbd "C-m")         'w3m-view-this-url)
  (define-key map (kbd "<return>")    'w3m-view-this-url)
  (define-key map (kbd "<S-return>")  'w3m-view-this-url-new-session)
  (define-key map (kbd "<backspace>") 'w3m-view-previous-page)
  (define-key map (kbd "H")           'w3m-view-previous-page) ;; back
  (define-key map (kbd "L")           'w3m-view-next-page) ;; forward
  (define-key map (kbd "q")           'w3m-previous-buffer) ;; previous tab
  (define-key map (kbd "w")           'w3m-next-buffer) ;; next tab
  (define-key map (kbd "x")           'w3m-delete-buffer)
  (define-key map (kbd "a")           'w3m-bookmark-add-current-url) ;; bookmark current page
  (define-key map (kbd "b")           'w3m-bookmark-add-current-url)
  (define-key map (kbd "M-a")         'w3m-bookmark-add-this-url) ;; bookmark the link at cursor
  (define-key map (kbd "B")           'helm-w3m-bookmarks)
  (define-key map (kbd "v")           'w3m-bookmark-view)
  (define-key map (kbd "f")           'wicked/w3m-open-current-page-in-firefox)
  (define-key map (kbd "F")           'wicked/w3m-open-link-or-image-in-firefox)
  (define-key map (kbd "<delete>")    'w3m-scroll-down-or-previous-url)
  (define-key map (kbd "C-?")         'w3m-scroll-down-or-previous-url)
  (define-key map (kbd "<tab>")       'w3m-next-anchor) ;; Go to next link on page using TAB
  (define-key map (kbd "<backtab>")   'w3m-previous-anchor) ;; Go to prev link on page using S-TAB
  (define-key map (kbd "h")           'backward-char)
  (define-key map (kbd "j")           'next-line)
  (define-key map (kbd "k")           'previous-line)
  (define-key map (kbd "l")           'forward-char)
  (define-key map (kbd "J")           'w3m-scroll-up)
  (define-key map (kbd "K")           'w3m-scroll-down)
  (define-key map (kbd "p")           'scroll-down)
  (define-key map (kbd "n")           'scroll-up)
  (define-key map (kbd "SPC")         'scroll-up)
  (define-key map (kbd "+")           'w3m-antenna-add-current-url)
  (define-key map (kbd "A")           'w3m-antenna)
  (define-key map (kbd "c")           'w3m-print-this-url) ;; "print" actually copies the url
  (define-key map (kbd "C")           'w3m-print-current-url)
  (define-key map (kbd "d")           'w3m-download)
  (define-key map (kbd "D")           'w3m-download-this-url)
  ;; (define-key map (kbd "D")        'w3m-download-with-wget)
  ;; (define-key map (kbd "D")        'w3m-download-with-curl)
  (define-key map (kbd "g")           'w3m-goto-url)
  (define-key map (kbd "G")           'w3m-goto-url-new-session)
  (define-key map (kbd "I")           'w3m-toggle-inline-images)
  (define-key map (kbd "M-i")         'w3m-save-image)
  (define-key map (kbd "N")           'w3m-namazu)
  (define-key map (kbd "o")           'w3m-history)
  (define-key map (kbd "O")           'w3m-db-history)
  (define-key map (kbd "Q")           'w3m-quit)
  (define-key map (kbd "R")           'w3m-reload-this-page)
  (define-key map (kbd "/")           'isearch-forward)
  (define-key map (kbd "s")           'w3m-search) ;; google search
  (define-key map (kbd "S")           'w3m-interactive-search) ;; option to choose search engine
  (define-key map (kbd "u")           'w3m-view-parent-page)
  (define-key map (kbd "^")           'w3m-view-parent-page)
  (define-key map (kbd "W")           'w3m-weather)
  (define-key map (kbd "=")           'w3m-view-header)
  (define-key map (kbd "\\")          'w3m-view-source)
  (define-key map (kbd ">")           'scroll-left)
  (define-key map (kbd "<")           'scroll-right)
  (define-key map (kbd ".")           'beginning-of-buffer)
  (define-key map (kbd "]")           'w3m-next-form)
  (define-key map (kbd "[")           'w3m-previous-form)
  (define-key map (kbd "}")           'w3m-next-image)
  (define-key map (kbd "{")           'w3m-previous-image)
  (define-key map (kbd "C-c C-c")     'w3m-submit-form)
  (setq modi-w3m-map map))

(add-hook 'w3m-mode-hook '(lambda () (use-local-map modi-w3m-map)))


(setq setup-w3m-loaded t)
(provide 'setup-w3m)

;; Sources:
;; http://sachachua.com/blog/2008/09/emacs-w3m-open-pages-in-external-browsers/
;; http://sachachua.com/blog/2008/09/emacs-and-w3m-making-tabbed-browsing-easier/
;; http://sachachua.com/blog/2008/08/emacs-and-w3m-toggling-between-work-and-the-web/

;; To enter text into form fields, hit Enter
