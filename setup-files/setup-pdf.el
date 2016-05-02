;; Time-stamp: <2016-05-02 00:53:02 kmodi>

;; PDF

;; http://www.sigmafield.org/2009/10/03/using-doc-view-with-auto-revert-to-view-latex-pdf-output-in-emacs
(add-hook 'doc-view-mode-hook #'auto-revert-mode)

(use-package pdf-tools
  :if (not (bound-and-true-p disable-pkg-pdf-tools))
  :preface
  (progn
    (defvar pdf-tools-github-version-name "pdf-tools-0.60"))
  :commands (my/pdf-tools-install)
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (progn

    (setq-default pdf-view-display-size 'fit-page) ; fit page by default
    (setq pdf-view-resize-factor 1.10)

    (defun my/get-latest-pdf-tools-dir ()
      "Get the full directory path of the latest installed version of
pdf-tools package.

If pdf-tools is installed in the elisp/ directory, that path is returned
instead of the one present in `package-user-dir'."
      (let ((pdf-tools-epdfinfo-dir))
        ;; Use the package version in elisp/pdf-tools dir if available
        (if (locate-file "pdf-tools.el" (list (concat user-emacs-directory
                                                      "elisp/pdf-tools/lisp/")))
            (progn
              (setq pdf-tools-epdfinfo-dir (concat user-emacs-directory
                                                   "elisp/pdf-tools/"
                                                   pdf-tools-github-version-name
                                                   "/"))
              pdf-tools-epdfinfo-dir)
          ;; Else use the package manager installed version
          ;; Get a list of directories and files in `package-user-dir'
          (let ((my/package-dirs (directory-files package-user-dir)))
            ;; `break' implementation in elisp
            ;; http://ergoemacs.org/emacs/elisp_break_loop.html
            (catch 'break
              (dotimes (index (safe-length my/package-dirs))
                (let ((dir-name (pop my/package-dirs))
                      )
                  ;; (message "%s" dir-name) ; debug
                  ;; Find a directory name that matches "pdf-tools-*"
                  (when (string-match "pdf\\-tools\\-.*" dir-name)
                    (setq pdf-tools-epdfinfo-dir (concat package-user-dir "/" dir-name))
                    ;; To ensure that the directory is valid, ensure that it
                    ;; contains "pdf-tools.el"
                    (when (locate-file "pdf-tools.el" (list pdf-tools-epdfinfo-dir))
                      ;; break the `dotimes' loop on finding this directory
                      ;; and return its full path
                      (throw 'break pdf-tools-epdfinfo-dir))))))))))

    (defun modi/advice-scroll-other-window-pdf-view (&rest args)
      "Call the correct pdf-view scroll functions instead of calling the
advised function in that major mode."
      (let (do-not-call-orig-fn)
        (with-selected-window (other-window-for-scrolling)
          (when (setq do-not-call-orig-fn (derived-mode-p 'pdf-view-mode))
            (if (or (null args)
                    (> (car args) 0))
                (apply #'pdf-view-next-line-or-next-page args)
              (apply #'pdf-view-previous-line-or-previous-page args))))
        do-not-call-orig-fn))
    (advice-add 'scroll-other-window :before-until #'modi/advice-scroll-other-window-pdf-view)

    (defun my/pdf-tools-install ()
      (interactive)
      ;; Update the `pdf-info-epdfinfo-program' variable to point to
      ;; the directory containing the latest version of `pdf-tools'
      (setq pdf-info-epdfinfo-program
            (expand-file-name "epdfinfo" (my/get-latest-pdf-tools-dir)))
      ;; Call the original `pdf-tools-install' function after updating the
      ;; `pdf-info-epdfinfo-program' variable
      (pdf-tools-install))

    (dolist (pair '((beginning-of-buffer . pdf-view-first-page)
                    (end-of-buffer . pdf-view-last-page)
                    (modi/scroll-up . pdf-view-next-line-or-next-page)
                    (modi/scroll-down . pdf-view-previous-line-or-previous-page)))
      (let ((remap-from (car pair))
            (remap-to (cdr pair)))
        (define-key pdf-view-mode-map `[remap ,remap-from] remap-to)))

    (my/pdf-tools-install)))

;; https://github.com/rudolfochrist/interleave
(use-package interleave
  ;; :load-path "elisp/interleave"
  :init
  (progn
    (with-eval-after-load 'doc-view
      (bind-key "i" #'interleave--open-notes-file-for-pdf doc-view-mode-map))
    (with-eval-after-load 'pdf-view
      (bind-key "i" #'interleave--open-notes-file-for-pdf pdf-view-mode-map)))
  :commands (interleave interleave--open-notes-file-for-pdf))

(with-eval-after-load 'doc-view
  ;; In continuous mode, reaching the page edge advances to the next/prev page
  (setq doc-view-continuous t))


(provide 'setup-pdf)

;; * =pdf-tools= package
;; ** How to install
;; - git clone https://github.com/politza/pdf-tools
;; - Install `poppler-glib' library if not present
;;   + poppler-glib ( http://poppler.freedesktop.org/ )
;;   + ./configure --prefix=$HOME/usr_local
;; - make -s
;; - make install-package
;; ** Useful key bindings
;; |--------------------------------+-----------------------------|
;; | Key Binding                    | Description                 |
;; |--------------------------------+-----------------------------|
;; | n                              | Next page                   |
;; | p                              | Previous page               |
;; | SPC                            | Scroll up                   |
;; | S-SPC                          | Scroll down                 |
;; | C-n                            | Next line/page              |
;; | C-p                            | Previous line/page          |
;; |--------------------------------+-----------------------------|
;; | <goto-line binding>            | Go to page                  |
;; |--------------------------------+-----------------------------|
;; | + / =                          | Enlarge view                |
;; | -                              | Shrink view                 |
;; | 0                              | Reset view                  |
;; | W                              | Fit page width              |
;; | H                              | Fit page height             |
;; | P                              | Fit page                    |
;; | s m <drag mouse to select box> | PDF zooms to that selection |
;; | s r                            | Resets the above view slice |
;; |--------------------------------+-----------------------------|
;; | M-s w                          | isearch-forward-word        |
;; | M-s o                          | pdf-isearch-occur           |
;; |--------------------------------+-----------------------------|
;; | m                              | bookmark-set                |
;; |                                | (jump to bookmark using     |
;; |                                |  C-x r b)                   |
;; |--------------------------------+-----------------------------|
;; | View in Printed mode           | C-c C-r p                   |
;; | View in Midnight mode          | C-c C-r m                   |
;; |--------------------------------+-----------------------------|

;; * =interleave= package
;; Create a Org file that will keep your notes. In the Org header
;; section (#+TITLE, #+AUTHOR, etc.) add
;;
;;    #+INTERLEAVE_PDF: /the/path/to/pdf.pdf
;;
;; Then you can start interleave by typing
;;
;;    M-x interleave
;;
;; - This will display the PDF side by side to the org buffer for
;;   your notes. You can navigate the PDF as usual with `n' and
;;   `p'. Changing the page of the PDF will also narrow to the notes
;;   that are meant for this particular PDF page.
;;
;; - If you want to add some notes to the current page you can type `i'.
;;   This will create a new headline for your notes. If some notes are
;;   already present, `i' will switch over to the other buffer.
;;
;; - Typing `q' in the DocView will quit interleave.
