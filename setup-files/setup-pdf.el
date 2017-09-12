;; Time-stamp: <2017-09-12 16:35:55 kmodi>

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
    (defvar modi/pdf-tools-bin-directory (let ((dir (concat user-emacs-directory
                                                            "misc/pdf-tools/bin/"))) ; must end with /
                                           (make-directory dir :parents)
                                           dir)
      "Directory to hold the executable(s) for pdf-tools.")

    (setq-default pdf-view-display-size 'fit-page) ; fit page by default
    (setq pdf-view-resize-factor 1.10)

    (setq pdf-info-epdfinfo-program (expand-file-name "epdfinfo" modi/pdf-tools-bin-directory))

    ;; https://github.com/politza/pdf-tools/issues/312#issuecomment-328971105
    (defun modi/pdf-tools-install ()
      "Install `epdfinfo' at `pdf-info-epdfinfo-program' without prompts.

If the `pdf-info-epdfinfo-program' is not running and is not
executable or does not appear to be working, attempt to rebuild
it.  If this build succeeded, continue with the activation of the
package.  Otherwise fail silently, i.e. no error is signaled.

See `pdf-view-mode' and `pdf-tools-enabled-modes'."
      (interactive)
      (if (or noninteractive
              (pdf-info-running-p)
              (and (stringp pdf-info-epdfinfo-program)
                   (file-executable-p pdf-info-epdfinfo-program)
                   (ignore-errors (pdf-info-check-epdfinfo) :success)))
          (pdf-tools-install-noverify)
        (pdf-tools-build-server
         (lambda (success)
           (when success
             (pdf-tools-install)))
         (file-name-directory pdf-info-epdfinfo-program))))

    (modi/pdf-tools-install)

    ;; Update `pdf-view-mode-map' bindings
    (dolist (pair '((beginning-of-buffer . pdf-view-first-page)
                    (end-of-buffer . pdf-view-last-page)
                    (modi/scroll-up . pdf-view-next-line-or-next-page)
                    (modi/scroll-down . pdf-view-previous-line-or-previous-page)))
      (let ((remap-from (car pair))
            (remap-to (cdr pair)))
        (define-key pdf-view-mode-map `[remap ,remap-from] remap-to)))

    (bind-keys
     :map pdf-view-mode-map
     ("l" . pdf-history-backward)
     ("r" . pdf-history-forward))))

;; https://github.com/rudolfochrist/interleave
(use-package interleave
  :init
  (progn
    (bind-to-modi-map "i" #'interleave-mode)
    (with-eval-after-load 'doc-view
      (bind-key "i" #'interleave-open-notes-file-for-pdf doc-view-mode-map))
    (with-eval-after-load 'pdf-view
      (bind-key "i" #'interleave-open-notes-file-for-pdf pdf-view-mode-map)))
  :defer t)

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
