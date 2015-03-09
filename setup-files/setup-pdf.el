;; Time-stamp: <2015-03-09 10:24:04 kmodi>

;; PDF

;; Source: http://www.sigmafield.org/2009/10/03/using-doc-view-with-auto-revert-to-view-latex-pdf-output-in-emacs
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; pdf-tools
;; To install:
;; - git clone https://github.com/politza/pdf-tools
;; - ./configure --prefix=$HOME/usr_local
;;   + poppler-glib ( http://poppler.freedesktop.org/ ) is REQUIRED
;; - make -s
;; - make install-package
(use-package pdf-tools
    :mode "\\.pdf\\'"
    :commands (pdf-tools-install my/pdf-tools-install)
    :config
    (progn

      (defun my/get-latest-pdf-tools-dir ()
        "Get the full directory path of the latest installed version of
pdf-tools package."
        (interactive)
        ;; Get a list of directories and files in `package-user-dir'
        (let ((my/package-dirs (directory-files package-user-dir)))
          ;; `break' implementation in elisp
          ;; http://ergoemacs.org/emacs/elisp_break_loop.html
          (catch 'break
            (dotimes (index (safe-length my/package-dirs))
              (let ((dir-name (pop my/package-dirs))
                    full-dir-name)
                ;; (message "%s" dir-name) ; debug
                ;; Find a directory name that matches "pdf-tools-*"
                (when (string-match "pdf\\-tools\\-.*" dir-name)
                  (setq full-dir-name (concat package-user-dir "/" dir-name))
                  ;; To ensure that the directory is valid, ensure that it
                  ;; contains "pdf-tools.el"
                  (when (locate-file "pdf-tools.el" (list full-dir-name))
                    ;; break the `dotimes' loop on finding this directory
                    ;; and return its full path
                    (throw 'break full-dir-name))))))))

      (defun my/pdf-tools-install ()
        (interactive)
        ;; Update the `pdf-info-epdfinfo-program' variable to point to
        ;; the directory containing the latest version of `pdf-tools'
        (setq pdf-info-epdfinfo-program
              (concat (my/get-latest-pdf-tools-dir) "/epdfinfo"))
        ;; Call the original `pdf-tools-install' function after updating the
        ;; `pdf-info-epdfinfo-program' variable
        (pdf-tools-install))

      (my/pdf-tools-install)))

;; https://github.com/rudolfochrist/interleave
(use-package interleave
    :commands (interleave)
    :config
    (progn
      (when (featurep 'pdf-view) ; if `pdf-tools' is installed
        (defun interleave-go-to-next-page ()
          "Go to the next page in PDF. Look up for available notes."
          (interactive)
          (pdf-view-next-page-command 1)
          (interleave-go-to-page-note (pdf-view-current-page)))

        (defun interleave-go-to-previous-page ()
          "Go to the previous page in PDF. Look up for available notes."
          (interactive)
          (pdf-view-previous-page-command 1)
          (interleave-go-to-page-note (pdf-view-current-page)))

        (defun interleave-scroll-up ()
          "Scroll up the PDF. Look up for available notes."
          (interactive)
          (setq *interleave-page-marker* (pdf-view-current-page))
          (pdf-view-scroll-up-or-next-page)
          (unless (= *interleave-page-marker* (pdf-view-current-page))
            (interleave-go-to-page-note (pdf-view-current-page))))

        (defun interleave-scroll-down ()
          "Scroll down the PDF. Look up for available notes."
          (interactive)
          (setq *interleave-page-marker* (pdf-view-current-page))
          (pdf-view-scroll-down-or-previous-page)
          (unless (= *interleave-page-marker* (pdf-view-current-page))
            (interleave-go-to-page-note (pdf-view-current-page))))

        (defun interleave-add-note ()
          "Add note for the current page. If there are already notes for this page,
jump to the notes buffer."
          (interactive)
          (let ((page (pdf-view-current-page)))
            (with-current-buffer *interleave--org-buf*
              (save-excursion
                (if (interleave-go-to-page-note page)
                    (other-window 1)
                  (interleave-create-new-note page))))))))

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
    )


(provide 'setup-pdf)
