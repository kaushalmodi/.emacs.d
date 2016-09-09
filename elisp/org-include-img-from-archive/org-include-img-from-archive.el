;; Time-stamp: <2016-09-09 11:09:52 kmodi>

;; http://stackoverflow.com/q/28697108/1219634

;; Test org file:
;;   #+TITLE: Image from archive
;;   #+STARTUP: inlineimages
;;
;;   #+NAME: fig:image
;;   #+HEADER: :extractfromarchive t
;;   # The below caption line is optional
;;   #+CAPTION: My image image.png inside ./zippedimg.zip
;;   [[./zippedimg_zip/image.png]]

;; In the above example, the user has `zippedimg.zip' file containing
;; `image.png'. But when specifying the image path it is specified as
;; `./zippedimg_zip/image.png'; note that the . is replaced with an _
;; in the zip file name.

;; The reason is that a `FILE.EXT' archive is extracted to `FILE_EXT' folder
;; as you can't have a file and a folder with the exact same name in the same
;; folder.

;; How to use this package:
;; 
;;   ;; Execute `modi/org-include-img-from-archive' before saving the file.
;;   (defun modi/org-include-img-from-archive-before-save ()
;;     "Execute `modi/org-include-img-from-archive' just before saving the file."
;;     (add-hook 'before-save-hook #'modi/org-include-img-from-archive nil :local))
;;   (add-hook 'org-mode-hook #'modi/org-include-img-from-archive-before-save)
;;   ;; Execute `modi/org-include-img-from-archive' before exporting.
;;   (with-eval-after-load 'ox
;;     (add-hook 'org-export-before-processing-hook #'modi/org-include-img-from-archive))

;;;###autoload
(defun modi/org-include-img-from-archive (&rest _)
  "Extract image files from the archive files.
Only .zip archives are supported as of now.

Only looks at #+HEADER: lines that have \":extractfromarchive t\"."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min)) ; go to the beginning of the buffer
      (while (search-forward-regexp
              "^\\s-*#\\+HEADER:.*\\s-:extractfromarchive\\s-+t"
              nil :noerror)
        (let ((search-expr (concat "\\[\\[" ; [[
                                   "\\(file\\:\\)*" ; optional 'file:'
                                   "\\(.*/\\)\\(.*\\)_\\(.*?\\)" ; ./optional-dir/ARC-FILE_EXT
                                   "/\\(.*?\\)" ; optional image file dir in archive
                                   "\\([^/]+\\..*\\)" ; image file name
                                   "\\]\\]")) ; ]]
              arc-file-base arc-file-ext arc-file-full-path
              path-in-arc-file img-file img-file-full-path
              dest-dir-full-path
              cmd)
          ;; Keep on going on to the next line till it finds a line with
          ;; `[[./path/to/zip-file/path/inside/zip/to/the/image]]'
          (while (progn
                   (forward-line 1)
                   (or (not (looking-at search-expr))
                       (eobp))))
          (when (looking-at search-expr)
            (setq arc-file-base (match-string-no-properties 3))
            (setq arc-file-ext  (match-string-no-properties 4))
            (setq arc-file-full-path (expand-file-name
                                      (concat arc-file-base "." arc-file-ext)
                                      (match-string-no-properties 2)))
            (setq path-in-arc-file (match-string-no-properties 5))
            (setq img-file (match-string-no-properties 6))
            (setq dest-dir-full-path
                  (concat (file-name-sans-extension arc-file-full-path) "_"
                          arc-file-ext "/" path-in-arc-file))
            (setq img-file-full-path (concat dest-dir-full-path img-file))
            ;; (message (concat "arc-file-full-path: %s\npath-in-arc-file: %s\n"
            ;;                  "img-file: %s\nimg-file-full-path: %s\n"
            ;;                  "dest-dir-full-path: %s")
            ;;          arc-file-full-path path-in-arc-file
            ;;          img-file img-file-full-path
            ;;          dest-dir-full-path)

            (when (file-newer-than-file-p arc-file-full-path img-file-full-path)
              ;; This block is executed only if arc-file-full-path is newer than
              ;; img-file-full-path
              ;; or if img-file does not exist
              ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Testing-Accessibility.html
              (when (file-exists-p dest-dir-full-path)
                (delete-directory dest-dir-full-path t t))
              (make-directory dest-dir-full-path t)
              ;; http://unix.stackexchange.com/a/57522/57923
              (setq cmd (format "unzip -j %s %s%s -d %s"
                                arc-file-full-path path-in-arc-file img-file
                                (concat (file-name-sans-extension arc-file-full-path) "_"
                                        arc-file-ext "/" path-in-arc-file)))
              (message "%s" cmd)
              (shell-command cmd)
              (shell-command (concat "touch " img-file-full-path)))))))))


(provide 'org-include-img-from-archive)
