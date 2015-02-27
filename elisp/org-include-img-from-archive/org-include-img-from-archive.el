;; Time-stamp: <2015-02-27 13:52:09 kmodi>

;; http://stackoverflow.com/q/28697108/1219634

;; Test org file:
;;   #+TITLE: Image from archive
;;   #+STARTUP: inlineimages
;;
;;   #+NAME: fig:myimage
;;   #+HEADER: :extractfromarchive t
;;   # The below caption line is optional
;;   #+CAPTION: My image myimage.png inside ./zippedimg.zip
;;   [[./zippedimg_zip/myimage.png]]

;; Execute the `modi/org-include-img-from-archive' function just before saving the file
(add-hook 'before-save-hook #'modi/org-include-img-from-archive)
;; Execute the `modi/org-include-img-from-archive' function before processing the
;; file for export
(add-hook 'org-export-before-processing-hook #'modi/org-include-img-from-archive)

(defun modi/org-include-img-from-archive (&rest ignore)
  "Extract image files from the archive files. Only .zip files are supported
as of now.

Only looks at #HEADER: lines that have \":extractfromarchive t\".
This function does nothing if not in org-mode, so you can safely
add it to `before-save-hook'."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min)) ; go to the beginning of the buffer
      (while (search-forward-regexp
              "^\\s-*#\\+HEADER:.*\\s-:extractfromarchive\\s-+t"
              nil :noerror)
        (let (;; only .zip supported as of now
              (search-expr "\\[\\[\\(.*?\\)_zip/\\(.*?\\)\\([^/]+\\..*\\)\\]\\]")
              arc-file
              path-in-arc-file
              img-file img-file-full-path
              dest-dir dest-dir-full-path
              cmd)
          ;; Keep on going on to the next line till it finds a line with
          ;; `[[./path/to/zip-file/path/inside/zip/to/the/image]]'
          (while (progn
                   (forward-line 1)
                   (or (not (looking-at search-expr))
                       (eobp))))
          (when (looking-at search-expr)
            (setq arc-file (expand-file-name
                            (concat (match-string-no-properties 1) ".zip")))
            (setq path-in-arc-file (match-string-no-properties 2))
            (setq img-file (match-string-no-properties 3))
            (setq dest-dir (concat "./" (file-name-base arc-file)
                                   "_zip/" path-in-arc-file))
            (setq dest-dir-full-path (concat (file-name-sans-extension arc-file)
                                             "_zip/" path-in-arc-file))
            (setq img-file-full-path (expand-file-name img-file dest-dir))
            ;; (message (concat "arc-file: %s\npath-in-arc-file: %s\n"
            ;;                  "img-file: %s\nimg-file-full-path: %s\n"
            ;;                  "dest-dir: %s\ndest-dir-full-path: %s")
            ;;          arc-file path-in-arc-file
            ;;          img-file img-file-full-path
            ;;          dest-dir dest-dir-full-path)

            (when (file-newer-than-file-p arc-file img-file-full-path)
              ;; This block is executed only if arc-file is newer than
              ;; img-file-full-path
              ;; or if img-file does not exist
              ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Testing-Accessibility.html
              (when (file-exists-p dest-dir-full-path)
                (delete-directory dest-dir-full-path t t))
              (make-directory dest-dir-full-path t)
              (setq cmd (format "unzip -j %s %s%s -d ./%s."
                                arc-file path-in-arc-file img-file
                                (concat (file-name-base arc-file) "_zip/"
                                        path-in-arc-file)))
              (message "%s" cmd)
              (with-temp-buffer
                (shell-command cmd)
                (shell-command (concat "touch " img-file-full-path))))))))))


(provide 'org-include-img-from-archive)
