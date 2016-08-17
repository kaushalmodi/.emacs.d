;;; htmlize-r2f.el --- Export a region to an html file.

;; Export a region to an html file
;;   Output directory: `htmlize-r2f-output-directory'
;;   Faces used in exported html are taken from `htmlize-r2f-css-file'

;; http://emacs.stackexchange.com/a/14560/115
(defvar htmlize-r2f-output-directory
  (let ((dir (concat temporary-file-directory
                     (getenv "USER") "/.htmlize/"))) ; must end with /
    (make-directory dir :parents)
    dir)
  "Output directory for the exported html files.")

(defvar htmlize-r2f-css-file (concat user-emacs-directory
                                     "misc/css/leuven_theme.css")
  "CSS file to be embedded in the exported html file.")

(defun htmlize-r2f (option)
  "Export the selected region/whole buffer to an html file.

The output file is saved to `htmlize-r2f-output-directory' and its
fontification is done using `htmlize-r2f-css-file'.

If OPTION is non-nil (for example, using `\\[universal-argument]' prefix), copy
the output file name to kill ring.
If OPTION is \\='(16) (using `\\[universal-argument] \\[universal-argument]'
prefix), do the above and also open the html file in the default browser."
  (interactive "P")
  (require 'ox-html)
  (let ((src-link (concat "https://github.com/kaushalmodi/.emacs.d/blob/master/"
                          "elisp/htmlize-r2f/htmlize-r2f.el"))
        (org-html-htmlize-output-type 'css)
        (org-html-htmlize-font-prefix "org-")
        (fname (concat htmlize-r2f-output-directory
                       (if (buffer-file-name)
                           (file-name-nondirectory (buffer-file-name))
                         "temp")
                       ".html"))
        start end html-string)
    (if (use-region-p)
        (progn
          (setq start (region-beginning))
          (setq end (region-end)))
      (progn
        (setq start (point-min))
        (setq end (point-max))))
    (setq html-string (org-html-htmlize-region-for-paste start end))
    (with-temp-buffer
      ;; Insert the `htmlize-r2f-css-file' contents in the temp buffer
      (insert-file-contents htmlize-r2f-css-file nil nil nil :replace)
      ;; Go to the beginning of the buffer and insert comments and
      ;; opening tags for `html', `head' and `style'. These are
      ;; inserted *above* the earlier inserted css code.
      (goto-char (point-min))
      (insert (concat "<!-- This file is generated using the "
                      "`htmlize-r2f' function\n"
                      "from " src-link " -->\n"))
      (insert "<html>\n<head>\n<style media=\"screen\" type=\"text/css\">\n")
      ;; Go to the end of the buffer (end of the css code) and
      ;; insert the closing tags for `style' and `head' and opening
      ;; tag for `body'.
      (goto-char (point-max))
      (insert "</style>\n</head>\n<body>\n")
      ;; Insert the HTML for fontified text in `html-string'.
      (insert html-string)
      ;; Close the `body' and `html' tags.
      (insert "</body>\n</html>\n")
      (write-file fname)
      (when option
        (kill-new fname)
        (when (= 16 (car option))
          (browse-url-of-file fname))))
    fname))


(provide 'htmlize-r2f)
