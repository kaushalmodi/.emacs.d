;; Time-stamp: <2016-02-04 18:19:36 kmodi>

;; Insert fancybox class to all images when exporting org to html
;; Usage: Add the below to org files
;;   #+OPTIONS: fancybox:t

;; Update the below variables as per instructions from
;; http://fancyapps.com/fancybox/#instructions
(setq modi/ox-html-fancybox-jquery-library "
#+HTML_HEAD: <!-- Add jQuery library -->
#+HTML_HEAD: <script type=\"text/javascript\" src=\"http://code.jquery.com/jquery-latest.min.js\"></script>
")
(setq modi/ox-html-fancybox-html-header "
#+HTML_HEAD: <!-- Add fancyBox -->
#+HTML_HEAD: <link rel=\"stylesheet\" href=\"common/js/fancybox/source/jquery.fancybox.css?v=2.1.5\" type=\"text/css\" media=\"screen\" />
#+HTML_HEAD: <script type=\"text/javascript\" src=\"common/js/fancybox/source/jquery.fancybox.pack.js?v=2.1.5\"></script>
")
(setq modi/ox-html-fancybox-html-body "
#+BEGIN_EXPORT HTML
<!-- Source for fixing the issue of image disappearing about launch of fancybox.
     Using $(\"a.fancybox\").fancybox(); instead of $(\"fancybox\").fancybox();
     The issue is caused because org-mode assign class=\"fancybox\" to both <a> and
     <img> elements. Using \"a.fancybox\" limits the script to just the <a> element.
-->
<script type=\"text/javascript\">
	$(document).ready(function() {
		$(\"a.fancybox\").fancybox();
	});
</script>
#+END_EXPORT
")
(setq modi/ox-html-fancybox-img-file-prefix-regexp "\\(file\\|http\\|https\\)")
(setq modi/ox-html-fancybox-img-file-regexp "\\(png\\|jpg\\|svg\\)")
(setq modi/ox-html-fancybox-force-inline-img t)
(setq modi/ox-html-fancybox-img-highrez-suffix "")
(setq modi/ox-html-fancybox-img-thumb-suffix "")

(add-hook 'org-export-before-processing-hook #'modi/org-html-add-fancybox)

(defun modi/org-html-add-fancybox (&rest ignore)
  "Update TODAY macro to hold string with current date and time."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (let* ((enable-fancybox nil))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp
                "^\\s-*#\\+OPTIONS:.*fancybox:\\s-*t"
                nil 'noerror)
          (forward-line 1)
          ;; Insert jQuery library
          (when (bound-and-true-p modi/ox-html-fancybox-jquery-library)
            (insert modi/ox-html-fancybox-jquery-library))
          (forward-line 1)
          ;; Insert paths to fancybox css and js
          (when (bound-and-true-p modi/ox-html-fancybox-html-header)
            (insert modi/ox-html-fancybox-html-header))
          ;; Insert fancybox script in the html body
          (when (bound-and-true-p modi/ox-html-fancybox-html-body)
            (insert modi/ox-html-fancybox-html-body))
          (setq enable-fancybox t))
        ;; (message "Fancybox status: %s" enable-fancybox)
        (when enable-fancybox
          ;; Go back to top of the buffer
          (goto-char (point-min))
          ;; Search for [[FILE.png]] or [[FILE.jpg]] or
          ;;         or [[FILE.png][FILE.png]] or [[FILE.jpg][FILE.jpg]]
          (while (search-forward-regexp
                  (concat "^\\s-*\\[\\["
                          "\\(" modi/ox-html-fancybox-img-file-prefix-regexp ":\\)*" ; 1=file: 2=file
                          "\\(.*?\\)" ; 3=img-highrez
                          "\\." modi/ox-html-fancybox-img-file-regexp ; 4=img-highrez-ext
                          "\\]\\s-*\\[*"
                          "\\(" modi/ox-html-fancybox-img-file-prefix-regexp ":\\)*" ; 5=file: 6=file
                          "\\(.*?\\)" ; 7=img-thumb
                          "\\.*" modi/ox-html-fancybox-img-file-regexp "*" ; 8=img-thumb-ext
                          "\\]*"
                          "\\]")
                  nil 'noerror)
            (let* (file-prefix1 img-highrez img-highrez-ext
                                file-prefix2 img-thumb img-thumb-ext)
              (setq file-prefix1    (match-string-no-properties 1))
              (setq img-highrez     (match-string-no-properties 3))
              (setq img-highrez-ext (match-string-no-properties 4))
              (setq file-prefix2    (match-string-no-properties 5))
              (setq img-thumb       (match-string-no-properties 7))
              (setq img-thumb-ext   (match-string-no-properties 8))

              ;; (message "File prefixes: prefix 1: %s %s prefix 2: %s %s"
              ;;          file-prefix1 (string= file-prefix1 "")
              ;;          file-prefix2 (string= file-prefix2 ""))
              ;; (message "img-highrez: %s %s" img-highrez (string= img-highrez ""))
              ;; (message "img-highrez-ext: %s %s" img-highrez-ext (string= img-highrez-ext ""))
              ;; (message "img-thumb: %s %s" img-thumb (string= img-thumb ""))
              ;; (message "img-thumb-ext: %s %s %s"
              ;;          img-thumb-ext
              ;;          (string= img-thumb-ext "")
              ;;          (not (boundp 'img-thumb-ext)))
              ;; (message "%s %s %s %s %s %s %s %s \n"
              ;;          (match-string-no-properties 1)
              ;;          (match-string-no-properties 2)
              ;;          (match-string-no-properties 3)
              ;;          (match-string-no-properties 4)
              ;;          (match-string-no-properties 5)
              ;;          (match-string-no-properties 6)
              ;;          (match-string-no-properties 7)
              ;;          (match-string-no-properties 8))

              ;; If inserted image is of the style [[FILE.png]] or [[FILE.jpg]],
              ;; auto-populate the 'description' portion of image link so
              ;; that it translates to a hyper-linked inline image in HTML
              ;; NOTE: If the description part of the image link does not have
              ;; the a prefix like file:, the HTML export will show only a
              ;; hyper-linked image path instead of a hyper-linked inline image.
              ;; Source: http://orgmode.org/manual/Images-in-HTML-export.html
              (when (and (string= img-thumb "")
                         (string= img-thumb-ext nil))
                ;; (message "Here1")
                (setq img-thumb     img-highrez)
                (setq img-thumb-ext img-highrez-ext)
                (if (string= file-prefix1 nil)
                    (when modi/ox-html-fancybox-force-inline-img
                      (setq file-prefix2 "file:"))
                  ;; set file-prefix2 equal to file-prefix1 if file-prefix1 is
                  ;; non-nil
                  (setq file-prefix2 file-prefix1))
                (forward-line 0)
                (when (looking-at ".*\\[\\[\\(.*\\)\\]\\]")
                  (replace-match (concat file-prefix1
                                         img-highrez
                                         modi/ox-html-fancybox-img-highrez-suffix
                                         "." img-highrez-ext
                                         "]["
                                         file-prefix2
                                         img-thumb
                                         modi/ox-html-fancybox-img-thumb-suffix
                                         "." img-thumb-ext)
                                 :fixedcase :literal nil 1)))
              ;; If the image link is of the type [[FILE.ext][FILE.ext]] and
              ;; if inline image option is forced, convert the image link to
              ;; [[FILE.ext][file:FILE.ext]]
              (when (and modi/ox-html-fancybox-force-inline-img
                         (not (string= img-thumb ""))
                         (not (string= img-thumb-ext nil))
                         (string= file-prefix2 nil))
                (setq file-prefix2 "file:")
                (forward-line 0)
                (when (looking-at ".*\\[\\(.*\\)\\]\\]")
                  (replace-match (concat file-prefix2 img-highrez
                                         "." img-highrez-ext)
                                 :fixedcase :literal nil 1)))
              (forward-line 0)
              (open-line 1)
              (insert "#+ATTR_HTML: :class fancybox")
              (forward-line 2))))))))


(provide 'ox-html-fancybox)
