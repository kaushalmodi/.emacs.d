;; Time-stamp: <2015-11-02 11:38:11 kmodi>

;; `ox-latex' needs to be required before `load'ing this patch file

;; org-mode version 8.3+
(defun org-latex-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (when (org-string-nw-p (org-element-property :value src-block))
    (let* ((lang (org-element-property :language src-block))
           (caption (org-element-property :caption src-block))
           (caption-above-p (org-latex--caption-above-p src-block info))
           (label (org-element-property :name src-block))
           (custom-env (and lang
                            (cadr (assq (intern lang)
                                        org-latex-custom-lang-environments))))
           (num-start (case (org-element-property :number-lines src-block)
                        (continued (org-export-get-loc src-block info))
                        (new 0)))
           (retain-labels (org-element-property :retain-labels src-block))
           (attributes (org-export-read-attribute :attr_latex src-block))
           (float (plist-get attributes :float))
           (listings (plist-get info :latex-listings)))
      (cond
       ;; Case 1.  No source fontification.
       ((not listings)
        (let* ((caption-str (org-latex--caption/label-string src-block info))
               (float-env
                (cond ((string= "multicolumn" float)
                       (format "\\begin{figure*}[%s]\n%s%%s\n%s\\end{figure*}"
                               (plist-get info :latex-default-figure-position)
                               (if caption-above-p caption-str "")
                               (if caption-above-p "" caption-str)))
                      (caption (concat
                                (if caption-above-p caption-str "")
                                "%s"
                                (if caption-above-p "" (concat "\n" caption-str))))
                      (t "%s"))))
          (format
           float-env
           (concat (format "\\begin{verbatim}\n%s\\end{verbatim}"
                           (org-export-format-code-default src-block info))))))
       ;; Case 2.  Custom environment.
       (custom-env
        (let ((caption-str (org-latex--caption/label-string src-block info)))
          (format "\\begin{%s}\n%s\\end{%s}\n"
                  custom-env
                  (concat (and caption-above-p caption-str)
                          (org-export-format-code-default src-block info)
                          (and (not caption-above-p) caption-str))
                  custom-env)))
       ;; Case 3.  Use minted package.
       ((eq listings 'minted)
        (let* ((caption-str (org-latex--caption/label-string src-block info))
               (float-env
                (cond
                 ((string= "multicolumn" float)
                  (format "\\begin{listing*}\n%s%%s\n%s\\end{listing*}"
                          (if caption-above-p caption-str "")
                          (if caption-above-p "" caption-str)))
                 ;; (caption
                 ;;  (concat (if caption-above-p caption-str "")
                 ;;          "%s"
                 ;;          (if caption-above-p "" (concat "\n" caption-str))))
                 ((or caption float)
                  (format "\\begin{listing}[H]\n%s\\end{listing}\n%%s" ; Mon Apr 06 09:57:21 EDT 2015 - kmodi
                          caption-str))
                 (t "%s")))
               (options (plist-get info :latex-minted-options))
               (body
                (format
                 "\\begin{minted}[%s]{%s}\n%s\\end{minted}"
                 ;; Options.
                 (concat
                  (org-latex--make-option-string
                   (if (or (not num-start) (assoc "linenos" options))
                       options
                     (append
                      `(("linenos")
                        ("firstnumber" ,(number-to-string (1+ num-start))))
                      options)))
                  (let ((local-options (plist-get attributes :options)))
                    (and local-options (concat "," local-options))))
                 ;; Language.
                 (or (cadr (assq (intern lang)
                                 (plist-get info :latex-minted-langs)))
                     (downcase lang))
                 ;; Source code.
                 (let* ((code-info (org-export-unravel-code src-block))
                        (max-width
                         (apply 'max
                                (mapcar 'length
                                        (org-split-string (car code-info)
                                                          "\n")))))
                   (org-export-format-code
                    (car code-info)
                    (lambda (loc _num ref)
                      (concat
                       loc
                       (when ref
                         ;; Ensure references are flushed to the right,
                         ;; separated with 6 spaces from the widest line
                         ;; of code.
                         (concat (make-string (+ (- max-width (length loc)) 6)
                                              ?\s)
                                 (format "(%s)" ref)))))
                    nil (and retain-labels (cdr code-info)))))))
          ;; Return value.
          (format float-env body)))
       ;; Case 4.  Use listings package.
       (t
        (let ((lst-lang
               (or (cadr (assq (intern lang)
                               (plist-get info :latex-listings-langs)))
                   lang))
              (caption-str
               (when caption
                 (let ((main (org-export-get-caption src-block))
                       (secondary (org-export-get-caption src-block t)))
                   (if (not secondary)
                       (format "{%s}" (org-export-data main info))
                     (format "{[%s]%s}"
                             (org-export-data secondary info)
                             (org-export-data main info))))))
              (lst-opt (plist-get info :latex-listings-options)))
          (concat
           ;; Options.
           (format
            "\\lstset{%s}\n"
            (concat
             (org-latex--make-option-string
              (append
               lst-opt
               (cond
                ((and (not float) (plist-member attributes :float)) nil)
                ((string= "multicolumn" float) '(("float" "*")))
                ((and float (not (assoc "float" lst-opt)))
                 `(("float" ,(plist-get info :latex-default-figure-position)))))
               `(("language" ,lst-lang))
               (if label `(("label" ,label)) '(("label" " ")))
               (if caption-str `(("caption" ,caption-str)) '(("caption" " ")))
               `(("captionpos" ,(if caption-above-p "t" "b")))
               (cond ((assoc "numbers" lst-opt) nil)
                     ((not num-start) '(("numbers" "none")))
                     ((zerop num-start) '(("numbers" "left")))
                     (t `(("firstnumber" ,(number-to-string (1+ num-start)))
                          ("numbers" "left"))))))
             (let ((local-options (plist-get attributes :options)))
               (and local-options (concat "," local-options)))))
           ;; Source code.
           (format
            "\\begin{lstlisting}\n%s\\end{lstlisting}"
            (let* ((code-info (org-export-unravel-code src-block))
                   (max-width
                    (apply 'max
                           (mapcar 'length
                                   (org-split-string (car code-info) "\n")))))
              (org-export-format-code
               (car code-info)
               (lambda (loc _num ref)
                 (concat
                  loc
                  (when ref
                    ;; Ensure references are flushed to the right,
                    ;; separated with 6 spaces from the widest line of
                    ;; code
                    (concat (make-string (+ (- max-width (length loc)) 6) ? \s)
                            (format "(%s)" ref)))))
               nil (and retain-labels (cdr code-info))))))))))))
