;; Time-stamp: <2015-09-13 19:28:29 kmodi>

(use-package image-mode
  :config
  (progn
    ;; Force Imagemagick for viewing all images
    ;; I was unable to use Imagemagick required functions to do stuff like
    ;; scale images and so I filed this emacs bug report:
    ;;  http://debbugs.gnu.org/cgi/bugreport.cgi?bug=18797#5
    ;; Solution is to not allow `libjpeg' loader to load
    ;;  http://debbugs.gnu.org/cgi/bugreport.cgi?bug=10746
    ;; (setq image-type-header-regexps nil)
    ;; .. also to specify to always use `imagemagick'
    ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=10112
    ;; (add-to-list 'image-type-header-regexps '("\\`\377\330" . imagemagick))
    ;; (add-to-list 'image-type-header-regexps '("\\`\x89PNG\r\n\x1a\n" . imagemagick))
    (setq image-type-header-regexps
          `(
            ("\\`/[\t\n\r ]*\\*.*XPM.\\*/" . xpm)
            ("\\`P[1-6]\\\(?:\
             \\(?:\\(?:#[^\r\n]*[\r\n]\\)?[[:space:]]\\)+\
             \\(?:\\(?:#[^\r\n]*[\r\n]\\)?[0-9]\\)+\
             \\)\\{2\\}" . pbm)
            ("\\`GIF8[79]a" . gif)
            ("\\`\x89PNG\r\n\x1a\n" . png) ; Uncomment this (and comment the below line)
                                        ; to enable inline png images in org-mode
                                        ; ("\\`\x89PNG\r\n\x1a\n" . imagemagick) ; png
            ("\\`[\t\n\r ]*#define \\([a-z0-9_]+\\)_width [0-9]+\n\
             #define \\1_height [0-9]+\n\\(\
             #define \\1_x_hot [0-9]+\n\
             #define \\1_y_hot [0-9]+\n\\)?\
             static \\(unsigned \\)?char \\1_bits" . xbm)
            ;; ("\\`\\(?:MM\0\\*\\|II\\*\0\\)" . tiff)
            ("\\`\\(?:MM\0\\*\\|II\\*\0\\)" . imagemagick) ; tiff
            ("\\`[\t\n\r ]*%!PS" . postscript)
            ;; ("\\`\xff\xd8" . jpeg) ; Uncomment this (and comment the below line)
                                        ; to enable inline jpg images in org-mode
            ("\\`\xff\xd8" . imagemagick)    ; jpeg
            ("\\`\377\330" . imagemagick)    ; jpeg
            (,(let* ((incomment-re "\\(?:[^-]\\|-[^-]\\)")
                     (comment-re (concat "\\(?:!--" incomment-re "*-->[ \t\r\n]*<\\)")))
                (concat "\\(?:<\\?xml[ \t\r\n]+[^>]*>\\)?[ \t\r\n]*<"
                        comment-re "*"
                        "\\(?:!DOCTYPE[ \t\r\n]+[^>]*>[ \t\r\n]*<[ \t\r\n]*" comment-re "*\\)?"
                        "[Ss][Vv][Gg]"))
             ;; . svg)  ; Uncomment this (and comment the below line) to
                                        ; enable inline svg images in org-mode
             . imagemagick) ; svg
            ))

    ;; http://emacs.stackexchange.com/a/2458/115
    (defun modi/image-transform-fit-to-window()
      "Resize the image to fit the width or height based on the image and window ratios.
Imagemagick is required to run this function."
      (interactive)
      (let* ( (img-size (image-display-size (image-get-display-property) t))
              (img-width (car img-size))
              (img-height (cdr img-size))
              (img-h/w-ratio (/ (float img-height) (float img-width)))
              (win-width (- (nth 2 (window-inside-pixel-edges))
                            (nth 0 (window-inside-pixel-edges))))
              (win-height (- (nth 3 (window-inside-pixel-edges))
                             (nth 1 (window-inside-pixel-edges))))
              (win-h/w-ratio (/ (float win-height) (float win-width))))
        ;; (message "%s" img-width)
        ;; (message "%s" img-height)
        ;; (message "%s" img-h/w-ratio)
        ;; (message "%s" win-width)
        ;; (message "%s" win-height)
        ;; (message "%s" win-h/w-ratio)
        ;; Fit image by width if the h/w ratio of window is > h/w ratio of the image
        (if (> win-h/w-ratio img-h/w-ratio)
            (image-transform-fit-to-width)
          ;; Else fit by height
          (image-transform-fit-to-height))))
    (bind-keys
     :map image-mode-map
      ("r" . modi/image-transform-fit-to-window))))


(provide 'setup-image)
