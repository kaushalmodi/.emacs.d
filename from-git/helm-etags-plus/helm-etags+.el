;;; helm-etags+.el --- Another Etags helm.el interface

;; Created: 2011-02-23
;; Last Updated: 纪秀峰 2013-11-25 23:42:03 
;; Version: 0.1.7
;; Author: 纪秀峰(Joseph) <jixiuf@gmail.com>
;; Copyright (C) 2011~2012, 纪秀峰(Joseph), all rights reserved.
;; URL       :https://github.com/jixiuf/helm-etags-plus
;; screencast:http://screencast-repos.googlecode.com/files/emacs-anything-etags-puls.mp4.bz2
;; Keywords: helm, etags
;; Compatibility: (Test on GNU Emacs 23.2.1)
;;
;; Features that might be required by this library:
;;
;; `helm' `etags'
;;
;;
;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This package use `helm' as an interface to find tag with Etags.
;;
;;  it support multiple tag files.
;;  and it can recursively searches each parent directory for a file named
;;  'TAGS'. so you needn't add this special file to `tags-table-list'
;;
;;  if you use GNU/Emacs ,you can set `tags-table-list' like this.
;;  (setq tags-table-list '("/path/of/TAGS1" "/path/of/TAG2"))
;;
;;  (global-set-key "\M-." 'helm-etags+-select)
;;       `M-.' default use symbol under point as tagname
;;       `C-uM-.' use pattern you typed as tagname
;;
;; helm-etags+.el also support history go back ,go forward and list tag
;; histories you have visited.(must use commands list here:)
;;  `helm-etags+-history'
;;    List all tag you have visited with `helm'.
;;  `helm-etags+-history-go-back'
;;    Go back cyclely.
;;  `helm-etags+-history-go-forward'
;;    Go Forward cyclely.
;;
;; if you want to work with `etags-table.el' ,you just need
;; add this line to to init file after loading etags-table.el
;;
;;     (add-hook 'helm-etags+-select-hook 'etags-table-recompute)
;;    (setq etags-table-alist
;;     (list
;;        '("/home/me/Projects/foo/.*\\.[ch]$" "/home/me/Projects/lib1/TAGS" "/home/me/Projects/lib2/TAGS")
;;        '("/home/me/Projects/bar/.*\\.py$" "/home/me/Projects/python/common/TAGS")
;;        '(".*\\.[ch]$" "/usr/local/include/TAGS")
;;        ))
;;
;;; Installation:
;;
;; Just put helm-etags+.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'helm-etags+)
;;
;; No need more.
;;
;; I use GNU/Emacs,and this is my config file about etags
;; (require 'helm-etags+)
;; (global-set-key "\M-." 'helm-etags+-select)
;; ;;list all visited tags
;; (global-set-key "\M-*" 'helm-etags+-history)
;; ;;go back directly
;; (global-set-key "\M-," 'helm-etags+-history-action-go-back)
;; ;;go forward directly
;; (global-set-key "\M-/" 'helm-etags+-history-action-go-forward)
;;
;; and how to work with etags-table.el
;; (require 'etags-table)
;; (setq etags-table-alist
;;       (list
;;        '("/home/me/Projects/foo/.*\\.[ch]$" "/home/me/Projects/lib1/TAGS" "/home/me/Projects/lib2/TAGS")
;;        '("/home/me/Projects/bar/.*\\.py$" "/home/me/Projects/python/common/TAGS")
;;        '("/tmp/.*\\.c$"  "/java/tags/linux.tag" "/tmp/TAGS" )
;;        '(".*\\.java$"  "/opt/sun-jdk-1.6.0.22/src/TAGS" )
;;        '(".*\\.[ch]$"  "/java/tags/linux.ctags")
;;        ))
;; (add-hook 'helm-etags+-select-hook 'etags-table-recompute)

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `helm-etags+-select'
;;    Find Tag using `etags' and `helm'
;;  `helm-etags+-history-go-back'
;;    Go Back.
;;  `helm-etags+-history-go-forward'
;;    Go Forward.
;;  `helm-etags+-history'
;;    show all tag historys using `helm'
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `helm-etags+-use-short-file-name'
;;    t means use filename,
;;    default = nil
;;  `helm-etags+-filename-location'
;;    display src filename after src file name parent dir or not.
;;    default = (quote filename-after-dir)
;;  `helm-etags+-highlight-after-jump'
;;    *If non-nil, temporarily highlight the tag
;;    default = t
;;  `helm-etags+-highlight-delay'
;;    *How long to highlight the tag.
;;    default = 0.2

;;; Code:

;; Some functions are borrowed from helm-etags.el and etags-select.el.

;;; Require
;; (require 'custom)
(require 'etags)
(require 'helm)
;; (require 'helm-config nil t)        ;optional
(eval-when-compile
   (require 'helm-match-plugin nil t)
  )
;;  ;optional

;;; Custom

(defgroup helm-etags+ nil
  "Another Etags helm.el interface."
  :prefix "helm-etags+-"
  :group 'etags)

(defcustom helm-etags+-use-short-file-name nil
  "t means use filename,
  'absolute means use absolute filename
  nil means use relative filename as the display,
 search '(DISPLAY . REAL)' in helm.el for more info."
  :type '(choice (const nil) (const t) (const absolute))
  :group 'helm-etags+)

(defcustom helm-etags+-filename-location 'filename-after-dir
  "display src filename after src file name parent dir or not."
  :type '(choice (const filename-before-dir) (const filename-after-dir))
  :group 'helm-etags+)

(defcustom helm-etags+-highlight-after-jump t
  "*If non-nil, temporarily highlight the tag
  after you jump to it.
  (borrowed from etags-select.el)"
  :group 'helm-etags+
  :type 'boolean)

(defcustom helm-etags+-highlight-delay 0.2
  "*How long to highlight the tag.
  (borrowed from etags-select.el)"
  :group 'helm-etags+
  :type 'number)

(defface helm-etags+-highlight-face
  '((t (:foreground "white" :background "cadetblue4" :bold t)))
  "Font Lock mode face used to highlight tags.
  (borrowed from etags-select.el)"
  :group 'helm-etags+)

(defun helm-etags+-highlight (beg end)
  "Highlight a region temporarily.
   (borrowed from etags-select.el)"
  (let ((ov (make-overlay beg end)))
      (overlay-put ov 'face 'helm-etags+-highlight-face)
      (sit-for helm-etags+-highlight-delay)
      (delete-overlay ov)))

;;; Hooks

(defvar helm-etags+-select-hook nil
  "hooks run before `helm' funcion with
   source `helm-c-source-etags+-select'")

;;; Variables
(defvar  helm-etags+-markers (make-ring 8))

(defvar helm-etags+-cur-mark nil
  "a marker in `helm-etags+-markers', going back and going
forward are related to this variable.")

;; (defvar helm-etags+-history-tmp-marker nil
;;   "this variable will remember current position
;;    when you call `helm-etags+-history'.
;;   after you press `RET' execute `helm-etags+-history-action'
;;  it will be push into `helm-etags+-markers'")
(defvar helm-etags+-tag-table-buffers nil
  "each time `helm-etags+-select' is executed ,it
will set this variable.")
(defvar helm-input-idle-delay-4-helm-etags+ 0.8
  "see `helm-idle-delay'. I will set it locally
   in `helm-etags+-select'")

(defvar prev-opened-buf-in-persist-action nil
  "record it to kill-it in persistent-action,in order to
   not open too much buffer.")

(defvar helm-etags+-prev-matched-pattern nil
  "work with `helm-etags+-candidates-cache'.
  the value is (car (helm-mp-split-pattern helm-pattern))
:the first part of `helm-pattern', the matched
 candidates is saved in `helm-etags+-candidates-cache'. when current
'(car (helm-mp-split-pattern helm-pattern))' is equals to this value
then the cached candidates can be reused ,needn't find from the tag file.")

(defvar helm-etags+-candidates-cache nil
  "documents see `helm-etags+-prev-matched-pattern'")
(defvar helm-etags+-untransformed-helm-pattern
  "this variable is seted in func of transformed-pattern .and is used when
getting candidates.")

;;; Functions

(defun helm-etags+-case-fold-search ()
  "Get case-fold search."
  (when (boundp 'tags-case-fold-search)
    (if (memq tags-case-fold-search '(nil t))
        tags-case-fold-search
      case-fold-search)))

(defun helm-etags+-find-tags-file ()
  "recursively searches each parent directory for a file named 'TAGS' and returns the
path to that file or nil if a tags file is not found. Returns nil if the buffer is
not visiting a file"
(let ((tag-root-dir (locate-dominating-file default-directory "TAGS")))
    (if tag-root-dir
        (expand-file-name "TAGS" tag-root-dir)
      nil)))

(defun helm-etags+-get-tag-files()
  "Get tag files."
  (let ((local-tag  (helm-etags+-find-tags-file)))
      (when local-tag
        (add-to-list 'tags-table-list (helm-etags+-find-tags-file)))
      (dolist (tag tags-table-list)
        (when (not (file-exists-p tag))
          (setq  tags-table-list (delete tag tags-table-list))))
      (mapcar 'tags-expand-table-name tags-table-list)))

(defun helm-etags+-rename-tag-buffer-maybe(buf)
  (with-current-buffer buf
    (if (string-match "^ \\*Helm" (buffer-name))
        buf
      (rename-buffer (concat" *Helm etags+:" (buffer-name)
                            "-" (number-to-string (random)) "*") nil)
      ))buf)

(defun helm-etags+-get-tag-table-buffer (tag-file)
  "Get tag table buffer for a tag file."
  (when (file-exists-p tag-file)
    (let ((tag-table-buffer) (current-buf (current-buffer))
          (tags-revert-without-query t)
          (large-file-warning-threshold nil)
          (tags-add-tables t))

        (visit-tags-table-buffer tag-file)
        (setq tag-table-buffer (find-buffer-visiting tag-file))
      (set-buffer current-buf)
      (helm-etags+-rename-tag-buffer-maybe tag-table-buffer))))

(defun helm-etags+-get-avail-tag-bufs()
  "Get tag table buffer for a tag file."
  (setq helm-etags+-tag-table-buffers
        (delete nil (mapcar 'helm-etags+-get-tag-table-buffer
                            (helm-etags+-get-tag-files)))))

(defun helm-etags+-get-candidates-cache()
  "for example when the `helm-pattern' is 'toString System pub'
   only 'toString' is treated as tagname,and
`helm-etags+-candidates-from-all-file'
will search `toString' in all tag files. and the found
 candidates is stored in `helm-etags+-candidates-cache'
'toString' is stored in `helm-etags+-prev-matched-pattern'
so when the `helm-pattern' become to 'toString System public'
needn't search tag file again."
  (let ((pattern (car (helm-mp-split-pattern helm-etags+-untransformed-helm-pattern))));;default use whole helm-pattern to search in tag files
    ;; first collect candidates using first part of helm-pattern
    ;; (when (featurep 'helm-match-plugin)
    ;;   ;;for example  (helm-mp-split-pattern "boo far") -->("boo" "far")
    ;;   (setq pattern  (car (helm-mp-split-pattern helm-etags+-untransformed-helm-pattern))))
    (cond
     ((or (string-equal "" pattern) (string-equal "\\_<\\_>" pattern))
       nil)
     ((not  (string-equal helm-etags+-prev-matched-pattern pattern))
       (setq helm-etags+-prev-matched-pattern pattern)
       (setq helm-etags+-candidates-cache (helm-etags+-candidates-from-all-file pattern)))
     (t helm-etags+-candidates-cache))))

(defun helm-etags+-candidates-from-all-file(first-part-of-helm-pattern)
  (let (candidates)
    (dolist (tag-table-buffer helm-etags+-tag-table-buffers)
      (setq candidates
            (append
             candidates
             (helm-etags+-candidates-from-tag-file
              first-part-of-helm-pattern tag-table-buffer))))
    candidates))

(defun helm-etags+-candidates-from-tag-file (tagname tag-table-buffer)
  "find tagname in tag-table-buffer. "
  (catch 'failed
    (let ((case-fold-search (helm-etags+-case-fold-search))
          tag-info tag-line src-file-name full-tagname
          tag-regex
          tagname-regexp-quoted
          candidates)
      (if (string-match "\\\\_<\\|\\\\_>[ \t]*" tagname)
          (progn
            (setq tagname (replace-regexp-in-string "\\\\_<\\|\\\\_>[ \t]*" ""  tagname))
            (setq tagname-regexp-quoted (regexp-quote tagname))
            (setq tag-regex (concat "^.*?\\(" "\^?\\(.+[:.']"  tagname-regexp-quoted "\\)\^A"
                                    "\\|" "\^?"  tagname-regexp-quoted "\^A"
                                    "\\|" "\\<"  tagname-regexp-quoted "[ \f\t()=,;]*\^?[0-9,]"
                                    "\\)")))
        (setq tagname-regexp-quoted (regexp-quote tagname))
        (setq tag-regex (concat "^.*?\\(" "\^?\\(.+[:.'].*"  tagname-regexp-quoted ".*\\)\^A"
                                "\\|" "\^?.*"  tagname-regexp-quoted ".*\^A"
                                "\\|" ".*"  tagname-regexp-quoted ".*[ \f\t()=,;]*\^?[0-9,]"
                                "\\)")))
      (with-current-buffer tag-table-buffer
        (modify-syntax-entry ?_ "w")
        (goto-char (point-min))
        (while (search-forward  tagname nil t) ;;take care this is not re-search-forward ,speed it up
          (beginning-of-line)
          (when (re-search-forward tag-regex (point-at-eol) 'goto-eol)
            (setq full-tagname (or (match-string-no-properties 2) tagname))
            (beginning-of-line)
            (save-excursion (setq tag-info (etags-snarf-tag)))
            (re-search-forward "\\s-*\\(.*?\\)\\s-*\^?" (point-at-eol) t)
            (setq tag-line (match-string-no-properties 1))
            (setq tag-line (replace-regexp-in-string  "/\\*.*\\*/" "" tag-line))
            (setq tag-line (replace-regexp-in-string  "\t" (make-string tab-width ? ) tag-line))
            (end-of-line)
            ;;(setq src-file-name (etags-file-of-tag))
            (setq src-file-name   (file-truename (etags-file-of-tag)))
            (let ((display)(real (list  src-file-name tag-info full-tagname))
                  (src-location-display (file-name-nondirectory src-file-name)))
              (cond
               ((equal helm-etags+-use-short-file-name nil)
                (let ((tag-table-parent (file-truename (file-name-directory (buffer-file-name tag-table-buffer))))
                      (src-file-parent (file-name-directory src-file-name)))
                  (when (string-match  (regexp-quote tag-table-parent) src-file-name)
                    (if (equal 'filename-after-dir helm-etags+-filename-location)
                        (setq src-location-display (substring src-file-name (length  tag-table-parent)))
                      (setq src-location-display (concat src-location-display "\\"  (substring src-file-parent (length  tag-table-parent))))
                      ))))
               ((equal helm-etags+-use-short-file-name t)
                (setq src-location-display (file-name-nondirectory src-file-name)))
               ((equal helm-etags+-use-short-file-name 'absolute)
                (let ((src-file-parent (file-name-directory src-file-name)))
                  (if (equal 'filename-after-dir helm-etags+-filename-location)
                      (setq src-location-display src-file-name)
                    (setq src-location-display (concat src-location-display "\\"
                                                       (mapconcat 'identity (reverse (split-string src-file-parent "/")) "/" )))
                  )
                  )

                ))
              (setq display (concat tag-line
                                    (or (ignore-errors
                                          (make-string (- (window-width)
                                                          (string-width tag-line)
                                                          (string-width  src-location-display))
                                                       ? )) "")
                                    src-location-display))
              (add-to-list 'candidates (cons display real)))))
        (modify-syntax-entry ?_ "_"))
      candidates)))

(defun helm-etags+-find-tag(candidate)
  "Find tag that match CANDIDATE from `tags-table-list'.
   And switch buffer and jump tag position.."
  (let ((src-file-name (car candidate))
        (tag-info (nth 1 candidate))
        (tagname (nth 2 candidate))
        src-file-buf)
    (when (file-exists-p src-file-name)
      ;; Jump to tag position when
      ;; tag file is valid.
      (setq src-file-buf (find-file src-file-name))
      (etags-goto-tag-location  tag-info)

      (beginning-of-line)
      (when (search-forward tagname (point-at-eol) t)
        (goto-char (match-beginning 0))
        (setq tagname (thing-at-point 'symbol))
        (beginning-of-line)
        (search-forward tagname (point-at-eol) t)
        (goto-char (match-beginning 0))
        (when(and helm-etags+-highlight-after-jump
                  (not helm-in-persistent-action))
          (helm-etags+-highlight (match-beginning 0) (match-end 0))))

      (when (and helm-in-persistent-action ;;color
                 (fboundp 'helm-highlight-current-line))
        (helm-highlight-current-line))

      (if helm-in-persistent-action ;;prevent from opening too much buffer in persistent action
          (progn
            (if (and prev-opened-buf-in-persist-action
                     (not (equal prev-opened-buf-in-persist-action src-file-buf)))
                (kill-buffer  prev-opened-buf-in-persist-action))
            (setq prev-opened-buf-in-persist-action src-file-buf))
        (setq prev-opened-buf-in-persist-action nil)))))

(defun pos-in-same-symbol-p(marker1 marker2)
  "check whether `marker1' and `marker2' are at the same place or not"
  (cond
   ((and (helm-etags+-is-marker-available marker1)
         (helm-etags+-is-marker-available marker2)
         (equal (marker-buffer marker1) (marker-buffer marker2)))
    (let((pos1 (marker-position marker1))
         (pos2 (marker-position marker2))
         bounds1 bounds2)
      (setq bounds1
            (unwind-protect
                (save-excursion
                  (goto-char pos1)
                  (bounds-of-thing-at-point 'symbol) )
              nil))
      (setq bounds2
            (unwind-protect
                (save-excursion
                  (goto-char pos2)
                  (bounds-of-thing-at-point 'symbol) )
              nil))
      (and bounds1 bounds1
           (equal bounds1 bounds2))))
   (t nil)))

(defun helm-c-etags+-goto-location (candidate)
  (unless helm-in-persistent-action
    ;;you can use `helm-etags+-history' go back
    (when (or  (ring-empty-p helm-etags+-markers)
               (not (pos-in-same-symbol-p  (point-marker)
                                           (ring-ref helm-etags+-markers 0))))
      (let ((index (ring-member helm-etags+-markers (point-marker))))
        (when index (ring-remove helm-etags+-markers index)))
      (ring-insert helm-etags+-markers (point-marker))
      ))

  (helm-etags+-find-tag candidate);;core func.

  (when (or  (ring-empty-p helm-etags+-markers)
             (not (pos-in-same-symbol-p  (point-marker)
                                         (ring-ref helm-etags+-markers 0))))
    (let ((index (ring-member helm-etags+-markers (point-marker))))
      (when index (ring-remove helm-etags+-markers index)))
    (ring-insert helm-etags+-markers (point-marker))
    (setq helm-etags+-cur-mark (point-marker))))

;; if you want call helm-etags in your special function
;; you can do it like this
;; (condition-case nil
;;     (helm-etags+-select-internal   (concat "\\_<" "helm-etags+-dddselect-internal" "\\_>"))
;;   (error (message "do something when no candidates found")))
(defun helm-etags+-select-internal(&optional pattern)
  "Find Tag using `etags' and `helm' `pattern' is a regexp."
  (interactive "P")
  (let ((helm-execute-action-at-once-if-one t)
        (helm-quit-if-no-candidate  (lambda()(error "no candidates")))
        (helm-maybe-use-default-as-input nil)
        (helm-candidate-number-limit nil)
        (helm-input-idle-delay helm-input-idle-delay-4-helm-etags+))
    (when (and pattern (string-equal "" pattern) ) (setq helm-quit-if-no-candidate nil) )
    (run-hooks 'helm-etags+-select-hook)
    (helm  :sources 'helm-c-source-etags+-select
           ;; :default (concat "\\_<" (thing-at-point 'symbol) "\\_>")
           ;; Initialize input with current symbol
           :input (or pattern (concat "\\_<" (thing-at-point 'symbol) "\\_>"))
           :prompt "Find Tag(require 3 char): ")))

;; if you want call helm-etags in your special function
;; you can do it like this
;; (condition-case nil
;;     (helm-etags+-select)
;;   (error (message "do something when no candidates found")))
;;;###autoload
(defun helm-etags+-select(&optional arg)
  "Find Tag using `etags' and `helm'"
  (interactive "P")
  (cond
   ((equal arg '(4))                  ;C-u
    (helm-etags+-select-internal "")) ;waiting for you input pattern
   (t (helm-etags+-select-internal))))  ;use thing-at-point as symbol

(defvar helm-c-source-etags+-select
  '((name . "Etags+")
    (init . helm-etags+-get-avail-tag-bufs)
    (candidates . helm-etags+-get-candidates-cache)
    (volatile);;candidates
    (pattern-transformer (lambda (helm-pattern)
                           (setq helm-etags+-untransformed-helm-pattern helm-pattern)
                           (regexp-quote (replace-regexp-in-string "\\\\_<\\|\\\\_>" ""  helm-pattern))))
    (requires-pattern  . 3);;need at least 3 char
    ;; (delayed);; (setq helm-input-idle-delay-4-helm-etags+ 1)
    (action ("Goto the location" . helm-c-etags+-goto-location))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Go Back and Go Forward

;;util func

;;(helm-etags+-is-marker-avaiable (ring-ref helm-etags+-markers 0))
(defun helm-etags+-is-marker-available(marker)
  "return nil if marker is nil or  in dead buffer ,
   return marker if it is live"
  (if (and marker
           (markerp marker)
           (marker-buffer marker))
      marker
    ))
;;; func about history
(defun helm-etags+-hist-get-candidate-from-marker(marker)
  "genernate candidate from marker candidate= (display . marker)."
  (let ((buf (marker-buffer marker))
        (pos (marker-position marker))
        line-num line-text candidate display
        file-name empty-string)
    (when  buf
      ;;      (save-excursion
      ;;        (set-buffer buf)
      (with-current-buffer buf
        (setq file-name  (buffer-name))
        (goto-char pos)
        (setq line-num (int-to-string (count-lines (point-min) pos)))
        (setq line-text (buffer-substring-no-properties (point-at-bol)(point-at-eol)))
        (setq line-text (replace-regexp-in-string "^[ \t]*\\|[ \t]*$" "" line-text))
        (setq line-text (replace-regexp-in-string  "/\\*.*\\*/" "" line-text))
        (setq line-text (replace-regexp-in-string  "\t" (make-string tab-width ? ) line-text)))
      ;;          )
      (if (equal marker helm-etags+-cur-mark)
          ;;this one will be preselected
          (setq line-text (concat line-text "\t")))
      (setq empty-string  (or (ignore-errors
                                (make-string (- (window-width) 4
                                                (string-width  line-num)
                                                (string-width file-name)
                                                (string-width line-text))
                                             ? )) " "))
      (setq display (concat line-text empty-string
                            file-name ":[" line-num "]"))
      (setq candidate  (cons display marker)))))

;; time_init
(defun helm-etags+-history-candidates()
  "generate candidates from `helm-etags+-markers'.
  and remove unavailable markers in `helm-etags+-markers'"
   (mapcar 'helm-etags+-hist-get-candidate-from-marker (ring-elements helm-etags+-markers)))

(defun helm-etags+-history-init()
  "remove #<marker in no buffer> from `helm-etags+-markers'.
   and remove those markers older than #<marker in no buffer>."
  (let ((tmp-marker-ring))
    (while (not (ring-empty-p helm-etags+-markers))
      (helm-aif (helm-etags+-is-marker-available (ring-remove helm-etags+-markers 0))
          (setq tmp-marker-ring (append tmp-marker-ring (list it)));;new item first
        (while (not (ring-empty-p helm-etags+-markers));;remove all old marker
          (ring-remove helm-etags+-markers))))
    ;;reinsert all available marker to `helm-etags+-markers'
    (mapcar (lambda(marker) (ring-insert-at-beginning helm-etags+-markers marker)) tmp-marker-ring))
  )

(defun helm-etags+-history-clear-all(&optional candidate)
  "param `candidate' is unused."
  (while (not (ring-empty-p helm-etags+-markers));;remove all marker
    (ring-remove helm-etags+-markers)))


;;;###autoload
(defun helm-etags+-history-go-back()
  "Go Back."
  (interactive)
  (helm-etags+-history-init)
  (when
      (let ((next-marker))
        (cond ((and (helm-etags+-is-marker-available helm-etags+-cur-mark)
                    (ring-member helm-etags+-markers helm-etags+-cur-mark))
               (setq next-marker (ring-next helm-etags+-markers helm-etags+-cur-mark)))
              ((not(ring-empty-p helm-etags+-markers))
               (setq next-marker  (ring-ref helm-etags+-markers 0)))
              (t nil))
        (when next-marker
          (helm-etags+-history-go-internel next-marker)
          (setq helm-etags+-cur-mark next-marker)))))

;;;###autoload
(defun helm-etags+-history-go-forward()
  "Go Forward."
  (interactive)
  (helm-etags+-history-init)
  (when
      (let ((prev-marker))
        (cond ((and (helm-etags+-is-marker-available helm-etags+-cur-mark)
                    (ring-member helm-etags+-markers helm-etags+-cur-mark))
               (setq prev-marker (ring-previous helm-etags+-markers helm-etags+-cur-mark)))
              ((not(ring-empty-p helm-etags+-markers))
               (setq prev-marker  (ring-ref helm-etags+-markers 0)))
              (t nil))
        (when prev-marker
          (helm-etags+-history-go-internel prev-marker)
          (setq helm-etags+-cur-mark prev-marker)))))

(defun helm-etags+-history-go-internel (candidate-marker)
  "Go to the location depend on candidate."
  (let ((buf (marker-buffer candidate-marker))
        (pos (marker-position candidate-marker)))
    (when buf
      (switch-to-buffer buf)
      (set-buffer buf)
      (goto-char pos))))

;; (action .func),candidate=(Display . REAL), now in this func
;; param candidate is 'REAL' ,the marker.
(defun helm-etags+-history-action-go(candidate)
  "List all history."
  (helm-etags+-history-go-internel candidate)
  (unless  helm-in-persistent-action
    (setq helm-etags+-cur-mark candidate))
  (when  helm-in-persistent-action
    (helm-highlight-current-line)))

(defvar helm-c-source-etags+-history
  '((name . "Etags+ History: ")
    (header-name .( (lambda (name) (concat name "`RET': Go ,`C-z' Preview. `C-e': Clear all history."))))
    (init .  helm-etags+-history-init)
    (candidates . helm-etags+-history-candidates)
    ;;        (volatile) ;;maybe needn't
    (action . (("Go" . helm-etags+-history-action-go)
               ("Clear all history" . helm-etags+-history-clear-all)))))

;;;###autoload
(defun helm-etags+-history()
  "show all tag historys using `helm'"
  (interactive)
  (let ((helm-execute-action-at-once-if-one t)
        (helm-quit-if-no-candidate
         (lambda () (message "No history record in `helm-etags+-markers'"))))
    (helm :sources    '(helm-c-source-etags+-history)
          :input      ""
          :preselect  "\t")))           ;if an candidate ,then this line is preselected

(provide 'helm-etags+)
;;;helm-etags+.el ends here.
