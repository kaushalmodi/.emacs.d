;; Time-stamp: <2019-03-22 17:16:51 kmodi>

;; Info

(use-package info
  :defer t
  :config
  (progn
    (>=e "25.0"                    ;`Info-quoted' was a new face introduced then
        (with-eval-after-load 'setup-font-check
          (when font-dejavu-sans-mono-p
            (set-face-attribute 'Info-quoted nil :family "DejaVu Sans Mono"))))

    ;; https://www.emacswiki.org/emacs/download/info%2b.el
    (use-package info+
      :load-path "elisp/manually-synced/info-plus"
      :config
      (progn
        ;; The faces implementation to highlight strings in "..." is incomplete;
        ;; it does not work well in text having a mix of regular and escaped
        ;; double quotes (" and \"). So the workaround is to disable highlighting
        ;; the double quotes.
        (setq info-quoted+<>-regexp
              (concat
               ;; "\"\\(?:[^\"]\\|\\\\\\(?:.\\|[\n]\\)\\)*\"\\|"           ;"..."
               "`\\(?:[^']\\|\\\\\\(.\\|[\n]\\)\\)*'\\|"                                ;`...'
               "‘\\(?:[^’]\\|\\\\\\(.\\|[\n]\\)\\)*’\\|"                                ;‘...’
               "\“\\(?:[^”]\\|\\\\\\(.\\|[\n]\\)\\)*”\\|"                               ;“...”
               "<\\(?:[[:alpha:]][^>]*\\|\\(\\\\\\(.\\|[\n]\\)\\)*\\)>" ;<...>
               ))

        (defun modi/Info-mode-customization ()
          "My customization for `Info-mode'."
          ;; Show the Info node breadcrumbs only in the header
          ;; Tue Dec 06 23:10:05 EST 2016 - kmodi
          ;; Using both anzu and info+ results in error if info+ breadcrumbs are
          ;; shown in the mode line because anzu modifies the mode line by adding
          ;; its info as a cons, whereas info+ updates the mode line directly.
          (when (not Info-breadcrumbs-in-header-flag)
            (Info-toggle-breadcrumbs-in-header))
          (Info-breadcrumbs-in-mode-line-mode -1))
        (add-hook 'Info-mode-hook #'modi/Info-mode-customization)

        (defun modi/Info--get-current-node-hierarchy ()
          "Return the hierarchy for the current node.
An alist with elements of type (INDEX . NODE) is returned where
INDEX is the hierarchy level starting from \"Top\" of the
`Info-current-file' and NODE is the node name corresponding to
that level."
          (let* ((nodes (Info-toc-nodes Info-current-file))
                 (node Info-current-node)
                 (index 0)
                 (node-hier `((,index . ,node))))
            (while (not (equal "Top" node))
              ;; (message "modi/Info--get-current-node-hierarchy: %0d %s" index node)
              (setq node (nth 1 (assoc node nodes)))
              (setq index (1+ index))
              (add-to-list 'node-hier `(,index . ,node)))
            (dolist (node node-hier)
              (setcar node (- index (car node))))
            ;; (message "modi/Info--get-current-node-hierarchy: %S" node-hier)
            node-hier))

        (defun modi/Info--python3-slug (node &optional nospace)
          "Return the slug string for NODE string for Python 3 documentation.

Replace non-alphabet characters with spaces and then one or more
spaces with a single hyphen.  The returned string contains all
lower-cased letters.

When NOSPACE is non-nil, space characters if present in NODE are
removed instead of replacing with hyphens."
          (let* ((lower-node (downcase node))
                 (nosymbols-node (replace-regexp-in-string "[^A-Za-z]" " " lower-node))
                 (trim-node (replace-regexp-in-string "\\`[\t ]+" "" nosymbols-node))
                 (trim-node (replace-regexp-in-string "[\t ]+\\'" "" trim-node)))
            (if nospace
                ;; Remove all spaces
                (replace-regexp-in-string " +" "" trim-node)
              ;; Replace one or more spaces with a single hyphen
              (replace-regexp-in-string " +" "-" trim-node))))

        (defun modi/Info-python3-doc-url ()
          "Return the Python 3 Documentation URL for the current node."
          (let* ((base-url "https://docs.python.org/3/")
                 (node-hier (modi/Info--get-current-node-hierarchy))
                 (level1 (cdr (assoc 1 node-hier)))
                 (url-dir-name "")
                 (level2 (cdr (assoc 2 node-hier)))
                 (level3 (cdr (assoc 3 node-hier)))
                 (level4 (cdr (assoc 4 node-hier)))
                 (url-page-name "")
                 (section-id-name "")
                 (current-level (car (rassoc Info-current-node node-hier))))
            ;; (message "dbg: level1: %s current level: %0d" level1 current-level)
            ;; Default section ID
            (when (or (= 3 current-level)
                      (= 4 current-level))
              (setq section-id-name (concat "#" (modi/Info--python3-slug Info-current-node))))
            (cond
             ((string-match-p "What.s New in Python" level1)
              (setq url-dir-name "whatsnew/")
              (when level2
                (setq url-page-name (concat (nth 4 (split-string level2)) ;Major version number (example: "3")
                                            "."
                                            (nth 5 (split-string level2)))))) ;Minor version number (example: "6")
             ((string= level1 "The Python Tutorial")
              (setq url-dir-name "tutorial/")
              (when level2
                (string-match (regexp-opt '("Appetite"
                                            "Interpreter"
                                            "Introduction"
                                            "Control Flow"
                                            "Data Structures"
                                            "Modules"
                                            "Input and Output"
                                            "Errors"
                                            "Classes"
                                            "Standard Library"
                                            "Virtual"
                                            "What Now"
                                            "Interactive"
                                            "Floating Point"
                                            "Appendix"))
                              level2)
                (setq url-page-name (cond
                                     ((string-match-p "Standard Library\\'" level2)
                                      "stdlib")
                                     ((string-match-p "Standard Library.+Part II\\'" level2)
                                      "stdlib2")
                                     ((string-match-p "Input and Output" level2)
                                      "inputoutput")
                                     ((string-match-p "Input and Output" level2)
                                      "inputoutput")
                                     ((string-match-p "Virtual" level2)
                                      "venv")
                                     (t
                                      (modi/Info--python3-slug (match-string-no-properties 0 level2) :nospace))))))
             ((string= level1 "Python Setup and Usage")
              (setq url-dir-name "using/")
              )
             ((string= level1 "The Python Language Reference")
              (setq url-dir-name "reference/")
              )
             ((string= level1 "The Python Standard Library")
              (setq url-dir-name "library/")
              (when level2
                (string-match (regexp-opt '("Intro"
                                            "Functions"
                                            "Constants"
                                            "Exceptions"
                                            "Text"
                                            "Binary"
                                            "Data Types"
                                            "Numeric"
                                            "Functional"
                                            "Persistence"
                                            "Archiving"
                                            "File Formats"
                                            "Crypto"
                                            "Markup"
                                            "Internet" ;"Internet Protocols and Support"
                                            "Frameworks"
                                            " Tk"
                                            "Development"
                                            "Debug"
                                            "Distribution"
                                            "Python"
                                            "Modules"
                                            "Language"
                                            "Misc"
                                            "Windows"
                                            "Unix"
                                            "Superseded"
                                            "Undoc"))
                              level2)
                (setq url-page-name (cond
                                     ((string-match-p "Built.in Types" level2)
                                      "stdtypes")
                                     ((string-match-p "File and Directory Access" level2)
                                      "filesys")
                                     ((string-match-p "Generic Operating System Services" level2)
                                      "allos")
                                     ((string-match-p "Concurrent Execution" level2)
                                      "concurrency")
                                     ((string-match-p "Interprocess Communication and Networking" level2)
                                      "ipc")
                                     ((string-match-p "Internet Data Handling" level2)
                                      "netdata")
                                     ((string-match-p "Multimedia Services" level2)
                                      "mm")
                                     ((string-match-p "Internationalization" level2)
                                      "i18n")
                                     ((string-match-p "Custom Python Interpreters" level2)
                                      "custominterp")
                                     (t
                                      (modi/Info--python3-slug (match-string-no-properties 0 level2) :nospace))))))
             ((string= level1 "Extending and Embedding the Python Interpreter")
              (setq url-dir-name "extending/")
              )
             ((string= level1 "Python/C API Reference Manual")
              (setq url-dir-name "c-api/")
              )
             ((string= level1 "Distributing Python Modules")
              (setq url-dir-name "distributing/")
              )
             ((string= level1 "Installing Python Modules")
              (setq url-dir-name "installing/")
              )
             ((string= level1 "Python HOWTOs")
              (setq url-dir-name "howto/")
              )
             ((string= level1 "Python Frequently Asked Questions")
              (setq url-dir-name "faq/")
              (when level2
                (setq url-page-name (car (split-string (downcase level2)))))
              ;; Use level 3 node name for section ID even when the current node
              ;; level is higher than that.. because the official Python 3
              ;; documentation gives up for section naming deeper FAQ sections.
              ;;   Example: https://docs.python.org/3/faq/general.html#id8
              (when (> current-level 3)
                (setq section-id-name (concat "#" (modi/Info--python3-slug level3)))))
             (t
              ))
            (unless (string= "" url-page-name)
              (setq url-page-name (concat url-page-name ".html")))
            (concat base-url url-dir-name url-page-name section-id-name)))

        (defun modi/Info-url-for-current-node (&rest _)
          "Copy the URL for the current node, and also return it.

The node can be a node in the GNU Emacs, GNU Elisp or Python 3 manual.

If this command is called with `\\[universal-argument]' prefix
argument, also open the URL in the default browser."
          (interactive)
          (unless Info-current-file
            (error "This command must be invoked from Info"))
          (let ((node Info-current-node)
                file url)
            (string-match "\\s *\\((\\s *\\([^\t)]*\\)\\s *)\\s *\\|\\)\\(.*\\)" node)
            (setq file (if (= (match-beginning 1) (match-end 1))
                           ""
                         (match-string 2 node)))
            (setq node (match-string 3 node))
            (when (equal node "")
              (setq node  "index"))     ;`Top' node
            (when-let* ((trim (string-match "\\s +\\'" file)))
              (setq file (substring file 0 trim)))
            (when-let* ((trim (string-match "\\s +\\'" node)))
              (setq node (substring node 0 trim)))
            (when (string= "" file)
              (setq file Info-current-file))
            (setq file (file-name-sans-extension (file-name-nondirectory file)))
            (cond
             ((member file '("emacs" "elisp"))
              (setq node (replace-regexp-in-string "[ \t]+" "-" node :fixedcase :literal))
              (setq url (concat "http://www.gnu.org/software/emacs/manual/html_node/"
                                file "/" node ".html")))
             ((string= file "python3")
              (setq url (modi/Info-python3-doc-url)))
             (t
              (error "Manual cannot be `%s'; it can only be `emacs', `elisp' or `python3'" file)))
            (kill-new url)
            (when current-prefix-arg
              (browse-url-of-file url))
            (message "URL: %s" url)
            url))
        (advice-add 'Info-url-for-node :override #'modi/Info-url-for-node)

        (bind-keys
         :map Info-mode-map
         ;; Allow mouse scrolling to do its normal thing
         ("<mouse-4>" . nil)
         ("<mouse-5>" . nil)
         ;; Override the Info-mode-map binding to "?" set by info+
         ("W" . modi/Info-url-for-current-node)
         ("?" . hydra-info/body))))

    (defhydra hydra-info (:color blue
                          :hint nil)
      "
Info-mode:

  ^^_]_ forward  (next logical node)       ^^_l_ast (←)                     _u_p (↑)                             _f_ollow reference       _d_irectory of all manuals
  ^^_[_ backward (prev logical node)       ^^_r_eturn (→)                   _m_enu (↓) (C-u for new window)      _i_ndex                  _T_OC of current manual
  ^^_n_ext (same level only)               ^^_H_istory                      _g_oto (C-u for new window)          _,_ next index item      _w_ copy node name
  ^^_p_rev (same level only)               _<_/_t_op of current manual      _b_eginning of buffer                virtual _I_ndex          _c_lone buffer
  regex _s_earch (_S_ case sensitive)      ^^_>_ final                      _e_nd of buffer                      ^^                       _a_propos

  _<backspace>_/_<SPC>_ Scroll up/down     _1_ .. _9_ Pick first .. ninth item in the node's menu.

"
      ("]"   Info-forward-node)
      ("["   Info-backward-node)
      ("n"   Info-next)
      ("p"   Info-prev)
      ("s"   Info-search)
      ("S"   Info-search-case-sensitively)

      ("l"   Info-history-back)
      ("r"   Info-history-forward)
      ("H"   Info-history)
      ("t"   Info-top-node)
      ("<"   Info-top-node)
      (">"   Info-final-node)

      ("u"   Info-up)
      ("^"   Info-up)
      ("m"   Info-menu)
      ("g"   Info-goto-node)
      ("b"   beginning-of-buffer)
      ("e"   end-of-buffer)

      ("f"   Info-follow-reference)
      ("i"   Info-index)
      (","   Info-index-next)
      ("I"   Info-virtual-index)

      ("d"   Info-directory)
      ("T"   Info-toc)
      ("w"   Info-copy-current-node-name) ;M-0 w will copy elisp form of current node name
      ("c"   clone-buffer)
      ("a"   info-apropos)

      ("1"   Info-nth-menu-item)
      ("2"   Info-nth-menu-item)
      ("3"   Info-nth-menu-item)
      ("4"   Info-nth-menu-item)
      ("5"   Info-nth-menu-item)
      ("6"   Info-nth-menu-item)
      ("7"   Info-nth-menu-item)
      ("8"   Info-nth-menu-item)
      ("9"   Info-nth-menu-item)

      ("<backspace>" Info-scroll-down)
      ("<SPC>" Info-scroll-up)

      ("?"   Info-summary "Info summary")
      ("h"   Info-help "Info help")
      ("q"   Info-exit "Info exit")
      ("C-g" nil "cancel" :color blue))

    (bind-keys
     :map Info-mode-map
     ("y" . bury-buffer)
     ;; Override the Info-mode-map binding to "?" set by info+
     ("?" . hydra-info/body))))

(defun counsel-ag-emacs-info (&optional initial-input)
  "Search in all Info manuals in the emacs 'info/' directory using ag.
This directory contains the emacs, elisp, eintr, org, calc Info manuals and other
manuals too for the packages that ship with emacs.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (counsel-ag initial-input (car Info-default-directory-list)
              " -z" "Search emacs/elisp info"))

;; http://oremacs.com/2015/03/17/more-info/
(defun ora-open-info (topic bufname)
  "Open info on TOPIC in BUFNAME."
  (if (get-buffer bufname)
      (progn
        (switch-to-buffer bufname)
        (unless (string-match topic Info-current-file)
          (Info-goto-node (format "(%s)" topic))))
    (info topic bufname)))

(defhydra hydra-info-to (:hint nil
                         :color teal)
  "
_i_nfo      _o_rg      e_l_isp      e_L_isp intro      _e_macs      _c_alc      _g_rep emacs info      _p_ython"
  ("i" info)
  ("o" (ora-open-info "org" "*Org Info*"))
  ("l" (ora-open-info "elisp" "*Elisp Info*"))
  ("L" (ora-open-info "eintr" "*Elisp Intro Info*"))
  ("e" (ora-open-info "emacs" "*Emacs Info*"))
  ("c" (ora-open-info "calc" "*Calc Info*"))
  ("C" (ora-open-info "cl" "* Emacs Common Lisp Info*"))
  ("p" (ora-open-info "python3" "*Python 3 Info*"))
  ("g" counsel-ag-emacs-info))
(bind-key "C-h i" #'hydra-info-to/body modi-mode-map)


(provide 'setup-info)
