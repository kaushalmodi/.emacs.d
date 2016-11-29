;; Time-stamp: <2016-11-29 09:54:04 kmodi>

;; Elisp Slime Nav
;; gtags/ctags like navigation into elisp source codes (even the compressed ones)
;; https://github.com/purcell/elisp-slime-nav

;; Note that starting from emacs 25.1, we do not need this package. The default
;; binding "M-." to `xref-find-definitions' works great!

(use-package elisp-slime-nav
  :ensure t
  :config
  (progn
    (dolist (hook '(emacs-lisp-mode-hook
                    ielm-mode-hook))
      (add-hook hook #'elisp-slime-nav-mode))

    ;;   (defun modi/elisp-slime-nav-find-elisp-interactive (sym-name)
    ;;       "Find the elisp thing at point, be it a function, variable, library or face
    ;; from minibuffer."
    ;;       (interactive
    ;;        (list (read-from-minibuffer "Type elisp function/var/library/face to search: ")))
    ;;       (when sym-name
    ;;         (let ((sym (intern sym-name)))
    ;;           (message "Searching for %s..." (pp-to-string sym))
    ;;           (cond
    ;;            ((fboundp sym) (find-function sym))
    ;;            ((boundp sym) (find-variable sym))
    ;;            ((or (featurep sym) (locate-library sym-name))
    ;;             (find-library sym-name))
    ;;            ((facep sym)
    ;;             (find-face-definition sym))
    ;;            (:else
    ;;             (progn
    ;;               (error "Don't know how to find `%s'" sym)))))))

    (defun modi/elisp-slime-nav-find-elisp-interactive (sym-name)
      "Find the elisp thing at point, be it a function, variable, library or face
    from minibuffer."
      (interactive
       (let ((v (cond
                 ((variable-at-point) (variable-at-point))
                 ((function-called-at-point) (function-called-at-point))
                 (:else nil)))
             val)
         (setq val (completing-read
                    ;; PROMPT
                    "Type elisp function/var/library/face to search: "
                    ;; COLLECTION
                    obarray
                    ;; PREDICATE
                    ;; (lambda (vv)
                    ;;   (or
                    ;;    ;; Filter valid variable names
                    ;;    ;; (get vv 'variable-documentation)
                    ;;    ;; (and (boundp vv) (not (keywordp vv)))
                    ;;    ;; Filter valid function names
                    ;;    (boundp vv)
                    ;;    (fboundp vv)
                    ;;    ))
                    nil
                    ;; (lambda (vv)
                    ;; (or
                    ;; (fboundp vv)
                    ;; (boundp vv)))
                    ;; REQUIRE-MATCH
                    nil
                    ;; INITIAL-INPUT
                    nil
                    ;; HIST
                    nil
                    ;; DEF
                    nil
                    ;; (if (symbolp v) (symbol-name v))
                    ;; INHERIT-INPUT-METHOD
                    nil
                    ))
         ;; (message "Here 1 %s" val)
         (list (if (equal val "")
                   v
                 (intern val)))))
      ;; (message "Here 2 %s" sym-name)
      (when sym-name
        ;; (let ((sym (intern sym-name)))
        (let ((sym sym-name))
          (message "Searching for %s..." (pp-to-string sym))
          (cond
           ((fboundp sym) (find-function sym))
           ((boundp sym) (find-variable sym))
           ((or (featurep sym) (locate-library sym-name))
            (find-library sym-name))
           ((facep sym)
            (find-face-definition sym))
           (:else
            (progn
              (error "Don't know how to find `%s'" sym)))))))
    (bind-to-modi-map "?" #'modi/elisp-slime-nav-find-elisp-interactive)

    ;; Unbind the default `elisp-slime-nav-mode-map' bindings
    (bind-keys
     :map elisp-slime-nav-mode-map
      ("C-c C-d" . nil))))


(provide 'setup-elisp-slime-nav)


;; elisp-slime-nav supports navigation to the definitions of variables,
;; functions, libraries and faces.
;; M-.                   <- Navigate to the symbol at point
;; M-,                   <- Pop back to previous marks
;; C-c C-d d/C-c C-d C-d <- Describe the symbol at point, whatever its type.
