;; Time-stamp: <2015-06-01 11:30:29 kmodi>

;; Elisp Slime Nav
;; gtags/ctags like navigation into elisp source codes (even the compressed ones)
;; https://github.com/purcell/elisp-slime-nav

(use-package elisp-slime-nav
  :config
  (progn
    (dolist (hook '(emacs-lisp-mode-hook
                    ielm-mode-hook))
      (add-hook hook #'elisp-slime-nav-mode))

    ;; Unbind the default `elisp-slime-nav-mode-map' bindings
    (define-key elisp-slime-nav-mode-map (kbd "C-c C-d d") nil)
    (define-key elisp-slime-nav-mode-map (kbd "C-c C-d C-d") nil)
    (define-key elisp-slime-nav-mode-map (kbd "C-c C-d") nil)

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
    ;;               (error "Don't know how to find '%s'" sym)))))))

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
              (error "Don't know how to find '%s'" sym)))))))

    (bind-to-modi-map "?" modi/elisp-slime-nav-find-elisp-interactive)))


(provide 'setup-elisp-slime-nav)


;; elisp-slime-nav supports navigation to the definitions of variables,
;; functions, libraries and faces.
;; M-.                   <- Navigate to the symbol at point
;; M-,                   <- Pop back to previous marks
;; C-c C-d d/C-c C-d C-d <- Describe the symbol at point, whatever its type.
