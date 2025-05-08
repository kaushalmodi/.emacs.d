;; Copilot
;; https://github.com/copilot-emacs/copilot.el

(use-package copilot
  :defer t
  :bind (:map copilot-completion-map
         ("TAB" . copilot-accept-completion)
         ("<tab>" . copilot-accept-completion)
         ("C-TAB" . copilot-accept-completion-by-word)
         ("C-<tab>" . copilot-accept-completion-by-word))
  :config
  (progn
    (defun enable-copilot-mode ()
      (interactive)
      (add-hook 'prog-mode-hook #'copilot-mode))

    (defun disable-copilot-mode ()
      (interactive)
      (remove-hook 'prog-mode-hook #'copilot-mode))

    (enable-copilot-mode)))

;; Copilot Chat
;; https://github.com/chep/copilot-chat.el

;; polymode is a dependency for copilot-chat.
(use-package polymode
  :ensure t
  :defer t)

(defvar modi/copilot-chat-dir (file-name-as-directory (expand-file-name "elisp/manually-synced/copilot-chat.el"
                                                                        user-emacs-directory))
  "Directory containing copilot-chat package.")

(defvar modi/copilot-chat-autoloads-file (expand-file-name "copilot-chat-autoloads.el" modi/copilot-chat-dir)
  "Path to copilot-chat package's generated autoloads file.")

(unless (file-exists-p modi/copilot-chat-autoloads-file)
  (let ((generated-autoload-file modi/copilot-chat-autoloads-file))
    (update-directory-autoloads modi/copilot-chat-dir)))
(load-file modi/copilot-chat-autoloads-file)

(use-package copilot-chat
  :load-path modi/copilot-chat-dir
  :defer t
  :bind (:map modi-mode-map
         ("C-c C-p" . hydra-copilot/body))
  :config
  (progn

    (defhydra hydra-copilot (:color teal
                             :hint  nil)
      "
_a_sk copilot from minibuffer
_d_isplay chat buffer
_e_xplain the selected code
"
      ("a" copilot-chat-custom-prompt-mini-buffer)
      ("d" copilot-chat-display)
      ("e" copilot-chat-explain)
      ("q" nil "cancel" :color blue))

    (defun modi/copilot-chat--org-format-code(code language)
      "Format code for Org frontend with lowercase begin/end strings."
      (if language
          (format "\n#+begin_src %s\n%s\n#+end_src\n" language code)
        code))
    (advice-add 'copilot-chat--org-format-code :override #'modi/copilot-chat--org-format-code)

    (defun modi/copilot-chat--org-create-req (prompt &optional no-context)
      "Create a request with Org mode syntax reminder."
      (format "%s\n\n%s"
              prompt
              "Use only Emacs Org mode (and NOT Markdown) markup in your responses.
Use https://orgmode.org/worg/org-syntax.html as a reference for Org mode markup.

- Use https://orgmode.org/worg/org-syntax.html as a reference for Org mode markup.
- Wrap inline code or monospace formatted lines with single tilde (\"~\") characters.
  If the wrapped string itself contains the \"~\" character, wrap with the equal (\"=\") character instead.
- Wrap the bold or emphasis text with only one asterisk (\"*\") character on each side.
  Do NOT use the Markdown-style double asterisks for bold or emphasis.
- Use single forward slash characters for italics.
- Wrap text to be underlined with single underscore characters.
- Wrap strike-through text with \"+\" character.
- Headings start with the number of asterisks matching the heading level followed by a space.
  Start headings at level 3.
- Start unordered lists with \"-\" character.
- Start ordered lists like we do in Markdown.
- Use lower-cased \"#+begin_src\" and \"#+end_src\" on top and bottom of code blocks with language specification.
  So a python code block with have \"#+begin_src python\" before the first line of code and \"#+end_src\" after the last.
- In the same way use \"#+begin_quote\" and \"#+end_quote\" on top and bottom of quotes."))

    ;;     (defun modi/copilot-chat--org-create-req (prompt &optional no-context)
    ;;       "Create a request with Org mode syntax reminder."
    ;;       (format "%s\n\n%s"
    ;;               prompt
    ;;               "Use only Emacs Org mode (and NOT Markdown) markup in your responses.
    ;; Use https://orgmode.org/worg/org-syntax.html as a reference for Org mode markup."))
    (advice-add 'copilot-chat--org-create-req :override #'modi/copilot-chat--org-create-req)

    ;; (add-hook 'git-commit-setup-hook #'copilot-chat-insert-commit-message)
    (remove-hook 'git-commit-setup-hook #'copilot-chat-insert-commit-message)
    ))


(provide 'setup-copilot)
