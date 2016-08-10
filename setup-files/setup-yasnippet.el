;; Time-stamp: <2016-08-09 23:13:23 kmodi>

;; yasnippet
;; https://github.com/capitaomorte/yasnippet

(use-package yasnippet
  :if (not (bound-and-true-p disable-pkg-yasnippet))
  :defer 20
  :bind (:map modi-mode-map
         ("s-y" . hydra-yas/body)
         ("C-c y" . hydra-yas/body))
  :config
  (progn
    (setq yas-prompt-functions '(yas-ido-prompt
                                 yas-completing-prompt))

    (setq modi/yas-snippets-dir (let ((dir (concat user-emacs-directory
                                                   "snippets/")))
                                  (make-directory dir :parents)
                                  dir))

    ;; The directories listed in `yas-snippet-dirs' should contain snippet
    ;; folders only for the major modes where you are ever going to use
    ;; yasnippet.
    ;;   By default, `yas-snippet-dirs' also contains the snippets
    ;; directory that comes with the package, which contains major mode dirs
    ;; like `fundamental-mode' in which you are never going to use yasnippet!
    ;;   So the solution is to copy only the snippet folders that I am ever
    ;; going to use to `modi/yas-snippets-dir'.
    (setq yas-snippet-dirs (list 'modi/yas-snippets-dir))

    (setq yas-new-snippet-default "# -*- mode: snippet -*-
# contributor: Kaushal Modi
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}${3:
# binding: ${4:direct-keybinding}}${5:
# expand-env: ((yas-indent-line 'auto) (yas-also-auto-indent-first-line t) (yas-wrap-around-region t))}
# --
$0")

    (defconst modi/yas-minor-mode-hooks '(verilog-mode-hook
                                          emacs-lisp-mode-hook
                                          org-mode-hook)
      "List of hooks of major modes in which `yas-minor-mode' should be enabled.")

    (defun modi/turn-on-yas-minor-mode ()
      "Turn on yas-minor-mode only for specific modes."
      (interactive)
      (yas-reload-all)
      (dolist (hook modi/yas-minor-mode-hooks)
        (add-hook hook #'yas-minor-mode)))

    (defun modi/turn-off-yas-minor-mode ()
      "Turn off yas-minor-mode only for specific modes."
      (interactive)
      (dolist (hook modi/yas-minor-mode-hooks)
        (remove-hook hook #'yas-minor-mode)))

    (modi/turn-on-yas-minor-mode)

    (defun modi/yas-org-path-to-caption (text)
      "Converts a file path in TEXT to caption.
Used in org-mode/figure-caption snippet.

Example: ./img/my-figure.png → My Figure"
      (capitalize
       (replace-regexp-in-string
        "[-_]" " " (replace-regexp-in-string
                    "\\(.*/\\)*\\(.*?\\)\\(\\..*\\)*" "\\2" text))))

    (defun modi/yas-org-caption-to-name (text)
      "Converts an org CAPTION in TEXT to NAME.
Used in org-mode/figure-caption and org-mode/table-caption snippets.

Example: My Figure → my_figure"
      (replace-regexp-in-string " " "_" (downcase text)))

    (defhydra hydra-yas (:color blue
                         :hint nil)
      "
[yasnippet]        _i_nsert        _n_ew        _v_isit snippet file        _r_eload all        e_x_pand        _?_ list snippets        "
      ("i" yas-insert-snippet)
      ("n" yas-new-snippet)
      ("v" yas-visit-snippet-file)
      ("r" yas-reload-all)
      ("x" yas-expand)
      ("?" yas-describe-tables)
      ("q" nil "cancel" :color blue))))


(provide 'setup-yasnippet)
