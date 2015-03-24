# .emacs.d

## Using my emacs setup

* You can clone my emacs setup using `source git_force_update.csh ~/.emacs.d`
* Few setup files have setup catered my personal work area so it wouldn't make sense to completely copy those setups
    - setup-files/setup-smart-mode-line.el
    - setup-files/setup-registers.el
* If you don't like the initial frame size when emacs start, you would need to edit it in setup-files/setup-windows-buffers.el
* If you want to change the default theme, font size, etc, you would want to edit setup-files/setup-visual.el

## Key points

* Using my minor mode `modi-map` to enable my custom key-bindings. Doing so allows me to force override my bindings in all major modes. If I ever need to use emacs default bindings, I can simply disable my minor mode.
* Use of `use-package` in load all packages for faster load times.
* Use of `bind-keys` allows me to review my custom bindings in a single buffer by doing `M-x describe-personal-keybindings`.
* Certain packages will be loaded only if you have the associated applications installed.
  * `ag`
  * `ctags`
  * `global`, `gtags`
  * `git`
  * `matlab`
  * `aspell` or `hunspell`

## Future plan

* Pack my whole emacs setup into a single org file.

## Feedback

I am looking forward to suggestions, corrections.
Thanks!
