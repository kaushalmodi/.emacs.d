# Emacs 31 upgrade review for `~/.emacs.d`

Date: 2026-09-14. Updated 2026-09-17 after implementing sections 3.1-3.6
and the verilog-ext and verilog-mode work in 3.13 and 3.14.

## 0. Snapshot

Original state, 2026-09-14:

| Item                    | Value                                                                            |
| ----------------------- | -------------------------------------------------------------------------------- |
| Running Emacs           | 30.2, `/Applications/Emacs.app` (Homebrew cask `emacs`, emacsformacosx)          |
| Native compilation      | **Not available** in this build (`native-comp-available-p` is nil)               |
| Tree-sitter             | Available, but **zero grammars installed** and no `*-ts-mode` in config          |
| Bundled Org             | 9.7.11 (Emacs 31.1 bundles Org 9.8)                                              |
| Emacs 31.1              | Released 2026-08-24. Homebrew cask `emacs` is at 31.1, `emacs-plus@31` is stable |
| Config size             | 121 `setup-*.el` files, ~16.7k lines, 20 git submodules, 118 ELPA packages       |
| Version gates in config | 33 `>=e` gates, 31 of them for Emacs < 29 (all dead code on 30.2)                |

Current state, 2026-09-17:

| Item              | Value                                                                                                                                 |
| ----------------- | ------------------------------------------------------------------------------------------------------------------------------------- |
| Running Emacs     | 31.1, native compilation available                                                                                                    |
| Minimum supported | **30.1**, enforced by a guard in `early-init.el`; all `>=e` gates and the macro are gone                                              |
| Tree-sitter       | 11 grammars compiled, `setup-treesitter.el` routes 10 modes, safe on builds without tree-sitter                                       |
| Bundled Org       | 9.8                                                                                                                                   |
| Config size       | 110 `setup-*.el` files, 15 git submodules, 131 ELPA packages                                                                          |
| Spell check       | `jinx` via libenchant/AppleSpell, with the flyspell setup kept as fallback                                                            |
| SystemVerilog     | `verilog-ts-mode` for all Verilog extensions, `verilog-ext` for xref/capf/hierarchy/navigation, `verilog-mode` submodule at `b07a0f7` |
| Startup           | No warnings, no errors                                                                                                                |

Sections 3.1, 3.2, 3.3, 3.5 and 3.6 are done; 3.4 is partly done. See
section 3.9 for what was implemented and section 3.10 for corrections to
this document. Sections 3.11 through 3.14 record the decisions taken on
project.el, `user-lisp/`, verilog-ext and the verilog-mode submodule.

Sources: `etc/NEWS` on the `emacs-31` branch (4302 lines), `etc/NEWS.30`
(2859 lines), `etc/ORG-NEWS`, plus a read of every file in `setup-files/`.

---

## 1. Emacs 31.1 vs 30.x

### 1.1 Major user-facing features

**Startup and environment**

- `xterm-mouse-mode` is **on by default** in compatible terminals. Affects
  `emacs -nw` inside tmux: mouse drag now sets the Emacs region, not the
  terminal selection. Set `(xterm-mouse-mode -1)` to revert.
- `site-start.el` now loads **before** `early-init.el`.
- New `user-lisp/` directory under `user-emacs-directory`: recursively
  byte-compiled, autoload-scraped and added to `load-path` automatically
  (`user-lisp-auto-scrape`, `prepare-user-lisp`).
- Daemon: first client frame shows `*Warnings*` if startup produced warnings.
- New `newcomers-presets` theme with alternative defaults.

**Completion and minibuffer**

- Eager `*Completions*` display and live update: `completion-eager-display`,
  `completion-eager-update`.
- `define-completion-category` for category inheritance.
- `flex` completion style rewritten (faster, better ranking).
- `completion-styles` entries can carry variable bindings.
- `minibuffer-completion-auto-choose` now nil; `RET` chooses the candidate
  selected with `M-<up>`/`M-<down>`.
- `completion-preview-sort-function` and `completion-preview-inhibit-functions`.
- New `crm-prompt` for `completing-read-multiple`.
- `read-multiple-choice` and `map-y-or-n-p` now read from the minibuffer.

**Windows, frames, mode line, tabs**

- Window layout commands: `C-x w r <left>/<right>` rotate,
  `C-x w f <up>/<down>/<left>/<right>` flip, `C-x w t` transpose,
  `C-x w o <left>/<right>` rotate buffers, `C-x O` `other-window-backward`.
- `split-window-preferred-direction` (default `longest`) and
  `split-width-threshold` lowered 160 to 150. Use `vertical` for the old
  behavior.
- `delete-frame` now selects the most recently used frame
  (`delete-frame-choose-selected`).
- Frames have stable ids (`frame-id`, `select-frame-by-id`,
  `undelete-frame-by-id`); new `split-frame`, `merge-frames`.
- `mode-line-collapse-minor-modes` (built-in minor-mode lighter hiding),
  `mode-line-invisible-mode`, `mode-line-modes-delimiters`.
- Mode line faces have separate dark-background definitions; no more minimum
  widths in `mode-line-position`.
- Tab bar: `tab-bar-define-keys` (avoids `TAB` conflicts with outline
  folding), `split-tab`, `merge-tabs`. Tab line: move tabs with
  `C-x M-<left>/<right>`, `tab-line-exclude-buffers`.

**Editing**

- `kill-region-dwim`: `C-w` with no region kills the last word.
- `unfill-paragraph` is built in.
- `delete-trailing-whitespace-mode` (buffer-local, runs before save).
- Semantic-linefeed filling: `fill-paragraph-semlf`,
  `fill-region-as-paragraph-function`.
- Electric Pair: multi-char pairs like `("/*" . "*/")`, prefix arg pairs N
  delimiters; `electric-indent-actions`.
- `unix-word-rubout`, `unix-filename-rubout`, `center-line-mode`,
  `delete-selection-local-mode`, `mouse-shift-adjust-mode`.
- `M-s t` swaps FROM/TO in `query-replace`.
- `M-~` during `C-x s` marks a buffer unmodified without saving.
- `show-paren-not-in-comments-or-strings`.
- Unicode 17.0; many new input methods.

**Programming**

- Tree-sitter: `treesit-enabled-modes` (single switch to prefer ts modes),
  `treesit-auto-install-grammar`, `treesit-simple-indent-override-rules`,
  `treesit-language-remap-alist`, `treesit-explore`, `hs-minor-mode` and
  `show-paren-mode` support, `forward-list`/`up-list` wired to treesit.
  New `mhtml-ts-mode` (HTML with embedded JS/CSS), `go-work-ts-mode`.
- Emacs Lisp: semantic highlighting (`elisp-fontify-semantically`), Checkdoc
  batch mode, `find-function-mode` replaces `find-function-setup-keys`.
- Hideshow: `hs-cycle`, `hs-toggle-all`, fringe/margin indicators
  (`hs-show-indicators`), `hs-indentation-mode`, behavior change via
  `hs-hide-block-behavior`.
- CC mode: enums are now indented like classes (new `enum-open` etc.
  syntactic symbols); add `(enum-open . brace-list-open)` to revert.
- Python: prefers `python` over `python3`; Python 2 support off by default
  (`python-2-support`); `repeat-map` for indent shifting.
- Flymake: `flymake-show-diagnostics-at-end-of-line` accepts `fancy`,
  `flymake-indicator-type` default `auto`, origin/code in diagnostics.
- Grep: **Grep Edit mode** (`e` in `*grep*`, `C-c C-c` to apply). Xref has an
  edit mode too. This is a built-in `wgrep`.
- Project: `project-find-matching-buffer` (worktrees),
  `project-save-some-buffers` (`C-x p C-x s`), `project-list-exclude`,
  `project-prune-zombie-projects`, numbered `project-shell`.
- Etags regen: `etags-regen-create-on-completion`. Emacs no longer ships its
  own `ctags` binary (use Universal Ctags or `etags --ctags`).
- Compilation: `compilation-search-extra-path`.

**VC and Diff (the "Magit-lite" release)**

- Git worktrees under `C-x v w` (add, switch, visit-in-other-tree, delete).
- Cherry-pick / revert / delete revisions from Log View (`C`, `R`, `x`, `X`).
- Incoming/outgoing diffs and logs (`vc-root-diff-incoming`,
  `vc-root-diff-outgoing`, `C-x v T ...`, `C-x v E ...`).
- Async checkin (`vc-async-checkin`), `vc-auto-revert-mode`,
  `vc-dir-save-some-buffers-on-revert`, `log-edit-maybe-show-diff`.
- `C-x v b l` rebound: `vc-print-fileset-branch-log`; the old root branch log
  moved to `C-x v b L`.
- VC Dir: `d` deletes unregistered files, `V` runs next action on root.
- Diff mode: `diff-revert-and-kill-hunk` (`u`), `diff-delete-other-hunks`,
  region-aware apply/kill, `diff-refine-threshold`.

**Packages**

- `package-refresh-contents` runs **asynchronously**.
- `package-autosuggest-mode` (suggests packages for unknown file types),
  `package-retention-policy`, `package-review-policy`.
- Package menu shows totals and highlights.
- `package-vc-install-from-checkout` obsolete; combining use-package `:vc`
  with `:load-path` is obsolete (use `user-lisp/`).

**Other**

- Eshell: `eshell-clear`, `for` over integer ranges, `if ... else` chaining,
  `M-r`/`M-s` history isearch rebinding.
- Dired: `@` re-opens file via sudo (Tramp), `dired-check-symlinks`,
  `dired-create-empty-file-in-current-directory`, project-relative
  `dired-copy-filename-as-kill` with prefix 1.
- Ibuffer: `recency` column, `ibuffer-human-readable-size`.
- Recentf: `recentf-autosave-interval`, exclusion by extension.
- Savehist merges externally modified history files.
- New packages: `icalendar-mode`, `lua-mode` (from NonGNU ELPA),
  `system-taskbar-mode`, `system-sleep`, `timeout` library,
  `conf-npmrc-mode`, `Custom-dirlocals-mode`.
- Org 9.8 bundled (see 1.4).

### 1.2 Backward-incompatible changes

| Change                                                                                                                                 | Migration                                                  |
| -------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------- |
| `if-let`, `when-let` obsolete                                                                                                          | `if-let*`, `when-let*`, `and-let*`                         |
| All 14 `font-lock-*-face` **variables** obsolete                                                                                       | Customize the faces; quote the symbol in Lisp              |
| `FOO-ts-mode-indent-offset` renamed `FOO-ts-indent-offset` (12 modes)                                                                  | Rename in config                                           |
| `hs-special-modes-alist` obsolete                                                                                                      | Buffer-local `hs-block-start-regexp` etc. in the mode hook |
| `display-comint-buffer-action`, `display-tex-shell-buffer-action` **removed**                                                          | `(category . comint)` in `display-buffer-alist`            |
| `xterm-mouse-mode` on by default                                                                                                       | `(xterm-mouse-mode -1)` if unwanted                        |
| `site-start.el` loads before `early-init.el`                                                                                           | Check anything relying on order                            |
| `split-width-threshold` 160 to 150; `split-window-preferred-direction` `longest`                                                       | `(setopt split-window-preferred-direction 'vertical)`      |
| `process-adaptive-read-buffering` now nil                                                                                              | None needed                                                |
| CC mode enum indentation like classes                                                                                                  | `(enum-open . brace-list-open)` in `c-offsets-alist`       |
| Hideshow hides innermost block of current line                                                                                         | `hs-hide-block-behavior`                                   |
| VC `C-x v b l` rebinding; Log View `m`/`u`; VC Dir `d` deletes files                                                                   | Relearn or rebind                                          |
| Icomplete `RET` selects default again                                                                                                  | Remap `minibuffer-complete-and-exit` to `icomplete-ret`    |
| `python-interpreter` prefers `python`; Python 2 builtins unhighlighted                                                                 | Set `python-shell-interpreter` explicitly                  |
| `(require 'midnight)` no longer activates                                                                                              | `(midnight-mode 1)`                                        |
| `imenu-allow-duplicate-menu-items` default t                                                                                           | None needed                                                |
| `date-to-time` rejects `"... EDT"` zone names                                                                                          | Numeric offsets or `Z`                                     |
| `text-property-default-nonsticky` buffer-local when `setq`                                                                             | `setq-default`                                             |
| `cursor-sensor-functions` obey stickiness                                                                                              | `rear-nonsticky`                                           |
| String mutation restricted (`aset` on unibyte/multibyte)                                                                               | Build new strings                                          |
| `overlays-in`/`overlays-at` ignore narrowing again                                                                                     | None needed                                                |
| `delete-frame` needs FORCE for the daemon frame                                                                                        | None needed                                                |
| Removed libs: `cc-compat`, `info-edit`, `meese`, `otodo-mode`, `rcompile`, `sup-mouse`, `terminal`, `vi`, `vip`, `ws-mode`, `yow`      | None                                                       |
| Removed: `redisplay-dont-pause`, `load-convert-to-unibyte`, `block-comment-start/end`, `:reverse-video`, group `wp`, `advertised-undo` | None                                                       |
| Unexec dumper and Emacs's `ctags` removed                                                                                              | Universal Ctags                                            |
| Hashcash removed from Gnus/Message                                                                                                     | None                                                       |

### 1.3 Obsoleted in 31 (suggested fixes that will become mandatory)

Functions and macros: `if-let`, `when-let`, `purecopy`, `cl-member-if`
(and `cl-incf`, `cl-decf`, `cl-oddp`, `cl-evenp`, `cl-plusp`, `cl-minusp`
announced as next), `cl-locally`, `cl-declare`, `cl-gensym`, `dom-text`,
`dom-texts`, `turn-on-flyspell`, `turn-off-flyspell`, `editorconfig-apply`,
`package-vc-install-from-checkout`, `find-function-setup-keys`,
`asm-mode-set-comment-hook`, `calendar-scroll-left/right-three-months`,
`replace-region-contents` with a function, rx atom `any`, treesit query
predicates `:equal`/`:match`/`:pred` (now `:eq?`/`:match?`/`:pred?`),
`treesit-explore-mode` as entry point.

Variables: the 14 `font-lock-*-face` variables,
`byte-compile-cond-use-jump-table`, `hs-special-modes-alist`,
`minibuffer-completion-auto-choose`, `revert-buffer-in-progress-p`,
`bibtex-user-optional-fields`, `bibtex-include-OPTkey`, `message-generate-hashcash`,
`spam-use-hashcash`, `gnus-dbus-close-on-sleep`, `follow-mode-prefix`,
`vc-annotate-parent-file`, `vc-annotate-parent-rev`, `setf24`/`setb24`,
deprecated `time-stamp` conversions such as `%:y`.

Libraries: `idlwave` (moved to GNU ELPA), `vc-dav`, `package-x`, `gnus-dbus`,
`cdl`, `echistory`, `hashcash`, `kermit`, `elint`.

### 1.4 Org 9.8 (bundled with Emacs 31.1)

- Emacs 26/27 support dropped.
- `org-edit-src-content-indentation` renamed **`org-src-content-indentation`**.
- `org-let`, `org-let2` removed.
- New link preview system (`org-link-preview`, `org-link-preview-region`,
  `-clear`, `-refresh`); `C-c C-x C-v` inline-image toggle reworked.
- `:results drawer` no longer verbatim.
- New: C# babel backend, `%\*N` capture placeholder, `org-edit-keep-region`,
  `org-archive-finalize-hook`.
- `org-protocol` bookmarklets may need updating.

---

## 2. Emacs 30.1 vs 29.x

### 2.1 Major user-facing features

**Installation and startup**

- **Native compilation on by default** when libgccjit is present.
- Android port. Native JSON always available (libjansson dropped).
- Emacs is the default `org-protocol` handler on GNU/Linux.

**New built-in packages and modes**

| Package                                                                        | Replaces (third-party)                    |
| ------------------------------------------------------------------------------ | ----------------------------------------- |
| `which-key`                                                                    | MELPA `which-key`                         |
| `editorconfig-mode`                                                            | MELPA `editorconfig`                      |
| `completion-preview-mode`                                                      | Lightweight `company`/`corfu` alternative |
| `visual-wrap-prefix-mode`                                                      | ELPA `adaptive-wrap`                      |
| `etags-regen-mode`                                                             | `ctags-update`, manual TAGS regeneration  |
| `window-tool-bar-mode`                                                         |                                           |
| `minibuffer-regexp-mode`                                                       | Enabled by default                        |
| `kill-ring-deindent-mode`                                                      |                                           |
| `kmacro-menu-mode`, `list-keyboard-macros`                                     |                                           |
| `shell-command-mode`                                                           | Default for async `shell-command`         |
| Modus themes (8 variants)                                                      | MELPA `modus-themes`                      |
| PEG library, Track-Changes library, Compat stub                                |                                           |
| `elixir-ts-mode`, `heex-ts-mode`, `html-ts-mode`, `lua-ts-mode`, `php-ts-mode` |                                           |

**Tree-sitter semantics change**

- `*-ts-mode` modes are now declared submodes of the classic modes, so
  `.dir-locals.el` and YASnippet collections apply. Loading a ts mode file
  remaps the classic mode to it. Control with `major-mode-remap-alist`,
  e.g. `(add-to-list 'major-mode-remap-alist '(c-mode))` to pin classic C mode.
- Thing-based navigation (`treesit-thing-settings`), `treesit-forward-sexp`,
  `treesit-transpose-sexps`, `outline-minor-mode` in ts modes.

**Completion and minibuffer**

- `minibuffer-visible-completions` (arrow keys pick candidates from the
  minibuffer), `completion-auto-deselect`, `completions-sort` `historical`.
- `completion-category-overrides` accepts sort/annotation/group functions.
- `M-TAB` in Text mode runs `completion-at-point`
  (`text-mode-ispell-word-completion` to restore ispell).

**Editing**

- `replace-regexp-as-diff`, `multi-file-replace-regexp-as-diff`,
  `dired-do-replace-regexp-as-diff`.
- `register-use-preview` (new register preview UI).
- Keyboard macro counter/register commands under `C-x C-k C-r`.
- `duplicate-region-final-position`, `mouse-prefer-closest-glyph`.
- New `C-x 8` translations (æ, low quotes, Euro on `C-x 8 E`).

**Windows, tabs, help, customize**

- `toggle-window-dedicated` on `C-x w d`; `d`/`D` in mode line.
- `display-buffer` alist entries `some-window`, `category`,
  `post-command-select-window`.
- `mode-line-format-right-align` for right-aligned mode line content.
- Tab bar: `tab-bar-select-restore-context`, `C-TAB` in `tab-bar-mode-map`.
- Help: `help-find-source` (`C-h 4 s`), `describe-function` shows inferred
  function type, `describe-mode` outlining, `echo-keystrokes-help`.
- Customize: `customize-dirlocals`, `customize-toggle-option`,
  `safe-local-variable-directories`.
- `trusted-content` (security; `elisp-flymake-byte-compile` disabled for
  untrusted files).

**Programming**

- Project: `project-mode-line`, `project-any-command` (`C-x p o`),
  `project-prefix-or-any-command`, `project-file-history-behavior`.
- Grep: `grep-use-headings`.
- Flymake: margin indicators (`flymake-indicator-type` default `margins`),
  `flymake-show-diagnostics-at-end-of-line`.
- Eldoc no longer truncates to one line by default.
- `prog-fill-reindent-defun` on `M-q` in prog modes.
- `which-func-display` (`mode`, `header`, `mode-and-header`).
- JS: `M-.` removed from `js-mode` keymaps; `js-json-mode` no longer derives
  from `js-mode`.
- Python: `python-shell-send-block`, `python-interpreter-args`.
- CPerl: Perl 5.40, signatures fontified.
- Emacs Lisp: `emacs-lisp-docstring-fill-column` 65 to 72; many new
  byte-compiler warnings (missing `lexical-binding` cookie, empty bodies,
  ignored return values, docstring control chars).
- use-package: **`:vc` keyword** (install from git via package-vc).

**VC, Diff, Dired, Eshell, Tramp**

- VC: `vc-annotate-use-short-revision`, `C-x v l` follows renames,
  `vc-change-backend`.
- Diff: `diff-refine-nonmodified`, `diff-apply-buffer` (`C-c RET a`).
- Dired: `dired-do-open` on `E`, `dired-movement-style`,
  `dired-filename-display-length`, `dired-omit-size-limit` 300k,
  `insert-directory-program` defaults to `gls` on macOS when present.
- Eshell: batch scripts, `$@` splice, negative/range indices,
  remote/local command prefixes, builtin `compile`, `rgrep`, `env`.
- Tramp: many new methods (`run0`, `dockercp`, `podmancp`, `toolbox`,
  `distrobox`, `flatpak`, `apptainer`, `nspawn`, `androidsu`),
  `tramp-use-connection-share` replaces `tramp-use-ssh-controlmaster-options`.
- EWW: browser-like history, `eww-readable` toggles, tab completion in the
  URL prompt.
- Dictionary: `dictionary-search-interface` `help`.
- Org 9.7 bundled.

### 2.2 Backward-incompatible changes in 30

| Change                                                                                  | Migration                                                  |
| --------------------------------------------------------------------------------------- | ---------------------------------------------------------- |
| Mouse wheel events are always `wheel-up/down/left/right`                                | Rebind `mouse-4`/`mouse-5` bindings; `mouse-wheel-buttons` |
| `mouse-wheel-{up,down,left,right}-event` obsolete                                       | Same                                                       |
| Evaluated `lambda` returns an `interpreted-function` object, not a list                 | Use `aref`, `interactive-form`, `help-function-arglist`    |
| `\x` not followed by hex is a reader error                                              | Fix string literals                                        |
| A body of a single string literal is a return value, not a docstring                    | Add an explicit return                                     |
| `derived-mode-p` multi-arg convention deprecated                                        | Pass a single list                                         |
| `sort` gains keyword args; `(sort SEQ PRED)` still in place                             | None needed                                                |
| `defadvice` obsolete                                                                    | `advice-add`, `define-advice`                              |
| `easy-mmode-define-minor-mode`, `easy-mmode-define-global-mode` obsolete                | `define-minor-mode`, `define-globalized-minor-mode`        |
| `define-globalized-minor-mode` requires `run-mode-hooks` in major modes                 | None for config authors                                    |
| Obarrays are opaque; `(make-vector N nil)` pseudo-obarrays fail                         | `obarray-make`                                             |
| Hash table `:rehash-size`/`:rehash-threshold` ignored                                   | None                                                       |
| Bytecode always loaded eagerly; `byte-compile-dynamic` no effect                        | None                                                       |
| Connection-local variables override dir/file-locals in remote buffers                   | Check Tramp setups                                         |
| `completion-auto-help` and `*Completions*` behavior with `icomplete-in-buffer`          | See NEWS                                                   |
| `pixel-scroll-precision-mode` sets `make-cursor-line-fully-visible` nil                 | None                                                       |
| NSM warns about 3DES and DH < 2048 at `medium`                                          | `network-security-protocol-checks`                         |
| URL never sends email addresses; `url-personal-mail-address` obsolete                   | `url-request-extra-headers`                                |
| Regexp zero-width assertion followed by operator: operator applies to the assertion     | Fix regexps                                                |
| `subr-native-elisp-p` renamed `native-comp-function-p`                                  | Rename                                                     |
| `sleep-for` MILLISEC arg obsolete; old `sit-for` convention removed                     | Float seconds                                              |
| `x-defined-colors`, `x-color-defined-p`, `x-color-values`, `x-display-color-p` obsolete | Non-`x-` names                                             |
| `idle-update-delay` obsolete                                                            | `which-func-update-delay`                                  |
| `display-comint-buffer-action`, `display-tex-shell-buffer-action` obsolete              | `(category . comint)` (removed in 31)                      |
| `tramp-completion-reread-directory-timeout` removed                                     | `remote-file-name-inhibit-cache`                           |
| Old `derived-mode-*` helper functions removed                                           | None                                                       |
| Gnus `nnweb-type` `gmane` removed                                                       | None                                                       |
| `--with-json` configure option removed                                                  | None                                                       |

### 2.3 Obsoleted in 30

`defadvice`, `cl-old-struct-compat-mode`, `easy-mmode-define-*`,
`mouse-wheel-*-event`, `url-personal-mail-address`,
`url-gateway-broken-resolution`, `url-gateway-nslookup-*`,
`display-comint-buffer-action`, `display-tex-shell-buffer-action`,
`idle-update-delay`, `tramp-use-ssh-controlmaster-options`, Tramp connection
property `direct-async-process`, `makefile-switch-to-browser`, CPerl Info
commands and `cperl-invalid-face`, `doc-view-svg-foreground/background`,
`pp-use-max-width`, `backtrace-ellipsis`, `subr-native-elisp-p`,
`x-color-*`, `sleep-for` MILLISEC, `eshell-NAME-unload-hook`,
`eshell-process-wait-seconds/milliseconds`, multi-arg `derived-mode-p`,
direct `derived-mode-parent` property access, `dnd-handle-one-url`,
`fetch-bytecode`, `byte-compile-dynamic`, `obarray-size`,
`obarray-default-size`, `hash-table-rehash-size/threshold`.

---

## 3. Review of `~/.emacs.d`

### 3.1 Already broken today (fix regardless of upgrade)

| Location                              | Problem                                                                                                                                                    | Fix                                                                                                |
| ------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------- |
| `setup-files/setup-spell.el:81`       | `<f12>` bound to `flyspell-correct-word-generic`, which no longer exists in the installed `flyspell-correct`                                               | Bind `flyspell-correct-wrapper`; or replace the whole flyspell stack with `jinx` (see 3.4)         |
| `setup-files/setup-info.el:258`       | Advice targets `modi/Info-url-for-node`, but the defun at line 218 is `modi/Info-url-for-current-node`                                                     | Delete the override; Emacs 29+ has `Info-url-for-node` natively and Emacs 30 adds `Info-url-alist` |
| `setup-files/setup-toggles.el:13`     | Hydra key `b` calls `modi/toggle-menu-bar`, defined only in the unreachable Emacs < 25 branch of `setup-visual.el:76`                                      | Call `menu-bar-mode` directly                                                                      |
| `setup-files/setup-magit.el:50-54`    | Hydra calls `magit-branch-popup`, `magit-rebase-popup`, `magit-fetch-popup`, `magit-push-popup`, `magit-pull-popup`, removed when Magit moved to transient | Use `magit-branch`, `magit-rebase`, `magit-fetch`, `magit-push`, `magit-pull`                      |
| `setup-files/setup-org.el:127-129`    | `org-modules` set to `'(org-info)`; the module is `ol-info` since Org 9.4                                                                                  | `(setq org-modules '(ol-info))` and drop the `modi/org-version-select` branch                      |
| `setup-files/setup-weather.el:22-46`  | `forecast` uses the Dark Sky API, shut down 2023                                                                                                           | Remove `forecast` (keep `sunshine` or drop weather entirely)                                       |
| `setup-files/setup-mastodon.el:17`    | Hardcoded `mastodon.technology`, shut down 2022                                                                                                            | Update instance or remove                                                                          |
| `setup-files/setup-org.el:1167`       | `org-reveal-root` points at `cdn.rawgit.com`, shut down 2019                                                                                               | Use `https://cdn.jsdelivr.net/npm/reveal.js`                                                       |
| `setup-files/setup-org.el:1045,1169`  | MathJax 2.7.0 CDN (end of life)                                                                                                                            | Org 9.7+ default MathJax 3 path; delete the override                                               |
| `setup-files/setup-launcher.el:42`    | Calls `sx-tab-all-questions` but `sx` is not installed                                                                                                     | Remove entry                                                                                       |
| `setup-files/setup-search.el:142-149` | `remove-if`, `remove-if-not`, `some` are un-prefixed `cl` names that only work if some other package happens to load deprecated `cl.el`                    | `cl-remove-if`, `cl-remove-if-not`, `cl-some` (or `seq-remove`, `seq-some`)                        |
| `setup-files/setup-compile.el:62`     | `lexical-let` (from deprecated `cl.el`)                                                                                                                    | Plain `let`; the file already has `lexical-binding: t`                                             |
| `init.el:48-153` `my-packages`        | Lists `use-package` and `which-key`, both bundled since Emacs 29/30. The MELPA copies shadow the bundled ones                                              | Remove both from the list                                                                          |
| `setup-files/setup-web-mode.el:12`    | `web-mode` loaded from submodule `elisp/web-mode` while `web-mode-20250827` is also installed in `elpa_30`                                                 | Keep one; MELPA version is newer                                                                   |

### 3.2 Obsolete APIs in use (fixes that will become mandatory)

| Location                                                        | Obsolete since | Symbol                                | Replacement                                                                                                                                                                                             |
| --------------------------------------------------------------- | -------------- | ------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `init.el:354,370`                                               | 27.1           | `focus-in-hook`                       | `after-focus-change-function` (still runs today; `setup-font-check` depends on it)                                                                                                                      |
| `setup-files/setup-org.el:529-548`                              | 29.1           | `point-at-bol` (4 uses)               | `pos-bol` or `line-beginning-position`. Better: the whole block (lines 499-562) resurrects easy templates removed in Org 9.2; use `org-insert-structure-template` (`C-c C-,`) or `(require 'org-tempo)` |
| `setup-files/setup-verilog.el:437,440`                          | 25.1           | `find-tag`, `pop-tag-mark`            | `xref-find-definitions`, `xref-go-back`                                                                                                                                                                 |
| `setup-files/setup-verilog.el:615`                              | **31.1**       | `hs-special-modes-alist`              | In `verilog-mode-hook`: `(setq-local hs-block-start-regexp ... hs-block-end-regexp ... hs-forward-sexp-function ...)`                                                                                   |
| `setup-files/setup-spell.el:65-74`                              | **31.1**       | `turn-on-flyspell` (hooks and advice) | `flyspell-mode`; advise `flyspell-mode` instead                                                                                                                                                         |
| `setup-files/setup-hugo.el:21`                                  | 29.1           | `update-directory-autoloads`          | `loaddefs-generate`                                                                                                                                                                                     |
| `setup-files/setup-term.el:52`                                  | 24.1           | `comint-dynamic-complete`             | `completion-at-point`                                                                                                                                                                                   |
| `setup-files/setup-bookmarks.el:50`                             | 22             | `find-file-hooks`                     | `find-file-hook`                                                                                                                                                                                        |
| `setup-files/setup-editing.el:367`                              | 25             | `preceding-sexp`                      | `elisp--preceding-sexp`                                                                                                                                                                                 |
| `setup-files/setup-editing.el:1179`                             | 31 (announced) | `cl-incf`                             | `incf` (built in as of 31)                                                                                                                                                                              |
| `setup-files/setup-misc.el:6`                                   | 28 idiom       | `(fset 'yes-or-no-p 'y-or-n-p)`       | `(setq use-short-answers t)`. The `fset` also silently changes `yes-or-no-p` callers such as `setup-windows-buffers.el:199`                                                                             |
| `setup-files/setup-org.el:1248`                                 | Org 9.8        | `org-edit-src-content-indentation`    | `org-src-content-indentation`                                                                                                                                                                           |
| `setup-files/setup-org.el:155-158`                              | Org 9.5        | `org-speed-commands-user` branch      | Drop the `version<` branch; keep `org-speed-commands`                                                                                                                                                   |
| `elisp/misc/fontawesome-choose.el`, `elisp/profile-dotemacs.el` | 31 warns       | Missing `lexical-binding` cookie      | Add `;; -*- lexical-binding: t; -*-`                                                                                                                                                                    |

Verified clean: no `if-let`/`when-let` (all uses are already starred), no
`defadvice`, no unquoted `font-lock-*-face` variables, no multi-arg
`derived-mode-p`, no `mouse-wheel-*-event`, no `*-ts-mode-indent-offset`,
no `display-comint-buffer-action`.

### 3.3 Installed packages superseded by Emacs built-ins

| Package (file)                                                                                                                    | Built-in replacement                                | Notes                                                                                                                                                                                                                      |
| --------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `which-key` (`setup-which-key.el`)                                                                                                | `which-key` (Emacs 30)                              | Config is compatible as is. Remove from `my-packages`                                                                                                                                                                      |
| `use-package`, `bind-key` (`init.el`)                                                                                             | Built in since 29                                   | Remove from `my-packages`; `use-package-chords` stays on MELPA                                                                                                                                                             |
| `fill-column-indicator` (`setup-fci.el`, plus `setup-visual.el:151-176`, `setup-navigation.el:296-322`, `setup-htmlize.el:15-29`) | `display-fill-column-indicator-mode` (27)           | Delete the file and all the popup/shell/htmlize workarounds; theme via face `fill-column-indicator`                                                                                                                        |
| `linum`, `nlinum` submodule (`setup-linum.el`)                                                                                    | `display-line-numbers-mode` (26)                    | You already default to native. Shrink the file to a hook list plus `display-line-numbers-type`; delete `elisp/nlinum`                                                                                                      |
| `adaptive-wrap` (`setup-visual.el:325-334`, `setup-toggles.el:12`)                                                                | `visual-wrap-prefix-mode` (30)                      | Same package, renamed. `adaptive-wrap-extra-indent` is now `visual-wrap-extra-indent`                                                                                                                                      |
| `unfill` submodule (`setup-editing.el:906`)                                                                                       | `unfill-paragraph` (31)                             | Built-in has no `unfill-region`/toggle; keep submodule only if you use those                                                                                                                                               |
| `ctags-update`, `etags-select`, `etags-table` (`setup-tags.el`)                                                                   | `etags-regen-mode` (30) + `xref`                    | `etags-regen-mode` regenerates TAGS per project automatically; set `etags-regen-program` to Universal Ctags with `--output-format=etags`. Unbinding `M-.` from `modi/find-tag` restores `xref-find-definitions` everywhere |
| `flycheck` (`setup-flycheck.el`)                                                                                                  | Flymake (`python-flymake`, `sh-shellcheck-flymake`) | Only Python and sh are enabled; both have built-in Flymake backends. Emacs 31 Flymake shows diagnostics at end of line (`fancy`)                                                                                           |
| `paradox` (`setup-paradox.el`, `setup-launcher.el`, `setup-projectile.el:155`)                                                    | Package menu (29+) and `package-upgrade-all`        | Paradox is abandoned. `my/package-upgrade-packages` in `setup-packages.el:144` is `package-upgrade-all`                                                                                                                    |
| `rich-minority` (`setup-mode-line.el:146-198`)                                                                                    | `mode-line-collapse-minor-modes` (31)               | Covers the blacklist. The unicode lighter renames have no built-in                                                                                                                                                         |
| `emojify` (`setup-mastodon.el`)                                                                                                   | `emoji` (29) with a color emoji font                | Mastodon.el works with built-in emoji                                                                                                                                                                                      |
| `dired-single` (`setup-dired.el:14`)                                                                                              | `dired-kill-when-opening-new-dired-buffer` (28)     |                                                                                                                                                                                                                            |
| `hl-line+` `hl-line-flash` (`setup-highlight.el:151`, `setup-launcher.el:25`)                                                     | `pulse-momentary-highlight-one-line`                | Emacs 31 adds `pulse-face-duration`, `pulse-faces`                                                                                                                                                                         |
| `wordnut` (`setup-wordnut.el`)                                                                                                    | `dictionary` (28)                                   | Optional; dictionary uses a dict server, not WordNet                                                                                                                                                                       |
| `ansi-color-apply-on-region` on `compilation-filter-hook` (`setup-compile.el:12-16`)                                              | `ansi-color-compilation-filter` (28)                |                                                                                                                                                                                                                            |
| `duplicate-current-line-or-region` (`setup-editing.el:164`)                                                                       | `duplicate-dwim` (29)                               |                                                                                                                                                                                                                            |
| `rename-current-buffer-file` (`setup-windows-buffers.el:206`)                                                                     | `rename-visited-file` (29)                          |                                                                                                                                                                                                                            |
| `modi/switch-to-scratch-and-back` (`setup-windows-buffers.el:310`)                                                                | `scratch-buffer` (29)                               | Only for the plain scratch case                                                                                                                                                                                            |
| `modi/delete-trailing-whitespace-buffer` (`setup-editing.el:252`)                                                                 | `delete-trailing-whitespace-mode` (31)              | Yours special-cases Org headings; keep if that matters                                                                                                                                                                     |
| `find-library-include-other-files` backport (`setup-search.el:158-212`)                                                           | Native in 29                                        | Delete (~55 lines)                                                                                                                                                                                                         |
| `kill-current-buffer` polyfill (`setup-windows-buffers.el:507`)                                                                   | Native since 26                                     | Delete                                                                                                                                                                                                                     |
| `sh-set-shell` copy (`setup-shell.el:42-149`)                                                                                     | Native since 28                                     | Delete (~107 lines)                                                                                                                                                                                                        |
| `modi/ffap-string-at-point` (`setup-navigation.el:138-243`)                                                                       | Fixed in 26.1                                       | Delete (~105 lines)                                                                                                                                                                                                        |
| `elisp-slime-nav` (`setup-elisp-slime-nav.el`)                                                                                    | `xref` `M-.`                                        | File is never loaded; delete it and the package                                                                                                                                                                            |
| `all`, `all-ext`                                                                                                                  | `occur-edit-mode` (`e` in `*Occur*`)                | Optional                                                                                                                                                                                                                   |
| `wgrep`, `wgrep-ag`                                                                                                               | Grep Edit mode (31, `e` in `*grep*`)                | Only if you move from `ag`/`counsel-rg` to built-in `grep`/`rgrep`                                                                                                                                                         |

### 3.4 Installed packages with better community successors

| Current                                                                                                                  | Successor                                                                                                                             | Why                                                                                                                                                                                                                              |
| ------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `ivy` + `counsel` + `swiper` + `ivy-rich` + `ivy-hydra` + `flyspell-correct-ivy`                                         | `vertico` + `consult` + `marginalia` + `orderless` + `embark`                                                                         | You already run `corfu` + `cape` from the same author; the minad stack uses the standard `completing-read` API so Emacs 30/31 completion improvements apply. Ivy is still maintained, so this is optional                        |
| `flyspell` + `flyspell-correct-ivy` + `ispell` config (`setup-spell.el`)                                                 | `jinx`                                                                                                                                | **Done.** Enchant-based, checks all visible text asynchronously. The flyspell setup is kept behind `modi/jinx-available-p` for machines without a compiler or libenchant                                                         |
| `projectile` + `ibuffer-projectile` (`setup-projectile.el`, 256 lines)                                                   | `project.el` + `ibuffer-project`                                                                                                      | **Decided against; keeping projectile.** See section 3.11 for the benchmark and the feature-by-feature comparison                                                                                                                |
| `ggtags` + GNU Global (`setup-tags.el`)                                                                                  | `eglot` + a SystemVerilog LSP (`verible-verilog-ls` or `svls`), or `citre` for ctags                                                  | Eglot is built in since 29; xref/imenu/eldoc for free                                                                                                                                                                            |
| `verilog-mode` git submodule                                                                                             | `verilog-ts-mode` + `verilog-ext` (MELPA, gmlarumbe)                                                                                  | **Partly done.** `verilog-ts-mode` handles all Verilog extensions; `verilog-ext` adds xref, capf, hierarchy, navigation, typedefs and ports. Its other features stay off -- see section 3.13                                     |
| `hydra` (29 `defhydra` forms)                                                                                            | `transient` (built in since 28) and `repeat-mode` (28)                                                                                | Hydra is in maintenance mode. `repeat-mode` with `defvar-keymap :repeat t` replaces the "sticky" hydras (window resize, font size, nav); Emacs 31 adds `repeat-continue`. Transient for menu hydras (projectile, magit, toggles) |
| `smart-mode-line` + `rich-minority`                                                                                      | `doom-modeline` or `mood-line`, or plain mode line with `mode-line-format-right-align` (30) and `mode-line-collapse-minor-modes` (31) | Both current packages are effectively unmaintained                                                                                                                                                                               |
| `multi-term`                                                                                                             | `eat` (GNU ELPA) or `vterm`                                                                                                           | multi-term is abandoned                                                                                                                                                                                                          |
| `indent-guide`                                                                                                           | `indent-bars`                                                                                                                         | Tree-sitter aware, faster                                                                                                                                                                                                        |
| `gist` + `gh`                                                                                                            | `igist`                                                                                                                               | `gist.el` cannot authenticate with the current GitHub API                                                                                                                                                                        |
| `deft` + `notdeft` + `denote`                                                                                            | `denote` only                                                                                                                         | Three overlapping note systems; you already have denote 4.x installed                                                                                                                                                            |
| `shackle`                                                                                                                | `display-buffer-alist` directly                                                                                                       | Emacs 30 added `category`, `some-window`, `post-command-select-window` entries; shackle is unmaintained but still works                                                                                                          |
| `yaml-mode`                                                                                                              | `yaml-ts-mode` (29)                                                                                                                   | Also fix `setup-yaml-mode.el` which only matches `.yml`, not `.yaml`                                                                                                                                                             |
| `go-mode`                                                                                                                | `go-ts-mode` (29), `go-work-ts-mode` (31)                                                                                             | Emacs 31 adds test-at-point commands                                                                                                                                                                                             |
| `python` (classic)                                                                                                       | `python-ts-mode`                                                                                                                      |                                                                                                                                                                                                                                  |
| `cc-mode` for `.pss/.psf`                                                                                                | `c-ts-mode`                                                                                                                           | Optional; note 31's enum indentation change in classic CC mode                                                                                                                                                                   |
| `sh-script`                                                                                                              | `bash-ts-mode`                                                                                                                        |                                                                                                                                                                                                                                  |
| `web-mode` for Hugo templates                                                                                            | Keep `web-mode`                                                                                                                       | `mhtml-ts-mode` (31) has no Go-template engine support                                                                                                                                                                           |
| `markdown-mode`                                                                                                          | Keep                                                                                                                                  | `markdown-ts-mode` is still weaker                                                                                                                                                                                               |
| `auto-highlight-symbol`                                                                                                  | `symbol-overlay`                                                                                                                      | Optional                                                                                                                                                                                                                         |
| `outshine` + `outorg` + `navi-mode`                                                                                      | `outline-minor-mode` with `outline-minor-mode-cycle`, `outline-minor-mode-highlight` (28+)                                            | Outshine is unmaintained since 2020; built-in outline now folds in ts modes (30)                                                                                                                                                 |
| `iy-go-to-char` (manually synced)                                                                                        | `zop-to-char` (already installed) or `avy`                                                                                            |                                                                                                                                                                                                                                  |
| `csv-nav` (manually synced)                                                                                              | `csv-mode` (GNU ELPA)                                                                                                                 |                                                                                                                                                                                                                                  |
| `dired+`, `info+`, `hl-line+`, `header2`, `hideshowvis`, `etags-*`, `spice-mode` (`elisp/manually-synced/`)              | Built-ins or drop                                                                                                                     | EmacsWiki-era code that no longer receives fixes. Emacs 31 hideshow has fringe indicators (`hs-show-indicators`), replacing `hideshowvis`                                                                                        |
| `org-contrib` submodule (only for `org-eldoc`)                                                                           | NonGNU ELPA `org-contrib`, or drop                                                                                                    |                                                                                                                                                                                                                                  |
| `ag`, `ag.el`                                                                                                            | Drop; you already have `rg`, `deadgrep`, `counsel-rg`                                                                                 | Five grep front-ends are configured; `ag` is unmaintained                                                                                                                                                                        |
| Git submodules for `ox-hugo`, `tomelr`, `git-link`, `ox-reveal`, `zenburn-emacs`, `devdocs-lookup`, `unfill`, `web-mode` | `use-package :vc` (30) or plain ELPA/MELPA installs                                                                                   | `ox-hugo` is on MELPA, `tomelr` on GNU ELPA, `git-link`/`zenburn-theme`/`devdocs` on MELPA. `ox-reveal` was removed outright. For `user-lisp/`, see section 3.12 -- moving `elisp/` there was considered and declined            |

### 3.5 Dead code to delete

- All 31 `>=e` gates for versions below 29.1, with their unreachable ELSE
  branches: `init.el:311`; `setup-packages.el:14,24,108`;
  `setup-editing.el:134,144`; `setup-visual.el:64,96,282,517`;
  `setup-info.el:10`; `setup-misc.el:20,123`; `setup-ido.el:147`;
  `setup-desktop.el:37`; `setup-server.el:20`; `setup-navigation.el:9,138`;
  `setup-mouse.el:21`; `setup-mode-line.el:36`; `setup-diff.el:173`;
  `setup-windows-buffers.el:507`; `setup-eww.el:131,172`;
  `setup-linum.el:12,188,195,200,205`; `setup-shell.el:42`.
  Then delete the `>=e` macro itself from `general.el:18` and update the
  README minimum to Emacs 29.1 or 30.1.
- `(version< emacs-version "29.1")` block in `setup-search.el:158-212`.
- `(version< (org-version) ...)` branches in `setup-org.el:155,1488`.
- `fboundp` guards for `menu-bar-mode`, `tool-bar-mode`, `scroll-bar-mode`
  in `setup-visual.el:60,94,101`; `org-fold-*` fallbacks in
  `setup-hugo.el:124-129`.
- Never-loaded files: `setup-elfeed.el`, `setup-guide-key.el`,
  `setup-keyfreq.el`, `setup-symon.el`, `setup-undo-tree.el`,
  `setup-elisp-slime-nav.el`, and the `with-eval-after-load 'undo-tree`
  blocks in `setup-fold.el:226-230`.
- `setup-image.el`: body is entirely commented out.
- `setup-global-text-scale-compat.el`: never loaded on 29+; the only live
  consumer is the unreachable ELSE branch in `setup-pragmata-ligatures.el:20`.
- `setup-desktop.el:68-71`: four `ido-*` history variables saved while ido
  is disabled.
- `(require 'subr-x)` in `init.el:156`.
- `setup-yasnippet.el:15`: `yas-ido-prompt` listed first while ido is off.
- `setup-spell.el:62-63`: `ac-flyspell-workaround` for `auto-complete`,
  which is not installed.

### 3.6 Better default settings for Emacs 30/31

Add to a new `setup-defaults.el` (or `setup-misc.el`):

```elisp
;; Prompts
(setq use-short-answers t)                 ; replaces (fset 'yes-or-no-p 'y-or-n-p)

;; Large files: EDA logs opened in text-mode benefit from this
(global-so-long-mode 1)

;; Minibuffer history survives restarts; replaces desktop-globals-to-save history list
(savehist-mode 1)

;; Paren context for Verilog end/endmodule when the opener is off screen (29)
(setq show-paren-context-when-offscreen 'overlay)

;; Smooth trackpad scrolling on macOS (29); replaces (setq scroll-step 1)
(pixel-scroll-precision-mode 1)

;; Search and grep
(setq xref-search-program 'ripgrep)        ; 28
(setq grep-use-headings t)                 ; 30
(setq kill-do-not-save-duplicates t)       ; 27

;; Dired
(setq dired-kill-when-opening-new-dired-buffer t)  ; 28, replaces dired-single
(setq dired-mouse-drag-files t)                    ; 29

;; Mode line (31)
(setq mode-line-collapse-minor-modes t)    ; replaces rich-minority blacklist

;; Emacs 31 changed these defaults; set explicitly if you want the old behavior
(setq split-window-preferred-direction 'vertical)  ; 31 default is 'longest
(xterm-mouse-mode -1)                              ; 31 turns it on in terminals

;; Tree-sitter (31)
(setq treesit-auto-install-grammar 'always)
(setq treesit-enabled-modes '(yaml-ts-mode go-ts-mode python-ts-mode bash-ts-mode
                               json-ts-mode toml-ts-mode c-ts-mode c++-ts-mode))

;; Native compilation (once on a native-comp build)
(setq native-comp-async-report-warnings-errors 'silent)
(setq package-native-compile t)

;; Flymake instead of flycheck
(add-hook 'python-mode-hook #'flymake-mode)
(add-hook 'python-ts-mode-hook #'flymake-mode)
(add-hook 'sh-mode-hook #'flymake-mode)
(add-hook 'bash-ts-mode-hook #'flymake-mode)
(setq flymake-show-diagnostics-at-end-of-line 'fancy)  ; 31

;; TAGS without ctags-update (30)
(setq etags-regen-program "ctags"
      etags-regen-program-options '("--output-format=etags"))
(etags-regen-mode 1)
```

Settings to change:

- `setup-packages.el:12` `(setq package-check-signature nil)`: restore the
  default `allow-unsigned`. The 2024 GNU ELPA failure was the expired signing
  key; `M-x package-install RET gnu-elpa-keyring-update` fixes it properly.
- `setup-visual.el:40` `(setq tooltip-mode nil)` does nothing; use
  `(tooltip-mode -1)`.
- `setup-visual.el:38` add `inhibit-startup-screen`,
  `initial-scratch-message nil`.
- `setup-completion.el:45` `tab-always-indent 'complete` is deliberate; keep,
  but note Emacs 30 changed TAB in Text mode too.
- `setup-org.el:1248` rename to `org-src-content-indentation` (Org 9.8).
- `setup-yaml-mode.el`: add `\\.yaml\\'` or switch to `yaml-ts-mode`.
- `early-init.el`: move `menu-bar-mode`/`tool-bar-mode`/`scroll-bar-mode`
  there as `default-frame-alist` entries (`(menu-bar-lines . 0)`,
  `(tool-bar-lines . 0)`, `(vertical-scroll-bars)`) and set
  `frame-inhibit-implied-resize t`, so the frame never flashes at startup.
- `init.el:7-8,378-379` GC hack: harmless; `gcmh` is the maintained version if
  you want to keep it.

### 3.7 Upgrade path to Emacs 31.1 on this Mac

1. Install a native-comp build. Your current emacsformacosx 30.2 has no
   native compilation, so you are leaving performance on the table:

   ```bash
   brew tap d12frosted/emacs-plus && brew install emacs-plus@31
   ```

   emacs-plus@31 enables native-comp and tree-sitter by default. Then repoint
   `/usr/local/bin/emacs` (currently a symlink into `/Applications/Emacs.app`).
2. Before the first launch, prune `my-packages` in `init.el`: remove
   `use-package`, `which-key`, `fill-column-indicator`, `paradox`, `flycheck`,
   `flyspell-correct-ivy`, `ag`, `wgrep-ag`, `gist`, `forecast`, `multi-term`,
   `all`, `all-ext`, `ggtags`, `ctags-update`, `fuzzy`, `emojify`. The new
   `elpa_31` directory starts empty and reinstalls everything else.
3. Restore `package-check-signature`. Expect `package-refresh-contents` to be
   asynchronous in 31.
4. Fix the 3.1 and 3.2 items. Byte-compile the config once
   (`emacs --batch -f batch-byte-compile setup-files/*.el`) to surface the new
   31 warnings.
5. Install tree-sitter grammars (`treesit-auto-install-grammar` above) and
   turn on `treesit-enabled-modes` for the languages you edit.
6. Check Org 9.8 renames (`org-src-content-indentation`), the reveal.js CDN,
   and MathJax path.
7. Verify `emacs -nw` in tmux: `xterm-mouse-mode` is on by default; disable
   if it fights tmux mouse mode.
8. Optional but high value for SystemVerilog work: add `verilog-ext` and
   `verilog-ts-mode`, and Eglot with `verible-verilog-ls`. Both packages
   are now in place; see section 3.13. Eglot is still not set up because no
   LSP server is installed.

### 3.8 Suggested order of work

| Priority | Work                                                                                                                             | Effort  | Status                                                                                                               |
| -------- | -------------------------------------------------------------------------------------------------------------------------------- | ------- | -------------------------------------------------------------------------------------------------------------------- |
| 1        | Fix broken bindings and dead URLs (3.1)                                                                                          | 1 hour  | Done                                                                                                                 |
| 2        | Remove `use-package`/`which-key` from `my-packages`; restore signature check                                                     | 10 min  | Done                                                                                                                 |
| 3        | Replace obsolete APIs (3.2), especially `hs-special-modes-alist`, `turn-on-flyspell`, `point-at-bol`, `focus-in-hook`            | 1 hour  | Done                                                                                                                 |
| 4        | Delete dead version gates and never-loaded files (3.5), drop `>=e`                                                               | 2 hours | Done                                                                                                                 |
| 5        | Delete `setup-fci.el`, shrink `setup-linum.el`, switch `adaptive-wrap` to `visual-wrap-prefix-mode`                              | 1 hour  | Done                                                                                                                 |
| 6        | Flycheck to Flymake; `ctags-update` to `etags-regen-mode`; drop `paradox`                                                        | 1 hour  | Done                                                                                                                 |
| 7        | Adopt the 3.6 defaults; move frame settings to `early-init.el`                                                                   | 30 min  | Done                                                                                                                 |
| 8        | Switch to emacs-plus@31 with native-comp; install grammars                                                                       | 1 hour  | Done                                                                                                                 |
| 9        | Larger migrations, one at a time: `jinx`, `verilog-ext`, `vertico`/`consult`, `project.el`, `transient`/`repeat-mode` for hydras | Weeks   | `jinx` and `verilog-ext` done; `project.el` declined (3.11); `vertico`/`consult` and the hydra migration not started |

### 3.9 What was implemented

Sections 3.1, 3.2, 3.3, 3.5 and 3.6 are complete, plus the `jinx` and
tree-sitter items from 3.4. Highlights beyond the plan:

- `setup-treesitter.el` added: 11 grammars, `treesit-enabled-modes` on 31
  with a `major-mode-remap-alist` fallback for 30, and everything gated on
  `treesit-available-p` so the config still works on a build without
  tree-sitter.
- Minimum version raised to 30.1 with a guard in `early-init.el`, all 26
  `>=e` call sites unwrapped and the macro deleted.
- Frame bars moved to `default-frame-alist` in `early-init.el` to stop the
  startup flash. Note that `frame-inhibit-implied-resize t` makes the three
  former `add-to-list` calls on that variable signal an error, so they had
  to go.
- `package-selected-packages` is now synced from `my-packages` plus the
  `:ensure` packages, so `package-autoremove` no longer offers to delete
  what the config just installed. This has to go through
  `customize-set-variable`: the value in `custom-file` belongs to the
  `user` theme, and every `enable-theme` call would otherwise restore it.

Bugs found that were not in the original review, all silent failures
rather than errors:

| Bug                                             | Effect                                                                                                                                           |
| ----------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| `.git` moved to `projectile-project-root-files` | Top-down root detection requires a *file*, so **no git repo was detected at all**; buffers under `.git/` were recorded as projects instead       |
| `ivy-mode` enabled in `:config`                 | Nothing loaded ivy at startup, so the first completing command of a session got the plain minibuffer                                             |
| `projectile-mode` enabled in `:config`          | The `find-file-hook` that records known projects was not installed until the first `C-c f`                                                       |
| `ace-window-display-mode` enabled in `:config`  | The mode-line window key was absent until the first `C-x o` -- exactly when it is needed                                                         |
| `(setq tooltip-mode nil)`                       | Assigning the minor-mode variable never ran the mode function, so `show-help-function` stayed at `tooltip-show-help` and tooltips kept appearing |
| `projectile-completion-system 'ivy`             | Current projectile honors that variable only when it is a function; the symbol was silently ignored                                              |
| No aspell or hunspell installed                 | `ispell-program-name` was nil, so flyspell was suppressed by its own guard and spell check had not worked in a long time                         |
| `jinx-languages` from the locale                | `current-locale-environment` of `"C"` yields a `"C"` dictionary that does not exist, so `<f12>` failed with "Invalid dictionary"                 |
| `show-paren-mode` called in two files           | Harmless duplicate, consolidated                                                                                                                 |
| Three bugs in the `nim-emacs-module` submodule  | Obsolete `when-let`, two 0-argument `file-name-base` calls, and a missing `cl-lib` require broke two of its three commands                       |

### 3.10 Corrections to this document

- Section 3.5 listed a `(version< emacs-version "29.1")` block in
  `setup-search.el:158-212`. It had already been removed during the 3.2
  work, so there was nothing to do.
- Section 3.6 suggested `(setq package-check-signature nil)` should be
  restored to the default by installing `gnu-elpa-keyring-update`. Not
  needed: the keyring already carried the 2024-10-22 replacement subkey,
  valid through 2034, so deleting the workaround was enough. Verified by
  installing a GNU ELPA package with checking enabled.
- Section 3.4's `user-lisp/` note named a function `user-lisp-auto-scrape`.
  That is a `defcustom`; the function is `prepare-user-lisp`.
- Section 3.6's `mode-line-collapse-minor-modes` suggestion does not apply
  while smart-mode-line is in use: it renders the lighter area through
  rich-minority, so `mode-line-modes` never consults that option. The
  setting was removed again.

### 3.11 project.el vs projectile: decided to keep projectile

Every customization in `setup-projectile.el` has a project.el equivalent:
`project-name`, `project-files` and `project-ignores` are all
`cl-defgeneric`, so the `/proj/<x>/` naming rule and the rg file listing
port as methods rather than advice. Submodules as separate projects work
through `project-vc-merge-submodules nil`, and `project-switch-commands`
reproduces the hydra's command set with custom keys.

What decided it:

- **No file-list caching.** project.el caches only `project-vc` and
  `project-vc-dir-locals`, in memory, with a 2 second interactive timeout.
  `project-files` calls the VC backend every time. Caching the file list is
  the first item in project.el's own TODO, deferred pending a filenotify
  design because "manual cache invalidation is not nice" -- which is
  exactly the `projectile-enable-caching 'persistent` plus manual
  invalidation setup in use here.
- Benchmarked on `~/.emacs.d` (6221 files), fresh process per call:
  projectile 0.007s, project.el 0.032s. Both imperceptible, so performance
  is **not** an argument either way on a repo this size. It would matter on
  a large EDA tree.
- `projectile-find-file-dwim`, `projectile-recentf` and
  `projectile-switch-open-project` have no equivalents.
- Projectile is referenced from seven other setup files, including
  `sml/use-projectile-p` in `setup-mode-line.el`, which has no project.el
  path, and `ibuffer-projectile`.

project.el is already active alongside projectile via
`project-find-functions`, so xref, Eglot, Flymake, vc-dir and compile
already resolve projects correctly.

### 3.12 `user-lisp/`: considered, not adopted

Emacs 31's `user-lisp-directory` works as documented -- verified that it
adds the tree recursively to `load-path`, scrapes autoload cookies and
byte-compiles lazily. The startup hook is gated on `init-file-user`, so it
does nothing under `--batch`.

Moving `elisp/` there was declined because that directory is two different
things. Eleven of its subdirectories are git submodules of upstream
projects; moving them means rewriting `.gitmodules` and re-cloning, and
auto-byte-compiling code that changes under `git submodule update` invites
breakage. The good candidates are the ~17 single-file libraries that are
genuinely local (`modi-mode.el`, `temp-mode.el`, `csh-mode`, `de-ansify`,
`insert-week`, the `org-include-*` exporters, `patches/`), which would lose
their explicit `:load-path` clauses and gain autoloading. That remains
available if the `:load-path` boilerplate becomes annoying.

### 3.13 verilog-ext: six features enabled

`verilog-ext-feature-list` is set to `xref`, `capf`, `hierarchy`,
`navigation`, `typedefs` and `ports`. `hierarchy` picks the tree-sitter
backend because the systemverilog grammar is installed, so Verilog-Perl
`vhier` is not needed.

Replaced, because the verilog-ext versions are better:

| Was                                              | Now                                       | Why                                                                                                                                                                                            |
| ------------------------------------------------ | ----------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `modi/verilog-jump-to-module-at-point`           | `verilog-ext-jump-to-module-at-point-def` | The custom one needed `ctags`, a pre-generated TAGS file and projectile. This one finds the instance through the tree-sitter parse and resolves it with xref, and has a references counterpart |
| `modi/verilog-find-parent-module`                | `verilog-ext-jump-to-parent-module`       | verilog-ext credits this config for the PCRE it uses, so it is the same search, maintained upstream                                                                                            |
| `modi/verilog-block-end-comments-to-block-names` | `verilog-ext-block-end-comments-to-names` | Same function upstreamed. Identical output on all four cases tested; it checks the captured name against `verilog-keywords` with `member` rather than a regexp that could partial-match        |

The chords carry over: `\\` jumps to the definition, `^^` to the parent
module, and `||` is new for references.

`modi/verilog-find-module-instance` is kept: which-func and
`modi/verilog-jump-to-header-dwim` both use it, and the verilog-ext
`which-func` feature is not enabled. Its regexp did **not** match a plain
`core u_core (.clk(clk));` instance, where the tree-sitter version returns
`("core" "u_core")`, so prefer the verilog-ext navigation commands.

Left off, with reasons, because this is the part that is easy to get wrong:

| Feature                               | Why not                                                                                                                                                                                                          |
| ------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `font-lock`                           | Adds keywords to `verilog-mode` only, and every Verilog extension here opens in `verilog-ts-mode`. Measured on one buffer: `verilog-mode` goes from 61 to 70 fontified characters, `verilog-ts-mode` stays at 70 |
| `hideshow`                            | Registers through `hs-special-modes-alist`, obsolete as of 31.1. The hideshow setup in `setup-verilog.el` uses the buffer-local variables instead                                                                |
| `imenu`                               | No outshine support; the custom one adds `// * Heading` levels                                                                                                                                                   |
| `which-func`                          | Does not feed `modi/verilog-which-func-xtra` into the mode line format used here                                                                                                                                 |
| `compilation`                         | Project-level and needs `:compile-cmd`; `modi/verilog-compile` is per-file with a `modi/verilog-tool-setup` hook for site tools. `verilog-ext-compile-project` is still on `C-c <f5>`                            |
| `template`                            | Duplicates `hydra-verilog-template` on the same `C-c C-t`                                                                                                                                                        |
| `eglot`, `lsp`, `lsp-bridge`, `lspce` | Need an LSP server; none installed                                                                                                                                                                               |
| `formatter`, `beautify`, `flycheck`   | Need verible; not installed                                                                                                                                                                                      |
| `time-stamp`                          | Trivial either way                                                                                                                                                                                               |

Two things to know:

- `verilog-ext-mode-map` is a minor mode map, so it shadows global
  bindings. `C-M-d`, `C-M-p` and `C-M-n` are unbound in it to keep
  `duplicate-dwim` and drag-stuff working in Verilog buffers.
- `xref`, `capf`, `hierarchy` and `typedefs` only act on buffers under a
  `:root` in `verilog-ext-project-alist`, which is nil by default. That is
  machine-specific, so it belongs in `setup-var-overrides.el`; then run
  `verilog-ext-tags-get` (`C-c C-u`) once per project. `navigation` and
  `ports` work without it.

### 3.14 verilog-mode submodule

Updated from `fb3972d` to veripool master `b07a0f7` (~90 commits) and
rebuilt with `build.sh`. Two of those commits are Emacs 31.1 specific:
`0603bad` and `54a0c9b`, both about `hs-forward-sexp-function`.

Two traps found here:

- `build.sh` has no error checking and its last step is an unconditional
  `cp`, so it reports success even when `make` fails. The XEmacs
  byte-compile step does fail, because xemacs is not installed. The Emacs
  step compiles cleanly and that is the output used.
- The generated `verilog-mode.el` has to be reindented with this config,
  which is how it had always been committed. Upstream reformatted its own
  tabs and spaces, so committing the raw build gives a 7758-line diff;
  reindenting brings it to 454 lines, 400 of which are real changes.

Separately, `verilog-ts-mode` does `(require 'verilog-mode)` as it loads
and `setup-treesitter.el` is required before `setup-verilog.el`, so the
copy bundled with Emacs won and the `:load-path` in `setup-verilog.el`
never took effect -- `locate-library` pointed at the submodule while the
loaded version was Emacs's own. `setup-treesitter.el` now puts the
submodule on `load-path` before `verilog-ts-mode` loads.
