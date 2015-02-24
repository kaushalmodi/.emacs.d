highlight-global
================

A highlight package for EMACS across all buffers. Unlike
[highlight-symobl](https://github.com/nschum/highlight-symbol.el)
which only do highlighting in current buffer, this package highlights
all matches accross ALL buffer. Multiple highlights are
supported. Different highlight shows in different faces.

There are 2 ways to select a highlight target:
1. Mark the selection by region(very usful when you want to highlight a pattern accross all symbols),
2. Put cursor on a symbol to pick the symbol to highlight.

Once you have chosen a highlight target, you can call *highlight-frame-toggle* to highlit/unhighlight the target. You could bind a key like this:
``` lisp
(global-set-key (kbd "M-H") 'highlight-frame-toggle)
```

You could have highlighted multiple targets with different faces, but
you want to clear all the highlights since you do not need them
anymore. You could select the highlighted target one by one, then call
*highlight-frame-toggle* on each of them. Or you could just call
*clear-highlight-frame* which will unhighlights all highlighted
target. You could bind it to a key as follows:

``` lisp
(global-set-key (kbd "M-+") 'clear-highlight-frame)
```
