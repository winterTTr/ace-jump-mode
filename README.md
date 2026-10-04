Ace Jump Mode
=============

[![Emacs](https://img.shields.io/badge/Emacs-24.4+-8e44bd.svg)](https://www.gnu.org/software/emacs/)
[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](LICENSE)
[![test](https://github.com/kostafey/ace-jump-mode/actions/workflows/test.yml/badge.svg)](https://github.com/kostafey/ace-jump-mode/actions/workflows/test.yml)

Ace jump mode is a minor mode of Emacs, which helps you to move the
cursor within Emacs.  You can move your cursor to **ANY** position
(across windows and frames) in Emacs by using only **3 key presses**.
Have a try and I am sure you will love it.

![Word mode: the words starting with "h" labeled a, b, c...](images/ace-jump-mode.png)

This repository carries on
[winterTTr/ace-jump-mode](https://github.com/winterTTr/ace-jump-mode),
not updated since 2014, and gathers the fixes left in its pull
requests and forks.


Usage
-----

With the keys from [Installation](#installation):

`M-a` ==> `ace-jump-word-or-line-mode`

> Go to a word by entering its first character, then selecting the
> highlighted key to move to it.  Go to a line by pressing `RET`
> instead of a character.

`C-u M-a` ==> `ace-jump-char-mode`

> Go to a character by entering that character, then selecting the
> highlighted key to move to it.

`C-u C-u M-a` ==> `ace-jump-line-mode`

> Go to a line by selecting the highlighted key to move to it.

`C-c M-a` ==> `ace-jump-mode-pop-mark`

> Jump back to where the last jump started.  Repeat it to go further
> back.

When there are more candidates than keys, the first key narrows the
choice, and a second one jumps.  An upper case character searches
case-sensitively.

Watch [Emacs Rocks! Episode 10: Jumping
around](https://www.youtube.com/watch?v=UZkpmegySnc) to see it in
action.


Installation
------------

The [MELPA](https://melpa.org/#/ace-jump-mode) package is still built
from the original repository, so install this one from git.  With
Emacs 30 or later:

```elisp
(use-package ace-jump-mode
  :vc (:url "https://github.com/kostafey/ace-jump-mode" :rev :newest)
  :bind (("M-a" . ace-jump-mode)               ; instead of `backward-sentence'
         ("C-c M-a" . ace-jump-mode-pop-mark))
  :config
  (ace-jump-mode-enable-mark-sync))
```

With [straight.el](https://github.com/radian-software/straight.el),
replace the `:vc` line with
`:straight (:host github :repo "kostafey/ace-jump-mode")`.  If another
package depends on ace-jump-mode, override its recipe instead, so that
this one is used for both:

```elisp
(straight-override-recipe
 '(ace-jump-mode :type git :host github :repo "kostafey/ace-jump-mode"))
```

Any other free keys will do as well, `C-c j` for one.
`ace-jump-mode-enable-mark-sync` keeps `ace-jump-mode-pop-mark` in sync
with the Emacs mark rings, and the other way round.

If you use evil or viper:

```elisp
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)
```


Customization
-------------

`M-x customize-group RET ace-jump RET` lists the options: the move
keys, the scope of a jump (all the windows of all the frames by
default, or the selected window only), case sensitivity and so on.
See also the [FAQ](https://github.com/winterTTr/ace-jump-mode/wiki/AceJump-FAQ)
of the original project.


License
-------

Copyright © 2011-2014 winterTTr <winterTTr@gmail.com>\
Copyright © 2026 Kostafey <kostafey@gmail.com>
and [contributors](https://github.com/kostafey/ace-jump-mode/graphs/contributors?from=6%2F27%2F2011)

Distributed under the [GNU General Public License 3.0+](LICENSE)
