Ace Jump Mode
=============

Ace jump mode is a minor mode of emacs, which help you to move the
cursor within Emacs.  You can move your cursor to **ANY** position (
across window and frame ) in emacs by using only **3 times key
press**. Have a try and I am sure you will love it.


What's new in 2.0 version?
--------------------------

In 1.0 version, ace jump mode can only work in current window.

However, this limitation has already been broken in 2.0 version.  With
ace jump mode 2.0, you can jump to any position you wish across the
bounder of window(c-x 2/3) and even frame(c-x 5).


Is there a demo to show the usage?
------------------------------------
Here is a simple one for you to learn how to use ace jump, [Demo](http://dl.dropbox.com/u/3254819/AceJumpModeDemo/AceJumpDemo.htm)

Usage: 

"C-c SPC" ==>  ace-jump-word-or-line-mode

>Go to a word by entering its first character, then selecting the highlighted key to move to it.
>Go to a line by pressing return, then selecting the highlighted key to move to it.

"C-u C-c SPC" ==>  ace-jump-char-mode

>Go to a character by entering that character, then selecting the highlighted key to move to it.

"C-u C-u C-c SPC" ==>  ace-jump-line-mode

>Go to a line by selecting the highlighted key to move to it.

Thanks emacsrocks website, they make a great show to ace jump mode,
refer to [here](http://www.youtube.com/watch?feature=player_embedded&v=UZkpmegySnc#!).


How to install it?
------------------

```elisp
;;
;; ace jump mode major function
;; 
(add-to-list 'load-path "/full/path/where/ace-jump-mode.el/in/")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)



;; 
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load 'ace-jump-mode
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;If you use viper mode :
(define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)
;;If you use evil
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
```

I want to know more about customized configuration?
---------------------------------------------------
See [FAQ ](http://github.com/winterTTr/ace-jump-mode/wiki/AceJump-FAQ)

License
-------

Copyright © 2011-2014 winterTTr <winterTTr@gmail.com>\
Copyright © 2026 Kostafey <kostafey@gmail.com>
and [contributors](https://github.com/kostafey/ace-jump-mode/graphs/contributors?from=6%2F27%2F2011)

Distributed under the [GNU General Public License 3.0+](LICENSE)
