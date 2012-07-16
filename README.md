Ace Jump Mode
=============

Ace jump mode is a minor mode of emacs, which help you to move the
cursor within Emacs.  You can move your cursor to **ANY** position (
across window and frame ) in emacs by using only **3 times key
press**. Have a try and I am sure you will love it.


What's new in 2.0 version?  --------------------------

In 1.0 version, ace jump mode can only work in current window.

However, this limitation has already been broken in 2.0 version.  With
ace jump mode 2.0, you can jump to any position you wish across the
bounder of window(c-x 2/3) and even frame(c-x 5).


Is there a demo?
------------------------------------
Here is a simple one for you to learn how to use ace jump, [Demo](http://dl.dropbox.com/u/3254819/AceJumpModeDemo/AceJumpDemo.htm)



How to install it?
------------------

    (add-to-list 'load-path "which-folder-ace-jump-mode-file-in/")
    (require 'ace-jump-mode)
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
    
    ;;If you also use viper mode :
    (define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)
    ;;If you use evil
    (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)


How to use it?
--------------
"C-c SPC" ==>  ace-jump-word-mode

>enter first character of a word, select the highlighted key to move to it.

"C-u C-c SPC" ==>  ace-jump-char-mode

>enter a character for query, select the highlighted key to move to it.

"C-u C-u C-c SPC" ==>  ace-jump-line-mode

>each non-empty line will be marked, select the highlighted key to move to it.



I want to know more about customized configuration?
---------------------------------------------------
See [FAQ ](http://github.com/winterTTr/ace-jump-mode/wiki/AceJump-FAQ)