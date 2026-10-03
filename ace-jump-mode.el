;;; ace-jump-mode.el --- A quick cursor location minor mode -*- coding: utf-8-unix; lexical-binding: t -*-

;; Copyright (C) 2011-2014 winterTTr <winterTTr@gmail.com>
;; Copyright (C) 2026 Kostafey <kostafey@gmail.com>

;; Author: winterTTr <winterTTr@gmail.com>
;; Maintainer: Kostafey <kostafey@gmail.com>
;; URL: https://github.com/kostafey/ace-jump-mode
;; Version: 3.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, motion, location, cursor

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; INTRODUCTION
;;

;; What's this?
;;
;; It is a minor mode for Emacs.  It can help you to move your cursor
;; to ANY position in Emacs by using only 3 times key press.

;; Where does ace jump mode come from ?
;;
;; I firstly see such kind of moving style is in a vim plugin called
;; EasyMotion.  It really attract me a lot.  So I decide to write
;; one for Emacs and MAKE IT BETTER.
;;
;; So I want to thank to :
;;         Bartlomiej P.   for his PreciseJump
;;         Kim Silkebækken for his EasyMotion


;; What's ace-jump-mode ?
;;
;; ace-jump-mode is an fast/direct cursor location minor mode.  It will
;; create the N-Branch search tree internal and marks all the possible
;; position with predefined keys in within the whole Emacs view.
;; Allowing you to move to the character/word/line almost directly.
;;

;;; Usage
;;
;; Add the following code to your init file, of course you can select
;; the key that you prefer to.
;; ----------------------------------------------------------
;; ;;
;; ;; ace jump mode major function
;; ;;
;; (add-to-list 'load-path "/full/path/where/ace-jump-mode.el/in/")
;; (autoload
;;   'ace-jump-mode
;;   "ace-jump-mode"
;;   "Emacs quick move minor mode"
;;   t)
;; ;; you can select the key you prefer to
;; (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;;
;; ;;
;; ;; enable a more powerful jump back function from ace jump mode
;; ;;
;; (autoload
;;   'ace-jump-mode-pop-mark
;;   "ace-jump-mode"
;;   "Ace jump back:-)"
;;   t)
;; (eval-after-load 'ace-jump-mode
;;   '(ace-jump-mode-enable-mark-sync))
;; (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
;;
;; ;;If you use viper mode :
;; (define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)
;; ;;If you use evil
;; (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
;; ----------------------------------------------------------

;;; For more information
;; Intro Doc: https://github.com/winterTTr/ace-jump-mode/wiki
;; FAQ      : https://github.com/winterTTr/ace-jump-mode/wiki/AceJump-FAQ

;;; Code:

(require 'cl-lib)

(declare-function server-buffer-done "server" (buffer &optional for-killing))

;;;; ============================================
;;;; Utilities for ace-jump-mode
;;;; ============================================

;; ---------------------
;; ace-jump--position
;; ---------------------

;; make a position in a visual area
(cl-defstruct ace-jump--position offset visual-area)

(defmacro ace-jump--position-buffer (position)
  "Return the buffer of POSITION, an `ace-jump--position'."
  `(ace-jump--visual-area-buffer (ace-jump--position-visual-area ,position)))

(defmacro ace-jump--position-window (position)
  "Return the window of POSITION, an `ace-jump--position'."
  `(ace-jump--visual-area-window (ace-jump--position-visual-area ,position)))

(defmacro ace-jump--position-frame (position)
  "Return the frame of POSITION, an `ace-jump--position'."
  `(ace-jump--visual-area-frame (ace-jump--position-visual-area ,position)))

(defmacro ace-jump--position-recover-buffer (position)
  "Return the recover-buffer of POSITION, an `ace-jump--position'."
  `(ace-jump--visual-area-recover-buffer (ace-jump--position-visual-area ,position)))


;; ---------------------
;; ace-jump--visual-area
;; ---------------------

;; a record for all the possible visual area
;; a visual area is a window that showing some buffer in some frame.
(cl-defstruct ace-jump--visual-area buffer window frame recover-buffer)


;; ---------------------
;; a FIFO queue implementation
;; ---------------------
(cl-defstruct ace-jump--queue head tail)

(defun ace-jump--queue-push (item q)
  "Add ITEM to the end of the queue Q."
  (let ((c (list item)))
    (cond
     ((null (ace-jump--queue-head q))
      (setf (ace-jump--queue-head q) c)
      (setf (ace-jump--queue-tail q) c))
     (t
      (setf (cdr (ace-jump--queue-tail q)) c)
      (setf (ace-jump--queue-tail q) c)))))

(defun ace-jump--queue-pop (q)
  "Remove the first item of the queue Q and return it."
  (if (null (ace-jump--queue-head q))
      (error "[AceJump] Internal Error: Empty queue"))

  (let ((ret (ace-jump--queue-head q)))
    (if (eq ret (ace-jump--queue-tail q))
        ;; only one item left
        (progn
          (setf (ace-jump--queue-head q) nil)
          (setf (ace-jump--queue-tail q) nil))
      ;; multi item left, move forward the head
      (setf (ace-jump--queue-head q) (cdr ret)))
    (car ret)))



;;; main code start here

;; register as a minor mode
(or (assq 'ace-jump-mode minor-mode-alist)
    (nconc minor-mode-alist
           (list '(ace-jump-mode ace-jump-mode))))

;;; user options

(defgroup ace-jump nil
  "Jump to any visible position with a few key presses."
  :group 'convenience)

(defcustom ace-jump-word-mode-use-query-char t
  "Non-nil means `ace-jump-word-mode' asks for the head char of the word.
If nil, it marks all the words in view."
  :type 'boolean
  :group 'ace-jump)

(defcustom ace-jump-mode-case-fold case-fold-search
  "If non-nil, the ace-jump mode will ignore case.

The default value is set to the same as `case-fold-search'.
See also `ace-jump-mode-upper-case'."
  :type 'boolean
  :group 'ace-jump)

(defcustom ace-jump-mode-upper-case t
  "If non-nil, an upper case query char makes the search case-sensitive.
So it does even when `ace-jump-mode-case-fold' is non-nil, as an upper
case letter does in an incremental search: see `search-upper-case'.
If nil, `ace-jump-mode-case-fold' alone decides."
  :type 'boolean
  :group 'ace-jump)

(defvar ace-jump-mode-mark-ring nil
  "The list that is used to store the history for jump back.")

(defcustom ace-jump-mode-mark-ring-max 100
  "The maximum length of `ace-jump-mode-mark-ring'."
  :type 'integer
  :group 'ace-jump)


(defcustom ace-jump-mode-gray-background t
  "Non-nil means gray the windows while choosing between candidates.
With more than one candidate, AceJump then grays the windows it
searches in, to make the labels stand out.  If nil, it leaves them
as they are."
  :type 'boolean
  :group 'ace-jump)

(defcustom ace-jump-mode-scope 'global
  "Define what is the scope that ace-jump-mode works.

Now, there are four kinds of values for this:
1. `global'  : ace jump can work across any window and frame,
               this is also the default.
2. `frame'   : ace jump will work for the all windows in current frame.
3. `visible' : ace jump will work for all windows in visible frames.
4. `window'  : ace jump will only work on current window only.
               This is the same behavior for 1.0 version."
  :type '(choice (const :tag "All windows of all frames" global)
                 (const :tag "All windows of the visible frames" visible)
                 (const :tag "All windows of the selected frame" frame)
                 (const :tag "The selected window only" window))
  :group 'ace-jump)

(defcustom ace-jump-mode-detect-punc t
  "Non-nil means word mode falls back to char mode for punctuation.
When the head char given to `ace-jump-word-mode' is a printable
character other than a letter or a digit, AceJump then searches for
it as `ace-jump-char-mode' does.  If nil, such a head char is an
error."
  :type 'boolean
  :group 'ace-jump)


(defcustom ace-jump-mode-submode-list
  '(ace-jump-word-or-line-mode
    ace-jump-char-mode
    ace-jump-line-mode)
  "The submodes `ace-jump-mode' chooses from by the prefix argument.
Without a prefix argument it starts the first one, and each
\\[universal-argument] moves on to the next one, the last one at most.  A numeric
prefix argument counts as \\[universal-argument] does, as 4 per press.  So by
default it starts `ace-jump-word-or-line-mode', where RET as the head
char gives line mode, `ace-jump-char-mode' with \\[universal-argument] and
`ace-jump-line-mode' with \\[universal-argument] \\[universal-argument].

The submodes are `ace-jump-word-mode', `ace-jump-char-mode',
`ace-jump-line-mode', `ace-jump-word-or-line-mode' and
`ace-jump-char-or-line-mode'."
  :type '(repeat (choice (function-item ace-jump-word-or-line-mode)
                         (function-item ace-jump-word-mode)
                         (function-item ace-jump-char-mode)
                         (function-item ace-jump-char-or-line-mode)
                         (function-item ace-jump-line-mode)
                         (function :tag "Other command")))
  :group 'ace-jump)

(defcustom ace-jump-mode-move-keys
  (nconc (cl-loop for i from ?a to ?z collect i)
         (cl-loop for i from ?A to ?Z collect i))
  "The keys that used to move when enter AceJump mode.
Each key should only an printable character, whose name will
fill each possible location.

If you want your own moving keys, you can custom that as follow,
for example, you only want to use lower case character:
\(setq ace-jump-mode-move-keys (cl-loop for i from ?a to ?z collect i))"
  :type '(repeat character)
  :group 'ace-jump)


;;; some internal variable for ace jump
(defvar ace-jump-mode nil
  "AceJump minor mode.")
(defvar ace-jump-background-overlay-list nil
  "Background overlay which will grey all the display.")
(defvar ace-jump-search-tree nil
  "The N-branch search tree.
Every leaf node holds the overlay that is used to highlight one of the
target positions.")
(defvar ace-jump-query-char nil
  "Save the query char used between internal mode.")
(defvar ace-jump-current-mode nil
  "Save the current mode.
See `ace-jump-mode-submode-list' for possible value.")

(defvar ace-jump-sync-emacs-mark-ring nil
  "Non-nil means `ace-jump-mode-pop-mark' syncs the Emacs mark rings.
Jumping back to a position, it then moves the same mark to the end
of the buffer local `mark-ring', or of `global-mark-ring' for another
buffer, as `pop-mark' and `pop-global-mark' would.

Never set this variable directly, it is for AceJump internal use: use
`ace-jump-mode-enable-mark-sync' or `ace-jump-mode-disable-mark-sync'.")

(defvar ace-jump-search-filter nil
  "A predicate to filter the candidates further, or nil.
`ace-jump-search-candidate' calls it with no arguments, with point
after a match and the match data set, and drops the match if the
predicate returns nil.")

;;; define the face
(defface ace-jump-face-background
  '((t (:foreground "gray40")))
  "Face for the grayed windows of AceJump."
  :group 'ace-jump)


(defface ace-jump-face-foreground
  '((((class color)) (:foreground "red" :underline nil :strike-through nil))
    (((background dark)) (:foreground "gray100" :underline nil :strike-through nil))
    (((background light)) (:foreground "gray0" :underline nil :strike-through nil))
    (t (:foreground "gray100" :underline nil)))
  "Face for the labels of AceJump."
  :group 'ace-jump)


(defcustom ace-jump-mode-before-jump-hook nil
  "Hook run just before moving the cursor to the chosen candidate."
  :type 'hook
  :group 'ace-jump)

(defcustom ace-jump-mode-end-hook nil
  "Hook run when AceJump ends after a jump."
  :type 'hook
  :group 'ace-jump)

(defcustom ace-jump-allow-invisible nil
  "Control if ace-jump should select the invisible char as candidate.
Normally, the ace jump mark cannot be seen if the target character
is invisible.  So default to be nil, which will not include those
invisible character as candidate."
  :type 'boolean
  :group 'ace-jump)

(defcustom ace-jump-translate-key-function
  #'ace-jump-translate-key-by-function-key-map
  "Function to translate a key typed on another keyboard layout.
AceJump calls it with the event of any key that is not a move key,
and the function should return the character that key stands for
on the layout of `ace-jump-mode-move-keys', or nil.  If the result
is a move key, it selects that label; otherwise AceJump stops.

The default looks the key up in `local-function-key-map', where
`reverse-input-method' puts the mapping of another layout.  That
map is terminal-local, so a function with a fixed table of its own
also works in frames on other terminals, for example:

  (setq ace-jump-translate-key-function
        (lambda (event) (cdr (assq event my-layout-alist))))

nil disables the translation: any key but a move key stops AceJump."
  :type '(choice (const :tag "No translation" nil)
                 (function-item ace-jump-translate-key-by-function-key-map)
                 (function :tag "Other function"))
  :group 'ace-jump)


(defun ace-jump-case-fold-p (query-char)
  "Return non-nil if the search for QUERY-CHAR should ignore case.
See `ace-jump-mode-case-fold' and `ace-jump-mode-upper-case'."
  (and ace-jump-mode-case-fold
       (not (and ace-jump-mode-upper-case
                 (characterp query-char)
                 (/= query-char (downcase query-char))))))

(defun ace-jump-char-category ( query-char )
  "Return the category of QUERY-CHAR.
For the ASCII table, refer to http://www.asciitable.com/

There are four possible return values:
1. `digit': the number character
2. `alpha': A-Z and a-z, and any other word constituent
            according to the syntax table (e.g. Cyrillic letters)
3. `punc' : all the printable punctuation, and any other printable
            character (e.g. typographic quotes and dashes)
4. `other': all the others"
  (cond
   ;; digit
   ((and (>= query-char #x30) (<= query-char #x39))
    'digit)
   ((or
     ;; capital letter
     (and (>= query-char #x41) (<= query-char #x5A))
     ;; lowercase letter
     (and (>= query-char #x61) (<= query-char #x7A)))
    'alpha)
   ((or
     ;; tab
     (equal query-char #x9)
     ;; punc before digit
     (and (>= query-char #x20) (<= query-char #x2F))
     ;; punc after digit before capital letter
     (and (>= query-char #x3A) (<= query-char #x40))
     ;; punc after capital letter before lowercase letter
     (and (>= query-char #x5B) (<= query-char #x60))
     ;; punc after lowercase letter
     (and (>= query-char #x7B) (<= query-char #x7E)))
    'punc)
   ;; a letter of any other script
   ((and (characterp query-char)
         (eq (char-syntax query-char) ?w))
    'alpha)
   ;; printable punctuation and symbols beyond ASCII
   ((and (characterp query-char)
         (aref printable-chars query-char))
    'punc)
   (t
    'other)))


(defun ace-jump-search-candidate (re-query-string visual-area-list)
  "Search RE-QUERY-STRING in the windows of VISUAL-AREA-LIST.
Return the candidate positions in view, a list of `ace-jump--position'.
RE-QUERY-STRING should be a valid regexp for `re-search-forward'.

Every `match-beginning' in view is collected, and
`ace-jump-mode-case-fold' decides whether case is ignored."
  (cl-loop for va in visual-area-list
           append (let* ((current-window (ace-jump--visual-area-window va))
                         (start-point (window-start current-window))
                         (end-point   (window-end   current-window t)))
                    (with-selected-window current-window
                      (save-excursion
                        (goto-char start-point)
                        (let ((case-fold-search ace-jump-mode-case-fold))
                          (cl-loop while (re-search-forward re-query-string nil t)
                                   ;; `window-end' is the first position out of
                                   ;; view.  Check where the match starts, not
                                   ;; where it ends: a match may end right at
                                   ;; the end of the buffer.  This also skips
                                   ;; "^" on the empty line after a final newline.
                                   until (>= (match-beginning 0) end-point)
                                   if (and (or ace-jump-allow-invisible (not (invisible-p (match-beginning 0))))
                                           (or (null ace-jump-search-filter)
                                               (ignore-errors
                                                 (funcall ace-jump-search-filter))))
                                   collect (make-ace-jump--position :offset (match-beginning 0)
                                                                    :visual-area va)
                                   ;; when we use "^" to search line mode,
                                   ;; re-search-backward will not move one
                                   ;; char after search success, as line
                                   ;; begin is not a valid visible char.
                                   ;; We need to help it to move forward.
                                   do (if (string-equal re-query-string "^")
                                          (goto-char (1+ (match-beginning 0)))))))))))

(defun ace-jump-tree-breadth-first-construct (total-leaf-node max-child-node)
  "Construct a search tree of TOTAL-LEAF-NODE leaves, breadth first.
Each node has at most MAX-CHILD-NODE children.  Each node is a cons
cell: its car is the type, `branch' or `leaf', and its cdr is the data
of a leaf, or the list of children of a branch."
  (let ((left-leaf-node (- total-leaf-node 1))
        (q (make-ace-jump--queue))
        (node nil)
        (root (cons 'leaf nil)) )
    ;; we push the node into queue and make candidate-sum -1, so
    ;; create the start condition for the while loop
    (ace-jump--queue-push root q)
    (while (> left-leaf-node 0)
      (setq node (ace-jump--queue-pop q))
      ;; when a node is picked up from stack, it will be changed to a
      ;; branch node, we lose a leaf node
      (setf (car node) 'branch)
      ;; so we need to add the sum of leaf nodes that we wish to create
      (setq left-leaf-node (1+ left-leaf-node))
      (if (<= left-leaf-node max-child-node)
          ;; current child can fill the left leaf
          (progn
            (setf (cdr node)
                  (cl-loop for i from 1 to left-leaf-node
                           collect (cons 'leaf nil)))
            ;; so this should be the last action for while
            (setq left-leaf-node 0))
        ;; the child can not cover the left leaf
        (progn
          ;; fill as much as possible. Push them to queue, so it have
          ;; the opportunity to become 'branch node if necessary
          (setf (cdr node)
                (cl-loop for i from 1 to max-child-node
                         collect (let ((n (cons 'leaf nil)))
                                   (ace-jump--queue-push n q)
                                   n)))
          (setq left-leaf-node (- left-leaf-node max-child-node)))))
    ;; return the root node
    root))

(defun ace-jump-tree-preorder-traverse (tree &optional leaf-func branch-func)
  "Traverse TREE in preorder.
Call BRANCH-FUNC on each branch node and LEAF-FUNC on each leaf node."
  ;; use stack to do preorder traverse
  (let ((s (list tree)))
    (while (not (null s))
      ;; pick up one from stack
      (let ((node (car s)))
        ;; update stack
        (setq s (cdr s))
        (cond
         ((eq (car node) 'branch)
          ;; a branch node
          (when branch-func
            (funcall branch-func node))
          ;; push all child node into stack
          (setq s (append (cdr node) s)))
         ((eq (car node) 'leaf)
          (when leaf-func
            (funcall leaf-func node)))
         (t
          (message "[AceJump] Internal Error: invalid tree node type")))))))


(defun ace-jump-populate-overlay-to-search-tree (tree candidate-list)
  "Populate the leaves of TREE with overlays on CANDIDATE-LIST.
Every leaf gets the overlay of one candidate."
  (let* (;; the candidates left to place, consumed by the closure below
         (position-list candidate-list)

         ;; make the function to create overlay for each leaf node,
         ;; here we only create each overlay for each candidate
         ;; position, , but leave the 'display property to be empty,
         ;; which will be fill in "update-overlay" function
         (func-create-overlay (lambda (node)
                                (let* ((p (car position-list))
                                       (o (ace-jump--position-offset p))
                                       (w (ace-jump--position-window p))
                                       (b (ace-jump--position-buffer p))
                                       ;; create one char overlay
                                       (ol (make-overlay o (1+ o) b)))
                                  ;; update leaf node to remember the ol
                                  (setf (cdr node) ol)
                                  (overlay-put ol 'face 'ace-jump-face-foreground)
                                  ;; this is important, because sometimes the different
                                  ;; window may display the same buffer, in that case,
                                  ;; overlay for different window (but the same buffer)
                                  ;; will show at the same time on both window
                                  ;; So we make it only on the specific window
                                  (overlay-put ol 'window w)
                                  ;; associate the ace-jump--position data with overlay
                                  ;; so that we can use it to do the final jump
                                  (overlay-put ol 'ace-jump--data p)
                                  ;; next candidate node
                                  (setq position-list (cdr position-list))))))
    (ace-jump-tree-preorder-traverse tree func-create-overlay)
    tree))


(defun ace-jump-delete-overlay-in-search-tree (tree)
  "Delete the overlays in the leaves of TREE."
  (let ((func-delete-overlay (lambda (node)
                               (delete-overlay (cdr node))
                               (setf (cdr node) nil))))
    (ace-jump-tree-preorder-traverse tree func-delete-overlay)))

(defun ace-jump-buffer-substring (pos)
  "Return the character at POS, an `ace-jump--position'."
  (let* ((w (ace-jump--position-window pos))
         (offset (ace-jump--position-offset pos)))
    (with-selected-window w
      (buffer-substring offset (1+ offset)))))

(defun ace-jump-update-overlay-in-search-tree (tree keys)
  "Label the candidates of TREE with KEYS, by the overlay `display'."
  (let* (;; the key of the subtree being labeled, set by the loop below
         (key ?\0)
         ;; populate each leaf node to be the specific key,
         ;; this only update 'display' property of overlay,
         ;; so that user can see the key from screen and select
         (func-update-overlay
          (lambda (node)
            (let ((ol (cdr node)))
              (overlay-put
               ol
               'display
               (concat (make-string 1 key)
                       (let* ((pos (overlay-get ol 'ace-jump--data))
                              (subs (ace-jump-buffer-substring pos)))
                         (cond
                          ;; when tab, we use more space to prevent screen
                          ;; from messing up, as wide as a tab is in
                          ;; the candidate's buffer
                          ((string-equal subs "\t")
                           (make-string (1- (buffer-local-value
                                             'tab-width
                                             (ace-jump--position-buffer pos)))
                                        ? ))
                          ;; when enter, we need to add one more enter
                          ;; to make the screen not change
                          ((string-equal subs "\n")
                           "\n")
                          (t
                           ;; there are wide-width characters
                           ;; so, we need paddings
                           (make-string (max 0 (1- (string-width subs))) ? ))))))))))
    (cl-loop for k in keys
             for n in (cdr tree)
             do (progn
                  ;; update "key" variable so that the function can use
                  ;; the correct context
                  (setq key k)
                  (if (eq (car n) 'branch)
                      (ace-jump-tree-preorder-traverse n
                                                       func-update-overlay)
                    (funcall func-update-overlay n))))))



(defun ace-jump-list-visual-area()
  "Return the windows to search in, as `ace-jump-mode-scope' says.
The windows are a list of `ace-jump--visual-area'."
  (cond
   ((eq ace-jump-mode-scope 'global)
    (cl-loop for f in (frame-list)
             append (cl-loop for w in (window-list f)
                             collect (make-ace-jump--visual-area :buffer (window-buffer w)
                                                                 :window w
                                                                 :frame f))))
   ((eq ace-jump-mode-scope 'visible)
    (cl-loop for f in (frame-list)
             if (eq t (frame-visible-p f))
             append (cl-loop for w in (window-list f)
                             collect (make-ace-jump--visual-area :buffer (window-buffer w)
                                                                 :window w
                                                                 :frame f))))
   ((eq ace-jump-mode-scope 'frame)
    (cl-loop for w in (window-list (selected-frame))
             collect (make-ace-jump--visual-area :buffer (window-buffer w)
                                                 :window w
                                                 :frame (selected-frame))))
   ((eq ace-jump-mode-scope 'window)
    (list
     (make-ace-jump--visual-area :buffer (current-buffer)
                                 :window (selected-window)
                                 :frame  (selected-frame))))
   (t
    (error "[AceJump] Invalid ace-jump-mode-scope, please check your configuration"))))



(defun ace-jump-do( re-query-string )
  "The main function to start the AceJump mode.
RE-QUERY-STRING should be a valid regexp string, which finally pass
to `search-forward-regexp'.

You can control whether use the case sensitive via
`ace-jump-mode-case-fold'."
  ;; we check the move key to make it valid, cause it can be customized by user
  (if (or (null ace-jump-mode-move-keys)
          (< (length ace-jump-mode-move-keys) 2)
          (not (cl-every #'characterp ace-jump-mode-move-keys)))
      (error "[AceJump] Invalid move keys: check ace-jump-mode-move-keys"))
  ;; search candidate position
  (let* ((visual-area-list (ace-jump-list-visual-area))
         (candidate-list (ace-jump-search-candidate re-query-string visual-area-list)))
    (cond
     ;; cannot find any one
     ((null candidate-list)
      (setq ace-jump-current-mode nil)
      (setq ace-jump-query-char nil)
      (user-error "[AceJump] No one found"))
     ;; we only find one, so move to it directly
     ((eq (cdr candidate-list) nil)
      (unwind-protect
          (progn
            (ace-jump-push-mark)
            (run-hooks 'ace-jump-mode-before-jump-hook)
            (ace-jump-jump-to (car candidate-list)))
        ;; AceJump mode is not entered, so `ace-jump-done' will not
        ;; clear the status flags: do it here, even if the jump fails
        (setq ace-jump-current-mode nil)
        (setq ace-jump-query-char nil))
      (message "[AceJump] One candidate, move to it directly")
      (run-hooks 'ace-jump-mode-end-hook))
     ;; more than one, we need to enter AceJump mode
     (t
      ;; create background for each visual area
      (if ace-jump-mode-gray-background
          (setq ace-jump-background-overlay-list
                (cl-loop for va in visual-area-list
                         collect (let* ((w (ace-jump--visual-area-window va))
                                        (b (ace-jump--visual-area-buffer va))
                                        (ol (make-overlay (window-start w)
                                                          (window-end w t)
                                                          b)))
                                   (overlay-put ol 'face 'ace-jump-face-background)
                                   ol))))

      ;; construct search tree and populate overlay into tree
      (setq ace-jump-search-tree
            (ace-jump-tree-breadth-first-construct (length candidate-list)
                                                   (length ace-jump-mode-move-keys)))
      (ace-jump-populate-overlay-to-search-tree ace-jump-search-tree
                                                candidate-list)
      (ace-jump-update-overlay-in-search-tree ace-jump-search-tree
                                              ace-jump-mode-move-keys)

      ;; do minor mode configuration
      (cond
       ((eq ace-jump-current-mode 'ace-jump-char-mode)
        (setq ace-jump-mode " AceJump - Char"))
       ((eq ace-jump-current-mode 'ace-jump-word-mode)
        (setq ace-jump-mode " AceJump - Word"))
       ((eq ace-jump-current-mode 'ace-jump-line-mode)
        (setq ace-jump-mode " AceJump - Line"))
       (t
        (setq ace-jump-mode " AceJump")))
      (force-mode-line-update)


      ;; override the local key map
      (setq overriding-local-map
            (let ( (map (make-keymap)) )
              (dolist (key-code ace-jump-mode-move-keys)
                (define-key map (make-string 1 key-code) 'ace-jump-move))
              (define-key map (kbd "C-c C-c") 'ace-jump-quick-exchange)
              ;; "C-c C-c" makes C-c a prefix, which [t] below does not
              ;; cover: any other C-c key must stop AceJump as well
              (define-key map [?\C-c t] 'ace-jump-done)
              (define-key map [t] 'ace-jump-move-translated)
              ;; switching the keyboard layout on MS-Windows sends this
              ;; event, which should neither be a label nor stop
              ;; AceJump: let it reach its global binding (`ignore')
              (define-key map [language-change] nil)
              map))

      (add-hook 'mouse-leave-buffer-hook 'ace-jump-done)
      (add-hook 'kbd-macro-termination-hook 'ace-jump-done)))))


(defun ace-jump-jump-to (position)
  "Jump to the POSITION.
POSITION is a `ace-jump--position' structure storing the position information."
  (let ((offset (ace-jump--position-offset position))
        (frame (ace-jump--position-frame position))
        (window (ace-jump--position-window position))
        (buffer (ace-jump--position-buffer position))
        (line-mode-column 0))

    ;; save the column before do line jump, so that we can jump to the
    ;; same column as previous line, make it the same behavior as C-n/C-p
    (if (eq ace-jump-current-mode 'ace-jump-line-mode)
        (setq line-mode-column (current-column)))

    ;; focus to the frame
    (if (and (frame-live-p frame)
             (not (eq frame (selected-frame))))
        (select-frame-set-input-focus (window-frame window)))

    ;; select the correct window
    (if (and (window-live-p window)
             (not (eq window (selected-window))))
        (select-window window))

    ;; switch to buffer
    (if (and (buffer-live-p buffer)
             (not (eq buffer (window-buffer window))))
        (switch-to-buffer buffer))

    ;; move to correct position
    (if (and (buffer-live-p buffer)
             (eq (current-buffer) buffer))
        (goto-char offset))

    ;; recover to the same column if we use the line jump mode
    (if (eq ace-jump-current-mode 'ace-jump-line-mode)
        (move-to-column line-mode-column))))

(defun ace-jump-push-mark ()
  "Push the current position information onto the `ace-jump-mode-mark-ring'.
Also push it onto the Emacs mark ring, unless the region is active:
then the mark stays where it is, and the jump extends the region, as
`isearch' does."
  ;; add mark to the emacs basic push mark
  (unless (and transient-mark-mode mark-active)
    (push-mark (point) t))
  ;; we also push the mark on the `ace-jump-mode-mark-ring', which has
  ;; more information for better jump back
  (let ((pos (make-ace-jump--position :offset (point)
                                      :visual-area (make-ace-jump--visual-area :buffer (current-buffer)
                                                                               :window (selected-window)
                                                                               :frame  (selected-frame)))))
    (setq ace-jump-mode-mark-ring (cons pos ace-jump-mode-mark-ring)))
  ;; when exceeding the max count, discard the last one
  (if (> (length ace-jump-mode-mark-ring) ace-jump-mode-mark-ring-max)
      (setcdr (nthcdr (1- ace-jump-mode-mark-ring-max) ace-jump-mode-mark-ring) nil)))


;;;###autoload
(defun ace-jump-mode-pop-mark ()
  "Jump back to where the last jump started.
Repeated calls go further back, around `ace-jump-mode-mark-ring'."
  (interactive)
  ;; we jump over the killed buffer position
  (while (and ace-jump-mode-mark-ring
              (not (buffer-live-p (ace-jump--position-buffer
                                   (car ace-jump-mode-mark-ring)))))
    (setq ace-jump-mode-mark-ring (cdr ace-jump-mode-mark-ring)))

  (if (null ace-jump-mode-mark-ring)
      ;; no valid history exist
      (user-error "[AceJump] No more history"))

  (if ace-jump-sync-emacs-mark-ring
      (let ((p (car ace-jump-mode-mark-ring)))
        ;; if we are jump back in the current buffer, that means we
        ;; only need to sync the buffer local mark-ring
        (if (eq (current-buffer) (ace-jump--position-buffer p))
            (if (equal (ace-jump--position-offset p) (marker-position (mark-marker)))
                ;; if the current marker is the same as where we need
                ;; to jump back, we do the same as pop-mark actually,
                ;; copy implementation from pop-mark, cannot use it
                ;; directly, as there is advice on it
                (when mark-ring
                  (setq mark-ring (nconc mark-ring (list (copy-marker (mark-marker)))))
                  (set-marker (mark-marker) (+ 0 (car mark-ring)) (current-buffer))
                  (move-marker (car mark-ring) nil)
                  (setq mark-ring (cdr mark-ring))
                  (deactivate-mark))

              ;;  But if there is other marker put before the wanted destination, the following scenario
              ;;
              ;;             +---+---+---+---+                                   +---+---+---+---+
              ;;   Mark Ring | 2 | 3 | 4 | 5 |                                   | 2 | 4 | 5 | 3 |
              ;;             +---+---+---+---+                                   +---+---+---+---+
              ;;             +---+                                               +---+
              ;;   Marker    | 1 |                                               | 1 | <-- Marker (not changed)
              ;;             +---+                                               +---+
              ;;             +---+                                               +---+
              ;;   Cursor    | X |                     Pop up AJ mark 3          | 3 | <-- Cursor position
              ;;             +---+                                               +---+
              ;;             +---+---+---+                                       +---+---+---+
              ;;   AJ Ring   | 3 | 4 | 5 |                                       | 4 | 5 | 3 |
              ;;             +---+---+---+                                       +---+---+---+
              ;;
              ;; So what we need to do, is put the found mark in mark-ring to the end
              (let ((po (ace-jump--position-offset p)))
                (setq mark-ring
                      (ace-jump-move-first-to-end-if mark-ring
                                                     (lambda (x)
                                                       (equal (marker-position x) po))))))


          ;; when we jump back to another buffer, do as the
          ;; pop-global-mark does. But we move the marker with the
          ;; same target buffer to the end, not always the first one
          (let ((pb (ace-jump--position-buffer p)))
            (setq global-mark-ring
                  (ace-jump-move-first-to-end-if global-mark-ring
                                                 (lambda (x)
                                                   (eq (marker-buffer x) pb))))))))


  ;; move the first element to the end of the ring
  (ace-jump-jump-to (car ace-jump-mode-mark-ring))
  (setq ace-jump-mode-mark-ring (nconc (cdr ace-jump-mode-mark-ring)
                                       (list (car ace-jump-mode-mark-ring)))))

(defun ace-jump-quick-exchange ()
  "Switch between char mode and word mode for the same query char."
  (interactive)
  (cond
   ((eq ace-jump-current-mode 'ace-jump-char-mode)
    (if ace-jump-query-char
        ;; ace-jump-done will clean the query char, so we need to save it
        (let ((query-char ace-jump-query-char))
          (ace-jump-done)
          (ace-jump-word-mode query-char))))
   ((eq ace-jump-current-mode 'ace-jump-word-mode)
    (if ace-jump-query-char
        ;; ace-jump-done will clean the query char, so we need to save it
        (let ((query-char ace-jump-query-char))
          (ace-jump-done)
          ;; restore the flag
          (ace-jump-char-mode query-char))))
   ((eq ace-jump-current-mode 'ace-jump-line-mode)
    nil)
   (t
    nil)))

;;;###autoload
(defun ace-jump-char-mode (query-char)
  "Jump to an occurrence of QUERY-CHAR in view.
An upper case QUERY-CHAR makes the search case-sensitive: see
`ace-jump-mode-upper-case'."
  (interactive (list (read-char "Query Char:")))

  ;; We should prevent recursion call this function.  This can happen
  ;; when you trigger the key for ace jump again when already in ace
  ;; jump mode.  So we stop the previous one first.
  (if ace-jump-current-mode (ace-jump-done))

  (if (eq (ace-jump-char-category query-char) 'other)
    (user-error "[AceJump] Non-printable character"))

  ;; others : digit , alpha, punc
  (setq ace-jump-query-char query-char)
  (setq ace-jump-current-mode 'ace-jump-char-mode)
  (let ((ace-jump-mode-case-fold (ace-jump-case-fold-p query-char)))
    (ace-jump-do (regexp-quote (make-string 1 query-char)))))


;;;###autoload
(defun ace-jump-word-mode (head-char)
  "Jump to a word in view that starts with HEAD-CHAR.
If HEAD-CHAR is nil, as when `ace-jump-word-mode-use-query-char' is
nil, mark all the words in view.  Punctuation as HEAD-CHAR falls back
to char mode: see `ace-jump-mode-detect-punc'."
  (interactive (list (if ace-jump-word-mode-use-query-char
                         (read-char "Head Char:")
                       nil)))

  ;; We should prevent recursion call this function.  This can happen
  ;; when you trigger the key for ace jump again when already in ace
  ;; jump mode.  So we stop the previous one first.
  (if ace-jump-current-mode (ace-jump-done))

  (cond
   ((null head-char)
    (setq ace-jump-current-mode 'ace-jump-word-mode)
    ;; \<  - start of word
    ;; \sw - word constituent
    (ace-jump-do "\\<\\sw"))
   ((memq (ace-jump-char-category head-char)
          '(digit alpha))
    (setq ace-jump-query-char head-char)
    (setq ace-jump-current-mode 'ace-jump-word-mode)
    (let ((ace-jump-mode-case-fold (ace-jump-case-fold-p head-char)))
      (ace-jump-do (concat "\\<" (make-string 1 head-char)))))
   ((eq (ace-jump-char-category head-char)
        'punc)
    ;; we do not query punctuation under word mode
    (if (null ace-jump-mode-detect-punc)
        (user-error "[AceJump] Not a valid word constituent"))
    ;; we will use char mode to continue search
    (setq ace-jump-query-char head-char)
    (setq ace-jump-current-mode 'ace-jump-char-mode)
    (ace-jump-do (regexp-quote (make-string 1 head-char))))
   (t
    (user-error "[AceJump] Non-printable character"))))


;;;###autoload
(defun ace-jump-line-mode ()
  "Jump to a line in view, keeping the column, as \\[next-line] does."
  (interactive)

  ;; We should prevent recursion call this function.  This can happen
  ;; when you trigger the key for ace jump again when already in ace
  ;; jump mode.  So we stop the previous one first.
  (if ace-jump-current-mode (ace-jump-done))

  (setq ace-jump-current-mode 'ace-jump-line-mode)
  (ace-jump-do "^"))

;;;###autoload
(defun ace-jump-char-or-line-mode (query-char)
  "AceJump char or line mode.
Like `ace-jump-char-mode' but will switch to `ace-jump-line-mode' if
return is given as QUERY-CHAR."
  (interactive (list (read-char "Query Char:")))

  (if (equal query-char #xD) ;; If Query Char is return
      (ace-jump-line-mode)
    (ace-jump-char-mode query-char)))

;;;###autoload
(defun ace-jump-word-or-line-mode (head-char)
  "AceJump word or line mode.
Like `ace-jump-word-mode' but will switch to `ace-jump-line-mode' if
return is given as HEAD-CHAR.  With `ace-jump-word-mode-use-query-char'
set to nil, no head char is asked for, and all the words are marked,
as `ace-jump-word-mode' does."
  (interactive (list (if ace-jump-word-mode-use-query-char
                         (read-char "Head Char:")
                       nil)))

  (if (equal head-char #xD) ;; If head-char is return
      (ace-jump-line-mode)
    (ace-jump-word-mode head-char)))

;;;###autoload
(defun ace-jump-mode(&optional prefix)
  "Jump to a position in view, by the submode PREFIX chooses.
See `ace-jump-mode-submode-list' for the submodes and how the prefix
argument chooses between them.

See also `ace-jump-word-mode-use-query-char', `ace-jump-mode-move-keys'
and `ace-jump-mode-case-fold'."
  (interactive "p")
  (setq prefix (or prefix 1))
  (if (< prefix 0)
      (user-error "[AceJump] Invalid prefix command"))
  (let ((index 0))
    ;; one submode further for each C-u, that is for each factor of 4
    (while (>= prefix 4)
      (setq prefix (/ prefix 4)
            index (1+ index)))
    (call-interactively
     (nth (min index (1- (length ace-jump-mode-submode-list)))
          ace-jump-mode-submode-list))))

(defun ace-jump-translate-key-by-function-key-map (event)
  "Return the character EVENT stands for in `local-function-key-map', or nil.
This is the mapping `reverse-input-method' builds, for instance.
The default value of `ace-jump-translate-key-function'."
  (let ((translation (and (characterp event)
                          (lookup-key local-function-key-map (vector event)))))
    (and (vectorp translation)
         (= (length translation) 1)
         (characterp (aref translation 0))
         (aref translation 0))))

(defun ace-jump-translate-move-key (event)
  "Return the move key EVENT types on another keyboard layout, or nil.
The translation is done by `ace-jump-translate-key-function'."
  (let ((key (and ace-jump-translate-key-function
                  (funcall ace-jump-translate-key-function event))))
    (and key (memq key ace-jump-mode-move-keys) key)))

(defun ace-jump-move-translated ()
  "Move by a key typed on another keyboard layout, or stop AceJump.
`overriding-local-map' catches every key, so `local-function-key-map'
never gets a chance to translate it: translate it here with
`ace-jump-translate-key-function'."
  (interactive)
  (let ((key (ace-jump-translate-move-key last-command-event)))
    (if key
        (ace-jump-move key)
      (ace-jump-done))))

(defun ace-jump-move (&optional key)
  "Move cursor based on user input.
KEY is the move key to use, the key that invoked the command by default."
  (interactive)
  (let* ((index (or (cl-position (or key last-command-event)
                                 ace-jump-mode-move-keys)
                    (length ace-jump-mode-move-keys)))
         (node (nth index (cdr ace-jump-search-tree))))
    (cond
     ;; we do not find key in search tree. This can happen, for
     ;; example, when there is only three selections in screen
     ;; (totally five move-keys), but user press the forth move key
     ((null node)
      (message "No such position candidate.")
      (ace-jump-done))
     ;; this is a branch node, which means there need further
     ;; selection
     ((eq (car node) 'branch)
      (let ((old-tree ace-jump-search-tree))
        ;; we use sub tree in next move, create a new root node
        ;; whose child is the sub tree nodes
        (setq ace-jump-search-tree (cons 'branch (cdr node)))
        (ace-jump-update-overlay-in-search-tree ace-jump-search-tree
                                                ace-jump-mode-move-keys)

        ;; this is important, we need remove the subtree first before
        ;; do delete, we set the child nodes to nil
        (setf (cdr node) nil)
        (ace-jump-delete-overlay-in-search-tree old-tree)))
     ;; if the node is leaf node, this is the final one
     ((eq (car node) 'leaf)
      ;; save the target, as `ace-jump-done' deletes its overlay
      (let ((target (overlay-get (cdr node) 'ace-jump--data)))
        ;; leave AceJump mode even if the jump fails
        (unwind-protect
            (progn
              (ace-jump-push-mark)
              (run-hooks 'ace-jump-mode-before-jump-hook)
              (ace-jump-jump-to target))
          (ace-jump-done)))
      (run-hooks 'ace-jump-mode-end-hook))
     (t
      (ace-jump-done)
      (error "[AceJump] Internal error: tree node type is invalid")))))



(defun ace-jump-done()
  "Stop AceJump: remove its labels and its keymap."
  (interactive)
  ;; clear the status flag
  (setq ace-jump-query-char nil)
  (setq ace-jump-current-mode nil)

  ;; clean the status line
  (setq ace-jump-mode nil)
  (force-mode-line-update)

  ;; delete background overlay
  (cl-loop for ol in ace-jump-background-overlay-list
           do (delete-overlay ol))
  (setq ace-jump-background-overlay-list nil)


  ;; delete overlays in search tree
  (ace-jump-delete-overlay-in-search-tree ace-jump-search-tree)
  (setq ace-jump-search-tree nil)

  (setq overriding-local-map nil)

  (remove-hook 'mouse-leave-buffer-hook 'ace-jump-done)
  (remove-hook 'kbd-macro-termination-hook 'ace-jump-done))

(defun ace-jump-kill-buffer(buffer)
  "Kill BUFFER, done with its server clients first, if any."
  (if (and (boundp 'server-buffer-clients)
           server-buffer-clients)
      (server-buffer-done buffer t))
  (kill-buffer buffer))

;;;; ============================================
;;;; advice to sync emacs mark ring
;;;; ============================================

(defun ace-jump-move-to-end-if ( l pred )
  "Move the elements of L for which PRED returns non-nil to its end.
PRED is called with one element at a time, for instance
\(lambda (x) (equal x 1))."
  (let (true-list false-list)
    (cl-loop for e in l
             do (if (funcall pred e)
                    (setq true-list (cons e true-list))
                  (setq false-list (cons e false-list))))
    (nconc (nreverse false-list)
           (and true-list (nreverse true-list)))))

(defun ace-jump-move-first-to-end-if (l pred)
  "Move the first element of L for which PRED returns non-nil to its end."
  (let (found)
    (ace-jump-move-to-end-if l
                             (lambda (x)
                               (if found
                                   nil
                                 (setq found (funcall pred x)))))))



(defun ace-jump-pop-mark-advice (&rest _)
  "Sync the mark ring when `pop-mark' is called to jump back.
Move the same position to the end of `ace-jump-mode-mark-ring'."
  (let ((mp (mark t))
        (cb (current-buffer)))
    (if mp
        (setq ace-jump-mode-mark-ring
              (ace-jump-move-first-to-end-if ace-jump-mode-mark-ring
                                             (lambda (x)
                                               (and (equal (ace-jump--position-offset x) mp)
                                                    (eq (ace-jump--position-buffer x) cb))))))))

(defun ace-jump-pop-global-mark-advice (&rest _)
  "Sync the mark ring when `pop-global-mark' is called to jump back.
Move the positions in the same buffer to the end of
`ace-jump-mode-mark-ring'."
  ;; find the one that will be jump to
  (let ((index global-mark-ring))
    ;; refer to the implementation of `pop-global-mark'
    (while (and index (not (marker-buffer (car index))))
      (setq index (cdr index)))
    (if index
        ;; find the mark
        (let ((mb (marker-buffer (car index))))
          (setq ace-jump-mode-mark-ring
                (ace-jump-move-to-end-if ace-jump-mode-mark-ring
                                         (lambda (x)
                                           (eq (ace-jump--position-buffer x) mb))))))))

(defun ace-jump-mode-enable-mark-sync ()
  "Keep `ace-jump-mode-mark-ring' in sync with the Emacs mark rings.
Advise `pop-mark' and `pop-global-mark' to move the position they jump
back to to the end of `ace-jump-mode-mark-ring', and set
`ace-jump-sync-emacs-mark-ring', so that `ace-jump-mode-pop-mark' does
the same in the Emacs mark rings."
  (advice-add 'pop-mark :before #'ace-jump-pop-mark-advice)
  (advice-add 'pop-global-mark :before #'ace-jump-pop-global-mark-advice)
  (setq ace-jump-sync-emacs-mark-ring t))

(defun ace-jump-mode-disable-mark-sync ()
  "Stop syncing `ace-jump-mode-mark-ring' with the Emacs mark rings.
Undo `ace-jump-mode-enable-mark-sync'."
  (advice-remove 'pop-mark #'ace-jump-pop-mark-advice)
  (advice-remove 'pop-global-mark #'ace-jump-pop-global-mark-advice)
  (setq ace-jump-sync-emacs-mark-ring nil))


(provide 'ace-jump-mode)

;;; ace-jump-mode.el ends here
