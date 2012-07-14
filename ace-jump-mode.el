;;; ace-jump-mode.el --- a quick cursor location minor mode for emacs

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author   : winterTTr <winterTTr@gmail.com>
;; URL      : https://github.com/winterTTr/ace-jump-mode/
;; Version  : 2.0
;; Keywords : motion, location, cursor

;; This file is part of GNU Emacs.

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

;;; INTRODUCTION
;;

;; What's this?
;;
;; It is a minor mode for Emacs, enabling fast/direct cursor-moving in
;; current view.

;; Where does minor mode come from ?
;;
;; I firstly see such kind of moving style is in a vim plugin called
;; EasyMotion. It really attract me a lot. EasyMotion provides a much
;; simpler way to use some motions in vim. It takes the out of w or
;; f{char} by highlighting all possible choices and allowing you to
;; press one key to jump directly to the target. So I decide to write
;; one for Emacs.
;;
;; So I must thank to :
;;         Bartlomiej P. for his PreciseJump
;;         Kim Silkebækken for his EasyMotion


;; What's ace-jump-mode ?
;;
;; ace-jump-mode is an fast/direct cursor location minor mode. It will
;; create the N-Branch search tree internal and marks all the possible
;; position with predefined keys in current view. Allowing you to move
;; to the character/word/line almost directly.
;;

;; What is implemented now ?
;;
;; I do not implement everything from EasyMotion. Because I what to
;; make AceJump as simple as possible, and you don’t even need to
;; spend more than 2 minutes to learn how to use it. So now, there is
;; only three sub-mode, which can help you to quick move to a specific
;; character , word and (non-empty) line. Enjoy it~
;;
;; Of course, if you have any cool suggestion, feel free to tell me at
;; anytime. I will put that to top of my TODO list :D
;;

;;; Usage
;;
;; Add the following code to your init file, of course you can select
;; the key which you prefer to.
;; ----------------------------------------------------------
;; (add-to-list 'load-path "which-folder-ace-jump-mode-file-in/")
;; (require 'ace-jump-mode)
;; (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;;
;; ;;If you also use viper mode :
;; (define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)
;; ----------------------------------------------------------
;;

;; Code goes here

(eval-when-compile
  (require 'cl))


;;; register as a minor mode
(or (assq 'ace-jump-mode minor-mode-alist)
    (nconc minor-mode-alist
           (list '(ace-jump-mode ace-jump-mode))))

;; custoize variable
(defvar ace-jump-word-mode-use-query-char t
  "If we need to ask for the query char before enter `ace-jump-word-mode'")

(defvar ace-jump-mode-case-fold case-fold-search
  "If non-nil, the ace-jump mode will ignore case.

The default value is set to the same as `case-fold-search'.")

(defvar ace-jump-mode-scope 'frame
  "Define ace-jump-mode work scope.
Now, there three kind of scope:
1. 'frame : ace jump can work with buffers across the frame, this is also the default.
2. 'window: ace jump will work with buffers within current window
3. 'buffer: ace jump will only work on current buffer")

(defvar ace-jump-mode-submode-list
  '(ace-jump-word-mode
    ace-jump-char-mode
    ace-jump-line-mode)
  "*The mode list when start ace jump mode.
The sequence is the calling sequence when give prefix argument.

Such as:
  If you use the default sequence, which is
      '(ace-jump-word-mode
        ace-jump-char-mode
        ace-jump-line-mode)
and using key to start up ace jump mode, such as 'C-c SPC',
then the usage to start each mode is as below:

   C-c SPC           ==> ace-jump-word-mode
   C-u C-c SPC       ==> ace-jump-char-mode
   C-u C-u C-c SPC   ==> ace-jump-line-mode

Currently, the valid submode is:
   `ace-jump-word-mode'
   `ace-jump-char-mode'
   `ace-jump-line-mode'

")

(defvar ace-jump-mode-move-keys
  (nconc (loop for i from ?a to ?z collect i)
         (loop for i from ?A to ?Z collect i))
  "*The keys that used to move when enter AceJump mode.
Each key should only an printable character, whose name will
fill each possible location.

If you want your own moving keys, you can custom that as follow,
for example, you only want to use lower case character:
\(setq ace-jump-mode-move-keys (loop for i from ?a to ?z collect i)) ")


;;; some buffer specific variable
(defvar ace-jump-mode nil
  "AceJump minor mode.")
(defvar ace-jump-background-overlay-list nil
  "Background overlay which will grey all the display")
(defvar ace-jump-search-tree nil
  "N-branch Search tree. Every leaf node holds the overlay that
is used to highlight the target positions.")
(defvar ace-jump-query-char nil
  "This is local to buffer, save the query char used between internal
mode change via \"M-n\" or \"M-p\"")
(defvar ace-jump-current-mode nil
  "Save the current mode")

(defvar ace-jump-recover-visual-area-list nil
  "Save the ace jump aj-visual-area structure list.

Sometimes, the different window may display the same buffer.  For
this case, we need to create a indirect buffer for them to make
ace jump overlay can work across the differnt window with the
same buffer. When ace jump ends, we need to recover the window to
its original buffer.")


(defgroup ace-jump nil
  "ace jump group"
  :group 'convenience)

;;; define the face
(defface ace-jump-face-background
  '((t (:foreground "gray40")))
  "Face for background of AceJump motion"
  :group 'ace-jump)


(defface ace-jump-face-foreground
  '((((class color)) (:foreground "red"))
    (((background dark)) (:foreground "gray100"))
    (((background light)) (:foreground "gray0"))
    (t (:foreground "gray100")))
  "Face for foreground of AceJump motion"
  :group 'ace-jump)


(defvar ace-jump-mode-hook nil
  "Funciton(s) to call after start AceJump mode")

(defvar ace-jump-mode-end-hook nil
  "Funciton(s) to call after stop AceJump mode")

(defvar ace-jump-mode-before-jump-hook nil
  "Function(s) to call just before moving the cursor to a selected match")

(defun ace-jump-query-char-p ( query-char )
  "Check if the query char is valid,
we can only allow to query printable ascii char"
  (and (> query-char #x1F) (< query-char #x7F)) )

(defun ace-jump-search-candidate( re-query-string visual-area-list)
  "Search the RE-QUERY-STRING in current view, and return the candidate position list.
RE-QUERY-STRING should be an valid regex used for `search-forward-regexp'.

You can control whether use the case sensitive or not by `ace-jump-mode-case-fold'.

Every possible `match-beginning' will be collected.
The returned value is a list of `aj-position' record."
  (loop for va in visual-area-list
        append (let* ((current-window (aj-visual-area-window va))
                      (start-point (window-start current-window))
                      (end-point   (window-end   current-window))
                      (current-buffer (window-buffer current-window)))
                 (with-selected-window current-window
                   (save-excursion
                     (goto-char start-point)
                     (let ((case-fold-search ace-jump-mode-case-fold))
                       (loop while (search-forward-regexp re-query-string end-point t)
                             collect (make-aj-position :offset (match-beginning 0)
                                                       :window current-window))))))))

(defun ace-jump-tree-breadth-first-construct (total-leaf-node max-child-node)
  "Constrct the search tree, each item in the tree is a cons cell.
The (car tree-node) is the type, which should be only 'branch or 'leaf.
The (cdr tree-node) is data stored in a leaf when type is 'leaf,
while a child node list when type is 'branch"
  (let ((left-leaf-node (- total-leaf-node 1))
        (q (make-aj-queue))
        (node nil)
        (root (cons 'leaf nil)) )
    ;; we push the node into queue and make candidate-sum -1, so
    ;; create the start condition for the while loop
    (aj-queue-push root q)
    (while (> left-leaf-node 0)
      (setq node (aj-queue-pop q))
      ;; when a node is picked up from stack, it will be changed to a
      ;; branch node, we lose a leaft node
      (setf (car node) 'branch)
      ;; so we need to add the sum of leaf nodes that we wish to create
      (setq left-leaf-node (1+ left-leaf-node))
      (if (<= left-leaf-node max-child-node)
          ;; current child can fill the left leaf
          (progn
            (setf (cdr node)
                  (loop for i from 1 to left-leaf-node
                        collect (cons 'leaf nil)))
            ;; so this should be the last action for while
            (setq left-leaf-node 0))
        ;; the child can not cover the left leaf
        (progn
          ;; fill as much as possible. Push them to queue, so it have
          ;; the oppotunity to become 'branch node if necessary
          (setf (cdr node)
                (loop for i from 1 to max-child-node
                      collect (let ((n (cons 'leaf nil)))
                                (aj-queue-push n q)
                                n)))
          (setq left-leaf-node (- left-leaf-node max-child-node)))))
    ;; return the root node
    root))

(defun ace-jump-tree-preorder-traverse (tree &optional leaf-func branch-func)
  "we move over tree via preorder, and call BRANCH-FUNC on each branch
node and call LEAF-FUNC on each leaf node"
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
  "Populate the overlay to search tree, every leaf will give one overlay"
  
  (let* (;; create the locally dynamic variable for the following function
         (position-list candidate-list)
         
         ;; make the function to create overlay for each leaf node,
         ;; here we only create each overlay for each candidate
         ;; position, , but leave the 'display property to be empty,
         ;; which will be fill in "update-overlay" function
         (func-create-overlay (lambda (node)
                                (let* ((p (car position-list))
                                       (offset (aj-position-offset p))
                                       (w (aj-position-window p))
                                       (b (window-buffer w))
                                       (ol (make-overlay offset (1+ offset) b)))
                                  ;; update leaf node to remember the ol
                                  (setf (cdr node) ol)
                                  (overlay-put ol 'face 'ace-jump-face-foreground)
                                  ;; associate the aj-position data with overlay
                                  ;; so that we can use it to do the final jump
                                  (overlay-put ol 'aj-data p)
                                  ;; next candidate node
                                  (setq position-list (cdr position-list))))))
    (ace-jump-tree-preorder-traverse tree func-create-overlay)
    tree))


(defun ace-jump-delete-overlay-in-search-tree (tree)
  "Delete all the overlay in search tree leaf node"
  (let ((func-delete-overlay (lambda (node)
                               (delete-overlay (cdr node))
                               (setf (cdr node) nil))))
    (ace-jump-tree-preorder-traverse tree func-delete-overlay)))

(defun ace-jump-buffer-substring (pos)
  "Get the char under the POS"
  (let ((w (aj-position-window pos))
        (offset (aj-position-offset pos)))
    (with-selected-window w
        (buffer-substring offset (1+ offset)))))

(defun ace-jump-update-overlay-in-search-tree (tree keys)
  "Update overlay 'display property using each name in keys"
  (let* (;; create dynamic variable for following function
         (key ?\0)
         ;; populdate each leaf node to be the specific key,
         ;; this only update 'display' property of overlay,
         ;; so that user can see the key from screen and select
         (func-update-overlay
          (lambda (node)
            (let ((ol (cdr node)))
              (overlay-put
               ol
               'display
               (concat (make-string 1 key)
                       ;; when tab, we use more space to prevent screen
                       ;; from messing up
                       (let ((pos (overlay-get ol 'aj-data)))
                         (if (string=
                              (ace-jump-buffer-substring pos)
                              "\t")
                             (make-string (1- tab-width) ? )
                           ""))))))))
    (loop for k in keys
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
  "Based on `ace-jump-mode-scope', search the possible buffers that is showing now."
  (cond
   ((eq ace-jump-mode-scope 'frame)
    (loop for f in (frame-list)
          append (loop for w in (window-list f)
                       collect (make-aj-visual-area :buffer (window-buffer w)
                                                    :window w
                                                    :frame f))))
   ((eq ace-jump-mode-scope 'window)
    (loop for w in (window-list (selected-frame))
          collect (make-aj-visual-area :buffer (window-buffer w)
                                       :window w
                                       :frame (selected-frame))))
   ((eq ace-jump-mode-scope 'buffer)
    (list 
     (make-aj-visual-area :buffer (current-buffer)
                          :window (selected-window)
                          :frame  (selected-frame))))
   (t
    (error "[AceJump] Invalid ace-jump-mode-scope, please check your configuration"))))


(defun ace-jump-mode-make-indirect-buffer (visual-area-list)
  "When the differnt window show the same buffer. The overlay
cannot work for same buffer at the same time. So the indirect
buffer need to create to make overlay can work correctly.

VISUAL-AREA-LIST is aj-visual-area list. This function will
return the structure list for those make a indirect buffer.

Side affect: All the created indirect buffer will show in its
relevant window."
  (loop for va in visual-area-list
        ;; check if the current visual-area (va) has the same buffer with
        ;; the previous ones (vai)
        if (loop for vai in visual-area-list
                 ;; stop at itself, don't need to find the ones behind it (va)
                 until (eq vai va)
                 ;; if the buffer is same, return those(vai) before
                 ;; it(va) so that we know the some visual area has
                 ;; the same buffer with current one (va)
                 if (eq (aj-visual-area-buffer va)
                        (aj-visual-area-buffer vai))
                 collect vai)
        ;; if indeed the same one find, we need create an indirect buffer
        ;; to current visual area(va)
        collect (with-selected-window (aj-visual-area-window va)
                  ;; store the orignal buffer
                  (setf (aj-visual-area-recover-buffer va)
                        (aj-visual-area-buffer va))
                  ;; create indirect buffer to use as working buffer
                  (setf (aj-visual-area-buffer va)
                        ;; Side affect: change the buffer to indirect
                        ;; buffer
                        (clone-indirect-buffer nil nil))
                  ;; update window to the indirect buffer
                  (let ((ws (window-start)))
                    (set-window-buffer (aj-visual-area-window va)
                                       (aj-visual-area-buffer va))
                    (set-window-start
                     (aj-visual-area-window va)
                     ws))
                  va)))


(defun ace-jump-do( re-query-string )
  "The main function to start the AceJump mode.
QUERY-STRING should be a valid regexp string, which finally pass to `search-forward-regexp'.

You can constrol whether use the case sensitive via `ace-jump-mode-case-fold'.
"
  ;; we check the move key to make it valid, cause it can be customized by user
  (if (or (null ace-jump-mode-move-keys)
          (< (length ace-jump-mode-move-keys) 2)
          (not (every #'characterp ace-jump-mode-move-keys)))
      (error "[AceJump] Invalid move keys: check ace-jump-mode-move-keys"))
  ;; search candidate position
  (let* ((visual-area-list (ace-jump-list-visual-area))
         (candidate-list (ace-jump-search-candidate re-query-string visual-area-list)))
    (cond
     ;; cannot find any one
     ((null candidate-list)
      (setq ace-jump-current-mode nil)
      (error "[AceJump] No one found"))
     ;; we only find one, so move to it directly
     ((eq (cdr candidate-list) nil)
      (push-mark (point) t)
      (run-hooks 'ace-jump-mode-before-jump-hook)
      (ace-jump-jump-to (car candidate-list))
      (message "[AceJump] One candidate, move to it directly"))
     ;; more than one, we need to enter AceJump mode
     (t
      ;; make indirect buffer for those windows that show the same buffer
      (setq ace-jump-recover-visual-area-list
            (ace-jump-mode-make-indirect-buffer visual-area-list))
      ;; create background for each visual area
      (setq ace-jump-background-overlay-list
            (loop for va in visual-area-list
                  collect (let* ((w (aj-visual-area-window va))
                                 (b (aj-visual-area-buffer va))
                                 (ol (make-overlay (window-start w)
                                                   (window-end w)
                                                   b)))
                            (overlay-put ol 'face 'ace-jump-face-background)
                            ol)))

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
              (define-key map [t] 'ace-jump-done)
              map))

      (run-hooks 'ace-jump-mode-hook)

      (add-hook 'mouse-leave-buffer-hook 'ace-jump-done)
      (add-hook 'kbd-macro-termination-hook 'ace-jump-done)))))


(defun ace-jump-jump-to (position)
  "Jump to the POSITION, which is a `aj-position' structure storing the position information"
  (let ((w (aj-position-window position)))
    (select-frame-set-input-focus (window-frame w))
    (select-window w)
    (goto-char (aj-position-offset position))))

(defun ace-jump-quick-exchange ()
  "The function that we can use to quick exhange the current mode between
word-mode and char-mode"
  (interactive)
  (cond
   ((eq ace-jump-current-mode 'ace-jump-char-mode)
    (if ace-jump-query-char
        ;; ace-jump-done will clean the query char, so we need to save it
        (let ((query-char ace-jump-query-char))
          (ace-jump-done)
          ;; restore the flag
          (setq ace-jump-query-char query-char)
          (setq ace-jump-current-mode 'ace-jump-word-mode)
          (ace-jump-do (concat "\\b"
                               (regexp-quote (make-string 1 query-char)))))))
   ((eq ace-jump-current-mode 'ace-jump-word-mode)
    (if ace-jump-query-char
        ;; ace-jump-done will clean the query char, so we need to save it
        (let ((query-char ace-jump-query-char))
          (ace-jump-done)
          ;; restore the flag
          (setq ace-jump-query-char query-char)
          (setq ace-jump-current-mode 'ace-jump-char-mode)
          (ace-jump-do (regexp-quote (make-string 1 query-char))))))
   ((eq ace-jump-current-mode 'ace-jump-line-mode)
    nil)
   (t
    nil)))




(defun ace-jump-char-mode (query-char)
  "AceJump char mode"
  (interactive (list (read-char "Query Char:")))
  (if (ace-jump-query-char-p query-char)
      (progn
        (setq ace-jump-query-char query-char)
        (setq ace-jump-current-mode 'ace-jump-char-mode)
        (ace-jump-do (regexp-quote (make-string 1 query-char))))
    (error "[AceJump] Non-printable char")))

(defun ace-jump-word-mode (head-char)
  "AceJump word mode.
You can set `ace-jump-word-mode-use-query-char' to nil to prevent
asking for a head char, that will mark all the word in current
buffer."
  (interactive (list (if ace-jump-word-mode-use-query-char
                         (read-char "Head Char:")
                       nil)))
  (cond
   ((null head-char)
    (ace-jump-do "\\b\\sw"))
   ((ace-jump-query-char-p head-char)
    (setq ace-jump-query-char head-char)
    (setq ace-jump-current-mode 'ace-jump-word-mode)
    (ace-jump-do (concat "\\b"
                         (regexp-quote (make-string 1 head-char)))))
   (t
    (error "[AceJump] Non-printable char"))))


(defun ace-jump-line-mode ()
  "AceJump line mode.
Marked each no empty line and move there"
  (interactive)
  (setq ace-jump-current-mode 'ace-jump-line-mode)
  (ace-jump-do "^."))

;;;###autoload
(defun ace-jump-mode(&optional prefix)
  "AceJump mode is a minor mode for you to quick jump to a
position in the curret view.
   There is three submode now:
     `ace-jump-char-mode'
     `ace-jump-word-mode'
     `ace-jump-line-mode'

You can specify the sequence about which mode should enter
by customize `ace-jump-mode-submode-list'.

If you do not want to query char for word mode, you can change
`ace-jump-word-mode-use-query-char' to nil.

If you don't like the default move keys, you can change it by
setting `ace-jump-mode-move-keys'.

You can constrol whether use the case sensitive via
`ace-jump-mode-case-fold'.
"
  (interactive "p")
  (let ((index (/ prefix 4))
        (submode-list-length (length ace-jump-mode-submode-list)))
    (if (< index 0)
        (error "[AceJump] Invalid prefix command"))
    (if (>= index submode-list-length)
        (setq index (1- submode-list-length)))
    (call-interactively (nth index ace-jump-mode-submode-list))))

(defun ace-jump-move ()
  "move cursor based on user input"
  (interactive)
  (let* ((index (let ((ret (position (aref (this-command-keys) 0)
                                     ace-jump-mode-move-keys)))
                  (if ret ret (length ace-jump-mode-move-keys))))
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
      ;; need to save aj data, as `ace-jump-done' will clean it
      (let ((aj-data (overlay-get (cdr node) 'aj-data)))
        (push-mark (point) t)
        (ace-jump-done)
        (run-hooks 'ace-jump-mode-before-jump-hook)
        (ace-jump-jump-to aj-data)))
     (t
      (ace-jump-done)
      (error "[AceJump] Internal error: tree node type is invalid")))))



(defun ace-jump-done()
  "stop AceJump motion"
  (interactive)
  ;; clear the status flag
  (setq ace-jump-query-char nil)
  (setq ace-jump-current-mode nil)

  ;; clean the status line
  (setq ace-jump-mode nil)
  (force-mode-line-update)

  ;; delete background overlay
  (loop for ol in ace-jump-background-overlay-list
        do (delete-overlay ol))
  (setq ace-jump-background-overlay-list nil)


  ;; we clean the indirect buffer
  (loop for va in ace-jump-recover-visual-area-list
        do (with-selected-window (aj-visual-area-window va)
             ;; recover display buffer
             (set-window-buffer (aj-visual-area-window va)
                                (aj-visual-area-recover-buffer va))
             ;; kill indirect buffer
             (kill-buffer (aj-visual-area-buffer va))))

  ;; delete overlays in search tree
  (ace-jump-delete-overlay-in-search-tree ace-jump-search-tree)
  (setq ace-jump-search-tree nil)

  (setq overriding-local-map nil)
  (run-hooks 'ace-jump-mode-end-hook)

  (remove-hook 'mouse-leave-buffer-hook 'ace-jump-done)
  (remove-hook 'kbd-macro-termination-hook 'ace-jump-done))


;;;; ============================================
;;;; Utilities for ace-jump-mode
;;;; ============================================

;; aj-position do not need the 'buffer' property
;;
;; because aj-position only mark a position for the buffer in a
;; specific window, it do not bind to a specific buffer. Sometimes, we
;; need to create indirect buffer, so every time we want to know the
;; buffer, use `window-buffer' to get it.
(defstruct aj-position offset window)

(defstruct aj-visual-area buffer window frame recover-buffer)

(defstruct aj-queue head tail)

(defun aj-queue-push (item q)
  "enqueue"
  (let ( (head (aj-queue-head q) )
         (tail (aj-queue-tail q) )
         (c (list item) ) )
    (cond
     ((null (aj-queue-head q))
      (setf (aj-queue-head q) c)
      (setf (aj-queue-tail q) c))
     (t
      (setf (cdr (aj-queue-tail q)) c)
      (setf (aj-queue-tail q) c)))))

(defun aj-queue-pop (q)
  "dequeue"
  (if (null (aj-queue-head q))
      (error "[AceJump] Interal Error: Empty queue"))

  (let ((ret (aj-queue-head q)))
    (if (eq ret (aj-queue-tail q))
        ;; only one item left
        (progn
          (setf (aj-queue-head q) nil)
          (setf (aj-queue-tail q) nil))
      ;; multi item left, move forward the head
      (setf (aj-queue-head q) (cdr ret)))
    (car ret)))



(provide 'ace-jump-mode)

;;; ace-jump-mode.el ends here
