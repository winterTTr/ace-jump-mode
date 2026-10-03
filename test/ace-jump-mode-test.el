;;; ace-jump-mode-test.el --- Tests for ace-jump-mode  -*- lexical-binding: t -*-

;;; Commentary:

;; Run with `make test', or:
;;
;;   emacs -Q -batch -L . -L test -l ace-jump-mode-test \
;;         -f ert-run-tests-batch-and-exit
;;
;; Batch mode has no redisplay: a window shows the whole of a small
;; test buffer, and `window-end' is stubbed where a test needs part of
;; the buffer out of view.  Keys are typed either through the command
;; loop (`ace-jump-test-type', which runs a keyboard macro and so ends
;; AceJump with it) or one at a time (`ace-jump-test-press'), which
;; leaves AceJump running between keys.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ace-jump-mode)

;;;; Helpers

(defvar ace-jump-test-map
  (let ((map (make-sparse-keymap)))
    (define-key map [f5] #'ace-jump-char-mode)
    (define-key map [f6] #'ace-jump-word-mode)
    (define-key map [f7] #'ace-jump-line-mode)
    (define-key map [f8] #'ace-jump-char-or-line-mode)
    (define-key map [f9] #'ace-jump-word-or-line-mode)
    (define-key map [f10] #'ace-jump-mode)
    map)
  "Local map of the test buffers: keys to start AceJump with.")

(defun ace-jump-test-reset ()
  "Leave AceJump mode and clear its state, whatever it is."
  (if ace-jump-search-tree
      (ace-jump-done)
    (setq ace-jump-current-mode nil
          ace-jump-query-char nil
          ace-jump-mode nil
          overriding-local-map nil))
  ;; `ace-jump-test-press' may have hidden these from `ace-jump-done'
  (remove-hook 'mouse-leave-buffer-hook 'ace-jump-done)
  (remove-hook 'kbd-macro-termination-hook 'ace-jump-done))

(defmacro ace-jump-test-with-buffer (text &rest body)
  "Run BODY in a buffer holding TEXT, alone in the selected window.
Point is at the start of the buffer, its local map is
`ace-jump-test-map', and the AceJump settings are bound to known
values.  AceJump state is cleared afterwards."
  (declare (indent 1) (debug t))
  (let ((buffer (make-symbol "buffer")))
    `(let ((ace-jump-mode-scope 'window)
           (ace-jump-mode-case-fold t)
           (ace-jump-mode-move-keys (append (number-sequence ?a ?z)
                                            (number-sequence ?A ?Z)))
           (ace-jump-mode-gray-background t)
           (ace-jump-word-mode-use-query-char t)
           (ace-jump-mode-detect-punc t)
           (ace-jump-allow-invisible nil)
           (ace-jump-search-filter nil)
           (ace-jump-translate-key-function nil)
           (ace-jump-mode-before-jump-hook nil)
           (ace-jump-mode-end-hook nil)
           (ace-jump-mode-mark-ring nil)
           (,buffer (generate-new-buffer " *ace-jump-test*")))
       (save-window-excursion
         (unwind-protect
             (progn
               (delete-other-windows)
               (switch-to-buffer ,buffer)
               (use-local-map ace-jump-test-map)
               (insert ,text)
               (goto-char (point-min))
               ,@body)
           (ace-jump-test-reset)
           (kill-buffer ,buffer))))))

(defmacro ace-jump-test-with-global-key (key command &rest body)
  "Run BODY with KEY bound to COMMAND in the global map."
  (declare (indent 2) (debug t))
  (let ((old (make-symbol "old"))
        (map (make-symbol "map")))
    `(let ((,old (current-global-map))
           (,map (make-sparse-keymap)))
       (set-keymap-parent ,map ,old)
       (define-key ,map ,key ,command)
       (use-global-map ,map)
       (unwind-protect
           (progn ,@body)
         (use-global-map ,old)))))

(defun ace-jump-test-type (&rest keys)
  "Type KEYS, vectors or strings, through the command loop.
This runs them as a keyboard macro, whose end stops AceJump."
  (execute-kbd-macro (apply #'vconcat keys)))

(defun ace-jump-test-press (event)
  "Type EVENT through the command loop.
Unlike with `ace-jump-test-type', AceJump keeps running afterwards
if the command leaves it so: the end of this keyboard macro does
not stop it."
  (let ((kbd-macro-termination-hook
         (remq 'ace-jump-done kbd-macro-termination-hook)))
    (execute-kbd-macro (vector event))))

(defun ace-jump-test-displays ()
  "Return the labels on display as (POSITION . DISPLAY), in buffer order."
  (sort (cl-loop for ol in (overlays-in (point-min) (point-max))
                 when (overlay-get ol 'aj-data)
                 collect (cons (overlay-start ol) (overlay-get ol 'display)))
        #'car-less-than-car))

(defun ace-jump-test-labels ()
  "Return the labels on display as (POSITION . KEY), in buffer order."
  (mapcar (lambda (d) (cons (car d) (aref (cdr d) 0)))
          (ace-jump-test-displays)))

(defun ace-jump-test-candidates (regexp)
  "Return the positions `ace-jump-search-candidate' finds for REGEXP."
  (mapcar #'aj-position-offset
          (ace-jump-search-candidate regexp (ace-jump-list-visual-area))))

(defun ace-jump-test-find-all (string)
  "Return the positions where STRING starts in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (search-forward string nil t)
             collect (match-beginning 0))))

;;;; Package

(ert-deftest ace-jump-test-no-cl ()
  "Loading the package does not load the obsolete `cl' library."
  (should-not (featurep 'cl)))

(ert-deftest ace-jump-test-char-category ()
  "Characters are classified as digits, letters, punctuation or other."
  (with-temp-buffer
    (dolist (case '((?5 . digit) (?a . alpha) (?Z . alpha)
                    (?, . punc) (?\s . punc) (?\t . punc)
                    ;; letters of other scripts are word constituents
                    (?п . alpha) (?Ж . alpha)
                    ;; printable punctuation beyond ASCII
                    (?« . punc) (?— . punc) (?№ . punc)
                    (?\C-a . other) (?\e . other) (?\d . other)))
      (should (equal (cons (car case) (ace-jump-char-category (car case)))
                     case)))
    ;; M-a is not a character at all
    (should (eq (ace-jump-char-category (+ ?a (ash 1 27))) 'other))))

;;;; Searching candidates

(ert-deftest ace-jump-test-candidate-at-end-of-buffer ()
  "A match that ends at the end of the buffer is a candidate."
  (ace-jump-test-with-buffer "abc xyz"
    (should (equal (ace-jump-test-candidates "z") '(7)))
    (should (equal (ace-jump-test-candidates "\\<x") '(5)))))

(ert-deftest ace-jump-test-line-candidates ()
  "Line mode marks each line, but not the empty one after a final newline."
  (ace-jump-test-with-buffer "a\nb\nc\n"
    (should (equal (ace-jump-test-candidates "^") '(1 3 5))))
  (ace-jump-test-with-buffer "a\nb\nc"
    (should (equal (ace-jump-test-candidates "^") '(1 3 5)))))

(ert-deftest ace-jump-test-candidates-before-window-end ()
  "A match that starts at `window-end' or later is out of view."
  ;; lines "ab" start at 1, 4, 7, 10...; the window shows up to 10
  (ace-jump-test-with-buffer (mapconcat #'identity (make-list 10 "ab") "\n")
    (cl-letf (((symbol-function 'window-end) (lambda (&rest _) 10)))
      (should (equal (ace-jump-test-candidates "a") '(1 4 7)))
      (should (equal (ace-jump-test-candidates "b") '(2 5 8)))
      (should (equal (ace-jump-test-candidates "^") '(1 4 7))))))

(ert-deftest ace-jump-test-invisible-candidates ()
  "Invisible text holds no candidates, unless `ace-jump-allow-invisible'."
  (ace-jump-test-with-buffer "a1 a2 a3"
    (put-text-property 4 6 'invisible t)
    (should (equal (ace-jump-test-candidates "a") '(1 7)))
    (let ((ace-jump-allow-invisible t))
      (should (equal (ace-jump-test-candidates "a") '(1 4 7))))))

(ert-deftest ace-jump-test-search-filter ()
  "`ace-jump-search-filter' drops the candidates it returns nil for."
  (ace-jump-test-with-buffer "a1 a2 a3"
    (let ((ace-jump-search-filter (lambda () (/= (match-beginning 0) 4))))
      (should (equal (ace-jump-test-candidates "a") '(1 7))))))

(ert-deftest ace-jump-test-case-fold ()
  "`ace-jump-mode-case-fold' decides whether case is ignored."
  (ace-jump-test-with-buffer "a A"
    (should (equal (ace-jump-test-candidates "a") '(1 3)))
    (let ((ace-jump-mode-case-fold nil))
      (should (equal (ace-jump-test-candidates "a") '(1))))))

;;;; Entering AceJump

(ert-deftest ace-jump-test-submode-by-prefix ()
  "`ace-jump-mode' picks the submode by the prefix argument."
  (dolist (case '((1 . ace-jump-word-mode)
                  (4 . ace-jump-char-mode)
                  (16 . ace-jump-line-mode)))
    (ace-jump-test-with-buffer "ab ab\nab"
      (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?a)))
        (let ((current-prefix-arg (car case)))
          (call-interactively #'ace-jump-mode)))
      (should (equal (cons (car case) ace-jump-current-mode) case)))))

(ert-deftest ace-jump-test-or-line-modes ()
  "RET as the query char starts line mode, any other char or word mode."
  (dolist (case '((ace-jump-char-or-line-mode ?a ace-jump-char-mode)
                  (ace-jump-char-or-line-mode ?\r ace-jump-line-mode)
                  (ace-jump-word-or-line-mode ?a ace-jump-word-mode)
                  (ace-jump-word-or-line-mode ?\r ace-jump-line-mode)))
    (ace-jump-test-with-buffer "a1 a2\nb3\nc4"
      (funcall (nth 0 case) (nth 1 case))
      (should (equal (list (nth 0 case) (nth 1 case) ace-jump-current-mode)
                     case)))))

(ert-deftest ace-jump-test-or-line-modes-by-typing ()
  "The same, typed; in a graphical frame RET is the `return' event."
  (ace-jump-test-with-buffer "a1 a2\nb3\nc4"
    (ace-jump-test-type [f8 return ?b])
    (should (= (point) 7))
    (goto-char (point-min))
    (ace-jump-test-type [f9 ?\r ?c])
    (should (= (point) 10))
    (goto-char (point-min))
    (ace-jump-test-type [f9 ?a ?b])
    (should (= (point) 4))))

(ert-deftest ace-jump-test-default-ret-starts-line-mode ()
  "With the default submodes, RET as the head char starts line mode."
  (ace-jump-test-with-buffer "a1 a2\nb3\nc4"
    (ace-jump-test-type [f10 return ?c])
    (should (= (point) 10))))

(ert-deftest ace-jump-test-word-mode-without-query-char ()
  "Without a head char to ask for, word mode is a mode all the same.
So starting it again while it runs replaces the jump in progress
instead of leaving its labels behind."
  (ace-jump-test-with-buffer "one two three"
    (let ((ace-jump-word-mode-use-query-char nil))
      (call-interactively #'ace-jump-word-mode)
      (should (eq ace-jump-current-mode 'ace-jump-word-mode))
      (should (equal ace-jump-mode " AceJump - Word"))
      (should (equal (mapcar #'car (ace-jump-test-labels)) '(1 5 9)))
      ;; started again from Lisp: no key gets through while it runs
      (ace-jump-word-mode nil)
      (ace-jump-done)
      (should-not (overlays-in (point-min) (point-max))))))

(ert-deftest ace-jump-test-word-or-line-without-query-char ()
  "Without a head char to ask for, word or line mode marks all words."
  (ace-jump-test-with-buffer "a1 a2\nb3\nc4"
    (let ((ace-jump-word-mode-use-query-char nil))
      (cl-letf (((symbol-function 'read-char)
                 (lambda (&rest _) (error "No head char to read"))))
        (call-interactively #'ace-jump-word-or-line-mode))
      (should (equal (mapcar #'car (ace-jump-test-labels)) '(1 4 7 10))))))

(ert-deftest ace-jump-test-labels ()
  "Each candidate gets a label from `ace-jump-mode-move-keys', in order."
  (ace-jump-test-with-buffer "a1 a2 a3"
    (ace-jump-char-mode ?a)
    (should (equal (ace-jump-test-labels) '((1 . ?a) (4 . ?b) (7 . ?c))))
    (should (eq ace-jump-current-mode 'ace-jump-char-mode))
    (should (equal ace-jump-mode " AceJump - Char"))
    (should overriding-local-map)))

(ert-deftest ace-jump-test-gray-background ()
  "The window is grayed while labels are shown, unless turned off."
  (ace-jump-test-with-buffer "a1 a2 a3"
    (ace-jump-char-mode ?a)
    (should (= (length ace-jump-background-overlay-list) 1))
    (should (eq (overlay-get (car ace-jump-background-overlay-list) 'face)
                'ace-jump-face-background))
    (ace-jump-test-reset)
    (let ((ace-jump-mode-gray-background nil))
      (ace-jump-char-mode ?a)
      (should-not ace-jump-background-overlay-list))))

(ert-deftest ace-jump-test-label-padding ()
  "A label takes the width of the character it covers."
  ;; a wide character
  (ace-jump-test-with-buffer "中x中"
    (ace-jump-char-mode ?中)
    (should (equal (ace-jump-test-displays) '((1 . "a ") (3 . "b ")))))
  ;; the label replaces the character; on an empty line, the newline
  ;; is kept
  (ace-jump-test-with-buffer "x\n\ny"
    (ace-jump-line-mode)
    (should (equal (ace-jump-test-displays)
                   '((1 . "a") (3 . "b\n") (4 . "c"))))))

(ert-deftest ace-jump-test-tab-label-padding ()
  "A label on a tab is padded by the tab width of the tab's own buffer."
  (ace-jump-test-with-buffer "\tx\n\ty"
    (setq tab-width 8)
    (let ((tabs (current-buffer))
          (ace-jump-mode-scope 'frame)
          (other (generate-new-buffer " *ace-jump-test-other*")))
      (unwind-protect
          (progn
            (select-window (split-window))
            (switch-to-buffer other)
            (setq tab-width 4)
            (ace-jump-char-mode ?\t)
            (with-current-buffer tabs
              (should (equal (ace-jump-test-displays)
                             '((1 . "a       ") (4 . "b       "))))))
        (ace-jump-test-reset)
        (kill-buffer other)))))

(ert-deftest ace-jump-test-non-ascii-query ()
  "Letters of any script start a word jump; other printable characters
a char jump."
  (ace-jump-test-with-buffer "привет пока спать «да» — «нет»"
    (ace-jump-word-mode ?п)
    (should (eq ace-jump-current-mode 'ace-jump-word-mode))
    ;; word starts only: not the "п" inside "спать"
    (should (equal (mapcar #'car (ace-jump-test-labels)) '(1 8)))
    (ace-jump-test-reset)
    (ace-jump-word-mode ?«)
    (should (eq ace-jump-current-mode 'ace-jump-char-mode))
    (should (equal (mapcar #'car (ace-jump-test-labels))
                   (ace-jump-test-find-all "«")))))

(ert-deftest ace-jump-test-quick-exchange ()
  "C-c C-c switches between char and word mode with the same query char."
  (ace-jump-test-with-buffer "ab ba ab"
    (ace-jump-char-mode ?a)
    (should (equal (mapcar #'car (ace-jump-test-labels)) '(1 5 7)))
    (call-interactively (key-binding (kbd "C-c C-c") t))
    (should (eq ace-jump-current-mode 'ace-jump-word-mode))
    (should (equal (mapcar #'car (ace-jump-test-labels)) '(1 7)))
    (call-interactively (key-binding (kbd "C-c C-c") t))
    (should (eq ace-jump-current-mode 'ace-jump-char-mode))))

;;;; Jumping

(ert-deftest ace-jump-test-jump-by-label ()
  "Typing a label jumps there and leaves AceJump."
  (ace-jump-test-with-buffer "a1 a2 a3"
    (ace-jump-char-mode ?a)
    (ace-jump-test-press ?c)
    (should (= (point) 7))
    (should-not (overlays-in (point-min) (point-max)))
    (should-not overriding-local-map)
    (should-not ace-jump-current-mode)
    (should-not ace-jump-query-char)))

(ert-deftest ace-jump-test-jump-by-typing ()
  "The same, typed through the command loop."
  (ace-jump-test-with-buffer "a1 a2 a3"
    (ace-jump-test-type [f5 ?a ?b])
    (should (= (point) 4))))

(ert-deftest ace-jump-test-two-level-labels ()
  "With more candidates than move keys, a first key narrows the choice."
  (ace-jump-test-with-buffer "a a a a a"
    (let ((ace-jump-mode-move-keys '(?x ?y ?z)))
      (ace-jump-char-mode ?a)
      ;; 5 candidates, 3 keys: x leads to the first three
      (should (equal (mapcar #'cdr (ace-jump-test-labels)) '(?x ?x ?x ?y ?z)))
      (ace-jump-test-press ?x)
      (should (equal (ace-jump-test-labels) '((1 . ?x) (3 . ?y) (5 . ?z))))
      (should ace-jump-current-mode)
      (ace-jump-test-press ?y)
      (should (= (point) 3))
      (should-not ace-jump-current-mode))))

(ert-deftest ace-jump-test-no-such-label ()
  "A move key with no candidate leaves AceJump where it was."
  (ace-jump-test-with-buffer "a1 a2 a3"
    (ace-jump-char-mode ?a)
    (ace-jump-test-press ?z)
    (should (= (point) 1))
    (should-not ace-jump-current-mode)
    (should-not overriding-local-map)))

(ert-deftest ace-jump-test-line-mode-keeps-column ()
  "A line jump keeps the column, as C-n and C-p do."
  (ace-jump-test-with-buffer "hello\nworld\nagain"
    (goto-char 4)
    (ace-jump-line-mode)
    (ace-jump-test-press ?c)
    (should (= (point) 16))))

(ert-deftest ace-jump-test-single-candidate ()
  "With one candidate, jump at once, without entering AceJump mode."
  (ace-jump-test-with-buffer "abc xyz"
    (let* ((ended nil)
           (ace-jump-mode-end-hook (list (lambda () (setq ended t)))))
      (ace-jump-char-mode ?y)
      (should (= (point) 6))
      (should ended)
      (should-not ace-jump-current-mode)
      (should-not ace-jump-query-char)
      (should-not overriding-local-map))))

(ert-deftest ace-jump-test-no-candidate ()
  "With no candidate, signal an error and keep no state."
  (ace-jump-test-with-buffer "abc"
    (should-error (ace-jump-char-mode ?q))
    (should-not ace-jump-current-mode)
    (should-not ace-jump-query-char)))

(ert-deftest ace-jump-test-failed-jump-cleans-up ()
  "If the jump signals an error, AceJump is left all the same."
  (ace-jump-test-with-buffer "a1 a2 a3"
    (let* ((ended nil)
           (ace-jump-mode-before-jump-hook (list (lambda () (error "Boom"))))
           (ace-jump-mode-end-hook (list (lambda () (setq ended t)))))
      ;; chosen by a label
      (ace-jump-char-mode ?a)
      (should-error (ace-jump-test-press ?b))
      (should-not (overlays-in (point-min) (point-max)))
      (should-not overriding-local-map)
      (should-not ace-jump-current-mode)
      ;; a single candidate, jumped to at once
      (should-error (ace-jump-char-mode ?3))
      (should-not ace-jump-current-mode)
      (should-not ace-jump-query-char)
      ;; the end hook runs after a successful jump only
      (should-not ended))))

(ert-deftest ace-jump-test-other-window-on-same-buffer ()
  "A jump leaves another window on the same buffer where it was."
  (ace-jump-test-with-buffer "alpha beta gamma delta\nepsilon zeta eta theta\n"
    (let ((this (selected-window))
          (that (split-window)))
      (set-window-point that 30)
      (ace-jump-char-mode ?z)
      (should (= (window-point this) 32))
      (should (= (window-point that) 30))
      (ace-jump-char-mode ?e)
      (ace-jump-test-press ?b)
      (should (= (window-point that) 30)))))

;;;; Keys while AceJump runs

(ert-deftest ace-jump-test-other-key-stops ()
  "A key that is no label stops AceJump and is not run itself."
  (ace-jump-test-with-buffer "a1 a2 a3"
    ;; "1" stops AceJump, then "x" is typed as usual
    (ace-jump-test-type [f5 ?a ?1 ?x])
    (should (equal (buffer-string) "xa1 a2 a3"))))

(ert-deftest ace-jump-test-c-c-keys-stop ()
  "Any C-c key but C-c C-c stops AceJump instead of running its command."
  (ace-jump-test-with-buffer "a1 a2 a3"
    (let ((ran nil))
      (ace-jump-test-with-global-key (kbd "C-c p")
          (lambda () (interactive) (setq ran t))
        (ace-jump-test-type [f5 ?a] (kbd "C-c p") [?b]))
      (should-not ran)
      ;; AceJump was stopped, so "b" was typed, not taken as a label
      (should (= (point) 2))
      (should (equal (buffer-string) "ba1 a2 a3")))))

(ert-deftest ace-jump-test-language-change ()
  "Switching the keyboard layout neither stops AceJump nor selects a label."
  (ace-jump-test-with-buffer "a1 a2 a3"
    ;; as w32-win.el binds it on MS-Windows
    (ace-jump-test-with-global-key [language-change] #'ignore
      (ace-jump-test-type [f5 ?a language-change ?b]))
    (should (= (point) 4))))

;;;; Labels typed on another keyboard layout

(ert-deftest ace-jump-test-translate-by-function-key-map ()
  "By default, a label is translated by `local-function-key-map'."
  (ace-jump-test-with-buffer "a1 a2 a3"
    (let ((ace-jump-translate-key-function
           #'ace-jump-translate-key-by-function-key-map)
          (local-function-key-map (make-sparse-keymap)))
      ;; what `reverse-input-method' would put there
      (define-key local-function-key-map [?и] [?b])
      (ace-jump-test-type [f5 ?a ?и])
      (should (= (point) 4)))))

(ert-deftest ace-jump-test-translate-by-own-function ()
  "`ace-jump-translate-key-function' may be any function."
  (ace-jump-test-with-buffer "a1 a2 a3"
    (let ((ace-jump-translate-key-function
           (lambda (event) (cdr (assq event '((?ф . ?a) (?с . ?c)))))))
      (ace-jump-test-type [f5 ?a ?с])
      (should (= (point) 7)))))

(ert-deftest ace-jump-test-translate-to-no-label ()
  "A key not translated to a move key stops AceJump."
  (dolist (fn (list nil
                    (lambda (_) nil)
                    (lambda (_) 'foo)
                    (lambda (_) ?№)))
    (ace-jump-test-with-buffer "a1 a2 a3"
      (let ((ace-jump-translate-key-function fn))
        (ace-jump-char-mode ?a)
        (ace-jump-test-press ?и)
        (should (= (point) 1))
        (should-not ace-jump-current-mode)))))

;;;; The mark

(ert-deftest ace-jump-test-jump-sets-mark ()
  "A jump sets the mark where it started, as other long motions do."
  (ace-jump-test-with-buffer "a1 a2 a3"
    (let ((transient-mark-mode t))
      (goto-char 2)
      (ace-jump-char-mode ?a)
      (ace-jump-test-press ?c)
      (should (= (point) 7))
      (should (= (mark t) 2))
      (should-not (region-active-p)))))

(ert-deftest ace-jump-test-jump-extends-active-region ()
  "With the region active, a jump leaves the mark alone, as isearch does.
So the region stretches from where it was started to where the jump
lands."
  (ace-jump-test-with-buffer "a1 a2 a3 xyz"
    (let ((transient-mark-mode t))
      (set-mark 1)
      (goto-char 4)
      ;; chosen by a label
      (ace-jump-char-mode ?a)
      (ace-jump-test-press ?c)
      (should (= (point) 7))
      (should (= (mark t) 1))
      (should (region-active-p))
      ;; a single candidate, jumped to at once
      (ace-jump-char-mode ?y)
      (should (= (point) 11))
      (should (= (mark t) 1))
      (should (region-active-p))
      ;; the jumps are still remembered for jumping back
      (ace-jump-mode-pop-mark)
      (should (= (point) 7)))))

(ert-deftest ace-jump-test-jump-sets-mark-without-transient-mark-mode ()
  "Without Transient Mark mode there is no active region to keep."
  (ace-jump-test-with-buffer "a1 a2 a3"
    (let ((transient-mark-mode nil))
      (set-mark 1)
      (goto-char 4)
      (ace-jump-char-mode ?a)
      (ace-jump-test-press ?c)
      (should (= (point) 7))
      (should (= (mark t) 4)))))

;;;; Jumping back

(ert-deftest ace-jump-test-pop-mark ()
  "`ace-jump-mode-pop-mark' goes back through the jumps."
  (ace-jump-test-with-buffer "a1 a2 a3"
    (ace-jump-char-mode ?a)
    (ace-jump-test-press ?c)
    (ace-jump-char-mode ?a)
    (ace-jump-test-press ?b)
    (should (= (point) 4))
    (ace-jump-mode-pop-mark)
    (should (= (point) 7))
    (ace-jump-mode-pop-mark)
    (should (= (point) 1))))

(ert-deftest ace-jump-test-pop-mark-after-line-jump ()
  "Jumping back restores the position, not the column of a line jump."
  (ace-jump-test-with-buffer "hello world"
    (goto-char 8)
    ;; a single line: jumped to at once, column kept
    (ace-jump-line-mode)
    (should (= (point) 8))
    (goto-char 3)
    (ace-jump-mode-pop-mark)
    (should (= (point) 8))))

(ert-deftest ace-jump-test-mark-sync-advice ()
  "Syncing with the Emacs mark ring adds and removes its advice."
  (unwind-protect
      (progn
        (ace-jump-mode-enable-mark-sync)
        (should ace-jump-sync-emacs-mark-ring)
        (should (advice-member-p #'ace-jump-pop-mark-advice 'pop-mark))
        (should (advice-member-p #'ace-jump-pop-global-mark-advice
                                 'pop-global-mark)))
    (ace-jump-mode-disable-mark-sync))
  (should-not ace-jump-sync-emacs-mark-ring)
  (should-not (advice-member-p #'ace-jump-pop-mark-advice 'pop-mark))
  (should-not (advice-member-p #'ace-jump-pop-global-mark-advice
                               'pop-global-mark)))

(ert-deftest ace-jump-test-move-to-end-if ()
  "The mark ring helpers move matching elements to the end of a list."
  (let ((two-p (lambda (x) (= x 2))))
    (should (equal (ace-jump-move-to-end-if '(1 2 3 2 4) two-p)
                   '(1 3 4 2 2)))
    (should (equal (ace-jump-move-first-to-end-if '(1 2 3 2 4) two-p)
                   '(1 3 2 4 2)))))

;;; ace-jump-mode-test.el ends here
