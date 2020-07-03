;;; smart-input-source.el --- Switch OS native input source smartly -*- lexical-binding: t; -*-

;; URL: https://github.com/laishulu/emacs-smart-input-source
;; Created: March 27th, 2020
;; Keywords: convenience
;; Package-Requires: ((names "0.5") (emacs "25") (terminal-focus-reporting "0.0"))
;; Version: 1.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provide modes to switch OS native input source smartly.
;; For more information see the README in the GitHub repo.

;;; Code:

;; `define-namespace' is autoloaded, so there's no need to require `names'.
;; However, requiring it here means it will also work for people who don't
;; install through package.el.
(eval-when-compile (require 'names))
(require 'subr-x)

;;;###autoload
(define-namespace smart-input-source-


(defvar external-ism "macism"
  "Path of external ism.")

(defvar do-get nil
  "Function to get the current input source.

Should return a string which is the id of the input source.")

(defvar do-set nil
  "Function to set the input source.

Should accept a string which is the id of the input source.")

(defvar english-pattern "[a-zA-Z]"
  "Pattern to identify a character as english.")

(defvar english "com.apple.keylayout.US"
  "Input source for english.")

(defvar default-cursor-color nil
  "Default cursor color, used for English.

`nil' means obtained from the envrionment.")

(defvar other-cursor-color "green"
  "Cursor color for other language.")

(defvar cursor-color-seconds 0.5
  "Idle timer interval to update cursor color.

`nil' to disable the timer.")

(defvar fixed-context nil
  "Context is fixed to a specific language.

Possible values:
nil: dynamic context
'english: English context
'other: other language context.")
(make-variable-buffer-local 'smart-input-source-fixed-context)

(defvar with-english t
  "Switch to english when `global-respect-mode' enabled.")

(defvar with-evil-normal-escape t
  "<escape> to english in normal state when `global-respect-mode' enabled.")

(defvar with-prefix-and-buffer t
  "Preserve buffer input source when `global-respect-mode' enabled.")

(defvar other-pattern "\\cc"
  "Pattern to identify a character as other lang.")
(make-variable-buffer-local 'smart-input-source-other-pattern)

(defvar blank-pattern "[:blank:]"
  "Pattern to identify a character as blank.")
(make-variable-buffer-local 'smart-input-source-blank-pattern)

(defvar other "com.sogou.inputmethod.sogou.pinyin"
  "Input source for other lang.")
(make-variable-buffer-local 'smart-input-source-other)

(defvar set-english-hook nil
  "Hook to run after set input source to English.")

(defvar set-other-hook nil
  "Hook to run after set input source to other language.")

(defvar aggressive-line t
  "Aggressively detect context across blank lines.")
(make-variable-buffer-local 'smart-input-source-aggressive-line)

(defvar preserve-save-triggers
  (list 'counsel-M-x
        'next-buffer 'previous-buffer
        'other-window 'windmove-last
        'ace-window
        'windmove-up 'windmove-down 'windmove-left 'windmove-right
        'centaur-tabs-forward 'centaur-tabs-backward 'centaur-tabs-do-select)
  "Triggers to save the input source for buffer.")

(defvar preserve-save-hooks
  (list 'focus-out-hook)
  "Hooks to save the input source for buffer.")

(defvar preserve-restore-hooks
  (list 'focus-in-hook)
  "Hooks to restore the input source from buffer.")

(defvar prefix-override-keys
  '("C-c" "C-x" "C-h")
  "Prefix keys to be overrided.")

(defvar prefix-override-recap-triggers
  '(evil-local-mode yas-minor-mode)
  "Commands trigger the recap of the prefix override.

Some functions take precedence of the override, need to recap after.")

(defvar follow-context-hooks
  '(evil-insert-state-entry-hook)
  "Hooks trigger the set of input source following context.")

(defface inline-english-face
  '()
  "Face of the inline english region overlay."
  :group 'smart-input-source)

(set-face-attribute
 'smart-input-source-inline-english-face nil
 :foreground (face-attribute 'font-lock-constant-face :foreground)
 :inverse-video t)

;;
;; Following symbols are not supposed to be used directly by end user.
;;

(declare-function evil-normal-state-p "ext:evil-states.el" (&optional state) t)
(declare-function evil-visual-state-p "ext:evil-states.el" (&optional state) t)
(declare-function evil-motion-state-p "ext:evil-states.el" (&optional state) t)
(declare-function evil-operator-state-p
                  "ext:evil-states.el" (&optional state) t)
(declare-function company--active-p "ext:company.el" () t)
(declare-function mac-input-source "ext:macfns.c" (&optional SOURCE FORMAT) t)
(declare-function mac-select-input-source "ext:macfns.c"
                  (SOURCE &optional SET-KEYBOARD-LAYOUT-OVERRIDE-P) t)

;; Following codes are mainly about cursor color mode

(defvar -current nil
  "Current input source.")

(defvar -previous nil
  "Previous input source.")

(defun -set-cursor-color-advice (fn color)
  "Advice for FN of `set-cursor-color'.

The advice is needed, because other packages may set cursor color in their only
way."
  (pcase -current
    ('english
     (funcall fn default-cursor-color))
    ('other
     (funcall fn other-cursor-color))
    (unknown
     (funcall fn color))))

(defun -update-cursor-color()
  "Update cursor color according to input source."
  (-get)

  (unless (eq -current -previous)
    (setq -previous -current)

    ;; for GUI
    (when (display-graphic-p)
      ;; actually which color passed to the function does not matter,
      ;; the advice will take care of it.
      (set-cursor-color default-cursor-color))

    ;; for TUI
    (unless (display-graphic-p)
      (pcase -current
        ('english
         (send-string-to-terminal (format "\e]12;%s\a" default-cursor-color)))
        ('other
         (send-string-to-terminal (format "\e]12;%s\a" other-cursor-color)))))))

(defvar -cursor-color-timer nil
  "Timer for `-cursor-color-timer-function'.")

(defun -cursor-color-timer-function ()
  "Update cursor color on idle timer."
  (when -cursor-color-timer
    (cancel-timer -cursor-color-timer))
  (-update-cursor-color)
  (setq -cursor-color-timer
        (run-with-idle-timer
         ;; every time the wait period increases by cursor-color-seconds
         (time-add (current-idle-time)
                   (* cursor-color-seconds -cursor-color-timer-count))
         nil
         #'-cursor-color-timer-function))
  (setq -cursor-color-timer-count (1+ -cursor-color-timer-count)))

(defvar -cursor-color-timer-count 0
  "Execution count of `-cursor-color-timer-function' in this idle period.")

(defun -cursor-color-timer-restart ()
  "Restart `-cursor-color-timer'."
  (setq -cursor-color-timer-count 0)
  (-cursor-color-timer-function))

:autoload
(define-minor-mode global-cursor-color-mode
  "Automaticly change cursor color according to input source."
  :global t
  :init-value nil
  (cond
   (; turn on the mode
    global-cursor-color-mode
    ;; save original cursor color
    (unless default-cursor-color
      (setq default-cursor-color
            (or (when (display-graphic-p)
                  (or (cdr (assq 'cursor-color default-frame-alist))
                      (face-background 'cursor)))
                "white")))
    (advice-add 'set-cursor-color :around #'-set-cursor-color-advice)
    (add-hook 'smart-input-source-set-english-hook #'-update-cursor-color)
    (add-hook 'smart-input-source-set-other-hook #'-update-cursor-color)
    (when cursor-color-seconds
      (run-with-idle-timer cursor-color-seconds t
                           #'-cursor-color-timer-restart)))
   (; turn off the mode
    (not global-cursor-color-mode)
    (advice-remove 'set-cursor-color #'-set-cursor-color-advice)
    (remove-hook 'smart-input-source-set-english-hook
                 #'-update-cursor-color)
    (remove-hook 'smart-input-source-set-other-hook
                 #'-update-cursor-color)
    (when -cursor-color-timer (cancel-timer -cursor-color-timer)))))

;;
;; Following codes are mainly about input source manager
;;
(defvar -ism nil "The input source manager.")
(defvar -ism-inited nil "Input source manager initialized.")

(defun -init-ism ()
  "Init input source manager."
  ;; EMP
  (when (and (string= (window-system) "mac")
             (fboundp 'mac-input-source))
    (setq -ism 'emp))

  ;; external ism
  (when (and (not -ism) external-ism)
    (let ((ism-path (executable-find external-ism)))
      (setq -ism ism-path)))

  ;; make `do-set' and `do-get'
  (when -ism
    ;; avoid override user customized do-get
    (unless (functionp do-get)
      (setq do-get (-mk-get-fn)))

    ;; avoid override user customized do-set
    (unless (functionp do-set)
      (setq do-set (-mk-set-fn))))

  ;; successfully inited
  (when (and (functionp 'do-get)
             (functionp 'do-set))
    ;; a t `-ism' means customized by `do-get' and `do-set'
    (unless -ism (setq -ism t)))

  ;; just inited, successfully or not
  (setq -ism-inited t))

(defmacro -ensure-ism (&rest body)
  "Only run BODY with valid ism."
  `(progn
     (unless smart-input-source--ism-inited
       (smart-input-source--init-ism))
     (when smart-input-source--ism
       ,@body)))

(defun -mk-get-fn ()
  "Make a function to be bound to `do-get'."
  (if (equal -ism 'emp)
      #'mac-input-source
    (lambda ()
      (string-trim (shell-command-to-string -ism)))))

(defun -mk-set-fn ()
  "Make a function to be bound to `do-set'."
  (if (equal -ism 'emp)
      (lambda (source) (mac-select-input-source source))
    (lambda (source)
      (string-trim
       (shell-command-to-string (concat -ism " " source))))))

(defun -get ()
  "Get the input source id."
  (when (functionp do-get)
    (let ((source (funcall do-get)))
      (pcase source
        ((pred (equal english))
         (setq -current 'english))
        ((pred (equal other))
         (setq -current 'other)))
      source)))

(defun -set (lang)
  "Set the input source according to lang LANG.

Unnecessary switching is avoided internally."
  (when (and lang (functionp do-set))
    ;; swith only when required
    (pcase (-get)
      (; set to english
       (pred (equal english))
       (when (member lang (list 'other other))
         (setq -current 'other)
         (funcall do-set other)))
      (; set to other
       (pred (equal other))
       (when (member lang (list 'english english))
         (setq -current 'english)
         (funcall do-set english))))

    ;; run hook whether switched or not
    (if (member lang (list 'other other))
        (run-hooks 'smart-input-source-set-other-hook)
      (run-hooks 'smart-input-source-set-english-hook)))
  (when log-mode (message (format "Do set input source: [%s]" lang))))

:autoload
(defun set-english ()
  "Set input source to `english'."
  (interactive)
  (-ensure-ism (-set 'english)))

:autoload
(defun set-other ()
  "Set input source to `other'."
  (interactive)
  (-ensure-ism (-set 'other)))

:autoload
(defun switch ()
  "Switch input source between english and other."
  (interactive)
  (-ensure-ism
   (pcase (-get)
     (; current is english
      (pred (equal english))
      (funcall do-set other)
      (run-hooks 'smart-input-source-set-other-hook)
      other)
     (; current is other
      (pred (equal other))
      (funcall do-set english)
      (run-hooks 'smart-input-source-set-english-hook)
      other))))

;;
;; Following codes are mainly about respect mode
;;

(defvar -for-buffer nil
  "Saved buffer input source.")
(make-variable-buffer-local 'smart-input-source--for-buffer)

(defun -save-to-buffer ()
  "Save buffer input source."
  (setq -for-buffer (-get)))

(defun -restore-from-buffer ()
  "Restore buffer input source."
  (-set (or -for-buffer 'english)))

(defvar -prefix-override-map-enable nil
  "Enabe the override keymap.")

(defvar -prefix-override-map-alist nil
  "Map alist for override.")

(defvar -prefix-handle-stage 'normal
  "Processing state of the prefix key.

Possible values: 'normal, 'prefix, 'sequence.")

(defvar -buffer-before-prefix nil
  "Current buffer before prefix.")

(defvar -buffer-before-command nil
  "Current buffer before prefix.")

(defvar -real-this-command nil
  "Real this command. Some commands overwrite it.")

(defun -prefix-override-recap-advice (&rest res)
  "Advice for `prefix-override-recap-triggers' with RES."
  (add-to-ordered-list
   'emulation-mode-map-alists
   'smart-input-source--prefix-override-map-alist
   1)
  res)

(defun -prefix-override-handler (arg)
  "Prefix key handler with ARG."
  (interactive "P")
  (let* ((keys (this-command-keys))
         (n (length keys))
         (key (aref keys (1- n))))
    ;; Don't record this command
    (setq this-command last-command)
    ;; Restore the prefix arg
    (setq prefix-arg arg)
    (prefix-command-preserve-state)
    ;; Push the key back on the event queue
    (setq unread-command-events (cons key unread-command-events))))

(defun -save-trigger-p (cmd)
  "CMD is a save trigger."
  (memq -real-this-command preserve-save-triggers))

(defun -preserve-save-handler ()
  "Handler for `preserve-save-hooks'"
  (when log-mode
    (message (format "Handle save hook, save [%s] to [%s]."
                     (-get) (current-buffer))))
  (unless (eq this-command 'mouse-drag-region)
    (-save-to-buffer)
    (set-english)))

(defun -preserve-restore-handler ()
  "Handler for `preserve-restore-hooks'"
  (when log-mode
    (message (format "Handle restore hook, restore [%s] from [%s] ."
                     -for-buffer (current-buffer))))
  (-restore-from-buffer))

(defun -preserve-pre-command-handler ()
  "Handler for `pre-command-hook' to preserve input source."
  (setq -buffer-before-command (current-buffer))
  (setq -real-this-command this-command)

  (when log-mode
    (message (format "pre@[%s]: [%s]@key [%s]@cmd [%s]@buf [%s]@override."
                     -prefix-handle-stage
                     (this-command-keys)
                     -real-this-command
                     (current-buffer)
                     -prefix-override-map-enable)))

  (pcase -prefix-handle-stage
    (; current is normal stage
     'normal
     (cond
      (; not prefix key
       (not (eq -real-this-command #'-prefix-override-handler))
       (when (and (not (-preserve-assume-english-p (current-buffer)))
                  (-save-trigger-p -real-this-command))
         (-save-to-buffer)
         (when log-mode
           (message (format "[%s] is a save trigger, save input source: [%s]."
                            -real-this-command -for-buffer)))))

      (; for prefix key
       (eq -real-this-command #'-prefix-override-handler)
       (when log-mode
         (message (format "[%s] is a prefix key, shortcut to pre@[prefix]."
                          (this-command-keys))))
       ;; go to pre@[prefix] directly
       (setq -prefix-handle-stage 'prefix)
       (-preserve-pre-command-handler))))
    (; current is prefix stage
     'prefix
     (setq -prefix-override-map-enable nil)
     (setq -buffer-before-prefix (current-buffer))
     (-save-to-buffer)
     (set-english)
     (when log-mode
       (message (format "Input source: [%s] (saved) => [%s]."
                        -for-buffer english))))
    (; current is sequence stage
     'sequence t)))

(defun -preserve-assume-english-p (&optional buffer)
  "BUFFER does not need input source preservation."
  (or (minibufferp)
      (and (-string-match-p "\*" (buffer-name buffer))
           (not (-string-match-p "\*New" (buffer-name buffer)))
           (not (-string-match-p "\*Scratch" (buffer-name buffer))))))

(defun -to-normal-stage (restore)
  "Transite to normal stage and restore input source if RESTORE is t."
  (when restore
    (when log-mode
      (message (format "restore: [%s]@[%s]." -for-buffer (current-buffer))))
    (-restore-from-buffer))
  (setq -prefix-override-map-enable t)
  (setq -prefix-handle-stage 'normal))

(defun -preserve-post-command-handler ()
  "Handler for `post-command-hook' to preserve input source."
  ;; (setq this-command -real-this-command)
  (when log-mode
    (message (format "post@[%s]: [%s]@key [%s]@cmd [%s]@buf [%s]@override."
                     -prefix-handle-stage
                     (this-command-keys)
                     -real-this-command
                     (current-buffer)
                     -prefix-override-map-enable)))

  (pcase -prefix-handle-stage
    (; current is prefix stage
     'prefix
     (setq -prefix-handle-stage 'sequence))
    (; current is sequence stage
     'sequence
     (cond
      (; still in progress
       (minibufferp)
       (setq -prefix-handle-stage 'sequence))
      (; key sequence is canceled
       (not -real-this-command)
       (when log-mode (message "Key sequence canceled."))
       (-to-normal-stage t)
       (when (and preserve-hint-mode
                  (not (-save-trigger-p -real-this-command)))
         (message
          (format "!! cmd [%s] switched %s to %s, add it to `save-triggers'\?"
                  -real-this-command
                  -buffer-before-command (current-buffer)))))

      (; end key sequence
       t
       (when log-mode (message "Key sequence ended."))
       (let ((restore (not (-preserve-assume-english-p (current-buffer)))))
         (-to-normal-stage restore)

         (when (and preserve-hint-mode restore
                    (not (-save-trigger-p -real-this-command)))
           (message
            (format "!! cmd [%s] switched %s to %s, add it to `save-triggers'\?"
                    -real-this-command
                    -buffer-before-command (current-buffer))))))))
    (; current is normal stage
     'normal
     (let ((restore (not (eq -buffer-before-command (current-buffer)))))
       (-to-normal-stage restore)

       (when (and preserve-hint-mode restore
                  (not (-save-trigger-p -real-this-command)))
         (message
          (format "!! cmd [%s] switched %s to %s, add it to `save-triggers'\?"
                  -real-this-command
                  -buffer-before-command (current-buffer))))))))

(define-minor-mode preserve-hint-mode
  "Hint to add command to related variables."
  :global t
  :init-value nil)

(define-minor-mode log-mode
  "Log the execution of this package."
  :global t
  :init-value nil)

:autoload
(define-minor-mode global-respect-mode
  "Respect buffer/mode by proper input source.

- Respect self: optional start this mode with English
- Respect ~evil~: switch to English when leaving ~evil~ ~insert~ mode.
- Respect prefix key: switch to English for ~C-c~/ ~C-x~/ ~C-h~.
- Respect buffer: recover buffer input source when it regain focus."
  :global t
  :init-value nil
  (cond
   (; turn on the mode
    global-respect-mode
    (-ensure-ism
     ;; set english when mode enabled
     (when with-english (set-english))

     (when with-prefix-and-buffer
       ;; set english when exit evil insert state
       (when (featurep 'evil)
         (add-hook 'evil-insert-state-exit-hook #'set-english)
         (when with-evil-normal-escape
           (define-key evil-normal-state-map (kbd "<escape>") #'set-english)))

       ;; preserve buffer input source
       (add-hook 'pre-command-hook #'-preserve-pre-command-handler)
       (add-hook 'post-command-hook #'-preserve-post-command-handler)

       ;; enable terminal focus event
       (unless (display-graphic-p)
         (require 'terminal-focus-reporting)
         (terminal-focus-reporting-mode t))

       (dolist (hook preserve-save-hooks)
         (add-hook hook #'-preserve-save-handler))
       (dolist (hook preserve-restore-hooks)
         (add-hook hook #'-preserve-restore-handler))

       ;; set english when prefix key pressed
       (setq -prefix-override-map-alist
             `((smart-input-source--prefix-override-map-enable
                .
                ,(let ((keymap (make-sparse-keymap)))
                   (dolist (prefix prefix-override-keys)
                     (define-key keymap
                       (kbd prefix) #'-prefix-override-handler))
                   keymap))))

       (setq -prefix-override-map-enable t)
       (-prefix-override-recap-advice)
       (dolist (trigger prefix-override-recap-triggers)
         (advice-add trigger :after #'-prefix-override-recap-advice)))))
   (; turn off the mode
    (not global-respect-mode)
    ;; for evil
    (when (featurep 'evil)
      (remove-hook 'evil-insert-state-exit-hook #'set-english)
      (when with-evil-normal-escape
        (define-key evil-normal-state-map (kbd "<escape>") nil)))

    (when with-prefix-and-buffer
      ;; for preserving buffer input source
      (remove-hook 'pre-command-hook #'-preserve-pre-command-handler)
      (remove-hook 'post-command-hook #'-preserve-post-command-handler)

      ;; for prefix key
      (setq emulation-mode-map-alists
            (delq 'smart-input-source--prefix-override-map-alist
                  emulation-mode-map-alists))
      (setq -prefix-override-map-enable nil)))))

;;
;; Following codes are mainly about follow-context-mode
;;

(defun -string-match-p (regexp str &optional start)
  "Robust wrapper of `string-match-p'.

Works when REGEXP or STR is not a string REGEXP, STR, START all has the same
meanings as `string-match-p'."
  (and (stringp regexp)
       (stringp str)
       (string-match-p regexp str start)))

(defun -english-p (str)
  "Predicate on STR is English."
  (-string-match-p english-pattern str))

(defun -not-english-p (str)
  "Predicate on STR is not English."
  (not (-string-match-p english-pattern str)))

(defun -other-p (str)
  "Predicate on STR is other language."
  (-string-match-p other-pattern str))

(defun -not-other-p (str)
  "Predicate on STR is not other language."
  (not (-string-match-p other-pattern str)))

(cl-defstruct back-detect ; result of backward detect
  to ; point after first non-blank char in the same line
  char ; first non-blank char at the same line (just before position `to')
  cross-line-to ; point after first non-blank char cross lines
  cross-line-char ; first non-blank char cross lines before the current position
  )

(defun -back-detect-chars ()
  "Detect char backward by two steps.

  First backward skip blank in the current line,
  then backward skip blank across lines."
  (save-excursion
    (skip-chars-backward blank-pattern)
    (let ((to (point))
          (char (char-before (point))))
      (skip-chars-backward (concat blank-pattern "[:cntrl:]"))
      (let ((cross-line-char (char-before (point))))
        (make-back-detect :to to
                          :char (when char (string char))
                          :cross-line-to (point)
                          :cross-line-char (when cross-line-char
                                             (string cross-line-char)))))))

(cl-defstruct fore-detect ; result of forward detect
  to ; point before first non-blank char in the same line
  char ; first non-blank char at the same line (just after position `to')
  cross-line-to ; point before first non-blank char cross lines
  cross-line-char ; first non-blank char cross lines after the current position
  )

(defun -fore-detect-chars ()
  "Detect char forward.

  Forward skip blank in the current line."
  (save-excursion
    (skip-chars-forward blank-pattern)
    (let ((to (point))
          (char (char-after (point))))
      (skip-chars-forward (concat blank-pattern "[:cntrl:]"))
      (let ((cross-line-char (char-after (point))))
        (make-fore-detect :to to
                          :char (when char (string char))
                          :cross-line-to (point)
                          :cross-line-char (when cross-line-char
                                             (string cross-line-char)))))))

(defun -guess-context ()
  "Guest the lang context for the current point."
  (let* ((back-detect (-back-detect-chars))
         (fore-detect (-fore-detect-chars))

         (back-to (back-detect-to back-detect))
         (back-char (back-detect-char back-detect))
         (cross-line-back-to (back-detect-cross-line-to back-detect))
         (cross-line-back-char (back-detect-cross-line-char back-detect))

         (fore-to (fore-detect-to fore-detect))
         (fore-char (fore-detect-char fore-detect))
         (cross-line-fore-to (fore-detect-cross-line-to fore-detect))
         (cross-line-fore-char (fore-detect-cross-line-char fore-detect)))

    (cond
     ;; context is fixed.
     (fixed-context fixed-context)

     ;; [line beginning][^][english]
     ;; [english][^][english]
     ;; [not english][blank][^][english]
     ((and (or (= back-to (line-beginning-position))
               (and (= back-to (point))
                    (-english-p back-char))
               (and (< back-to (point))
                    (-not-english-p back-char)))
           (< fore-to (line-end-position))
           (= fore-to (point))
           (-english-p fore-char))
      'english)

     ;; [english][^][blank][not english]
     ((and (and (> fore-to (point))
                (-not-english-p fore-char))
           (> back-to (line-beginning-position))
           (= back-to (point))
           (-english-p back-char))
      'english)

     ;; [:other lang:][^]
     ;; [^][:other lang:]
     ;; [:other lang:][:blank or not:][^][:blank or not:][:other lang:]
     ((or (and (= back-to (point))
               (-other-p back-char))
          (and (= fore-to (point))
               (-other-p fore-char))
          (and (-other-p back-char)
               (-other-p fore-char)))
      'other)

     ;; [english][^][line end]
     ((and (= back-to (point))
           (-english-p back-char)
           (= fore-to (line-end-position)))
      'english)

     ;; [english: include the previous line][blank][^]
     ((and (or aggressive-line
               (> cross-line-back-to (line-beginning-position 0)))
           (< cross-line-back-to (line-beginning-position))
           (-english-p cross-line-back-char))
      'english)

     ;; [other lang: include the previous line][blank][^]
     ((and (or aggressive-line
               (> cross-line-back-to (line-beginning-position 0)))
           (< cross-line-back-to (line-beginning-position))
           (-other-p cross-line-back-char))
      'other)

     ;; [^][blank][english: include the next line]
     ((and (or aggressive-line
               (< cross-line-fore-to (line-end-position 2)))
           (> cross-line-fore-to (line-end-position))
           (-english-p cross-line-fore-char))
      'english)

     ;; [^][blank][other lang: include the next line]
     ((and (or aggressive-line
               (< cross-line-fore-to (line-end-position 2)))
           (> cross-line-fore-to (line-end-position))
           (-other-p cross-line-fore-char))
      'other))))


;;;###autoload
(define-minor-mode follow-context-mode
  "Switch input source smartly according to context."
  :init-value nil
  (cond
   (; turn of the mode
    follow-context-mode
    (-ensure-ism
     (dolist (hook follow-context-hooks)
       (add-hook hook #'follow-context nil t))))
   (; turn off the mode
    (not follow-context-mode)
    (dolist (hook follow-context-hooks)
      (remove-hook hook #'follow-context nil t)))))

:autoload
(define-globalized-minor-mode
  smart-input-source-global-follow-context-mode
  follow-context-mode
  follow-context-mode)

(defun follow-context ()
  "Follow the context to switch input source."
  (let ((context (-guess-context)))
    (when context
      (-set context))))

;;
;; Following codes are mainly about the inline english region overlay
;;

(defvar -inline-overlay nil
  "The active inline overlay.")
(make-variable-buffer-local 'smart-input-source--inline-overlay)

(defun -inline-overlay-start ()
  "Start position of the inline overlay."
  (when -inline-overlay
    (overlay-start -inline-overlay)))

(defun -inline-overlay-end ()
  "End position of the inline overlay."
  (when -inline-overlay
    (overlay-end -inline-overlay)))

:autoload
(define-minor-mode inline-english-mode
  "English overlay mode for mixed language editing."
  :init-value nil
  (cond
   (; turn on the mode
    inline-english-mode
    (-ensure-ism
     (add-hook 'post-self-insert-hook #'check-to-activate-overlay nil t)))
   (; turn off the mode
    (not inline-english-mode)
    (remove-hook 'post-self-insert-hook #'check-to-activate-overlay t))))

:autoload
(define-globalized-minor-mode
  smart-input-source-global-inline-english-mode
  inline-english-mode
  inline-english-mode)

(defun check-to-activate-overlay()
  "Check whether to activate the inline english region overlay.

Check the context to determine whether the overlay should be activated or not,
if the answer is yes, then activate the /inline english region/, set the
input source to English."
  (when (and inline-english-mode
             (not (overlayp -inline-overlay))
             (not (button-at (point)))
             (not (and (featurep 'evil)
                       (or (evil-normal-state-p)
                           (evil-visual-state-p)
                           (evil-motion-state-p)
                           (evil-operator-state-p))))
             ;; around char is <spc> <DBC spc>
             (memq (preceding-char) (list ?\s 12288)))
    (let* ((back-detect (-back-detect-chars))
           (back-to (back-detect-to back-detect))
           (back-char (back-detect-char back-detect))
           (fore-detect (-fore-detect-chars))
           (fore-to (fore-detect-to fore-detect))
           (fore-char (fore-detect-char fore-detect)))

      (when (or
             ;; [other lang][:space:][^][:not none-english:]
             (and (> back-to (line-beginning-position))
                  (< back-to (point))
                  (-other-p back-char)
                  (not (and (< (1+ back-to) (point))
                            (= fore-to (point))
                            (-not-other-p back-char))))
             ;; [:not none-english:][^][:space:][other lang]
             (and (< fore-to (line-end-position))
                  (-other-p fore-char)
                  (not (and (> fore-to (point))
                            (-not-other-p back-char)))))
        (activate-inline-overlay (1- (point)))))))

:autoload
(defun activate-inline-overlay (start)
  "Activate the inline english region overlay from START."
  (interactive)
  (-ensure-ism
   (when (overlayp -inline-overlay)
     (delete-overlay -inline-overlay))

   (setq -inline-overlay (make-overlay start (point) nil t t ))
   (overlay-put -inline-overlay 'face 'smart-input-source-inline-english-face)
   (overlay-put -inline-overlay 'keymap
                (let ((keymap (make-sparse-keymap)))
                  (define-key keymap (kbd "RET")
                    #'ret-check-to-deactivate-inline-overlay)
                  (define-key keymap (kbd "<return>")
                    #'ret-check-to-deactivate-inline-overlay)
                  keymap))
   (add-hook 'post-command-hook #'fly-check-to-deactivate-inline-overlay nil t)
   (set-english)))

(defun fly-check-to-deactivate-inline-overlay ()
  "Check whether to deactivate the inline english region overlay."
  (interactive)
  (when (and inline-english-mode
             (overlayp -inline-overlay))
    ;; select input source
    (let* ((back-detect (-back-detect-chars))
           (back-to (back-detect-to back-detect)))
      (when (or
             ;; zero length overlay
             (= (-inline-overlay-start)
                (-inline-overlay-end))
             ;; out of range
             (or(< (point) (-inline-overlay-start))
                (> (point) (-inline-overlay-end)))
             ;; " inline english  ^"
             ;; but not "           ^"
             (and (= (point) (-inline-overlay-end))
                  (> back-to (-inline-overlay-start))
                  (= (+ 2 back-to) (point))))
        (deactivate-inline-overlay)))))

(defun ret-check-to-deactivate-inline-overlay ()
  "Deactivate the inline english region overlay."
  (interactive)
  (when (and inline-english-mode (overlayp -inline-overlay))
    ;; company
    (if (and (featurep 'company)
             (company--active-p))
        (company-complete-selection)
      (deactivate-inline-overlay))))

(defun deactivate-inline-overlay ()
  "Deactivate the inline english region overlay."
  (interactive)
  ;; clean up
  (remove-hook 'post-command-hook #'fly-check-to-deactivate-inline-overlay t)

  ;; select input source
  (let* ((back-detect (-back-detect-chars))
         (back-to (back-detect-to back-detect))
         (back-char (back-detect-char back-detect)))

    ;; [other lang][:blank inline overlay:]^
    ;; [:overlay with trailing blank :]^
    (when (or (and (= back-to (-inline-overlay-start))
                   (-other-p back-char))
              (and (> back-to (-inline-overlay-start))
                   (< back-to (-inline-overlay-end))
                   (< back-to (point))))
      (set-other))

    ;; only tighten for none-blank inline english region
    (when (and (<= (point) (-inline-overlay-end))
               (> back-to (-inline-overlay-start)))

      (save-excursion
        (goto-char (-inline-overlay-end))
        (let* ((tighten-back-detect (-back-detect-chars))
               (tighten-back-to (back-detect-to tighten-back-detect)))
          (when (and (< tighten-back-to (-inline-overlay-end))
                     (> tighten-back-to (-inline-overlay-start)))
            (delete-char -1))))

      (save-excursion
        (goto-char (-inline-overlay-start))
        (let* ((tighten-fore-detect (-fore-detect-chars))
               (tighten-fore-to (fore-detect-to tighten-fore-detect)))
          (when (> tighten-fore-to (-inline-overlay-start))
            (delete-char 1))))))
  (delete-overlay -inline-overlay)
  (setq -inline-overlay nil))


;; end of namespace
)

(provide 'smart-input-source)
;;; smart-input-source.el ends here
