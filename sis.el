;;; sis.el --- Switch native or OS input source (input method) smartly -*- lexical-binding: t; -*-

;; URL: https://github.com/laishulu/emacs-smart-input-source
;; Created: March 27th, 2020
;; Keywords: convenience
;; Package-Requires: ((emacs "25.1") (terminal-focus-reporting "0.0"))
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
(require 'subr-x)

(defvar sis-external-ism "macism"
  "Path of external ism.")

(defvar sis-do-get nil
  "Function to get the current input source.

Should return a string which is the id of the input source.")

(defvar sis-do-set nil
  "Function to set the input source.

Should accept a string which is the id of the input source.")

(defvar sis-english-pattern "[a-zA-Z]"
  "Pattern to identify a character as english.")

(defvar sis-english-source "com.apple.keylayout.US"
  "Input source for english.")

(defvar sis-other-pattern "\\cc"
  "Pattern to identify a character as other lang.")

(defvar sis-other-source "com.sogou.inputmethod.sogou.pinyin"
  "Input source for other lang.")

(defvar sis-blank-pattern "[:blank:]"
  "Pattern to identify a character as blank.")

(defvar sis-auto-refresh-seconds 0.2
  "Idle timer interval to auto refresh input source status from OS.

Emacs-nativ input method don't need it. nil to disable the timer.")

(defvar sis-change-hook nil
  "Hook to run when input source changes.")

(defvar sis-default-cursor-color nil
  "Default cursor color, used for English.

nil means obtained from the envrionment.")

(defvar sis-other-cursor-color "green"
  "Cursor color for other language.")

(defvar sis-respect-start 'english
  "Switch to specific input source when the /respect mode/ is enabled.")

(defvar sis-respect-evil-normal-escape t
  "<escape> to english in normal state when the /respect mode/ is enabled.")

(defvar sis-respect-prefix-and-buffer t
  "Preserve buffer input source when the /respect mode/ is enabled.")

(defvar sis-respect-go-english-triggers nil
  "Triggers to save input source to buffer and then go to english.")

(defvar sis-respect-restore-triggers nil
  "Triggers to restore the input source from buffer.")

(defvar sis-respect-dispatches
  (list 'magit-file-dispatch 'magit-dispatch)
  "Triggers for dispatchers.")

(defvar sis-prefix-override-keys
  (list "C-c" "C-x" "C-h")
  "Prefix keys to be overrided.")

(defvar sis-prefix-override-recap-triggers
  (list 'evil-local-mode 'yas-minor-mode)
  "Commands trigger the recap of the prefix override.

Some functions take precedence of the override, need to recap after.")

(defvar sis-follow-context-fixed nil
  "Context is fixed to a specific language in the /follow context mode/.

Possible values:
nil: dynamic context
'english: English context
'other: other language context.")

(defvar sis-follow-context-aggressive-line t
  "Aggressively detect context across blank lines.")

(defvar sis-follow-context-hooks
  '(evil-insert-state-entry-hook)
  "Hooks trigger the set of input source following context.")

(defvar sis-inline-english-activated-hook nil
  "Hook to run after inline english region activated.")

(defvar sis-inline-english-deactivated-hook nil
  "Hook to run after inline english region deactivated.")

(defvar sis-inline-other-activated-hook nil
  "Hook to run after inline other language region activated.")

(defvar sis-inline-other-deactivated-hook nil
  "Hook to run after inline other language region deactivated.")

(defface sis-inline-face
  '()
  "Face of the inline region overlay."
  :group 'sis)

(set-face-attribute
 'sis-inline-face nil
 :foreground (face-attribute 'font-lock-constant-face :foreground)
 :inverse-video t)

(defvar sis-inline-not-max-point t
  "Make sure there are other characters after inline region.

Insert new line when the whole buffer ends with the region, to avoid
autocomplete rendering a large area with the region background.")

(defvar sis-inline-tighten-head-rule 1
  "Rule to delete head spaces.

Possible values:
1: delete 1 space if exists
0: don't delete space
'all: delete all space.")

(defvar sis-inline-tighten-tail-rule 1
  "Rule to delete tail spaces.

Possible values:
1: delete 1 space if exists
0: don't delete space
'all: delete all space.")

(defvar sis-inline-single-space-close nil
  "Single space closes the inline region.")

(defvar sis-inline-with-english t
  "With the inline region.")

(defvar sis-inline-with-other nil
  "With the inline other lang region.")

;;;###autoload
(define-minor-mode sis-log-mode
  "Log the execution of this package."
  :global t
  :init-value nil)

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

(defun sis--do-nothing-advice (&rest _)
    "Advice to make existing function do nothing.")
;;
;; Following codes are mainly about input source manager
;;

(defvar sis--ism nil "The input source manager.")
(defvar sis--ism-inited nil "Input source manager initialized.")

(defvar sis--current nil
  "Current input source.")

(defvar sis--previous nil
  "Previous input source.")

(defvar sis--for-buffer nil
  "Saved buffer input source.")
(make-variable-buffer-local 'sis--for-buffer)

(defvar sis--for-buffer-locked nil
  "Buffer input source is locked.")
(make-variable-buffer-local 'sis--for-buffer-locked)

(defun sis--init-ism ()
  "Init input source manager."
  ;; `sis-do-get'and `sis-do-set' takes the first precedence.
  (unless (and (functionp sis-do-get)
               (functionp sis-do-set))
    ;; EMP
    (when (and (string= (window-system) "mac")
               (fboundp 'mac-input-source))
      ;; EMP
      (setq sis--ism 'emp))

    ;; external ism
    (when (and (not sis--ism) (stringp sis-external-ism))
      (let ((ism-path (executable-find sis-external-ism)))
        (when ism-path (setq sis--ism ism-path))))

    ;; make `sis-do-set' and `sis-do-get'
    (when sis--ism
      ;; avoid override user customized sis-do-get
      (unless (functionp sis-do-get)
        (setq sis-do-get (sis--mk-get-fn)))
      ;; avoid override user customized sis-do-set
      (unless (functionp sis-do-set)
        (setq sis-do-set (sis--mk-set-fn)))))

  ;; successfully inited
  (when (and (functionp sis-do-get)
             (functionp sis-do-set))
    ;; a t `sis--ism' means customized by `sis-do-get' and `sis-do-set'
    (unless sis--ism (setq sis--ism t)))

  ;; just inited, successfully or not
  (setq sis--ism-inited t))

(defmacro sis--ensure-ism (&rest body)
  "Only run BODY with valid ism."
  `(progn
     (unless sis--ism-inited
       (sis--init-ism))
     (when sis--ism
       ,@body)))

(defmacro sis--ensure-dir (&rest body)
  "Ensure BODY runs in home directory."
  `(let ((default-directory "~"))
     ,@body))

(defsubst sis--normalize-to-lang (lang)
  "Normalize LANG in the form of source id or lang to lang."
  (cond
   (; english
    (member lang (list 'english sis-english-source))
    'english)
   (; other
    (member lang (list 'other sis-other-source))
    'other)))

(defsubst sis--normalize-to-source (source)
  "Normalize SOURCE in the form of source id or lang to source."
  (cond
   (; english
    (member source (list 'english sis-english-source))
    sis-english-source)
   (; other
    (member source (list 'other sis-other-source))
    sis-other-source)))

(defun sis--mk-get-fn-by-cmd (cmd)
  "Make a function to be bound to `sis-do-get' from CMD."
  (lambda ()
    (sis--ensure-dir
     (string-trim (shell-command-to-string cmd)))))

(defun sis--mk-get-fn ()
  "Make a function to be bound to `sis-do-get'."
  (cond
   (; EMP
    (equal sis--ism 'emp)
    #'mac-input-source)
   (; external ism
    sis--ism
    (sis--mk-get-fn-by-cmd sis--ism))))

(defun sis--mk-set-fn ()
  "Make a function to be bound to `sis-do-set'."
  (cond
   (; EMP
    (equal sis--ism 'emp)
    (lambda (source) (mac-select-input-source source)))
   (; external ism
    sis--ism
    (lambda (source)
      (sis--ensure-dir
       (start-process "set-input-source" nil sis--ism source))))))

(defun sis--update-state (source)
  "Update input source state.

SOURCE should be 'english or 'other."

  (setq sis--previous sis--current)
  (setq sis--current source)
  (unless sis--for-buffer-locked
    (setq sis--for-buffer source))
  (when (not (eq sis--previous sis--current))
    (run-hooks 'sis-change-hook)))

(defsubst sis--get ()
  "Get the input source id."
  (sis--ensure-ism
   (sis--update-state (sis--normalize-to-lang (funcall sis-do-get)))))

(defsubst sis--set (source)
  "Set the input source according to source SOURCE."
  (sis--ensure-ism
   (sis--update-state (sis--normalize-to-lang source))
   (funcall sis-do-set (sis--normalize-to-source source))
   (when sis-log-mode
     (message "Do set input source: [%s]@%s, for-buffer: %s, locked: %s"
              source (current-buffer)
              sis--for-buffer sis--for-buffer-locked))))

;;;###autoload
(defun sis-get ()
  "Get input source."
  (interactive)
  (sis--get)
  sis--current)

(defun sis--set-english ()
  "Function to set input source to `english'."
  (sis--set 'english))

;;;###autoload
(defun sis-set-english ()
  "Command to set input source to `english'."
  (interactive)
  (setq sis--for-buffer-locked nil)
  (sis--set-english))

(defun sis--set-other ()
  "Function to set input source to `other'."
  (setq sis--for-buffer-locked nil)
  (sis--set 'other))

;;;###autoload
(defun sis-set-other ()
  "Command to set input source to `other'."
  (interactive)
  (setq sis--for-buffer-locked nil)
  (sis--set-other))

;;;###autoload
(defun sis-switch ()
  "Switch input source between english and other."
  (interactive)
  (setq sis--for-buffer-locked nil)
  (sis--ensure-ism
   (cond
    (; current is english
     (eq sis--current 'english)
     (sis--set-other))
    (; current is other
     (eq sis--current 'other)
     (sis--set-english)))))

;;;###autoload
(defun sis-ism-lazyman-config (english-source other-source &optional ism-type)
  "Config ism for lazy man.

ENGLISH-SOURCE: ENGLISH input source, nil means default,
                ignored by ISM-TYPE of 'fcitx, 'fcitx5, 'native.
OTHER-SOURCE: OTHER language input source, nil means default,
              ignored by ISM-TYPE of 'fcitx, 'fcitx5.
TYPE: TYPE can be 'native, 'emp, 'macism, 'im-select, 'fcitx, 'fcitx5, 'ibus.
      nil TYPE fits both 'emp and 'macism."
  (interactive)
  (when english-source
    (setq sis-english-source english-source))
  (when other-source
    (setq sis-other-source other-source))
  (when ism-type
    (setq sis-external-ism (pcase ism-type
                             ('native 'native)
                             ('emp 'emp)
                             ('macism "macism")
                             ('im-select "im-select.exe")
                             ('fcitx "fcitx-remote")
                             ('fcitx5 "fcitx5-remote")
                             ('ibus "ibus"))))

  (cond
   (; Emacs native input method, set do-get and do-set
    (eq ism-type 'native)
    (setq default-input-method other-source)
    (setq sis-english-source nil)
    (add-hook 'input-method-activate-hook
              (lambda () (sis--update-state sis-other-source)))
    (add-hook 'input-method-deactivate-hook
              (lambda () (sis--update-state sis-english-source)))
    (setq sis-do-get (lambda() current-input-method))
    (setq sis-do-set (lambda(source)
                       (unless (equal source current-input-method)
                         (toggle-input-method)))))
   (; for builtin supoort, use the default do-get and do-set
    (memq ism-type (list nil 'emp 'macism 'im-select))
    t)
   (; fcitx and fcitx5, use the default do-get, set do-set
    (memq ism-type (list 'fcitx 'fcitx5))
    (setq sis-english-source "1")
    (setq sis-other-source "2")
    (setq sis-do-set (lambda(source)
                       (sis--ensure-dir
                        (pcase source
                          ("1" (start-process "set-input-source"
                                              nil sis--ism "-c"))
                          ("2" (start-process "set-input-source"
                                              nil sis--ism "-o")))))))
   (; ibus, set do-get and do-set
    (eq ism-type 'ibus)
    (setq sis-do-get (sis--mk-get-fn-by-cmd (format "%s engine" sis--ism)))
    (setq sis-do-set (lambda(source)
                       (sis--ensure-dir
                        (start-process "set-input-source"
                                       nil sis--ism "engine" source)))))))

;;
;; Following codes are mainly about auto update mode
;;

(defvar sis--auto-refresh-timer nil
  "Timer for `sis--auto-refresh-timer-function'.")

(defvar sis--auto-refresh-manager-timer nil
  "Timer to manage `sis--auto-refresh-timer'.")

(defun sis--auto-refresh-timer-function ()
  "Auto refresh input source on idle timer."
  (when sis--auto-refresh-timer
    (cancel-timer sis--auto-refresh-timer))

  (if (and sis--respect-to-restore-after-minibuffer
           (not sis--respect-in-dispatcher))
      (progn
        (sis--restore-from-buffer)
        (setq sis--respect-to-restore-after-minibuffer nil))
  (sis--save-to-buffer))

  (when (and sis-auto-refresh-seconds sis-auto-refresh-mode)
    (setq sis--auto-refresh-timer
          (run-with-idle-timer
           ;; every time the wait period increases by auto-refresh-seconds
           (time-add (current-idle-time)
                     (* sis-auto-refresh-seconds sis--auto-refresh-timer-scale))
           nil
           #'sis--auto-refresh-timer-function))
    (setq sis--auto-refresh-timer-scale
          (* 1.05 sis--auto-refresh-timer-scale))))

(defvar sis--auto-refresh-timer-scale 1
  "Interval scale during this idle period.")

(defun sis--auto-refresh-timer-restart ()
  "Restart `sis--auto-refresh-timer'."
  (when (and sis-auto-refresh-seconds sis-auto-refresh-mode)
    (setq sis--auto-refresh-timer-scale 1)
    (sis--auto-refresh-timer-function)))

;;;###autoload
(define-minor-mode sis-auto-refresh-mode
  "Automaticly refresh input source."
  :global t
  :init-value nil
  (cond
   (; turn on the mode
    sis-auto-refresh-mode
    (when sis-auto-refresh-seconds
      (when sis--auto-refresh-manager-timer
        (cancel-timer sis--auto-refresh-manager-timer))
      (setq sis--auto-refresh-manager-timer
            (run-with-idle-timer sis-auto-refresh-seconds t
                                 #'sis--auto-refresh-timer-restart))))
   (; turn off the mode
    (not sis-auto-refresh-mode)
    (when sis--auto-refresh-manager-timer
      (cancel-timer sis--auto-refresh-manager-timer))
    (when sis--auto-refresh-timer (cancel-timer sis--auto-refresh-timer)))))

(defun sis--try-disable-auto-refresh-mode ()
  "Try to disable auto refresh mode."
  (when (and (not sis-global-cursor-color-mode)
             (not sis-global-respect-mode))
    (sis-auto-refresh-mode -1)))

;;
;; Following codes are mainly about cursor color mode
;;

(defun sis--set-cursor-color-advice (fn color)
  "Advice for FN of `set-cursor-color' with COLOR.

The advice is needed, because other packages may set cursor color in their only
way."
  (pcase sis--current
    ('english
     (funcall fn sis-default-cursor-color))
    ('other
     (funcall fn sis-other-cursor-color))
    (_
     (funcall fn color))))

(defun sis--update-cursor-color()
  "Update cursor color according to input source."
  ;; for GUI
  (when (display-graphic-p)
    ;; actually which color passed to the function does not matter,
    ;; the advice will take care of it.
    (set-cursor-color sis-default-cursor-color))

  ;; for TUI
  (unless (display-graphic-p)
    (pcase sis--current
      ('english
       (send-string-to-terminal
        (format "\e]12;%s\a" sis-default-cursor-color)))
      ('other
       (send-string-to-terminal
        (format "\e]12;%s\a" sis-other-cursor-color))))))

;;;###autoload
(define-minor-mode sis-global-cursor-color-mode
  "Automaticly change cursor color according to input source."
  :global t
  :init-value nil
  (cond
   (; turn on the mode
    sis-global-cursor-color-mode

    ;; auto refresh input source
    (sis-auto-refresh-mode t)
    ;; save original cursor color
    (unless sis-default-cursor-color
      (setq sis-default-cursor-color
            (or (when (display-graphic-p)
                  (or (cdr (assq 'cursor-color default-frame-alist))
                      (face-background 'cursor)))
                "white")))
    (advice-add 'set-cursor-color :around #'sis--set-cursor-color-advice)
    (add-hook 'sis-change-hook #'sis--update-cursor-color))
   (; turn off the mode
    (not sis-global-cursor-color-mode)
    (sis--try-disable-auto-refresh-mode)
    (advice-remove 'set-cursor-color #'sis--set-cursor-color-advice)
    (remove-hook 'sis-change-hook #'sis--update-cursor-color))))

;;
;; Following codes are mainly about respect mode
;;

(defvar sis--prefix-override-map-alist nil
  "Map alist for override.")

(defvar sis--prefix-handle-stage 'normal
  "Processing state of the prefix key.

Possible values: 'normal, 'prefix, 'sequence.")

(defvar sis--buffer-before-prefix nil
  "Current buffer before prefix.")

(defvar sis--buffer-before-command nil
  "Current buffer before prefix.")

(defvar sis--real-this-command nil
  "Real this command. Some commands overwrite it.")

(defvar sis--respect-to-restore-after-minibuffer nil
  "Already restored input source after minibuffer exit.")

(defvar sis--prefix-override-order -1000
  "Order of the prefix override in `emulation-mode-map-alists'.")


(defsubst sis--save-to-buffer ()
  "Save buffer input source."
  (sis--get))

(defsubst sis--restore-from-buffer ()
  "Restore buffer input source."
  (setq sis--for-buffer-locked nil)
  (sis--set (or sis--for-buffer 'english)))

(defvar sis--respect-in-dispatcher nil
  "In processing a dispatcher.")
(make-variable-buffer-local 'sis--respect-in-dispatcher)

(defsubst sis--respect-dispatch-advice ()
  "Advice for `sis-respect-dispatches'."
  (when sis-log-mode
    (message "dispatcher-advice: %s@%s, %s@locked"
             sis--for-buffer (current-buffer)
             sis--for-buffer-locked))
  (setq sis--for-buffer-locked t)
  (sis--set-english)
  (setq sis--respect-in-dispatcher t))

(defsubst sis--respect-go-english-advice ()
  "Advice for `sis-respect-go-english-triggers'."
  (sis--save-to-buffer)
  (when sis-log-mode
    (message "go-english-advice: %s@%s, %s@locked"
             sis--for-buffer (current-buffer)
             sis--for-buffer-locked))
  (setq sis--for-buffer-locked t)
  (sis--set-english))

(defsubst sis--respect-restore-advice ()
  "Restore buffer input source."
  (when sis-log-mode
    (message "restore-advice: %s@%s, %s@locked"
             sis--for-buffer (current-buffer)
             sis--for-buffer-locked))
  (sis--restore-from-buffer))

(defvar sis--prefix-override-map-enable nil
  "Enabe the override keymap.")

;;;###autoload
(defun sis-prefix-override-buffer-disable ()
  "Disable prefix override in current buffer."
  (interactive)
  (make-local-variable
   'sis--prefix-override-map-enable)
  (setq sis--prefix-override-map-enable nil))

;;;###autoload
(defun sis-prefix-override-buffer-enable ()
  "Disable prefix override in current buffer."
  (interactive)
  (when (local-variable-p 'sis--prefix-override-map-enable)
    (kill-local-variable 'sis--prefix-override-map-enable)))

(defun sis--prefix-override-recap-advice (&rest res)
  "Advice for `prefix-override-recap-triggers' with RES."
  (add-to-ordered-list
   'emulation-mode-map-alists
   'sis--prefix-override-map-alist
   sis--prefix-override-order)
  res)

(defun sis--prefix-override-handler (arg)
  "Prefix key handler with ARG."
  (interactive "P")
  ;; Restore the prefix arg
  (setq prefix-arg arg)
  (prefix-command-preserve-state)
  ;; Push the key back on the event queue
  (setq unread-command-events
        (append (mapcar (lambda (e) (cons t e))
                        (listify-key-sequence (this-command-keys)))
                unread-command-events)))

(defun sis--respect-focus-out-handler ()
  "Handler for `focus-out-hook'."

  ;; `mouse-drag-region' causes lots of noise.
  (unless (eq this-command 'mouse-drag-region)
    ;; can't use `sis--save-to-buffer' directly
    ;; because OS may has already changed input source
    ;; when other windows get focus.
    ;; so, don't get the current OS input source
    (setq sis--for-buffer-locked t)
    (sis--set-english))

  (when sis-log-mode
    (message "Handle save hook, save [%s] to [%s]."
             sis--for-buffer (current-buffer))))

(defun sis--respect-focus-in-handler ()
  "Handler for `focus-in-hook'."
  (when sis-log-mode
    (message "Handle restore hook, restore [%s] from [%s] ."
             sis--for-buffer (current-buffer)))
  (sis--restore-from-buffer))

(defun sis--respect-pre-command-handler ()
  "Handler for `pre-command-hook' to preserve input source."
  (setq sis--buffer-before-command (current-buffer))
  (setq sis--real-this-command this-command)
  (setq sis--respect-in-dispatcher nil)

  (when sis-log-mode
    (message "pre@[%s]: [%s]@key [%s]@cmd [%s]@buf [%s]@override."
             sis--prefix-handle-stage
             (this-command-keys)
             sis--real-this-command
             (current-buffer)
             sis--prefix-override-map-enable))

  (pcase sis--prefix-handle-stage
    (; current is normal stage
     'normal
     (cond
      (; not prefix key
       (not (eq sis--real-this-command #'sis--prefix-override-handler))
       t)

      (; for prefix key
       (eq sis--real-this-command #'sis--prefix-override-handler)

       ;; go to pre@[prefix] directly
       (when sis-log-mode
         (message
          "[%s] is a prefix key, short circuit to prefix phase."
          (this-command-keys)))
       (setq sis--prefix-handle-stage 'prefix)
       (sis--respect-pre-command-handler))))
    (; current is prefix stage
     'prefix
     (setq sis--prefix-override-map-enable nil)
     (setq sis--buffer-before-prefix (current-buffer))
     (sis--save-to-buffer)
     (setq sis--for-buffer-locked t)
     (sis--set-english)
     (when sis-log-mode
       (message "Input source: [%s] (saved) => [%s]."
                sis--for-buffer sis-english-source)))
    (; current is sequence stage
     'sequence t)))

(defvar sis-prefix-override-buffer-disable-predicates
  (list 'minibufferp
        (;; magit revision
         lambda (buffer)
         (sis--string-match-p "^magit-revision:" (buffer-name buffer)))
        (;; special buffer
         lambda (buffer)
         (and (sis--string-match-p "^\*" (buffer-name buffer))
              (not (sis--string-match-p "^\*new\*"
                                        (downcase (buffer-name buffer))))
              (not (sis--string-match-p "^\*scratch\*"
                                        (downcase (buffer-name buffer)))))))
  "Predicates on buffers to disable prefix overriding.")

(defsubst sis--prefix-override-buffer-disable-p (buffer)
  "Final predicate on disabling prefix override in BUFFER."
  (let ((value nil))
    (dolist (p sis-prefix-override-buffer-disable-predicates)
      (setq value (or value (funcall p buffer))))
    value))

(defsubst sis--to-normal-stage (restore)
  "Transite to normal stage and restore input source if RESTORE is t."
  (when restore
    ;; entering minibuffer is handled separately.
    ;; some functions like `exit-minibuffer' won't trigger post-command-hook
    (unless (or (minibufferp) sis--respect-in-dispatcher)
      (when sis-log-mode
        (message "restore: [%s]@[%s]" sis--for-buffer (current-buffer)))
      (sis--restore-from-buffer)
      ;; indicate that input source is already restored after minibuffer.
      ;; no harm if not the case of just exiting minibuffer.
      (setq sis--respect-to-restore-after-minibuffer nil))

    (when (and (not (local-variable-p
                     'sis--prefix-override-map-enable))
               (sis--prefix-override-buffer-disable-p (current-buffer)))
      (sis-prefix-override-buffer-disable))

    (unless (local-variable-p 'sis--prefix-override-map-enable)
      (setq sis--prefix-override-map-enable t)))

  (setq sis--prefix-handle-stage 'normal))

(defun sis--respect-post-command-handler ()
  "Handler for `post-command-hook' to preserve input source."
  ;; (setq this-command sis--real-this-command)
  (when sis-log-mode
    (message "post@[%s]: [%s]@key [%s]@cmd [%s]@buf [%s]@override."
             sis--prefix-handle-stage
             (this-command-keys)
             sis--real-this-command
             (current-buffer)
             sis--prefix-override-map-enable))

  (pcase sis--prefix-handle-stage
    (; current is prefix stage
     'prefix
     (setq sis--prefix-handle-stage 'sequence))
    (; current is sequence stage
     'sequence
     (cond
      (; still in progress
       (minibufferp)
       (setq sis--prefix-handle-stage 'sequence))
      (; key sequence is canceled
       (not sis--real-this-command)
       (when sis-log-mode (message "Key sequence canceled."))
       (sis--to-normal-stage t))

      (; end key sequence
       t
       (when sis-log-mode (message "Key sequence ended."))
       (sis--to-normal-stage t))))
    (; current is normal stage
     'normal
     (let ((restore (not (eq sis--buffer-before-command (current-buffer)))))
       (sis--to-normal-stage restore)))))

(defun sis--minibuffer-setup-handler ()
  "Handler for `minibuffer-setup-hook'."
  (when sis-log-mode
    (message "enter minibuffer: [%s]@current [%s]@last [%s]@command"
             (current-buffer)
             sis--buffer-before-command
             this-command))
  (setq sis--respect-to-restore-after-minibuffer nil)
  (sis--set-english))

(defun sis--minibuffer-exit-handler ()
  "Handler for `minibuffer-exit-hook'."
  (when sis-log-mode (message "exit minibuffer: [%s]@command" this-command))
  (setq sis--respect-to-restore-after-minibuffer t))

;;;###autoload
(define-minor-mode sis-global-respect-mode
  "Respect buffer/mode by proper input source.

- Respect start: start this mode with specific input source.
- Respect ~evil~: switch to English when leaving ~evil~ ~insert~ mode.
- Respect prefix key: switch to English for ~C-c~/ ~C-x~/ ~C-h~.
- Respect buffer: restore buffer input source when it regain focus."
  :global t
  :init-value nil
  (cond
   (; turn on the mode
    sis-global-respect-mode
    (sis--ensure-ism
     ;; /respect mode/ depends on /auto refresh mode/
     (sis-auto-refresh-mode t)
     ;; set english when mode enabled
     (when sis-respect-start (sis--set sis-respect-start))

     ;; set english when exit evil insert state
     (when (featurep 'evil)
       (add-hook 'evil-insert-state-exit-hook #'sis-set-english)
       ;; let sis to manage input method
       (advice-add 'evil-activate-input-method :around
                   #'sis--do-nothing-advice)
       (advice-add 'evil-deactivate-input-method :around
                   #'sis--do-nothing-advice)
       (when sis-respect-evil-normal-escape
         (define-key evil-normal-state-map
           (kbd "<escape>") #'sis-set-english)))

     (when sis-respect-prefix-and-buffer
       ;; preserve buffer input source
       (add-hook 'pre-command-hook #'sis--respect-pre-command-handler)
       (add-hook 'post-command-hook #'sis--respect-post-command-handler)
       (add-hook 'minibuffer-setup-hook #'sis--minibuffer-setup-handler)
       (add-hook 'minibuffer-exit-hook #'sis--minibuffer-exit-handler)

       ;; enable terminal focus event
       (unless (display-graphic-p)
         (require 'terminal-focus-reporting)
         (terminal-focus-reporting-mode t))

       (add-hook 'focus-out-hook #'sis--respect-focus-out-handler)
       (add-hook 'focus-in-hook #'sis--respect-focus-in-handler)

       (dolist (trigger sis-respect-go-english-triggers)
         (advice-add trigger :after #'sis--respect-go-english-advice))

       (dolist (trigger sis-respect-restore-triggers)
         (advice-add trigger :after #'sis--respect-restore-advice))

       (dolist (trigger sis-respect-dispatches)
         (advice-add trigger :after #'sis--respect-dispatch-advice))

       ;; set english when prefix key pressed
       (setq sis--prefix-override-map-alist
             `((sis--prefix-override-map-enable
                .
                ,(let ((keymap (make-sparse-keymap)))
                   (dolist (prefix sis-prefix-override-keys)
                     (define-key keymap
                       (kbd prefix) #'sis--prefix-override-handler))
                   keymap))))

       (setq sis--prefix-override-map-enable t)
       (sis--prefix-override-recap-advice)
       (dolist (trigger sis-prefix-override-recap-triggers)
         (advice-add trigger :after #'sis--prefix-override-recap-advice)))))
   (; turn off the mode
    (not sis-global-respect-mode)
    (sis--try-disable-auto-refresh-mode)
    ;; for evil
    (when (featurep 'evil)
      (advice-remove 'evil-activate-input-method #'sis--do-nothing-advice)
      (advice-remove 'evil-deactivate-input-method #'sis--do-nothing-advice)
      (remove-hook 'evil-insert-state-exit-hook #'sis-set-english)
      (when sis-respect-evil-normal-escape
        (define-key evil-normal-state-map (kbd "<escape>") nil)))

    (when sis-respect-prefix-and-buffer
      ;; for preserving buffer input source
      (remove-hook 'focus-out-hook #'sis--respect-focus-out-handler)
      (remove-hook 'focus-in-hook #'sis--respect-focus-in-handler)

      ;; for prefix key
      (setq emulation-mode-map-alists
            (delq 'sis--prefix-override-map-alist
                  emulation-mode-map-alists))
      (setq sis--prefix-override-map-enable nil)))))

;;
;; Following codes are mainly about follow-context-mode
;;

(defsubst sis--string-match-p (regexp str &optional start)
  "Robust wrapper of `string-match-p'.

Works when REGEXP or STR is not a string REGEXP, STR, START all has the same
meanings as `string-match-p'."
  (and (stringp regexp)
       (stringp str)
       (string-match-p regexp str start)))

(defsubst sis--english-p (str)
  "Predicate on STR is English."
  (sis--string-match-p sis-english-pattern str))

(defsubst sis--not-english-p (str)
  "Predicate on STR is not English."
  (not (sis--string-match-p sis-english-pattern str)))

(defsubst sis--other-p (str)
  "Predicate on STR is other language."
  (sis--string-match-p sis-other-pattern str))

(defsubst sis--not-other-p (str)
  "Predicate on STR is not other language."
  (not (sis--string-match-p sis-other-pattern str)))

(cl-defstruct sis-back-detect ; result of backward detect
  to ; point after first non-blank char in the same line
  char ; first non-blank char at the same line (just before position `to')
  cross-line-to ; point after first non-blank char cross lines
  cross-line-char ; first non-blank char cross lines before the current position
  )

(defun sis--back-detect-chars ()
  "Detect char backward by two steps.

  First backward skip blank in the current line,
  then backward skip blank across lines."
  (save-excursion
    (skip-chars-backward sis-blank-pattern)
    (let ((to (point))
          (char (char-before (point))))
      (skip-chars-backward (concat sis-blank-pattern "[:cntrl:]"))
      (let ((cross-line-char (char-before (point))))
        (make-sis-back-detect :to to
                              :char (when char (string char))
                              :cross-line-to (point)
                              :cross-line-char (when cross-line-char
                                                 (string cross-line-char)))))))

(cl-defstruct sis-fore-detect ; result of forward detect
  to ; point before first non-blank char in the same line
  char ; first non-blank char at the same line (just after position `to')
  cross-line-to ; point before first non-blank char cross lines
  cross-line-char ; first non-blank char cross lines after the current position
  )

(defun sis--fore-detect-chars ()
  "Detect char forward.

  Forward skip blank in the current line."
  (save-excursion
    (skip-chars-forward sis-blank-pattern)
    (let ((to (point))
          (char (char-after (point))))
      (skip-chars-forward (concat sis-blank-pattern "[:cntrl:]"))
      (let ((cross-line-char (char-after (point))))
        (make-sis-fore-detect :to to
                              :char (when char (string char))
                              :cross-line-to (point)
                              :cross-line-char (when cross-line-char
                                                 (string cross-line-char)))))))

(defun sis--context-other-p (back-detect fore-detect &optional position)
  "Predicate for context of other language.

`back-detect' BACK-DETECT and `fore-detect' FORE-DETECT are required.
If POSITION is not provided, then default to be the current position."
  (let* ((back-to (sis-back-detect-to back-detect))
         (back-char (sis-back-detect-char back-detect))
         (cross-line-back-to (sis-back-detect-cross-line-to back-detect))
         (cross-line-back-char (sis-back-detect-cross-line-char back-detect))

         (fore-to (sis-fore-detect-to fore-detect))
         (fore-char (sis-fore-detect-char fore-detect)))
    (cond
     (; [other]^
      (and (= back-to (or position (point))) (sis--other-p back-char))
      t)
     (; ^[other]
      (and (= fore-to (or position (point))) (sis--other-p fore-char))
      t)
     (; [other lang][blank or not][^][blank or not][not english]
      (and (sis--other-p back-char) (sis--not-english-p fore-char))
      t)
     (; [not english][blank or not][^][blank or not][other lang]
      (and (sis--not-english-p back-char) (sis--other-p fore-char))
      t)
     (; [other lang: to the previous line][blank][^]
      (and (or sis-follow-context-aggressive-line
               (> cross-line-back-to (line-beginning-position 0)))
           (< cross-line-back-to (line-beginning-position))
           (sis--other-p cross-line-back-char))
      t))))

(defun sis--context-english-p (back-detect fore-detect &optional position)
  "Predicate for context of English.

`back-detect' BACK-DETECT and `fore-detect' FORE-DETECT are required.
If POSITION is not provided, then default to be the current position."
  (let* ((back-to (sis-back-detect-to back-detect))
         (back-char (sis-back-detect-char back-detect))
         (cross-line-back-to (sis-back-detect-cross-line-to back-detect))
         (cross-line-back-char (sis-back-detect-cross-line-char back-detect))

         (fore-to (sis-fore-detect-to fore-detect))
         (fore-char (sis-fore-detect-char fore-detect)))
    (cond
     (; [english]^
      (and (= back-to (or position (point))) (sis--english-p back-char))
      t)
     (; ^[english]
      (and (= fore-to (or position (point))) (sis--english-p fore-char))
      t)
     (; [english][blank or not][^][blank or not][not other]
      (and (sis--english-p back-char) (sis--not-other-p fore-char))
      t)
     (; [not other][blank or not][^][blank or not][english]
      (and (sis--not-other-p back-char) (sis--english-p fore-char))
      t)
     (; [english: to the previous line][blank][^]
      (and (or sis-follow-context-aggressive-line
               (> cross-line-back-to (line-beginning-position 0)))
           (< cross-line-back-to (line-beginning-position))
           (sis--english-p cross-line-back-char))
      t))))

(defun sis--context-guess ()
  "Guest the lang context for the current point."
  (let* ((back-detect (sis--back-detect-chars))
         (fore-detect (sis--fore-detect-chars)))
    (cond
     (; context is fixed.
      sis-follow-context-fixed
      sis-follow-context-fixed)
     (; english context
      (sis--context-english-p back-detect fore-detect)
      'english)
     (; other lang context
      (sis--context-other-p back-detect fore-detect)
      'other))))

;;;###autoload
(define-minor-mode sis-follow-context-mode
  "Switch input source smartly according to context."
  :init-value nil
  (cond
   (; turn of the mode
    sis-follow-context-mode
    (sis--ensure-ism
     (dolist (hook sis-follow-context-hooks)
       (add-hook hook #'sis-follow-context nil t))))
   (; turn off the mode
    (not sis-follow-context-mode)
    (dolist (hook sis-follow-context-hooks)
      (remove-hook hook #'sis-follow-context nil)))))

;;;###autoload
(define-globalized-minor-mode
  sis-global-follow-context-mode
  sis-follow-context-mode
  sis-follow-context-mode)

;;;###autoload
(defun sis-follow-context ()
  "Follow the context to switch input source."
  (let ((context (sis--context-guess)))
    (when context
      (sis--set context))))

;;
;; Following codes are mainly about the inline region overlay
;;

(defvar sis--inline-overlay nil
  "The active inline overlay.")
(make-variable-buffer-local 'sis--inline-overlay)

(defvar sis--inline-lang nil
  "Language of the active inline overlay.")
(make-variable-buffer-local 'sis--inline-lang)

(defvar sis--inline-first-space-point nil
  "First effective space point to check overlay activation.")
(make-variable-buffer-local 'sis--inline-first-space-point)

(defsubst sis--inline-overlay-start ()
  "Start position of the inline overlay."
  (when sis--inline-overlay
    (overlay-start sis--inline-overlay)))

(defsubst sis--inline-overlay-end ()
  "End position of the inline overlay."
  (when sis--inline-overlay
    (overlay-end sis--inline-overlay)))

;;;###autoload
(define-minor-mode sis-inline-mode
  "English overlay mode for mixed language editing."
  :init-value nil
  (cond
   (; turn on the mode
    sis-inline-mode
    (sis--ensure-ism
     (add-hook 'post-self-insert-hook #'sis--inline-check-to-activate nil t)))
   (; turn off the mode
    (not sis-inline-mode)
    (remove-hook 'post-self-insert-hook #'sis--inline-check-to-activate t))))

;;;###autoload
(define-globalized-minor-mode
  sis-global-inline-mode
  sis-inline-mode
  sis-inline-mode)

(defsubst sis--inline-effect-space-inserted-p ()
  "A effective space is inserted."
  (and sis-inline-mode
       (not (overlayp sis--inline-overlay))
       (not (button-at (point)))
       (not (and (featurep 'evil)
                 (or (evil-normal-state-p)
                     (evil-visual-state-p)
                     (evil-motion-state-p)
                     (evil-operator-state-p))))
       ;; around char is <spc> <DBC spc>
       (memq (preceding-char) (list ?\s 12288))))

(defun sis--inline-check-to-activate()
  "Check whether to activate the inline region overlay.

Check the context to determine whether the overlay should be activated or not,
if the answer is yes, then activate the /inline region/, set the
input source to English."
  (let ((effective-space (sis--inline-effect-space-inserted-p)))
    (cond
     (;if not effective space inserted, reset times to 0
      (not effective-space)
      (setq sis--inline-first-space-point nil))
     (;if effective space inserted
      effective-space
      (let* ((back-detect (sis--back-detect-chars))
             (fore-detect (sis--fore-detect-chars)))

        (unless sis--inline-first-space-point
          (setq sis--inline-first-space-point (point)))

        (cond
         (;inline english region
          (and sis-inline-with-english
               (sis--context-other-p back-detect fore-detect (1- (point)))
               (equal sis--for-buffer 'other))
          (sis--inline-activate 'english (1- (point))))

         (;inline other lang region
          (and sis-inline-with-other
               (= (1+ sis--inline-first-space-point) (point))
               (sis--context-english-p back-detect fore-detect (- (point) 2))
               (equal sis--for-buffer 'english))
          (sis--inline-activate 'other (- (point) 2)))))))))

(defun sis--inline-activate (lang start)
  "Activate the inline region overlay from START.

LANG: the inline region language.
START: start position of the inline region."
  (interactive)
  (sis--ensure-ism
   (setq sis--inline-lang lang)
   (when (overlayp sis--inline-overlay)
     (delete-overlay sis--inline-overlay))

   (setq sis--inline-overlay (make-overlay start (point) nil t t ))
   (overlay-put sis--inline-overlay 'face 'sis-inline-face)
   (overlay-put sis--inline-overlay 'keymap
                (let ((keymap (make-sparse-keymap)))
                  (define-key keymap (kbd "RET")
                    #'sis--inline-ret-check-to-deactivate)
                  (define-key keymap (kbd "<return>")
                    #'sis--inline-ret-check-to-deactivate)
                  keymap))
   (add-hook 'post-command-hook #'sis--inline-fly-check-deactivate nil t)
   (sis--set sis--inline-lang))

  (pcase sis--inline-lang
    ('other (run-hooks 'sis-inline-other-activated-hook))
    ('english (run-hooks 'sis-inline-english-activated-hook))))

(defun sis--inline-fly-check-deactivate ()
  "Check whether to deactivate the inline region overlay."
  (interactive)
  (when (and sis-inline-mode
             (overlayp sis--inline-overlay))

    (when sis-inline-not-max-point
      ;; When cursor is at point-max,
      ;; autocomplete may display with a huge inline overlay background.
      (when (= (point) (point-max))
        (save-excursion (insert-char ?\n))))

    ;; In case some package automatically insert \n before EOF,
    ;; then kick \n out of the the overlay
    (when (and (= (char-before (sis--inline-overlay-end))
                  ?\n)
               (< (sis--inline-overlay-start)
                  (sis--inline-overlay-end)))
      (move-overlay sis--inline-overlay
                    (sis--inline-overlay-start)
                    (1- (sis--inline-overlay-end))))

    ;; select input source
    (let* ((back-detect (sis--back-detect-chars))
           (back-to (sis-back-detect-to back-detect)))
      (when (or
             ;; zero length overlay
             (= (sis--inline-overlay-start)
                (sis--inline-overlay-end))
             ;; out of range
             (or(< (point) (sis--inline-overlay-start))
                (> (point) (sis--inline-overlay-end)))
             ;; " inline  ^"
             ;; but not "           ^"
             (and (= (point) (sis--inline-overlay-end))
                  (> back-to (sis--inline-overlay-start))
                  (= (+ (if sis-inline-single-space-close 1 2)
                        back-to) (point))))
        (sis--inline-deactivate)))))

(defun sis--inline-ret-check-to-deactivate ()
  "Deactivate the inline region overlay."
  (interactive)
  (when (and sis-inline-mode (overlayp sis--inline-overlay))
    ;; company
    (if (and (featurep 'company)
             (company--active-p))
        (company-complete-selection)
      (sis--inline-deactivate))))

(defun sis--inline-deactivate ()
  "Deactivate the inline region overlay."
  (interactive)
  ;; clean up
  (remove-hook 'post-command-hook #'sis--inline-fly-check-deactivate t)

  ;; select input source
  (let* ((back-detect (sis--back-detect-chars))
         (back-to (sis-back-detect-to back-detect))
         (back-char (sis-back-detect-char back-detect)))


    (cond
     (; inline english region
      (eq sis--inline-lang 'english)
      ;; [other lang][blank inline overlay]^
      ;; [overlay with trailing blank]^
      (when (or (and (= back-to (sis--inline-overlay-start))
                     (sis--other-p back-char))
                (and (> back-to (sis--inline-overlay-start))
                     (< back-to (sis--inline-overlay-end))
                     (< back-to (point))))
        (sis-set-other)))

     (; inline english region
      (eq sis--inline-lang 'other)
      ;; [not-other][blank inline overlay]^
      ;; [overlay with trailing blank]^
      (when (or (and (= back-to (sis--inline-overlay-start))
                     (sis--not-other-p back-char))
                (and (> back-to (sis--inline-overlay-start))
                     (< back-to (sis--inline-overlay-end))
                     (< back-to (point))))
        (sis-set-english))))

    ;; only tighten for none-blank inline region
    (when (and (<= (point) (sis--inline-overlay-end))
               (> back-to (sis--inline-overlay-start)))

      (save-excursion
        (goto-char (sis--inline-overlay-end))
        (let* ((tighten-back-detect (sis--back-detect-chars))
               (tighten-back-to (sis-back-detect-to tighten-back-detect)))
          (when (and (< tighten-back-to (sis--inline-overlay-end))
                     (> tighten-back-to (sis--inline-overlay-start)))
            (cond
             (; just delete one space
              (eq sis-inline-tighten-tail-rule 1)
              (delete-char -1))
             (; don't delete space
              (eq sis-inline-tighten-tail-rule 0)
              t)
             (; don't delete space
              (eq sis-inline-tighten-tail-rule 'all)
              (delete-region (point) tighten-back-to))))))

      (save-excursion
        (goto-char (sis--inline-overlay-start))
        (let* ((tighten-fore-detect (sis--fore-detect-chars))
               (tighten-fore-to (sis-fore-detect-to tighten-fore-detect)))
          (when (> tighten-fore-to (sis--inline-overlay-start))
            (cond
             (; just delete one space
              (eq sis-inline-tighten-head-rule 1)
              (delete-char 1))
             (; don't delete space
              (eq sis-inline-tighten-head-rule 0)
              t)
             (; don't delete space
              (eq sis-inline-tighten-head-rule 'all)
              (delete-region (point) tighten-fore-to))))))))
  (delete-overlay sis--inline-overlay)
  (setq sis--inline-overlay nil)
  (pcase sis--inline-lang
    ('other (run-hooks 'sis-inline-other-deactivated-hook))
    ('english (run-hooks 'sis-inline-english-deactivated-hook))))

(provide 'sis)
;;; sis.el ends here
