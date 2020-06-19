;;; smart-input-source.el --- Switch OS native input source smartly -*- lexical-binding: t; -*-

;; URL: https://github.com/laishulu/emacs-smart-input-source
;; Created: March 27th, 2020
;; Keywords: convenience
;; Package-Requires: ((names "0.5") (emacs "25"))
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

(defvar fixed-context nil
  "Context is fixed to a specific language.

Possible values:
nil: dynamic context
smart-input-source-ENGLISH: English context
smart-input-source-OTHER: other language context.")
(make-variable-buffer-local 'smart-input-source-fixed-context)

(defvar start-with-english t
  "Switch to english when `global-auto-english-mode' enabled.")

(defvar other-pattern "\\cc"
  "Pattern to identify a character as other lang.")
(make-variable-buffer-local 'smart-input-source-other-pattern)

(defvar blank-pattern "[:blank:]"
  "Pattern to identify a character as blank.")
(make-variable-buffer-local 'smart-input-source-blank-pattern)

(defvar other "com.sogou.inputmethod.sogou.pinyin"
  "Input source for other lang.")
(make-variable-buffer-local 'smart-input-source-other)

(defvar aggressive-line t
  "Aggressively detect context across blank lines.")
(make-variable-buffer-local 'smart-input-source-aggressive-line)

(defvar preserve-triggers
  '(switch-to-buffer switch-to-prev-buffer switch-to-next-buffer
    pop-to-buffer
    other-window windmove-up windmove-down windmove-left windmove-right)
  "A list of commands which would trigger the save/restore of input source.")

(defvar save-hook-triggers
  '(mouse-leave-buffer-hook focus-out-hook)
  "A list of hooks which would trigger the save of input source.")

(defvar restore-hook-triggers
  '()
  "A list of hooks which would trigger the restore of input source.")

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

;;
;; Following codes are mainly about input source manager
;;
(defconst ENGLISH 1)
(defconst OTHER 2)

;; Emacs mac port builtin input source manager
(defconst ISM-EMP 1)

(defvar -ism nil "The input source manager.")
(defvar -ism-inited nil "Input source manager initialized.")

(defun -init-ism ()
  "Init input source manager."
  (when (and (string= (window-system) "mac")
             (fboundp 'mac-input-source))
    (setq -ism ISM-EMP))

  (when (and (not -ism) external-ism)
    (let ((ism-path (executable-find external-ism)))
      (setq -ism ism-path)))

  (when -ism
    (unless (functionp do-get)
      (setq do-get (-mk-get-fn)))

    (unless (functionp do-set)
      (setq do-set (-mk-set-fn))))

  (setq -ism-inited t))

(defmacro -ensure-ism (&rest body)
  "Only run ARGS with valid ism"
  `(progn
     (unless smart-input-source--ism-inited
       (print "initing...")
       (smart-input-source--init-ism))
     (when smart-input-source--ism
       ,@body)))

(defun -mk-get-fn ()
  "Make a function to be bound to `do-get'."
  (if (equal -ism ISM-EMP)
      #'mac-input-source
    (lambda ()
      (string-trim (shell-command-to-string -ism)))))

(defun -mk-set-fn ()
  "Make a function to be bound to `do-set'."
  (if (equal -ism ISM-EMP)
      (lambda (source) (mac-select-input-source source))
    (lambda (source)
      (string-trim
       (shell-command-to-string (concat -ism " " source))))))

(defun -get ()
  "Get the input source id."
  (when (functionp do-get)
    (funcall do-get)))

(defun -set (lang)
  "Set the input source according to lang LANG, avoiding unnecessary switch."
  (when (and lang (functionp do-set))
    (pcase (-get)
      ((pred (equal english))
       (when (or (equal lang OTHER)
                 (equal lang other))
         (funcall do-set other)))
      ((pred (equal other))
       (when (or (equal lang ENGLISH)
                 (equal lang english))
         (funcall do-set english))))))

;;;###autoload
(defun set-english ()
  "Set input source to `english'."
  (interactive)
  (-ensure-ism (-set ENGLISH)))

;;;###autoload
(defun set-other ()
  "Set input source to `other'."
  (interactive)
  (-ensure-ism (-set OTHER)))

;;;###autoload
(defun switch ()
  "Switch input source between english and other."
  (interactive)
  (-ensure-ism
    (pcase (-get)
      ((pred (equal english))
       (funcall do-set other)
       other)
      ((pred (equal other))
       (funcall do-set english)
       other))))

(defvar -saved-in-global nil
  "Saved global input source.")

;;;###autoload
(defun save-to-global ()
  "Save global input source."
  (-ensure-ism (setq -saved-in-global (-get))))

;;;###autoload
(defun save-to-global-set-english ()
  "Save to global input source and then set to english."
  (-ensure-ism
    (save-to-global)
    (set-english)))

;;;###autoload
(defun restore-from-global ()
  "Restore global input source."
  (-ensure-ism (-set -saved-in-global)))

;;;###autoload
(define-minor-mode global-auto-english-mode
  "Automatically select english input source when startup or with evil.

- For GUI session of ~emacs mac port~, use native API to select input source
  for better performance.
- If ~emacs mac port~ is unavailable, or in terminal session, use external ism
  tool to select input source.
- If no ism found, then do nothing."
  :global t
  :init-value nil
  (-ensure-ism
    (if global-auto-english-mode
        (progn
          (when start-with-english (set-english))
          (add-hook 'minibuffer-setup-hook
                    #'smart-input-source-save-to-global-set-english)
          (add-hook 'minibuffer-exit-hook
                    #'smart-input-source-restore-from-global)
          (when (featurep 'evil)
            (add-hook 'evil-insert-state-exit-hook
                      #'smart-input-source-set-english)))
      (remove-hook 'minibuffer-setup-hook
                   #'smart-input-source-save-to-global-set-english)
      (remove-hook 'minibuffer-exit-hook
                   #'smart-input-source-restore-from-global)
      (when (featurep 'evil)
        (remove-hook 'evil-insert-state-exit-hook
                     #'smart-input-source-set-english)))))

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

(defun -other-lang-p (str)
  "Predicate on STR is other language."
  (-string-match-p other-pattern str))

(defun -not-other-lang-p (str)
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
      ENGLISH)

     ;; [english][^][blank][not english]
     ((and (and (> fore-to (point))
                (-not-english-p fore-char))
           (> back-to (line-beginning-position))
           (= back-to (point))
           (-english-p back-char))
      ENGLISH)

     ;; [:other lang:][^]
     ;; [^][:other lang:]
     ;; [:other lang:][:blank or not:][^][:blank or not:][:other lang:]
     ((or (and (= back-to (point))
               (-other-lang-p back-char))
          (and (= fore-to (point))
               (-other-lang-p fore-char))
          (and (-other-lang-p back-char)
               (-other-lang-p fore-char)))
      OTHER)

     ;; [english][^][line end]
     ((and (= back-to (point))
           (-english-p back-char)
           (= fore-to (line-end-position)))
      ENGLISH)

     ;; [english: include the previous line][blank][^]
     ((and (or aggressive-line
               (> cross-line-back-to (line-beginning-position 0)))
           (< cross-line-back-to (line-beginning-position))
           (-english-p cross-line-back-char))
      ENGLISH)

     ;; [other lang: include the previous line][blank][^]
     ((and (or aggressive-line
               (> cross-line-back-to (line-beginning-position 0)))
           (< cross-line-back-to (line-beginning-position))
           (-other-lang-p cross-line-back-char))
      OTHER)

     ;; [^][blank][english: include the next line]
     ((and (or aggressive-line
               (< cross-line-fore-to (line-end-position 2)))
           (> cross-line-fore-to (line-end-position))
           (-english-p cross-line-fore-char))
      ENGLISH)

     ;; [^][blank][other lang: include the next line]
     ((and (or aggressive-line
               (< cross-line-fore-to (line-end-position 2)))
           (> cross-line-fore-to (line-end-position))
           (-other-lang-p cross-line-fore-char))
      OTHER))))


;;;###autoload
(define-minor-mode follow-context-mode
  "Switch input source smartly according to context.

- For GUI session of ~emacs mac port~, use native API to select input source
  for better performance.
- If ~emacs mac port~ is unavailable, or in terminal session, use external ism
  tool to select input source.
- If no ism found, then do nothing."
  :init-value nil
  (-ensure-ism
    (if follow-context-mode
        (when (featurep 'evil)
          (add-hook 'evil-insert-state-entry-hook
                    #'smart-input-source-follow-context
                    nil t
                    ))
      (when (featurep 'evil)
        (remove-hook 'evil-insert-state-entry-hook
                     #'smart-input-source-follow-context
                     t)))))

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

;;;###autoload
(define-minor-mode inline-english-mode
  "English overlay mode for mixed language editing.

- For GUI session of ~emacs mac port~, use native API to select input source
  for better performance.
- If ~emacs mac port~ is unavailable, or in terminal session, use external ism
  tool to select input source.
- If no ism found, then do nothing."
  :init-value nil
  (-ensure-ism
    (if inline-english-mode
        (add-hook 'post-self-insert-hook
                  #'smart-input-source-check-to-activate-overlay
                  nil t)
      (remove-hook 'post-self-insert-hook
                   #'smart-input-source-check-to-activate-overlay
                   t))))

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
             (or (= (preceding-char) ?\s)
                 (= (preceding-char) 12288)
                 ))
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
                  (-other-lang-p back-char)
                  (not (and (< (1+ back-to) (point))
                            (= fore-to (point))
                            (-not-other-lang-p back-char))))
             ;; [:not none-english:][^][:space:][other lang]
             (and (< fore-to (line-end-position))
                  (-other-lang-p fore-char)
                  (not (and (> fore-to (point))
                            (-not-other-lang-p back-char)))))
        (activate-inline-overlay (1- (point)))))))

;;;###autoload
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
                     'smart-input-source-ret-check-to-deactivate-inline-overlay)
                   (define-key keymap (kbd "<return>")
                     'smart-input-source-ret-check-to-deactivate-inline-overlay)
                   keymap))
    (add-hook 'post-command-hook
              #'smart-input-source-fly-check-to-deactivate-inline-overlay
              nil t)
    (set-english)
    (message "Press <RETURN> to close inline english region.")))

(defun fly-check-to-deactivate-inline-overlay ()
  "Check whether to deactivate the inline english region overlay."
  (interactive)
  (when (and inline-english-mode
             (overlayp -inline-overlay))
    ;; select input source
    (let* ((back-detect (-back-detect-chars))
           (back-to (back-detect-to back-detect)))
      (when (or (= (-inline-overlay-start)
                   (-inline-overlay-end))
                (or(< (point) (-inline-overlay-start))
                   (> (point) (-inline-overlay-end)))
                (and (> back-to (-inline-overlay-start))
                     (< (1+ back-to) (-inline-overlay-end))))
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
  (remove-hook 'post-command-hook
               #'smart-input-source-fly-check-to-deactivate-inline-overlay
               t)

  ;; select input source
  (let* ((back-detect (-back-detect-chars))
         (back-to (back-detect-to back-detect))
         (back-char (back-detect-char back-detect)))

    ;; [other lang][:blank inline overlay:]^
    ;; [:overlay with trailing blank :]^
    (when (or (and (= back-to (-inline-overlay-start))
                   (-other-lang-p back-char))
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

;;
;; Following codes are mainly about preserve input source for buffer
;;

(defvar -saved-in-buffer nil
  "Saved buffer input source.")
(make-variable-buffer-local 'smart-input-source--saved-in-buffer)


(defvar -preserve-inited nil
  "Preserve input source initialized.")

(defun -save-to-buffer-advice (&rest _args)
  "A simple wrapper around `-save-to-buffer' that's advice-friendly."
  (-save-to-buffer))

(defun -restore-from-buffer-advice (&rest _args)
  "A simple wrapper around `-restore-from-buffer' that's advice-friendly."
  (-restore-from-buffer))

;;;###autoload
(define-minor-mode global-preserve-mode
  "Preserve input source for buffer."
  :global t
  :init-value nil
  (if global-preserve-mode
      (progn
        (-ensure-ism 
         (unless -preserve-inited
           (dolist (command preserve-triggers)
             (advice-add command :before
                         #'smart-input-source--save-to-buffer-advice)
             (advice-add command :after
                         #'smart-input-source--restore-from-buffer-advice))
           (dolist (hook save-hook-triggers)
             (add-hook hook #'smart-input-source--save-to-buffer-advice))
           (dolist (hook restore-hook-triggers)
             (add-hook hook #'smart-input-source--restore-from-buffer-advice))
           (setq -preserve-inited t))))
    (when -preserve-inited
      (dolist (command preserve-triggers)
        (advice-remove command
                       #'smart-input-source--save-to-buffer-advice)
        (advice-remove command
                       #'smart-input-source--restore-from-buffer-advice))
      (dolist (hook save-hook-triggers)
        (remove-hook hook #'smart-input-source--save-to-buffer-advice))
      (dolist (hook restore-hook-triggers)
        (remove-hook hook #'smart-input-source--restore-from-buffer-advice))
      (setq -preserve-inited nil))))

(defun -save-to-buffer ()
  "Save buffer input source."
  (setq -saved-in-buffer (-get)))

(defun -restore-from-buffer ()
  "Restore buffer input source."
  (-set (or -saved-in-buffer ENGLISH)))

;; end of namespace
)

(provide 'smart-input-source)
;;; smart-input-source.el ends here
