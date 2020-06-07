;;; smart-input-source.el --- Switch OS native input source smartly -*- lexical-binding: t; -*-

;; URL: https://github.com/laishulu/emacs-smart-input-source
;; Created: March 27th, 2020
;; Keywords: convenience
;; Package-Requires: ((names "0.5") (emacs "25"))
;; Version: 0.1

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

(defvar english-pattern "[a-zA-Z]"
  "Pattern to identify a character as english.")
(make-variable-buffer-local (quote english-pattern))

(defvar other-pattern "\\cc"
  "Pattern to identify a character as other language.")
(make-variable-buffer-local (quote other-pattern))

(defvar blank-pattern "[:blank:]"
  "Pattern to identify a character as blank.")
(make-variable-buffer-local (quote blank-pattern))

(defvar english-input-source "com.apple.keylayout.US"
  "Input source for english.")
(make-variable-buffer-local (quote english-input-source))

(defvar other-input-source "com.sogou.inputmethod.sogou.pinyin"
  "Input source for other language.")
(make-variable-buffer-local (quote other-input-source))

(defvar external-ism "macism"
  "Path of external ism.")
(make-variable-buffer-local (quote external-ism))

(defvar aggressive-line t
  "Aggressively detect context across blank lines.")
(make-variable-buffer-local (quote aggressive-line))

(defvar do-get-input-source nil
  "Function to get the current input source.

Should return a string which is the id of the input source.")
(make-variable-buffer-local (quote do-get-input-source))

(defvar do-set-input-source nil
  "Function to set the input source.

Should accept a string which is the id of the input source.")
(make-variable-buffer-local (quote do-set-input-source))

(defvar with-inline-english t
  "Enable the inline english feature.")
(make-variable-buffer-local (quote with-inline-english))

(defface inline-english-face
  '()
  "Face of the inline english region overlay.")

(set-face-attribute
 'smart-input-source-inline-english-face nil
 :background (face-attribute 'font-lock-constant-face :foreground)
 :foreground (face-attribute 'default :background))

;;
;; Following symbols are not supposed to be used directly by end user.
;;

(defvar -inline-overlay nil
  "The active inline overlay.")
(make-variable-buffer-local (quote -inline-overlay))

(defvar -last-inline-overlay-start-position nil
  "Start position of the last inline overlay (already deactivated).")
(make-variable-buffer-local (quote -last-inline-overlay-start-position))

(defvar -last-inline-overlay-end-position nil
  "End position of the last inline overlay (already deactivated).")
(make-variable-buffer-local (quote -last-inline-overlay-end-position))

(declare-function evil-normal-state-p "ext:evil-states.el" (&optional state) t)
(declare-function evil-visual-state-p "ext:evil-states.el" (&optional state) t)
(declare-function evil-motion-state-p "ext:evil-states.el" (&optional state) t)
(declare-function evil-operator-state-p "ext:evil-states.el" (&optional state) t)
(declare-function company--active-p "ext:company.el" () t)
(declare-function mac-input-source "ext:macfns.c" (&optional SOURCE FORMAT) t)
(declare-function mac-select-input-source "ext:macfns.c"
                  (SOURCE &optional SET-KEYBOARD-LAYOUT-OVERRIDE-P) t)

(defconst ENGLISH 1)
(defconst OTHER 2)

;; Emacs mac port builtin input source manager
(defconst ISM-EMP 1)

(defvar -ism nil "The input source manager.")
(make-variable-buffer-local (quote -ism))

(defun -string-match-p (regexp str &optional start)
  "Robust wrapper of `string-match-p'.

Works when REGEXP or STR is not a string REGEXP, STR, START all has the same
meanings as `string-match-p'."
  (and (stringp regexp)
       (stringp str)
       (string-match-p regexp str start)))

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
      (skip-chars-backward (concat blank-pattern "\n"))
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
      (skip-chars-forward (concat blank-pattern "\n"))
      (let ((cross-line-char (char-after (point))))
        (make-fore-detect :to to
                          :char (when char (string char))
                          :cross-line-to (point)
                          :cross-line-char (when cross-line-char
                                             (string cross-line-char)))))))

(defun -guess-context ()
  "Guest the language context for the current point."
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
     ;; [lastest overlay: last char is not other language]
     ;; [blank: in or out of lastest overlay][^][not english]
     ((and -last-inline-overlay-start-position
           -last-inline-overlay-end-position
           (>= back-to -last-inline-overlay-start-position)
           (<= back-to -last-inline-overlay-end-position)
           (< back-to (point))
           (not (-string-match-p other-pattern back-char))
           (not (-string-match-p english-pattern fore-char)))
      OTHER)
     ;; [^][blank][other lanuage]
     ((and (< fore-to (line-end-position))
           (> fore-to (point))
           (-string-match-p other-pattern fore-char))
      ENGLISH)
     ;; [line beginning][^][english]
     ;; [english][^][english]
     ;; [not english][blank][^][english]
     ((and (or (= back-to (line-beginning-position))
               (and (= back-to (point))
                    (-string-match-p english-pattern back-char))
               (and (< back-to (point))
                    (not (-string-match-p english-pattern back-char))))
           (< fore-to (line-end-position))
           (= fore-to (point))
           (-string-match-p english-pattern fore-char))
      ENGLISH)
     ;; [line beginning][^][other lanuage]
     ;; [other language][^][other lanuage]
     ;; [not other language][blank][^][other lanuage]
     ((and (or (= back-to (line-beginning-position))
               (and (= back-to (point))
                    (-string-match-p other-pattern back-char))
               (and (< back-to (point))
                    (not (-string-match-p other-pattern back-char))))
           (< fore-to (line-end-position))
           (= fore-to (point))
           (-string-match-p other-pattern fore-char))
      OTHER)
     ;; [english: include the previous line][blank][^]
     ((and (or aggressive-line
               (> cross-line-back-to (line-beginning-position 0)))
           (< cross-line-back-to (line-beginning-position))
           (-string-match-p english-pattern cross-line-back-char))
      ENGLISH)
     ;; [other lanuage: include the previous line][blank][^]
     ((and (or aggressive-line
               (> cross-line-back-to (line-beginning-position 0)))
           (< cross-line-back-to (line-beginning-position))
           (-string-match-p other-pattern cross-line-back-char))
      OTHER)
     ;; [^][blank][english: include the next line]
     ((and (or aggressive-line
               (< cross-line-fore-to (line-end-position 2)))
           (> cross-line-fore-to (line-end-position))
           (-string-match-p english-pattern cross-line-fore-char))
      ENGLISH)
     ;; [^][blank][other lanuage: include the next line]
     ((and (or aggressive-line
               (< cross-line-fore-to (line-end-position 2)))
           (> cross-line-fore-to (line-end-position))
           (-string-match-p other-pattern cross-line-fore-char))
      OTHER))))

(defun -mk-get-input-source-fn ()
  "Make a function to be bound to `do-get-input-source'."
  (when -ism
    (if (equal -ism ISM-EMP)
        #'mac-input-source
      (lambda ()
        (string-trim (shell-command-to-string -ism))))))

(defun -mk-set-input-source-fn ()
  "Make a function to be bound to `do-set-input-source'."
  (when -ism
    (if (equal -ism ISM-EMP)
        (lambda (source) (mac-select-input-source source))
      (lambda (source)
        (string-trim
         (shell-command-to-string (concat -ism " " source)))))))

(defun -get-input-source ()
  "Get the input source id."
  (when (functionp do-get-input-source)
    (funcall do-get-input-source)))

(defun -set-input-source (lang)
  "Set the input source according to language LANG, avoiding unnecessary switch."
  (when (functionp do-set-input-source)
    (let ((ENGLISH_SOURCE english-input-source)
          (OTHER_SOURCE other-input-source))
      (pcase (-get-input-source)
        ((pred (equal ENGLISH_SOURCE))
         (when (equal lang OTHER)
           (funcall do-set-input-source OTHER_SOURCE)))
        ((pred (equal OTHER_SOURCE))
         (when (equal lang ENGLISH)
           (funcall do-set-input-source ENGLISH_SOURCE)))))))

;;;###autoload
(define-minor-mode mode
  "Switch input source smartly.

- For GUI session of ~emacs mac port~, use native API to select input source
  for better performance.
- If ~emacs mac port~ is unavailable, or in terminal session, use external ism
  tool to select input source.
- If no ism found, then do nothing."
  :init-value nil
  (when (and (string= (window-system) "mac")
             (fboundp 'mac-input-source))
    (setq -ism ISM-EMP))

  (when (and (not -ism) external-ism)
    (let ((ism-path (executable-find external-ism)))
      (setq -ism ism-path)))

  (when -ism
    (unless (functionp do-get-input-source)
      (setq do-get-input-source (-mk-get-input-source-fn)))

    (unless (functionp do-set-input-source)
      (setq do-set-input-source (-mk-set-input-source-fn)))

    (if mode
        (progn
          (add-hook 'post-self-insert-hook
                    #'smart-input-source-check-to-activate-overlay)
          (when (featurep 'evil)
            (add-hook 'evil-insert-state-entry-hook
                      #'smart-input-source-do)
            (add-hook 'evil-insert-state-exit-hook
                      #'smart-input-source-set-input-source-english)))
      (remove-hook 'post-self-insert-hook
                   #'smart-input-source-check-to-activate-overlay)
      (when (featurep 'evil)
        (remove-hook 'evil-insert-state-entry-hook
                     #'smart-input-source-do)
        (remove-hook 'evil-insert-state-exit-hook
                     #'smart-input-source-set-input-source-english)))))

(defun check-to-activate-overlay()
  "Check whether to activate the inline english region overlay.

Check the context to determine whether the overlay should be activated or not,
if the answer is yes, then activate the /inline english region/, set the
input source to English, and then return ~t~."
  (when (and with-inline-english
             mode
             (not (overlayp -inline-overlay))
             (not (button-at (point)))
             (not (and (featurep 'evil)
                       (or (evil-normal-state-p)
                           (evil-visual-state-p)
                           (evil-motion-state-p)
                           (evil-operator-state-p)))))
    (let* ((back-detect (-back-detect-chars))
           (back-to (back-detect-to back-detect))
           (back-char (back-detect-char back-detect)))
      ;; [other lanuage][blank][^]
      (when (and (> back-to (line-beginning-position))
                 (< back-to (point))
                 (-string-match-p other-pattern back-char))
        (activate-inline-overlay back-to)
        (set-input-source-english)
        t))))

(defun do ()
  "Do the smart input source switching.

If no need to activate the overlay, then follow the context to switch input
source."
  (unless (check-to-activate-overlay)
    (follow-context)))

(defun follow-context ()
  "Follow the context to switch input source."
  (when mode
    (let ((context (-guess-context)))
      (when context
        (-set-input-source context)))))

(defun set-input-source-english ()
  "Set input source to `english-input-source'."
  (when mode
    (-set-input-source ENGLISH)))

(defun set-input-source-other ()
  "Set input source to `other-input-source'."
  (-set-input-source OTHER))

;;
;; The following is about the inline english region overlay
;;
(defun check-to-deactivate-overlay ()
  "Check whether to deactivate the inline english region overlay."
  (when (and mode (overlayp -inline-overlay))
    (cond

     ((= (overlay-start -inline-overlay)
         (overlay-end -inline-overlay))
      (deactivate-inline-overlay)
      ;; revert input source
      (set-input-source-other))

     ((or(< (point) (overlay-start -inline-overlay))
         (> (point) (overlay-end -inline-overlay)))
      (deactivate-inline-overlay)))))

(defun activate-inline-overlay (start)
  "Activate the inline english region overlay from START."
  (when (overlayp -inline-overlay)
    (delete-overlay -inline-overlay))
  (setq -inline-overlay (make-overlay start (point) nil t t ))
  (overlay-put -inline-overlay 'face 'smart-input-source-inline-english-face)
  (overlay-put -inline-overlay 'keymap
               (let ((keymap (make-sparse-keymap)))
                 (define-key keymap (kbd "RET")
                   'smart-input-source-end-inline-overlay)
                 (define-key keymap (kbd "<return>")
                   'smart-input-source-end-inline-overlay)
                 keymap))
  (setq -last-inline-overlay-start-position nil)
  (setq -last-inline-overlay-end-position nil)
  (add-hook 'post-command-hook
            #'smart-input-source-check-to-deactivate-overlay)
  (message "Press <RETURN> to enable input source switching again."))

(defun end-inline-overlay ()
  "End the current active inline overlay."
  (interactive)
  (if (and (featurep 'evil)
           (company--active-p))
      (company-complete-selection)
    (smart-input-source-deactivate-inline-overlay)
    (smart-input-source-follow-context)))

(defun deactivate-inline-overlay ()
  "Deactivate the inline english region overlay."
  (interactive)
  (remove-hook 'post-command-hook
               #'smart-input-source-check-to-deactivate-overlay)
  (when (overlayp -inline-overlay)
    (setq -last-inline-overlay-start-position (overlay-start -inline-overlay))
    (setq -last-inline-overlay-end-position (overlay-end -inline-overlay))
    (delete-overlay -inline-overlay)
    (setq -inline-overlay nil)))

;; end of namespace
)

(provide 'smart-input-source)
;;; smart-input-source.el ends here
