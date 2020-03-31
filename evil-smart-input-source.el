;;; evil-smart-input-source.el --- Switch input source smartly according to context language and evil mode -*- lexical-binding: t; -*-

;; URL: https://github.com/laishulu/evil-smart-input-source
;; Created: March 27th, 2020
;; Keywords: convenience
;; Package-Requires: ((names "0.5") (evil "1.0") (emacs "25"))
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
;; This package provide modes to switch input source for smartly according to
;; context language and evil mode.
;; For more information see the README in the GitHub repo.

;;; Code:

;; `define-namespace' is autoloaded, so there's no need to require `names'.
;; However, requiring it here means it will also work for people who don't
;; install through package.el.
(eval-when-compile (require 'names))

(define-namespace evil-smart-input-source-

(defconst ENGLISH "english")
(defconst OTHER "other")

(defvar -english-pattern "[a-zA-Z]"
  "Pattern to identify a character as english.")

(defvar -other-pattern "\\cc"
  "Pattern to identify a character as other language.")

(defvar -blank-pattern "[:blank:]"
  "Pattern to identify a character as blank.")

(defvar -english-input-source "com.apple.keylayout.US"
  "Input source for english.")

(defvar -other-input-source "com.sogou.inputmethod.sogou.pinyin"
  "Input source for other lanugage.")

(defvar -macism (executable-find "macism")
  "Path of macism.")

(defvar -do-get-input-source nil
  "Function to get the current input source

Should return a string which is the id of the input source
")

(defvar -do-set-input-source nil
  "Function to set the input source

Should accept a string which is the id of the input source
")

(defun -string-match-p (regexp str &optional start)
  "Robust wrapper of `string-match-p'.

works when REGEXP or STR is not a string REGEXP, STR, START all has the same
meanings as `string-match-p`.
"
  (and (stringp regexp)
       (stringp str)
       (string-match-p regexp str start)))

(defun -back-detect-chars ()
  "Detect char backward by two steps.

  First backward skip blank in the current line,
  then backward skip blank across lines."
  (save-excursion
    (skip-chars-backward -blank-pattern)
    (let ((backward (point))
          (before (char-before (point))))
      (skip-chars-backward (concat -blank-pattern "\n"))
      (let ((cross-line-before (char-before (point))))
        (list (when cross-line-before (string cross-line-before))
              (when before (string before))
              backward)))))

(defun -fore-detect-chars ()
  "Detect char forward.

  Forward skip blank in the current line."
  (save-excursion
    (skip-chars-forward -blank-pattern)
    (let ((forward (point))
          (after (char-after (point))))
      (list forward
            (when after (string after))))))

(defun -guess-context ()
  "Guest the language context for the current point."
  (let* ((back-detection (-back-detect-chars))
         (fore-detection (-fore-detect-chars))
         (cross-line-before (nth 0 back-detection))
         (before (nth 1 back-detection))
         (back (nth 2 back-detection))
         (fore (nth 0 fore-detection))
         (after (nth 1 fore-detection))
         (context nil))
    (cond ((and (> back (line-beginning-position))
                (< back (point))
                (-string-match-p -other-pattern before))
           ENGLISH)
          ((and (< fore (line-end-position))
                (> fore (point))
                (-string-match-p -other-pattern after))
           ENGLISH)
          ((and (< fore (line-end-position))
                (= fore (point))
                (-string-match-p -other-pattern after))
           OTHER)
          ((-string-match-p -english-pattern cross-line-before) ENGLISH)
          ((-string-match-p -other-pattern cross-line-before) OTHER)
          (t context))))

(defun -prober ()
  "Determine which language to use."
  (let ((lang nil))
    (when (or (button-at (point))
              (or (evil-normal-state-p)
                  (evil-visual-state-p)
                  (evil-motion-state-p)
                  (evil-operator-state-p)))
      (setq lang (or lang ENGLISH)))
    (setq lang (or lang (-guess-context)))
    lang))

(defun -mk-get-input-source-fn ()
  "Make a function to be bound to -do-get-input-source."
  (cond ((and (string= (window-system) "mac") (fboundp 'mac-input-source))
         'mac-input-source)
        ((and (string= system-type "darwin")
              (file-executable-p evil-smart-input-source--macism))
         #'(lambda ()
             (string-trim (shell-command-to-string
                           evil-smart-input-source--macism))))))

(defun -mk-set-input-source-fn ()
  "Make a function to be bound to -do-set-input-source."
  (cond ((and (string= (window-system) "mac") (fboundp 'mac-input-source))
         #'(lambda (source) (mac-select-input-source source)))
        ((and (string= system-type "darwin")
              (file-executable-p evil-smart-input-source--macism))
         #'(lambda (source)
             (string-trim (shell-command-to-string
                           (concat evil-smart-input-source--macism " " source)))))))

(defun -get-input-source ()
  "Get the input source id."
  (funcall -do-get-input-source))

(defun -set-input-source (lang)
  "Set the input source according to lanuage LANG, avoiding unecessary switch."
  (let ((ENGLISH_SOURCE -english-input-source)
        (OTHER_SOURCE -other-input-source))
    (pcase (-get-input-source)
      ((pred (equal ENGLISH_SOURCE))
       (when (equal lang OTHER)
         (funcall -do-set-input-source OTHER_SOURCE)))
      ((pred (equal OTHER_SOURCE))
       (when (equal lang ENGLISH)
         (funcall -do-set-input-source ENGLISH_SOURCE))))))

(defun adaptive-input-source ()
  "Adaptively switch to the input source."
  (let ((source (-prober)))
    (when source
      (-set-input-source source))))

(defun set-input-source-english ()
  "Set input source to `english-input-source`."
  (-set-input-source ENGLISH))

(defun set-input-source-other ()
  "Set input source to `other-input-source`."
  (-set-input-source OTHER))

;;;###autoload
(define-minor-mode mode
  "Automatically switch input method for evil.

For GUI session of `emacs mac port`, use native API to select input source
for better performance.
If `emacs mac port` is unavailable, or in terminal session, use `macism` or
other compatible CLI tool to select input source.
If even `macism` like tool is unailable, then do nothing.
"
  :init-value nil
  (when (or (and (string= (window-system) "mac")
                 (fboundp 'mac-input-source))
            (file-executable-p -macism))

    (unless (functionp -do-get-input-source)
      (setq -do-get-input-source (-mk-get-input-source-fn)))

    (unless (functionp -do-set-input-source)
      (setq -do-set-input-source (-mk-set-input-source-fn)))

    (if mode
        (progn
          (add-hook 'evil-insert-state-entry-hook
                    #'evil-smart-input-source-adaptive-input-source)
          (add-hook 'post-self-insert-hook
                    #'evil-smart-input-source-adaptive-input-source)
          (add-hook 'evil-insert-state-exit-hook
                    #'evil-smart-input-source-set-input-source-english))
      (remove-hook 'evil-insert-state-entry-hook
                   #'evil-smart-input-source-adaptive-input-source)
      (remove-hook 'post-self-insert-hook
                   #'evil-smart-input-source-adaptive-input-source)
      (remove-hook 'evil-insert-state-exit-hook
                   #'evil-smart-input-source-set-input-source-english))))

;; end of namespace
)

;;;###autoload
(define-globalized-minor-mode
  global-evil-smart-input-source-mode
  evil-smart-input-source-mode
  (lambda () (evil-smart-input-source-mode t))
  :group 'evil-smart-input-source)

(provide 'evil-smart-input-source)
;;; evil-smart-input-source.el ends here
