;;; sis-cursor-test.el --- Cursor color regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'sis)

(defface sis-test-inherited-cursor
  '((t (:inherit cursor)))
  "Test modal cursor inheriting the frame cursor."
  :group 'sis)

(defface sis-test-explicit-cursor
  '((t (:background "blue")))
  "Test modal cursor with its own color."
  :group 'sis)

(defmacro sis-test--with-gui-cursor (&rest body)
  "Run BODY with GUI cursor updates simulated in batch Emacs."
  (declare (indent 0) (debug t))
  `(let ((sis-default-cursor-color "#cf7fa7")
         (sis-other-cursor-color "orange")
         (sis--current 'english)
         (sis--previous nil)
         (sis--for-buffer nil)
         (sis--for-buffer-locked nil)
         (sis-change-hook '(sis--update-cursor-color))
         (sis-cursor-color-restore-hook nil)
         (original-color (face-background 'cursor)))
     (unwind-protect
         (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t))
                   ;; GUI `set-cursor-color' also changes the cursor face;
                   ;; batch Emacs has no graphical frame to do this itself.
                   ((symbol-function 'set-cursor-color)
                    (lambda (color) (set-face-background 'cursor color))))
           (set-cursor-color sis-default-cursor-color)
           ,@body)
       (set-face-background 'cursor original-color))))

(ert-deftest sis-cursor-restores-inherited-color ()
  "A modal cursor inheriting `cursor' must not retain the other color."
  (sis-test--with-gui-cursor
    (setq sis-cursor-color-restore-hook
          (list (lambda ()
                  (set-cursor-color
                   (face-attribute 'sis-test-inherited-cursor :background nil t)))))
    (dotimes (_ 2)
      (sis--update-state 'other)
      (should (equal (face-background 'cursor) "orange"))
      (sis--update-state 'english)
      (should (equal (face-background 'cursor) "#cf7fa7")))))

(ert-deftest sis-cursor-preserves-explicit-modal-color ()
  "A modal editor can still override the default English color."
  (sis-test--with-gui-cursor
    (setq sis-cursor-color-restore-hook
          (list (lambda ()
                  (set-cursor-color
                   (face-background 'sis-test-explicit-cursor)))))
    (sis--update-state 'other)
    (should (equal (face-background 'cursor) "orange"))
    (sis--update-state 'english)
    (should (equal (face-background 'cursor) "blue"))))

(ert-deftest sis-cursor-restores-default-without-modal-editor ()
  "No restore hook is needed to recover the English color."
  (sis-test--with-gui-cursor
    (sis--update-state 'other)
    (should (equal (face-background 'cursor) "orange"))
    (sis--update-state 'english)
    (should (equal (face-background 'cursor) "#cf7fa7"))))

;;; sis-cursor-test.el ends here
