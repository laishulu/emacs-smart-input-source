;;; sis-context-test.el --- Context detection regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'sis)

(ert-deftest sis-context-prefers-current-line ()
  (dolist (example '(("English\n" "中文" other)
                     ("English\n " " 中文" other)
                     ("中文\n" "English" english)
                     ("English\n\n" "" english)
                     ("中文\n\n" "" other)
                     ("a" "中" english)))
    (pcase-let ((`(,before ,after ,expected) example))
      (with-temp-buffer
        (insert before)
        (save-excursion (insert after))
        (should (eq (sis--context-guess) expected))))))

;;; sis-context-test.el ends here
