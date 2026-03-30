;;; 40-evil-test.el --- Regression tests for Evil clipboard handling -*- lexical-binding: t; -*-

(require 'ert)
(require '40-evil)

(ert-deftest my/restore-yank-handler-from-clipboard-restores-known-kill-ring-entry ()
  "Clipboard text copied from Emacs should keep its Evil yank metadata."
  (let* ((handler '(evil-yank-line-handler nil t))
         (entry (copy-sequence "linewise\n"))
         (kill-ring (list entry))
         (result nil))
    (put-text-property 0 (length entry) 'yank-handler handler entry)
    (setq result
          (my/restore-yank-handler-from-clipboard
           (lambda (&rest _) (copy-sequence "linewise\n"))))
    (should (equal (get-text-property 0 'yank-handler result) handler))))

(ert-deftest my/restore-yank-handler-from-clipboard-ignores-external-newline-text ()
  "External clipboard text should not become linewise just because it ends in a newline."
  (let ((kill-ring nil)
        (result nil))
    (setq result
          (my/restore-yank-handler-from-clipboard
           (lambda (&rest _) (copy-sequence "external\n"))))
    (should-not (get-text-property 0 'yank-handler result))))
