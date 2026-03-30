;;; 30-editor-test.el --- Regression tests for editor plumbing -*- lexical-binding: t; -*-

(require 'ert)
(require '30-editor)

(ert-deftest my/configure-clipboard-keeps-default-request-types ()
  "Clipboard reads should include the Wayland UTF-8 MIME target."
  (let ((x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
        (select-enable-clipboard nil)
        (select-enable-primary t))
    (my/configure-clipboard)
    (should (equal x-select-request-type
                   '(UTF8_STRING COMPOUND_TEXT TEXT STRING
                                 text/plain\;charset=utf-8)))
    (should select-enable-clipboard)
    (should-not select-enable-primary)))
