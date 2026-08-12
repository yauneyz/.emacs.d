;;; 32-fountain-test.el --- Tests for Fountain export -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require '32-fountain)

(ert-deftest +fountain-pdf-output-file-uses-script-pdf-directory ()
  (should
   (equal (+fountain--pdf-output-file "/tmp/My Script.fountain")
          "/tmp/script_pdf/My Script.pdf")))

(ert-deftest +fountain-export-pdf-creates-directory-and-overwrites ()
  (let* ((test-directory (make-temp-file "fountain-export-test-" t))
         (source-file (expand-file-name "My Script.fountain" test-directory))
         (output-file (expand-file-name "script_pdf/My Script.pdf"
                                        test-directory))
         (old-pdf "%PDF-old")
         (new-pdf "%PDF-new"))
    (unwind-protect
        (progn
          (write-region "INT. ROOM - DAY\n\nSome action.\n" nil source-file nil 'silent)
          (make-directory (file-name-directory output-file) t)
          (write-region old-pdf nil output-file nil 'silent)
          (with-temp-buffer
            (setq buffer-file-name source-file)
            (cl-letf (((symbol-function 'executable-find)
                       (lambda (_program) "/nix/store/screenplain/bin/screenplain"))
                      ((symbol-function 'save-buffer) #'ignore)
                      ((symbol-function 'process-file)
                       (lambda (_program _infile _destination _display
                                _source temporary-file)
                         (write-region new-pdf nil temporary-file nil 'silent)
                         0)))
              (should (equal (+fountain/export-pdf) output-file))))
          (should (file-directory-p (file-name-directory output-file)))
          (should (equal (with-temp-buffer
                           (insert-file-contents-literally output-file)
                           (buffer-string))
                         new-pdf)))
      (delete-directory test-directory t))))

(ert-deftest +fountain-export-pdf-screenplain-integration ()
  (skip-unless (executable-find "screenplain"))
  (let* ((test-directory (make-temp-file "fountain-screenplain-test-" t))
         (source-file (expand-file-name "Integration Test.fountain"
                                        test-directory))
         (output-file (+fountain--pdf-output-file source-file)))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name source-file)
          (insert "Title: Integration Test\nAuthor: Codex\n\n"
                  "INT. TEST ROOM - DAY\n\n"
                  "A real Screenplain export succeeds.\n")
          (should (equal (+fountain/export-pdf) output-file))
          (should (+fountain--pdf-file-p output-file)))
      (delete-directory test-directory t))))

(provide '32-fountain-test)
;;; 32-fountain-test.el ends here
