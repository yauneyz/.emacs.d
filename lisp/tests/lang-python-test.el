;;; lang-python-test.el --- Python environment regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'lang-python)
(require 'dap-python)
(require 'python-pytest)

(ert-deftest +python/lsp-clients-load-without-invalid-predicate ()
  "The ts-query client must not abort Python's language-server startup."
  (should (require 'lsp-ts-query))
  (should (vectorp lsp-ts-query-parser-install-directories)))

(defun +python-test/make-venv (root name)
  "Make a fake executable in ROOT/NAME and return the environment path."
  (let* ((venv (expand-file-name name root))
         (python (expand-file-name "bin/python" venv)))
    (make-directory (file-name-directory python) t)
    (write-region "#!/bin/sh\nexit 0\n" nil python nil 'silent)
    (set-file-modes python #o755)
    venv))

(ert-deftest +python/environment-is-shared-and-buffer-local ()
  "Nested source files use one interpreter without changing other buffers."
  (let* ((root (make-temp-file "python-project-" t))
         (venv (+python-test/make-venv root ".venv"))
         (python (expand-file-name "bin/python" venv))
         (original-env (copy-sequence process-environment))
         (original-exec-path (copy-sequence exec-path)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "src/pkg" root) t)
          (with-temp-buffer
            (setq default-directory (expand-file-name "src/pkg/" root))
            (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
              (+python/setup-environment)
              (+python/setup-environment))
            (should (equal python-shell-interpreter python))
            (should (equal (funcall (car lsp-pyright-python-search-functions)) python))
            (should (equal flycheck-python-mypy-python-executable python))
            (should (equal dap-python-executable python))
            (should (equal python-pytest-executable
                           (concat (shell-quote-argument python) " -m pytest")))
            (should (equal (getenv "VIRTUAL_ENV") venv))
            (should (member (expand-file-name "src" root)
                            (split-string (getenv "PYTHONPATH") path-separator t)))
            (should (= 1 (cl-count (expand-file-name "bin" venv)
                                   exec-path :test #'equal))))
          (should (equal process-environment original-env))
          (should (equal exec-path original-exec-path)))
      (delete-directory root t))))

(ert-deftest +python/nearest-environment-wins ()
  "A nested venv takes precedence over an outer .venv and active venv."
  (let* ((root (make-temp-file "python-project-" t))
         (outer (+python-test/make-venv root ".venv"))
         (nested (expand-file-name "nested" root))
         (inner (+python-test/make-venv nested "venv")))
    (unwind-protect
        (with-temp-buffer
          (setq default-directory (file-name-as-directory nested))
          (let ((process-environment (copy-sequence process-environment)))
            (setenv "VIRTUAL_ENV" outer)
            (should (equal (+python/find-venv) inner))))
      (delete-directory root t))))

(ert-deftest +python/active-environment-fallback ()
  "An activated environment is used when the directory has no local venv."
  (let* ((root (make-temp-file "python-project-" t))
         (venv (+python-test/make-venv root "external")))
    (unwind-protect
        (with-temp-buffer
          (setq default-directory (file-name-as-directory root))
          (let ((process-environment (copy-sequence process-environment)))
            (setenv "VIRTUAL_ENV" venv)
            (should (equal (+python/find-venv) venv))))
      (delete-directory root t))))

(ert-deftest +python/remote-environment-is-untouched ()
  "Local Python and driver paths must not leak into TRAMP buffers."
  (with-temp-buffer
    (setq default-directory "/ssh:example.invalid:/project/")
    (let ((original-env process-environment)
          (original-exec-path exec-path))
      (+python/setup-environment)
      (should (eq process-environment original-env))
      (should (eq exec-path original-exec-path)))))

(ert-deftest +python/pytest-buffer-inherits-project-environment ()
  "The pytest process buffer must find the same venv and src path."
  (let* ((root (make-temp-file "python-project-" t))
         (venv (+python-test/make-venv root ".venv")))
    (unwind-protect
        (with-temp-buffer
          (setq default-directory (file-name-as-directory root))
          (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
            (run-hooks 'python-pytest-setup-hook))
          (should (equal (getenv "VIRTUAL_ENV") venv)))
      (delete-directory root t))))

(ert-deftest +python/no-conflicting-formatters ()
  "Both Python modes leave formatting to Apheleia."
  (dolist (mode '(python-mode python-ts-mode))
    (with-temp-buffer
      (setq major-mode mode)
      (+format/enable-format-all)
      (should-not (bound-and-true-p format-all-mode)))))

(provide 'lang-python-test)
;;; lang-python-test.el ends here
