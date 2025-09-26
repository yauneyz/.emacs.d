;;; 53-lang-testing.el --- Universal testing framework -*- lexical-binding: t; -*-

;;; Commentary:
;; Language-agnostic testing framework that provides consistent testing
;; operations across all programming languages, with intelligent backend
;; selection and test result management.

;;; Code:

(require 'hydra)

;; =============================================================================
;; Testing Configuration
;; =============================================================================

(defvar +test/last-command nil
  "Last test command that was run.")

(defvar +test/results-buffer "*test-results*"
  "Buffer name for unified test results.")

(defvar +test/coverage-enabled nil
  "Whether coverage is currently enabled.")

;; =============================================================================
;; Test Backend Detection
;; =============================================================================

(defun +test/get-backend ()
  "Get the appropriate testing backend for current language."
  (pcase major-mode
    ((or 'clojure-mode 'clojurescript-mode 'clojurec-mode)
     (if (and (bound-and-true-p cider-mode) (cider-connected-p))
         'cider
       'shadow-cljs))
    ((or 'go-mode 'go-ts-mode)
     (if (+test/has-gotestsum?)
         'gotestsum
       'gotest))
    ((or 'python-mode 'python-ts-mode)
     (cond
      ((+test/has-pytest?) 'pytest)
      ((+test/has-nose?) 'nose)
      (t 'unittest)))
    ((or 'rust-mode 'rustic-mode 'rust-ts-mode) 'cargo)
    ((or 'javascript-mode 'typescript-mode 'tsx-mode) 'jest)
    ((or 'haskell-mode 'haskell-ts-mode) 'stack)
    (_ 'generic)))

(defun +test/has-gotestsum? ()
  "Check if gotestsum is available."
  (executable-find "gotestsum"))

(defun +test/has-pytest? ()
  "Check if pytest is available."
  (executable-find "pytest"))

(defun +test/has-nose? ()
  "Check if nose is available."
  (executable-find "nosetests"))

;; =============================================================================
;; Test Execution Functions
;; =============================================================================

(defun +test/run-at-point ()
  "Run test at point using appropriate backend."
  (interactive)
  (let ((backend (+test/get-backend)))
    (pcase backend
      ('cider (+test/cider-run-test-at-point))
      ('gotest (+test/go-run-test-at-point))
      ('gotestsum (+test/go-run-test-at-point-gotestsum))
      ('pytest (+test/python-run-test-at-point))
      ('cargo (+test/rust-run-test-at-point))
      ('jest (+test/js-run-test-at-point))
      ('stack (+test/haskell-run-test-at-point))
      (_ (+test/generic-run-test-at-point)))))

(defun +test/run-file ()
  "Run all tests in current file."
  (interactive)
  (let ((backend (+test/get-backend)))
    (pcase backend
      ('cider (+test/cider-run-file-tests))
      ('gotest (+test/go-run-file-tests))
      ('gotestsum (+test/go-run-file-tests-gotestsum))
      ('pytest (+test/python-run-file-tests))
      ('cargo (+test/rust-run-file-tests))
      ('jest (+test/js-run-file-tests))
      ('stack (+test/haskell-run-file-tests))
      (_ (+test/generic-run-file-tests)))))

(defun +test/run-project ()
  "Run all tests in current project."
  (interactive)
  (let ((backend (+test/get-backend)))
    (pcase backend
      ('cider (+test/cider-run-project-tests))
      ('gotest (+test/go-run-project-tests))
      ('gotestsum (+test/go-run-project-tests-gotestsum))
      ('pytest (+test/python-run-project-tests))
      ('cargo (+test/rust-run-project-tests))
      ('jest (+test/js-run-project-tests))
      ('stack (+test/haskell-run-project-tests))
      (_ (+test/generic-run-project-tests)))))

(defun +test/run-all ()
  "Run all tests including integration tests."
  (interactive)
  (let ((backend (+test/get-backend)))
    (pcase backend
      ('cider (+test/cider-run-all-tests))
      ('gotest (+test/go-run-all-tests))
      ('gotestsum (+test/go-run-all-tests-gotestsum))
      ('pytest (+test/python-run-all-tests))
      ('cargo (+test/rust-run-all-tests))
      ('jest (+test/js-run-all-tests))
      ('stack (+test/haskell-run-all-tests))
      (_ (+test/generic-run-all-tests)))))

(defun +test/rerun ()
  "Rerun the last test command."
  (interactive)
  (if +test/last-command
      (progn
        (message "Rerunning: %s" +test/last-command)
        (funcall +test/last-command))
    (message "No previous test to rerun")))

;; =============================================================================
;; Coverage Functions
;; =============================================================================

(defun +test/toggle-coverage ()
  "Toggle test coverage reporting."
  (interactive)
  (setq +test/coverage-enabled (not +test/coverage-enabled))
  (let ((backend (+test/get-backend)))
    (pcase backend
      ('cider (+test/cider-toggle-coverage))
      ('gotest (+test/go-toggle-coverage))
      ('pytest (+test/python-toggle-coverage))
      ('cargo (+test/rust-toggle-coverage))
      ('stack (+test/haskell-toggle-coverage))
      (_ (message "Coverage toggled %s for %s"
                  (if +test/coverage-enabled "ON" "OFF")
                  backend)))))

;; =============================================================================
;; Clojure/Cider Test Functions
;; =============================================================================

(defun +test/cider-run-test-at-point ()
  "Run Clojure test at point using Cider."
  (setq +test/last-command #'+test/cider-run-test-at-point)
  (if (cider-connected-p)
      (cider-test-run-test)
    (message "Cider not connected. Connect REPL first.")))

(defun +test/cider-run-file-tests ()
  "Run all Clojure tests in current file."
  (setq +test/last-command #'+test/cider-run-file-tests)
  (if (cider-connected-p)
      (cider-test-run-ns-tests)
    (message "Cider not connected. Connect REPL first.")))

(defun +test/cider-run-project-tests ()
  "Run all Clojure tests in current project."
  (setq +test/last-command #'+test/cider-run-project-tests)
  (if (cider-connected-p)
      (cider-test-run-project-tests)
    (message "Cider not connected. Connect REPL first.")))

(defun +test/cider-run-all-tests ()
  "Run all Clojure tests including loaded namespaces."
  (setq +test/last-command #'+test/cider-run-all-tests)
  (if (cider-connected-p)
      (cider-test-run-all-tests)
    (message "Cider not connected. Connect REPL first.")))

(defun +test/cider-toggle-coverage ()
  "Toggle Clojure test coverage."
  (if (cider-connected-p)
      (if +test/coverage-enabled
          (cider-eval-string "(require 'cloverage.coverage) (cloverage.coverage/instrument)")
        (message "Coverage support requires cloverage plugin"))
    (message "Cider not connected.")))

;; =============================================================================
;; Go Test Functions
;; =============================================================================

(defun +test/go-run-test-at-point ()
  "Run Go test at point using go test."
  (setq +test/last-command #'+test/go-run-test-at-point)
  (if (fboundp 'go-test-current-test)
      (go-test-current-test)
    (+test/go-compile-test (format "go test -run %s" (+test/go-test-at-point-name)))))

(defun +test/go-run-test-at-point-gotestsum ()
  "Run Go test at point using gotestsum."
  (setq +test/last-command #'+test/go-run-test-at-point-gotestsum)
  (+test/go-compile-test (format "gotestsum -- -run %s" (+test/go-test-at-point-name))))

(defun +test/go-run-file-tests ()
  "Run all Go tests in current file."
  (setq +test/last-command #'+test/go-run-file-tests)
  (if (fboundp 'go-test-current-file)
      (go-test-current-file)
    (+test/go-compile-test (format "go test %s" (file-name-directory buffer-file-name)))))

(defun +test/go-run-file-tests-gotestsum ()
  "Run Go file tests using gotestsum."
  (setq +test/last-command #'+test/go-run-file-tests-gotestsum)
  (+test/go-compile-test (format "gotestsum -- %s" (file-name-directory buffer-file-name))))

(defun +test/go-run-project-tests ()
  "Run all Go tests in project."
  (setq +test/last-command #'+test/go-run-project-tests)
  (+test/go-compile-test "go test ./..."))

(defun +test/go-run-project-tests-gotestsum ()
  "Run Go project tests using gotestsum."
  (setq +test/last-command #'+test/go-run-project-tests-gotestsum)
  (+test/go-compile-test "gotestsum -- ./..."))

(defun +test/go-run-all-tests ()
  "Run all Go tests including integration tests."
  (setq +test/last-command #'+test/go-run-all-tests)
  (+test/go-compile-test "go test -tags=integration ./..."))

(defun +test/go-run-all-tests-gotestsum ()
  "Run all Go tests using gotestsum."
  (setq +test/last-command #'+test/go-run-all-tests-gotestsum)
  (+test/go-compile-test "gotestsum -- -tags=integration ./..."))

(defun +test/go-toggle-coverage ()
  "Toggle Go test coverage."
  (if +test/coverage-enabled
      (if (fboundp 'go-coverage-toggle)
          (go-coverage-toggle)
        (+test/go-compile-test "go test -coverprofile=coverage.out ./... && go tool cover -html=coverage.out"))
    (when (fboundp 'go-coverage-clear-all)
      (go-coverage-clear-all))))

(defun +test/go-compile-test (command)
  "Run Go test COMMAND in project root."
  (let ((default-directory (+go/project-root)))
    (compile command)))

(defun +test/go-test-at-point-name ()
  "Get the name of the Go test function at point."
  (save-excursion
    (beginning-of-defun)
    (when (re-search-forward "func \\(Test[A-Za-z0-9_]*\\)" nil t)
      (match-string 1))))

;; =============================================================================
;; Python Test Functions
;; =============================================================================

(defun +test/python-run-test-at-point ()
  "Run Python test at point."
  (setq +test/last-command #'+test/python-run-test-at-point)
  (let ((test-name (+test/python-test-at-point-name)))
    (if test-name
        (+test/python-compile-test (format "pytest -v %s::%s" (buffer-file-name) test-name))
      (message "No test function at point"))))

(defun +test/python-run-file-tests ()
  "Run all Python tests in current file."
  (setq +test/last-command #'+test/python-run-file-tests)
  (+test/python-compile-test (format "pytest -v %s" (buffer-file-name))))

(defun +test/python-run-project-tests ()
  "Run all Python tests in project."
  (setq +test/last-command #'+test/python-run-project-tests)
  (+test/python-compile-test "pytest"))

(defun +test/python-run-all-tests ()
  "Run all Python tests including slow tests."
  (setq +test/last-command #'+test/python-run-all-tests)
  (+test/python-compile-test "pytest --runslow"))

(defun +test/python-toggle-coverage ()
  "Toggle Python test coverage."
  (if +test/coverage-enabled
      (+test/python-compile-test "pytest --cov --cov-report=html")
    (message "Coverage disabled")))

(defun +test/python-compile-test (command)
  "Run Python test COMMAND."
  (compile command))

(defun +test/python-test-at-point-name ()
  "Get the name of the Python test function at point."
  (save-excursion
    (beginning-of-defun)
    (when (re-search-forward "def \\(test_[A-Za-z0-9_]*\\)" nil t)
      (match-string 1))))

;; =============================================================================
;; Rust Test Functions
;; =============================================================================

(defun +test/rust-run-test-at-point ()
  "Run Rust test at point."
  (setq +test/last-command #'+test/rust-run-test-at-point)
  (let ((test-name (+test/rust-test-at-point-name)))
    (if test-name
        (+test/rust-compile-test (format "cargo test %s" test-name))
      (message "No test function at point"))))

(defun +test/rust-run-file-tests ()
  "Run Rust tests in current file."
  (setq +test/last-command #'+test/rust-run-file-tests)
  (+test/rust-compile-test "cargo test"))

(defun +test/rust-run-project-tests ()
  "Run all Rust tests."
  (setq +test/last-command #'+test/rust-run-project-tests)
  (+test/rust-compile-test "cargo test"))

(defun +test/rust-run-all-tests ()
  "Run all Rust tests including ignored ones."
  (setq +test/last-command #'+test/rust-run-all-tests)
  (+test/rust-compile-test "cargo test -- --ignored"))

(defun +test/rust-toggle-coverage ()
  "Toggle Rust test coverage using tarpaulin."
  (if +test/coverage-enabled
      (if (executable-find "cargo-tarpaulin")
          (+test/rust-compile-test "cargo tarpaulin --out Html")
        (message "Install cargo-tarpaulin for coverage support"))
    (message "Coverage disabled")))

(defun +test/rust-compile-test (command)
  "Run Rust test COMMAND."
  (compile command))

(defun +test/rust-test-at-point-name ()
  "Get the name of the Rust test function at point."
  (save-excursion
    (beginning-of-defun)
    (when (re-search-forward "#\\[test\\]\\s-*fn \\([A-Za-z0-9_]*\\)" nil t)
      (match-string 1))))

;; =============================================================================
;; Haskell Test Functions
;; =============================================================================

(defun +test/haskell-run-test-at-point ()
  "Run Haskell test at point if detectable, otherwise fall back to file tests."
  (setq +test/last-command #'+test/haskell-run-test-at-point)
  (message "Stack does not expose per-test runs yet; running file tests instead")
  (+test/haskell-run-file-tests))

(defun +test/haskell-run-file-tests ()
  "Run Stack tests scoped to the current module (best effort)."
  (setq +test/last-command #'+test/haskell-run-file-tests)
  (if buffer-file-name
      (compile (format "stack test --fast --test-arguments \"--match %s\""
                       (file-name-base buffer-file-name)))
    (compile "stack test --fast")))

(defun +test/haskell-run-project-tests ()
  "Run Stack test for the entire project."
  (setq +test/last-command #'+test/haskell-run-project-tests)
  (compile "stack test"))

(defun +test/haskell-run-all-tests ()
  "Run complete Stack test suite including integration tests." 
  (setq +test/last-command #'+test/haskell-run-all-tests)
  (compile "stack test --fast"))

(defun +test/haskell-toggle-coverage ()
  "Toggle Stack coverage reporting."
  (if +test/coverage-enabled
      (compile "stack test --coverage")
    (message "Coverage disabled")))

;; =============================================================================
;; Generic Test Functions
;; =============================================================================

(defun +test/generic-run-test-at-point ()
  "Generic test runner for unsupported languages."
  (message "Test at point not supported for %s" major-mode))

(defun +test/generic-run-file-tests ()
  "Generic file test runner."
  (compile "make test"))

(defun +test/generic-run-project-tests ()
  "Generic project test runner."
  (compile "make test"))

(defun +test/generic-run-all-tests ()
  "Generic all tests runner."
  (compile "make test-all"))

;; =============================================================================
;; Test Results Management
;; =============================================================================

(defun +test/show-results ()
  "Show test results in a dedicated buffer."
  (interactive)
  (let ((results-buf (get-buffer "*compilation*")))
    (if results-buf
        (pop-to-buffer results-buf)
      (message "No test results available"))))

(defun +test/clear-results ()
  "Clear test results."
  (interactive)
  (let ((results-buf (get-buffer "*compilation*")))
    (when results-buf
      (with-current-buffer results-buf
        (let ((inhibit-read-only t))
          (erase-buffer))))))

;; =============================================================================
;; Test Hydra
;; =============================================================================

(defhydra +test/hydra (:color blue :hint nil)
  "
^Run Tests^         ^Scope^           ^Coverage^        ^Results^
─────────────────────────────────────────────────────────────────
_tt_: at point      _tf_: file        _c_: toggle       _r_: show results
_T_:  all (slow)    _tp_: project     _C_: view report  _R_: clear results
_tr_: rerun last    _ta_: all         ^                 ^
^                   ^                 ^                 ^Utils^
^Watch^             ^Debug Tests^     ^Integration^     ─────────────────
_tw_: watch file    _td_: debug       _ti_: integration _q_: quit
_tW_: watch project _tD_: debug all   _tI_: e2e         _?_: help
"
  ;; Run Tests
  ("tt" +test/run-at-point)
  ("T" +test/run-all)
  ("tr" +test/rerun)

  ;; Scope
  ("tf" +test/run-file)
  ("tp" +test/run-project)
  ("ta" +test/run-all)

  ;; Coverage
  ("c" +test/toggle-coverage)
  ("C" +test/view-coverage-report)

  ;; Results
  ("r" +test/show-results :color red)
  ("R" +test/clear-results)

  ;; Watch (placeholder for future implementation)
  ("tw" +test/watch-file)
  ("tW" +test/watch-project)

  ;; Debug Tests (placeholder)
  ("td" +test/debug-test)
  ("tD" +test/debug-all-tests)

  ;; Integration (placeholder)
  ("ti" +test/run-integration-tests)
  ("tI" +test/run-e2e-tests)

  ;; Utils
  ("?" (lambda () (interactive) (describe-function '+test/hydra/body)) :color red)
  ("q" nil))

;; Placeholder functions for future implementation
(defun +test/view-coverage-report ()
  "View coverage report."
  (interactive)
  (message "Coverage report viewing not yet implemented"))

(defun +test/watch-file ()
  "Watch current file for changes and run tests."
  (interactive)
  (message "Test watching not yet implemented"))

(defun +test/watch-project ()
  "Watch project for changes and run tests."
  (interactive)
  (message "Test watching not yet implemented"))

(defun +test/debug-test ()
  "Debug test at point."
  (interactive)
  (message "Test debugging will be implemented in debugging module"))

(defun +test/debug-all-tests ()
  "Debug all tests."
  (interactive)
  (message "Test debugging will be implemented in debugging module"))

(defun +test/run-integration-tests ()
  "Run integration tests."
  (interactive)
  (let ((backend (+test/get-backend)))
    (pcase backend
      ('cider (cider-eval-string "(run-tests 'integration)"))
      ('gotest (+test/go-compile-test "go test -tags=integration ./..."))
      ('pytest (+test/python-compile-test "pytest -m integration"))
      (_ (compile "make integration-test")))))

(defun +test/run-e2e-tests ()
  "Run end-to-end tests."
  (interactive)
  (let ((backend (+test/get-backend)))
    (pcase backend
      ('jest (+test/js-compile-test "npm run test:e2e"))
      (_ (compile "make e2e-test")))))

;; JavaScript/TypeScript test functions (placeholders)
(defun +test/js-run-test-at-point ()
  "Run JavaScript test at point."
  (message "JS test at point not yet implemented"))

(defun +test/js-run-file-tests ()
  "Run JavaScript file tests."
  (+test/js-compile-test (format "npm test -- %s" (buffer-file-name))))

(defun +test/js-run-project-tests ()
  "Run all JavaScript tests."
  (+test/js-compile-test "npm test"))

(defun +test/js-run-all-tests ()
  "Run all JavaScript tests."
  (+test/js-compile-test "npm run test:all"))

(defun +test/js-compile-test (command)
  "Run JavaScript test COMMAND."
  (compile command))

(provide '53-lang-testing)
;;; 53-lang-testing.el ends here
