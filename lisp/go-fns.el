;;; go-fns.el --- Go development helper functions -*- lexical-binding: t; -*-

;;; Code:

(defun +go/exe? (name)
  "Check if executable NAME exists."
  (and (executable-find name) t))

(defun +go/project-root ()
  "Get the project root directory."
  (cond
   ((fboundp 'projectile-project-root) (projectile-project-root))
   ((fboundp 'project-current) (when-let* ((pr (project-current))) (project-root pr)))
   (t default-directory)))

(defun +go/compile-in-root (cmd)
  "Run CMD in project root using `compile'."
  (let ((default-directory (+go/project-root)))
    (compile cmd)))

(defun +go/build ()
  "Build the Go project."
  (interactive)
  (+go/compile-in-root "go build ./..."))

(defun +go/run ()
  "Run the current Go project (defaults to `go run .').
Customize per-project via .dir-locals.el if needed."
  (interactive)
  (+go/compile-in-root "go run ."))

(defun +go/test-project ()
  "Run tests for the entire project."
  (interactive)
  (let ((cmd (if (+go/exe? "gotestsum")
                 "gotestsum -- -race ./..."
               "go test -race ./...")))
    (+go/compile-in-root cmd)))

(defun +go/test-file ()
  "Run tests for the current file."
  (interactive)
  (if (fboundp 'go-test-current-file)
      (go-test-current-file)
    (+go/compile-in-root (format "go test -race %s" (shell-quote-argument (buffer-file-name))))))

(defun +go/test-at-point ()
  "Run test at point."
  (interactive)
  (if (fboundp 'go-test-current-test)
      (go-test-current-test)
    (message "Install `gotest` package for test-at-point support.")))

(defun +go/test-benchmark ()
  "Run benchmarks."
  (interactive)
  (+go/compile-in-root "go test -run=^$ -bench=. ./..."))

(defun +go/coverage-toggle ()
  "Toggle coverage overlay in the current package."
  (interactive)
  (require 'go-mode)
  (go-coverage))

(defun +go/recompile ()
  "Re-run the last build/test/run command."
  (interactive)
  (recompile))

(defun +go/lsp-format+imports ()
  "Format buffer and organize imports via LSP."
  (when (lsp-feature? "textDocument/formatting")
    (lsp-format-buffer)
    (lsp-organize-imports)))

(provide 'go-fns)
;;; go-fns.el ends here