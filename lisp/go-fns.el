;;; go-fns.el --- Go development helper functions -*- lexical-binding: t; -*-

;;; Code:

(defun +go/exe? (name)
  "Check if executable NAME exists."
  (and (executable-find name) t))

(defun +go/project-root ()
  "Get the Go project root directory.
Looks for go.mod file first, then falls back to VCS root."
  (or
   ;; First, look for go.mod starting from current buffer
   (when (buffer-file-name)
     (locate-dominating-file (buffer-file-name) "go.mod"))
   ;; Fall back to current directory if no buffer file
   (locate-dominating-file default-directory "go.mod")
   ;; Finally fall back to VCS project root
   (cond
    ((fboundp 'projectile-project-root) (projectile-project-root))
    ((fboundp 'project-current) (when-let* ((pr (project-current))) (project-root pr)))
    (t default-directory))))

(defun +go/compile-in-root (cmd)
  "Run CMD in project root using `compile'."
  (let ((default-directory (+go/project-root)))
    (compile cmd)
    ;; Auto-open hydra after compilation starts
    (run-with-timer 0.1 nil (lambda ()
                              (when (fboundp '+go/hydra/body)
                                (+go/hydra/body))))))

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
      (progn
        (go-test-current-file)
        ;; Auto-open hydra after gotest command
        (run-with-timer 0.1 nil (lambda ()
                                  (when (fboundp '+go/hydra/body)
                                    (+go/hydra/body)))))
    (+go/compile-in-root (format "go test -race %s" (shell-quote-argument (buffer-file-name))))))

(defun +go/test-at-point ()
  "Run test at point."
  (interactive)
  (if (fboundp 'go-test-current-test)
      (progn
        (go-test-current-test)
        ;; Auto-open hydra after gotest command
        (run-with-timer 0.1 nil (lambda ()
                                  (when (fboundp '+go/hydra/body)
                                    (+go/hydra/body)))))
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

(defun +go/git-root ()
  "Get the Git repository root directory."
  (cond
   ((fboundp 'projectile-project-root) (projectile-project-root))
   ((fboundp 'project-current) (when-let* ((pr (project-current))) (project-root pr)))
   (t default-directory)))

(defun +go/compile-in-git-root (cmd)
  "Run CMD in Git repository root using `compile'."
  (let ((default-directory (+go/git-root)))
    (compile cmd)
    ;; Auto-open hydra after compilation starts
    (run-with-timer 0.1 nil (lambda ()
                              (when (fboundp '+go/hydra/body)
                                (+go/hydra/body))))))

(defun +go/build-from-root ()
  "Build Go project from Git repository root (for multi-module projects)."
  (interactive)
  (+go/compile-in-git-root "go build ./..."))

(defun +go/test-from-root ()
  "Test Go project from Git repository root (for multi-module projects)."
  (interactive)
  (let ((cmd (if (+go/exe? "gotestsum")
                 "gotestsum -- -race ./..."
               "go test -race ./...")))
    (+go/compile-in-git-root cmd)))

(defun +go/lsp-format+imports ()
  "Format buffer and organize imports via LSP."
  (when (lsp-feature? "textDocument/formatting")
    (lsp-format-buffer)
    (lsp-organize-imports)))

(provide 'go-fns)
;;; go-fns.el ends here

;; 1) Use Emacs to build grammars that match its own ABI
(setq treesit-language-source-alist
      '((go    "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")))

;; 2) Purge any mismatched Go grammars Emacs might be loading first
(let* ((dirs (append treesit-extra-load-path
                     (list (expand-file-name "tree-sitter" user-emacs-directory))))
       (rx (rx-to-string '(seq (or "go" "gomod") (+ any) (or ".so" ".dylib" ".dll")))))
  (dolist (d dirs)
    (when (and d (file-directory-p d))
      (dolist (f (directory-files d t rx))
        (ignore-errors (delete-file f))))))

;; 3) (Re)install matching grammars with Emacs’ built-in installer
;;    This compiles against Emacs’ own Tree-sitter headers → ABI match.
(mapc #'treesit-install-language-grammar '(go gomod))

;; 4) Prefer go-ts-mode (optional but recommended)
(when (fboundp 'go-ts-mode)
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode)))
