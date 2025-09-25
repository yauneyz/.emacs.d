;;============== Clojure =============

;; Global timeout settings for network requests
(setq url-queue-timeout 5) ; Timeout for URL requests (ClojureDocs, etc.)

;; ClojureDocs cache configuration
(defcustom +clojure/clojuredocs-cache-dir
  (expand-file-name "~/.cache/orchard/clojuredocs/")
  "Directory for ClojureDocs cache storage."
  :type 'string
  :group 'cider)

;; Ensure cache directory exists on startup
(unless (file-exists-p +clojure/clojuredocs-cache-dir)
  (make-directory +clojure/clojuredocs-cache-dir t))

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode))
  :hook ((clojure-mode . lsp-deferred)
         (clojurescript-mode . lsp-deferred)
         (clojurec-mode . lsp-deferred))
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'clj-refactor-mode)
  ;; Enable spec validation instrumentation in development
  (add-hook 'clojure-mode-hook
            (lambda ()
              (when (string-match-p "development\\|dev" (or (getenv "NODE_ENV") "development"))
                (setq-local cider-auto-select-error-buffer t)
                (setq-local cider-show-error-buffer t)))))

;; Enhanced Cider configuration for testing and debugging
(unless (package-installed-p 'cider)
  (package-install 'cider))

(use-package paredit)

(evil-define-key 'normal 'global (kbd "C-p") 'paredit-splice-sexp-killing-backward)

;; clj-refactor for enhanced refactoring
(use-package clj-refactor
  :after clojure-mode
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m"))

;; flycheck-clj-kondo for static analysis
(use-package flycheck-clj-kondo
  :after flycheck)

(use-package cider
  :after clojure-mode
  :custom
  (cider-completion-system 'ivy)
  (cider-eldoc-display-for-symbol-at-point nil) ; Reduce noise
  (cider-eldoc-display-context-dependent-info nil) ; Disable context-dependent info to prevent fetching
  (cider-eldoc-max-class-names-to-display 0) ; Disable class name display
  (cider-eldoc-ns-function nil) ; Don't fetch namespace docs
  ;; Documentation and performance optimizations
  (cider-doc-auto-select-buffer nil) ; Don't auto-select doc buffers
  (cider-connection-timeout 10) ; Set connection timeout
  (eldoc-idle-delay 1.0) ; Increase eldoc delay to reduce requests
  ;; Enhanced test integration
  (cider-test-show-report-on-success t)
  (cider-auto-select-test-report-buffer t)
  (cider-test-defining-forms '("deftest" "defspec")) ; Add defspec support
  ;; REPL enhancements
  (cider-repl-history-file "~/.emacs.d/cider-repl-history")
  (cider-repl-history-size 1000)
  (cider-repl-wrap-history t)
  ;; Error handling
  (cider-show-error-buffer t)
  (cider-auto-select-error-buffer t)
  (cider-repl-display-help-banner nil)
  ;; Debugging enhancements
  (cider-debug-use-overlays t)
  (cider-debug-prompt 'overlay)
  :config
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  ;; Load test utilities on REPL start
  (add-hook 'cider-connected-hook
            (lambda ()
              (when (cider-repls 'cljs)
                (cider-eval-string-up-to-point
                 "(require '[test.dev.repl-utils :as repl] :reload) (repl/init!)"))))
  ;; Enhanced error display
  (add-hook 'cider-stacktrace-mode-hook
            (lambda ()
              (setq-local truncate-lines nil))))

;; Enable paredit mode for Clojure buffers, CIDER mode and CIDER REPL buffers
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
;; Disable paredit in emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 0)))

;; Enhanced spy function with better defaults
(defun my-spy-and-slurp ()
  "Insert '(spy )', move point before the closing parenthesis, then call `paredit-forward-slurp-sexp`."
  (interactive)
  (insert "(spy )")
  (backward-char 1)  ;; Position point before the closing parenthesis
  (paredit-forward-slurp-sexp))

(defun my-spy-with-label ()
  "Insert '(spy \"label\" )', prompt for label, then slurp."
  (interactive)
  (let ((label (read-string "Spy label: " "debug")))
    (insert (format "(spy \"%s\" )" label))
    (backward-char 1)
    (paredit-forward-slurp-sexp)))

;; Test running utilities
(defun cider-test-run-ns-and-focus ()
  "Run tests for current namespace and focus on test buffer."
  (interactive)
  (cider-test-run-ns-tests)
  (when-let ((buf (get-buffer "*cider-test-report*")))
    (pop-to-buffer buf)))

(defun cider-test-run-all-and-focus ()
  "Run all tests and focus on test buffer."
  (interactive)
  (cider-test-run-all-tests)
  (when-let ((buf (get-buffer "*cider-test-report*")))
    (pop-to-buffer buf)))

;; REPL utilities for testing
(defun cider-repl-load-test-utils ()
  "Load test utilities into the REPL."
  (interactive)
  (cider-eval-string-up-to-point
   "(require '[test.dev.repl-utils :as repl] :reload)"))

(defun cider-repl-run-test-suite ()
  "Run test suite from REPL."
  (interactive)
  (let ((suite (completing-read "Test suite: " '("unit" "integration" "all"))))
    (cider-eval-string-up-to-point
     (format "(repl/run-test-suite :%s)" suite))))

;; Scope-capture integration
(defun cider-scope-capture-last ()
  "View last scope-capture execution point."
  (interactive)
  (cider-eval-string-up-to-point "(sc.api/last-ep)"))

(defun cider-scope-capture-ep-info ()
  "View scope-capture execution point info."
  (interactive)
  (cider-eval-string-up-to-point "(sc.api/ep-info)"))

;; Spec validation helpers
(defun cider-toggle-spec-instrumentation ()
  "Toggle spec instrumentation for development."
  (interactive)
  (cider-eval-string-up-to-point
   "(if (app.specs.validation/get-instrumented-functions)
      (app.specs.validation/uninstrument-all-functions!)
      (app.specs.validation/instrument-all-functions!))"))

(global-set-key (kbd "C-S-p") 'my-spy-and-slurp)
(global-set-key (kbd "C-M-S-p") 'my-spy-with-label)

;;============== Python =============



(use-package python-mode
  :mode "\\.py\\'"
  :hook (python-mode . lsp-deferred))

;; ---------- Pyright (types, IntelliSense) ----------
;; npm i -g pyright
(use-package lsp-pyright
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  :custom
  ;; "off" | "basic" | "strict"
  (lsp-pyright-typechecking-mode "strict")
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-use-library-code-for-types t))

;;  Ruff (lint, imports, optional format) ----------
;; pipx install ruff-lsp   (or pip install ruff-lsp)
;; (use-package lsp-ruff
;;   :after lsp-mode
;;   :hook (python-mode . lsp-ruff-enable)  ;; starts ruff-lsp alongside pyright
;;   :custom
;;   ;; Let Ruff handle linting; keep flake8/pycodestyle OFF to avoid dupes
;;   (lsp-ruff-lsp-server-command '("ruff-lsp")))

(with-eval-after-load 'lsp-mode
  ;; Register ruff-lsp as an add-on Python LSP
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ruff-lsp"))
    :activation-fn (lsp-activate-on "python")
    :add-on? t                      ;; run in addition to your main Python server
    :server-id 'ruff-lsp)))

;; Formatting
(use-package python-black
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim)
  :custom (python-black-extra-args '("--line-length" "120")))

;; Envs
(use-package direnv :config (direnv-mode))

;;============== Typescript =============

(use-package typescript-mode                ; .ts / .tsx files
  :ensure t
  :mode (("\\.ts\\'"  . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :hook (typescript-mode . (lambda ()
                             (setq-local indent-tabs-mode nil
                                         tab-width 2
                                         typescript-indent-level 2))))

(when (fboundp 'tsx-ts-mode)
  (add-to-list 'major-mode-remap-alist
               '(typescript-ts-base-mode . tsx-ts-mode)))

;;============== Rust =============


(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . lsp-deferred)
  :config
  (setq lsp-rust-server 'rust-analyzer)
  (setq indent-tabs-mode nil))

;; ============== Go =============

;; Set tree-sitter library path
(setq treesit-extra-load-path (list (expand-file-name "~/.emacs.d/tree-sitter/")))

;; Load Go helper functions
(require 'go-fns)

;; ;; ;; Tree-sitter Go + auto-install grammars (Emacs 29+)
;; (use-package treesit-auto
;;   :ensure t
;;   :custom (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

;; Keep go-mode around for handy commands (coverage, go-run/go-build helpers)
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook (go-mode . lsp-deferred)
  :config
  ;; Disable go-mode's own definition lookup (use LSP instead)
  (setq godoc-at-point-function nil
        go-guess-gopath-functions nil)

  ;; Ensure xref uses LSP for go-to-definition
  (add-hook 'go-mode-hook
            (lambda ()
              (setq-local xref-backend-functions '(lsp--xref-backend)))))

;; Test runner helpers (prefers gotestsum if present)
(use-package gotest
  :ensure t
  :commands (go-test-current-test go-test-current-file go-test-current-project))

;; Go hydra menu (replaces transient)
(use-package hydra :ensure t)

(defhydra +go/hydra (:color blue :hint nil)
  "
^Build/Run^         ^Tests^           ^Debug^           ^Utils^
─────────────────────────────────────────────────────────────────
_b_: build          _t_: at point     _d_: debug        _g_: show compilation
_r_: run            _f_: file         _._: breakpoint   _x_: close compilation
_R_: re-run         _p_: project      _,_: continue     _q_: quit
_B_: build root     _k_: benchmark
_P_: test root      _c_: coverage
"
  ("b" +go/build)
  ("r" +go/run)
  ("R" +go/recompile)
  ("B" +go/build-from-root)
  ("P" +go/test-from-root)
  ("t" +go/test-at-point)
  ("f" +go/test-file)
  ("p" +go/test-project)
  ("k" +go/test-benchmark)
  ("c" +go/coverage-toggle)
  ("d" dap-debug)
  ("." dap-breakpoint-toggle)
  ("," dap-continue)
  ("g" (lambda () (interactive) (pop-to-buffer "*compilation*")) :color red)
  ("x" +go/dismiss-compilation :color red)
  ("q" nil))

;; Compilation pane UX (bottom split, auto-scroll, color)
(setq compilation-scroll-output 'first-error
      compilation-always-kill t
      compilation-ask-about-save nil)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . bottom) (slot . 0) (window-height . 0.33)))


;; ;; 0) Prefer tree-sitter Go (keeps go-mode for utilities, but avoids its keymap)
(when (fboundp 'go-ts-mode)
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode)))


;; ============== Misc =============

(use-package yaml-mode
  :mode "\\.yml\\'"
  :hook (yaml-mode . highlight-indent-guides-mode))

;; Markdown
(use-package markdown-mode
  :mode "\\.md\\'"
  :hook (markdown-mode . visual-line-mode))
