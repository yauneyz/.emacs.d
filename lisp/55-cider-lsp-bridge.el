;;; 55-cider-lsp-bridge.el --- Cider-LSP integration bridge -*- lexical-binding: t; -*-

;;; Commentary:
;; Bridge between Cider and LSP that provides intelligent tool selection,
;; fallback mechanisms, and seamless integration. Prioritizes Cider for
;; Clojure when REPL is available, uses LSP for other languages, and
;; provides graceful fallbacks.

;;; Code:

;; =============================================================================
;; Tool Priority and Selection Logic
;; =============================================================================

(defvar +bridge/tool-priorities
  '((navigation
     ;; For navigation: Cider (when connected) > LSP > xref
     ((clojure-mode . (cider lsp xref))
      (clojurescript-mode . (cider lsp xref))
      (clojurec-mode . (cider lsp xref))
      (default . (lsp xref))))
    (completion
     ;; For completion: Cider > LSP > company-dabbrev
     ((clojure-mode . (cider lsp company))
      (clojurescript-mode . (cider lsp company))
      (clojurec-mode . (cider lsp company))
      (default . (lsp company))))
    (documentation
     ;; For docs: Cider > LSP > eldoc
     ((clojure-mode . (cider lsp eldoc))
      (clojurescript-mode . (cider lsp eldoc))
      (clojurec-mode . (cider lsp eldoc))
      (default . (lsp eldoc))))
    (refactoring
     ;; For refactoring: clj-refactor + Cider > LSP
     ((clojure-mode . (clj-refactor lsp))
      (clojurescript-mode . (clj-refactor lsp))
      (clojurec-mode . (clj-refactor lsp))
      (default . (lsp))))
    (diagnostics
     ;; For diagnostics: Cider > clj-kondo > LSP > flycheck
     ((clojure-mode . (cider clj-kondo lsp flycheck))
      (clojurescript-mode . (cider clj-kondo lsp flycheck))
      (clojurec-mode . (cider clj-kondo lsp flycheck))
      (default . (lsp flycheck)))))
  "Tool priority mappings for different operations and modes.")

(defun +bridge/get-tool-priorities (operation &optional mode)
  "Get prioritized list of tools for OPERATION in MODE."
  (let* ((mode (or mode major-mode))
         (operation-config (alist-get operation +bridge/tool-priorities))
         (mode-config (or (alist-get mode operation-config)
                          (alist-get 'default operation-config))))
    mode-config))

(defun +bridge/tool-available? (tool)
  "Check if TOOL is available and functional."
  (pcase tool
    ('cider (and (bound-and-true-p cider-mode)
                 (cider-connected-p)
                 (cider-current-repl)))
    ('lsp (bound-and-true-p lsp-mode))
    ('clj-refactor (bound-and-true-p clj-refactor-mode))
    ('clj-kondo (and (bound-and-true-p flycheck-mode)
                     (flycheck-checker-get 'clj-kondo-clj 'executable)))
    ('flycheck (bound-and-true-p flycheck-mode))
    ('company (bound-and-true-p company-mode))
    ('xref t)  ; always available
    ('eldoc t) ; always available
    (_ nil)))

(defun +bridge/select-tool (operation &optional mode)
  "Select the best available tool for OPERATION in MODE."
  (let ((priorities (+bridge/get-tool-priorities operation mode)))
    (seq-find #'+bridge/tool-available? priorities)))

;; =============================================================================
;; Smart Navigation Functions
;; =============================================================================

(defun +bridge/goto-definition ()
  "Go to definition using best available tool."
  (interactive)
  (let ((tool (+bridge/select-tool 'navigation)))
    (pcase tool
      ('cider (cider-find-var))
      ('lsp (lsp-find-definition))
      ('xref (xref-find-definitions))
      (_ (message "No definition lookup available")))))

(defun +bridge/find-references ()
  "Find references using best available tool."
  (interactive)
  (let ((tool (+bridge/select-tool 'navigation)))
    (pcase tool
      ('cider (cider-xref-fn-refs))
      ('lsp (lsp-find-references))
      ('xref (xref-find-references))
      (_ (message "No reference lookup available")))))

(defun +bridge/goto-implementation ()
  "Go to implementation using best available tool."
  (interactive)
  (let ((tool (+bridge/select-tool 'navigation)))
    (pcase tool
      ('cider (cider-find-var)) ; Cider doesn't distinguish implementation
      ('lsp (lsp-find-implementation))
      ('xref (xref-find-definitions))
      (_ (message "No implementation lookup available")))))

(defun +bridge/show-documentation ()
  "Show documentation using best available tool."
  (interactive)
  (let ((tool (+bridge/select-tool 'documentation)))
    (pcase tool
      ('cider (cider-doc))
      ('lsp (lsp-describe-thing-at-point))
      ('eldoc (eldoc-print-current-symbol-info))
      (_ (message "No documentation available")))))

;; =============================================================================
;; Smart Completion Integration
;; =============================================================================

(defun +bridge/setup-completion ()
  "Set up completion backends based on available tools."
  ;; Check if using Corfu (modern) vs Company (legacy)
  (cond
   ;; Modern Corfu completion - use completion-at-point-functions
   ((bound-and-true-p corfu-mode)
    (message "Using Corfu completion via completion-at-point-functions"))

   ;; Legacy Company completion
   ((bound-and-true-p company-mode)
    (let ((backends '()))
      ;; Add backends based on priority, but avoid company-cider if using Corfu elsewhere
      (dolist (tool (+bridge/get-tool-priorities 'completion))
        (when (+bridge/tool-available? tool)
          (pcase tool
            ('cider (push 'company-capf backends)) ; Use capf instead of company-cider
            ('lsp (push 'company-capf backends))
            ('company (push 'company-dabbrev-code backends)))))

      ;; Set company backends
      (when backends
        (setq-local company-backends (list (nreverse backends))))))

   ;; Fallback message
   (t (message "No completion system detected"))))

;; =============================================================================
;; Smart Diagnostics Integration
;; =============================================================================

(defun +bridge/setup-diagnostics ()
  "Set up diagnostics based on available tools."
  (let ((tool (+bridge/select-tool 'diagnostics)))
    (pcase tool
      ('cider
       ;; Cider provides runtime errors and warnings
       (when (bound-and-true-p flycheck-mode)
         (setq-local flycheck-disabled-checkers '(clj-kondo-clj))))
      ('clj-kondo
       ;; Enable clj-kondo for static analysis
       (when (bound-and-true-p flycheck-mode)
         (setq-local flycheck-checkers '(clj-kondo-clj))))
      ('lsp
       ;; LSP provides diagnostics
       (when (bound-and-true-p flycheck-mode)
         (setq-local flycheck-checkers '(lsp))))
      ('flycheck
       ;; Use default flycheck setup
       nil))))

;; =============================================================================
;; Smart Refactoring Functions
;; =============================================================================

(defun +bridge/rename-symbol ()
  "Rename symbol using best available tool."
  (interactive)
  (let ((tool (+bridge/select-tool 'refactoring)))
    (pcase tool
      ('clj-refactor
       (if (fboundp 'cljr-rename-symbol)
           (cljr-rename-symbol)
         (message "clj-refactor not available, use cider-refactor commands")))
      ('lsp (lsp-rename))
      (_ (message "No rename capability available")))))

(defun +bridge/extract-function ()
  "Extract function using best available tool."
  (interactive)
  (let ((tool (+bridge/select-tool 'refactoring)))
    (pcase tool
      ('clj-refactor
       (if (fboundp 'cljr-extract-function)
           (cljr-extract-function)
         (message "Extract function not available")))
      ('lsp (lsp-execute-code-action))
      (_ (message "No extract function capability available")))))

;; =============================================================================
;; Tool Status and Information
;; =============================================================================

(defun +bridge/status ()
  "Show bridge status and tool availability."
  (interactive)
  (let* ((nav-tool (+bridge/select-tool 'navigation))
         (comp-tool (+bridge/select-tool 'completion))
         (doc-tool (+bridge/select-tool 'documentation))
         (refac-tool (+bridge/select-tool 'refactoring))
         (diag-tool (+bridge/select-tool 'diagnostics)))

    (message (concat "Bridge Status - "
                     "Nav: %s | Completion: %s | Docs: %s | "
                     "Refactor: %s | Diagnostics: %s")
             nav-tool comp-tool doc-tool refac-tool diag-tool)))

(defun +bridge/tool-info (tool)
  "Get detailed information about TOOL availability."
  (pcase tool
    ('cider
     (list :available (+bridge/tool-available? 'cider)
           :connected (and (bound-and-true-p cider-mode) (cider-connected-p))
           :repl-type (when (cider-connected-p)
                        (when-let ((repl (cider-current-repl)))
                          (cider-repl-type repl)))
           :version (when (bound-and-true-p cider-version) cider-version)))
    ('lsp
     (list :available (+bridge/tool-available? 'lsp)
           :server (when (bound-and-true-p lsp-mode) (lsp--workspace-server-id (cl-first (lsp-workspaces))))
           :status (when (bound-and-true-p lsp-mode) "connected")))
    ('clj-refactor
     (list :available (+bridge/tool-available? 'clj-refactor)
           :version (when (bound-and-true-p clj-refactor-version) clj-refactor-version)))
    (_ (list :available (+bridge/tool-available? tool)))))

;; =============================================================================
;; Cider Enhancement Functions
;; =============================================================================

(defun +bridge/cider-enhanced-connect ()
  "Enhanced Cider connection with better feedback."
  (interactive)
  (if (cider-connected-p)
      (progn
        (message "Already connected to %s REPL"
                 (if-let ((repl (cider-current-repl)))
                     (cider-repl-type repl)
                   "unknown"))
        (cider-switch-to-repl-buffer))
    (progn
      (message "Starting Cider REPL...")
      (call-interactively #'cider-jack-in)
      ;; Set up completion and diagnostics after connection
      (add-hook 'cider-connected-hook #'+bridge/on-cider-connected))))

(defun +bridge/on-cider-connected ()
  "Hook function called when Cider connects."
  (message "Cider connected! Setting up integrations...")
  (+bridge/setup-completion)
  (+bridge/setup-diagnostics)
  ;; Load test utilities if they exist
  (when (file-exists-p "test/dev/repl_utils.cljs")
    (cider-eval-string-up-to-point
     "(try (require '[test.dev.repl-utils :as repl] :reload) (repl/init!) (catch Exception e nil))"))
  (message "Cider integration complete"))

(defun +bridge/cider-disconnect-all ()
  "Disconnect all Cider REPLs with cleanup."
  (interactive)
  (when (cider-connected-p)
    (cider-quit)
    (message "All Cider connections closed")
    ;; Revert to LSP if available
    (when (bound-and-true-p lsp-mode)
      (message "Falling back to LSP mode"))
    (+bridge/setup-completion)
    (+bridge/setup-diagnostics)))

;; =============================================================================
;; LSP Enhancement Functions
;; =============================================================================

(defun +bridge/lsp-ensure-started ()
  "Ensure LSP is started for current buffer."
  (unless (bound-and-true-p lsp-mode)
    (when (and buffer-file-name
               (featurep 'lsp-mode)
               (condition-case nil
                   (lsp-find-workspace)
                 (error nil)))
      (lsp-deferred))))

;; =============================================================================
;; Mode-specific Setup
;; =============================================================================

(defun +bridge/setup-clojure-mode ()
  "Set up bridge for Clojure mode."
  (when (derived-mode-p 'clojure-mode)
    ;; Try to connect Cider if not connected
    (unless (cider-connected-p)
      ;; Enable LSP as fallback if available
      (+bridge/lsp-ensure-started))

    ;; Set up completion regardless of connection status
    (+bridge/setup-completion)
    (+bridge/setup-diagnostics)

    ;; Set up local keybindings
    (local-set-key (kbd "M-.") #'+bridge/goto-definition)
    (local-set-key (kbd "M-?") #'+bridge/find-references)
    (local-set-key (kbd "C-c C-d d") #'+bridge/show-documentation)))

(defun +bridge/setup-other-modes ()
  "Set up bridge for non-Clojure modes."
  (unless (derived-mode-p 'clojure-mode)
    ;; Ensure LSP is started
    (+bridge/lsp-ensure-started)

    ;; Set up completion and diagnostics
    (+bridge/setup-completion)
    (+bridge/setup-diagnostics)

    ;; Set up local keybindings
    (local-set-key (kbd "M-.") #'+bridge/goto-definition)
    (local-set-key (kbd "M-?") #'+bridge/find-references)))

;; =============================================================================
;; Auto-setup and Hooks
;; =============================================================================

(defun +bridge/auto-setup ()
  "Automatically set up bridge based on current mode."
  (if (derived-mode-p 'clojure-mode)
      (+bridge/setup-clojure-mode)
    (+bridge/setup-other-modes)))

;; Set up bridge when modes are loaded
(add-hook 'clojure-mode-hook #'+bridge/setup-clojure-mode)
(add-hook 'clojurescript-mode-hook #'+bridge/setup-clojure-mode)
(add-hook 'prog-mode-hook #'+bridge/setup-other-modes)

;; Set up completion when company loads
(with-eval-after-load 'company
  (add-hook 'company-mode-hook #'+bridge/setup-completion))

;; Set up diagnostics when flycheck loads
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'+bridge/setup-diagnostics))

;; =============================================================================
;; Integration with Universal Dev System
;; =============================================================================

;; Override the universal dev functions to use bridge
(defun +dev/goto-definition ()
  "Go to definition using bridge."
  (interactive)
  (+bridge/goto-definition))

(defun +dev/goto-references ()
  "Find references using bridge."
  (interactive)
  (+bridge/goto-references))

(defun +dev/goto-implementation ()
  "Go to implementation using bridge."
  (interactive)
  (+bridge/goto-implementation))

(defun +dev/show-documentation ()
  "Show documentation using bridge."
  (interactive)
  (+bridge/show-documentation))

(defun +dev/rename-symbol ()
  "Rename symbol using bridge."
  (interactive)
  (+bridge/rename-symbol))

;; =============================================================================
;; Debug and Utility Functions
;; =============================================================================

(defun +bridge/debug-info ()
  "Show detailed debug information about bridge state."
  (interactive)
  (let ((buffer (get-buffer-create "*Bridge Debug*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== Cider-LSP Bridge Debug Information ===\n\n")

      (insert (format "Major Mode: %s\n" major-mode))
      (insert (format "Buffer: %s\n\n" (buffer-name)))

      (insert "=== Tool Availability ===\n")
      (dolist (tool '(cider lsp clj-refactor clj-kondo flycheck company xref eldoc))
        (insert (format "%-12s: %s\n" tool
                        (if (+bridge/tool-available? tool) "✓" "✗"))))

      (insert "\n=== Selected Tools ===\n")
      (dolist (operation '(navigation completion documentation refactoring diagnostics))
        (insert (format "%-12s: %s\n" operation
                        (+bridge/select-tool operation))))

      (when (derived-mode-p 'clojure-mode)
        (insert "\n=== Cider Information ===\n")
        (let ((cider-info (+bridge/tool-info 'cider)))
          (insert (format "Available: %s\n" (plist-get cider-info :available)))
          (insert (format "Connected: %s\n" (plist-get cider-info :connected)))
          (when (plist-get cider-info :repl-type)
            (insert (format "REPL Type: %s\n" (plist-get cider-info :repl-type))))))

      (when (bound-and-true-p lsp-mode)
        (insert "\n=== LSP Information ===\n")
        (let ((lsp-info (+bridge/tool-info 'lsp)))
          (insert (format "Available: %s\n" (plist-get lsp-info :available)))
          (when (plist-get lsp-info :server)
            (insert (format "Server: %s\n" (plist-get lsp-info :server))))))

      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun +bridge/reset ()
  "Reset bridge configuration and re-setup."
  (interactive)
  (message "Resetting bridge configuration...")
  (+bridge/auto-setup)
  (message "Bridge reset complete"))

(provide '55-cider-lsp-bridge)
;;; 55-cider-lsp-bridge.el ends here