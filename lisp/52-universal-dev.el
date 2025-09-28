;;; 52-universal-dev.el --- Universal development interface -*- lexical-binding: t; -*-

;;; Commentary:
;; Universal development interface that provides consistent keybindings and
;; functionality across all programming languages, with intelligent backend
;; selection (Cider for Clojure, LSP for others, fallbacks as needed).

;;; Code:

(require 'hydra)

;; =============================================================================
;; Language Detection and Tool Selection
;; =============================================================================

(defvar +dev/language-configs
  '((clojure-mode       :name "Clojure"       :repl cider :lsp t :test cider   :debug cider)
    (clojurescript-mode :name "ClojureScript" :repl cider :lsp t :test cider   :debug cider)
    (clojurec-mode      :name "Clojure"       :repl cider :lsp t :test cider   :debug cider)
    (go-mode            :name "Go"            :repl nil   :lsp t :test gotest  :debug dap)
    (go-ts-mode         :name "Go"            :repl nil   :lsp t :test gotest  :debug dap)
    (python-mode        :name "Python"        :repl nil   :lsp t :test pytest  :debug dap)
    (python-ts-mode     :name "Python"        :repl nil   :lsp t :test pytest  :debug dap)
    (rust-mode          :name "Rust"          :repl nil   :lsp t :test cargo   :debug dap)
    (rustic-mode        :name "Rust"          :repl nil   :lsp t :test cargo   :debug dap)
    (rust-ts-mode       :name "Rust"          :repl nil   :lsp t :test cargo   :debug dap)
    (typescript-mode    :name "TypeScript"    :repl nil   :lsp t :test jest    :debug dap)
    (tsx-mode           :name "TypeScript"    :repl nil   :lsp t :test jest    :debug dap)
    (haskell-mode       :name "Haskell"       :repl ghci  :lsp t :test stack   :debug dap)
    (haskell-ts-mode    :name "Haskell"       :repl ghci  :lsp t :test stack   :debug dap))
  "Configuration for each language mode.")

(defun +dev/get-language-config (&optional mode)
  "Get language configuration for current or specified MODE."
  (let ((current-mode (or mode major-mode)))
    (assoc current-mode +dev/language-configs)))

(defun +dev/get-language-name ()
  "Get friendly name for current language."
  (or (plist-get (cdr (+dev/get-language-config)) :name)
      (capitalize (symbol-name major-mode))))

(defun +dev/has-repl? ()
  "Check if current language supports REPL."
  (plist-get (cdr (+dev/get-language-config)) :repl))

(defun +dev/repl-connected? ()
  "Check if REPL is connected for current language."
  (pcase (+dev/has-repl?)
    ('cider (and (bound-and-true-p cider-mode) (cider-connected-p)))
    (_ nil)))

(defun +dev/has-lsp? ()
  "Check if LSP is available for current language."
  (and (featurep 'lsp-mode)
       (bound-and-true-p lsp-mode)
       (plist-get (cdr (+dev/get-language-config)) :lsp)))

(defun +dev/get-preferred-tool (operation)
  "Get preferred tool for OPERATION in current language."
  (let* ((config (cdr (+dev/get-language-config)))
         (repl-tool (plist-get config :repl)))
    (pcase operation
      ('navigation
       (cond
        ((and (eq repl-tool 'cider) (+dev/repl-connected?)) 'cider)
        ((+dev/has-lsp?) 'lsp)
        (t 'fallback)))
      ('debugging
       (cond
        ((and (eq repl-tool 'cider) (+dev/repl-connected?)) 'cider)
        ((bound-and-true-p dap-mode) 'dap)
        (t 'fallback)))
      ('testing
       (plist-get config :test))
      (_ 'fallback))))

;; =============================================================================
;; Universal Operations
;; =============================================================================

(defun +dev/goto-definition ()
  "Go to definition using best available tool."
  (interactive)
  (pcase (+dev/get-preferred-tool 'navigation)
    ('cider (cider-find-var))
    ('lsp (lsp-find-definition))
    ('fallback (xref-find-definitions))))

(defun +dev/goto-references ()
  "Find references using best available tool."
  (interactive)
  (pcase (+dev/get-preferred-tool 'navigation)
    ('cider (cider-xref-fn-refs))
    ('lsp (lsp-find-references))
    ('fallback (xref-find-references))))

(defun +dev/goto-implementation ()
  "Go to implementation using best available tool."
  (interactive)
  (pcase (+dev/get-preferred-tool 'navigation)
    ('cider (cider-find-var))  ; Cider doesn't distinguish implementation
    ('lsp (lsp-find-implementation))
    ('fallback (message "Implementation search not available"))))

(defun +dev/show-documentation ()
  "Show documentation for symbol at point."
  (interactive)
  (pcase (+dev/get-preferred-tool 'navigation)
    ('cider (cider-doc))
    ('lsp (lsp-describe-thing-at-point))
    ('fallback (eldoc-print-current-symbol-info))))

(defun +dev/rename-symbol ()
  "Rename symbol at point."
  (interactive)
  (pcase (+dev/get-preferred-tool 'navigation)
    ('cider (message "Use cider-refactor for renaming in Clojure"))
    ('lsp (lsp-rename))
    ('fallback (message "Rename not available"))))

;; =============================================================================
;; Build Operations
;; =============================================================================

(defun +dev/build ()
  "Build current project."
  (interactive)
  (pcase major-mode
    ((or 'go-mode 'go-ts-mode) (+go/build))
    ((or 'clojure-mode 'clojurescript-mode 'clojurec-mode)
     (cider-eval-string "(compile 'user)"))
    ((or 'python-mode 'python-ts-mode)
     (compile "python -m build"))
    ((or 'rust-mode 'rustic-mode 'rust-ts-mode)
     (compile "cargo build"))
    ((or 'typescript-mode 'tsx-mode)
     (compile "npm run build"))
    ((or 'haskell-mode 'haskell-ts-mode)
     (compile "stack build"))
    (_ (if (file-exists-p "Makefile")
           (compile "make")
         (compile "echo 'No build command configured'")))))

(defun +dev/run ()
  "Run current project."
  (interactive)
  (pcase major-mode
    ((or 'go-mode 'go-ts-mode) (+go/run))
    ((or 'clojure-mode 'clojurescript-mode 'clojurec-mode)
     (message "Use REPL to run Clojure code"))
    ((or 'python-mode 'python-ts-mode)
     (compile "python -m main"))
    ((or 'rust-mode 'rustic-mode 'rust-ts-mode)
     (compile "cargo run"))
    ((or 'typescript-mode 'tsx-mode)
     (compile "npm start"))
    ((or 'haskell-mode 'haskell-ts-mode)
     (+haskell/run-main))
    (_ (compile "echo 'No run command configured'"))))

(defun +dev/clean ()
  "Clean build artifacts."
  (interactive)
  (pcase major-mode
    ((or 'go-mode 'go-ts-mode) (compile "go clean ./..."))
    ((or 'clojure-mode 'clojurescript-mode 'clojurec-mode)
     (compile "lein clean"))
    ((or 'python-mode 'python-ts-mode)
     (compile "rm -rf build/ dist/ *.egg-info/"))
    ((or 'rust-mode 'rustic-mode 'rust-ts-mode)
     (compile "cargo clean"))
    ((or 'typescript-mode 'tsx-mode)
     (compile "npm run clean"))
    ((or 'haskell-mode 'haskell-ts-mode)
     (compile "stack clean"))
    (_ (compile "make clean"))))

(defun +dev/recompile ()
  "Rerun last compilation command."
  (interactive)
  (if (fboundp 'recompile)
      (recompile)
    (message "No previous compilation to repeat")))

;; =============================================================================
;; REPL Operations
;; =============================================================================

(defun +dev/start-repl ()
  "Start REPL for current language."
  (interactive)
  (pcase (+dev/has-repl?)
    ('cider
     (if (cider-connected-p)
         (cider-switch-to-repl-buffer)
       (call-interactively #'cider-jack-in)))
    (_ (message "No REPL available for %s" (+dev/get-language-name)))))

(defun +dev/eval-buffer ()
  "Evaluate current buffer."
  (interactive)
  (pcase (+dev/has-repl?)
    ('cider (cider-eval-buffer))
    (_ (message "Buffer evaluation not available for %s" (+dev/get-language-name)))))

(defun +dev/eval-last-sexp ()
  "Evaluate last s-expression."
  (interactive)
  (pcase (+dev/has-repl?)
    ('cider (cider-eval-last-sexp))
    (_ (message "S-expression evaluation not available for %s" (+dev/get-language-name)))))

(defun +dev/switch-to-repl ()
  "Switch to REPL buffer."
  (interactive)
  (pcase (+dev/has-repl?)
    ('cider
     (if (cider-connected-p)
         (cider-switch-to-repl-buffer)
       (message "REPL not connected. Use SPC h h to start REPL.")))
    (_ (message "No REPL available for %s" (+dev/get-language-name)))))

;; =============================================================================
;; Status and Information
;; =============================================================================

(defun +dev/status ()
  "Show development status for current buffer."
  (interactive)
  (let* ((lang (+dev/get-language-name))
         (has-repl (+dev/has-repl?))
         (repl-connected (+dev/repl-connected?))
         (has-lsp (+dev/has-lsp?))
         (preferred-nav (+dev/get-preferred-tool 'navigation))
         (preferred-debug (+dev/get-preferred-tool 'debugging)))

    (message (concat
              "Language: %s | "
              "REPL: %s | "
              "LSP: %s | "
              "Navigation: %s | "
              "Debug: %s")
             lang
             (cond
              ((not has-repl) "N/A")
              (repl-connected "Connected")
              (t "Available"))
             (if has-lsp "Active" "N/A")
             preferred-nav
             preferred-debug)))

;; =============================================================================
;; Universal Development Hydra
;; =============================================================================

(defhydra +dev/hydra (:color blue :hint nil)
  "
^Build/Run^         ^REPL^            ^Navigation^      ^Utils^
─────────────────────────────────────────────────────────────────
_bb_: build         _rr_: start/repl  _gd_: definition  _x_: dismiss popups
_br_: run           _rb_: eval buffer _gr_: references  _cc_: compilation
_bx_: recompile     _rl_: eval sexp   _gi_: implement   _s_: status
_bc_: clean         _rs_: switch repl _gh_: docs        _h_: help
^                   ^                 _rn_: rename      _q_: quit
^Test^              ^Debug^           ^Spec/Lint^       ^Quick^
─────────────────────────────────────────────────────────────────
_tt_: at point      _db_: breakpoint  _cv_: check       _ff_: file
_tf_: file          _dd_: debug       _cl_: lint        _pp_: project
_tp_: project       _dc_: continue    _cf_: format      _gg_: grep
_ta_: all           _dn_: step next   ^                 _mm_: menu
_tc_: coverage      _di_: step into   ^                 ^
_tr_: rerun         _do_: step out    ^                 ^
"
  ;; Build/Run
  ("bb" +dev/build)
  ("br" +dev/run)
  ("bx" +dev/recompile)
  ("bc" +dev/clean)

  ;; REPL
  ("rr" +dev/start-repl)
  ("rb" +dev/eval-buffer)
  ("rl" +dev/eval-last-sexp)
  ("rs" +dev/switch-to-repl)

  ;; Navigation
  ("gd" +dev/goto-definition)
  ("gr" +dev/goto-references)
  ("gi" +dev/goto-implementation)
  ("gh" +dev/show-documentation)
  ("rn" +dev/rename-symbol)

  ;; Testing (will be implemented in next file)
  ("tt" +test/run-at-point)
  ("tf" +test/run-file)
  ("tp" +test/run-project)
  ("ta" +test/run-all)
  ("tc" +test/toggle-coverage)
  ("tr" +test/rerun)

  ;; Debugging (will be implemented in debugging file)
  ("db" +debug/toggle-breakpoint)
  ("dd" +debug/start)
  ("dc" +debug/continue)
  ("dn" +debug/step-next)
  ("di" +debug/step-into)
  ("do" +debug/step-out)

  ;; Spec/Lint
  ("cv" flycheck-verify-setup :color red)
  ("cl" flycheck-list-errors :color red)
  ("cf" +dev/format-buffer)

  ;; Utils
  ("x" dismiss-popup-buffer :color red)
  ("cc" (lambda () (interactive) (pop-to-buffer "*compilation*")) :color red)
  ("s" +dev/status :color red)
  ("h" (lambda () (interactive) (describe-function '+dev/hydra/body)) :color red)

  ;; Quick access
  ("ff" counsel-projectile-find-file)
  ("pp" counsel-projectile-switch-project)
  ("gg" counsel-projectile-rg)
  ("mm" +dev/mode-specific-menu)

  ("q" nil))

(defun +dev/format-buffer ()
  "Format current buffer using appropriate formatter."
  (interactive)
  (cond
   ((and (derived-mode-p 'clojure-mode) (bound-and-true-p cider-mode))
    (cider-format-buffer))
   ((bound-and-true-p lsp-mode)
    (lsp-format-buffer))
   (t (indent-region (point-min) (point-max)))))

(defun +dev/mode-specific-menu ()
  "Open mode-specific development menu."
  (interactive)
  (pcase major-mode
    ('go-mode (+go/hydra/body))
    ('go-ts-mode (+go/hydra/body))
    (_ (message "No mode-specific menu for %s" (+dev/get-language-name)))))

;; =============================================================================
;; Keybinding Setup
;; =============================================================================

(defmacro +dev/define-lang-keys (mode-name &rest extra-bindings)
  "Define consistent development keybindings for MODE-NAME.
EXTRA-BINDINGS is a list of (key function) pairs for mode-specific commands."
  `(defun ,(intern (format "+%s/set-dev-keys" mode-name)) ()
     (let ((map (current-local-map)))
       ;; Universal hydra - primary interface
       (define-key map (kbd "<leader>hh") #'+dev/hydra/body)
       (define-key map (kbd "<f12>") #'+dev/hydra/body)

       ;; Quick access bindings
       (define-key map (kbd "gd") #'+dev/goto-definition)
       (define-key map (kbd "gr") #'+dev/goto-references)
       (define-key map (kbd "gi") #'+dev/goto-implementation)

       ;; Mode-specific extra bindings
       ,@(mapcar (lambda (binding)
                   `(define-key map (kbd ,(car binding)) ,(cadr binding)))
                 extra-bindings))))

;; Register universal keys for programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "<leader>hh") #'+dev/hydra/body)
            (local-set-key (kbd "<f12>") #'+dev/hydra/body)
            (local-set-key (kbd "gd") #'+dev/goto-definition)
            (local-set-key (kbd "gr") #'+dev/goto-references)
            (local-set-key (kbd "gi") #'+dev/goto-implementation)))

(provide '52-universal-dev)
;;; 52-universal-dev.el ends here
