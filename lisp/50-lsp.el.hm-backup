(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-on-type-formatting nil
        lsp-log-io t
        lsp-idle-delay 0.2
        lsp-completion-provider :none)
  (setenv "CLOJURE_LSP_LOG_LEVEL" "DEBUG")
  (setenv "CLOJURE_LSP_LOG_PATH" (expand-file-name "~/.cache/clojure-lsp/log.txt"))

  ;; Corfu handles CAPF

  ;; clojure-lsp cache locations
  (setq lsp-clojure-workspace-dir       (expand-file-name "~/.cache/clojure-lsp/workspace/")
        lsp-clojure-workspace-cache-dir (expand-file-name "~/.cache/clojure-lsp/"))

  :hook ((python-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (go-ts-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (haskell-mode . lsp-deferred)
         (yaml-mode . lsp-deferred)
         (json-mode . lsp-deferred)
         (toml-mode . lsp-deferred)
         (dockerfile-mode . lsp-deferred)
         (sh-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (clojure-mode . lsp-deferred)
         (clojurescript-mode . lsp-deferred)
         (clojurec-mode . lsp-deferred))
  :config
  ;; Tree-sitter variants
  (dolist (mode '(go-ts-mode rust-ts-mode python-ts-mode tsx-ts-mode
                             haskell-ts-mode bash-ts-mode c-ts-mode c++-ts-mode))
    (add-hook (intern (format "%s-hook" mode)) #'lsp-deferred))

  (lsp-enable-which-key-integration t)
  ;; (evil-define-key 'normal 'global (kbd "<leader>l") lsp-command-map)

  ;; Diagnostic UX
  (setq lsp-modeline-diagnostics-enable t
        lsp-signature-auto-activate nil
        lsp-semantic-tokens-enable t
        lsp-headerline-breadcrumb-enable t)

  ;; Go-specific tweaks
  (setq lsp-go-use-gofumpt t
        lsp-go-staticcheck t)

  ;; gopls registration (ensures explicit binary usage even with custom paths)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "gopls")
                    :activation-fn (lsp-activate-on "go")
                    :server-id 'gopls))

  ;; rust-analyzer sensible defaults
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable t)

  ;; Haskell
  (setq lsp-haskell-server-path "haskell-language-server-wrapper"
        lsp-haskell-formatting-provider "ormolu"))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq imenu-auto-rescan t)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-sideline-show-diagnostics t))

(use-package lsp-treemacs
  :after lsp-mode
  :config (lsp-treemacs-sync-mode 1))

(use-package lsp-ivy :after lsp-mode)


(provide '50-lsp)
;;; 50-lsp.el ends here
