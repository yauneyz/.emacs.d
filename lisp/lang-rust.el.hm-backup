;;; 59-rust.el --- Rust tooling -*- lexical-binding: t; -*-

;;; Commentary:
;; Rust development via rustic (cargo helpers, rust-analyzer integration) and
;; on-save formatting/lints.

;;; Code:

(use-package rustic
  :ensure t
  :mode ("\\.rs\\'" . rustic-mode)
  :hook ((rustic-mode . lsp-deferred)
         (rustic-mode . rustic-format-on-save-mode))
  :custom
  (rustic-format-on-save t)
  (rustic-lsp-client 'lsp-mode)
  (rustic-cargo-use-last-stored-command t)
  :config
  (setq rustic-analyzer-command '("rust-analyzer")))

(use-package cargo
  :ensure t
  :hook (rustic-mode . cargo-minor-mode))

(use-package toml-mode
  :mode "\\.toml\\'")

(provide 'lang-rust)
;;; 59-rust.el ends here
