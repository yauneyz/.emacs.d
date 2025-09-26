;;; 61-haskell.el --- Haskell tooling -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides Haskell language support with haskell-language-server, formatting,
;; REPL helpers, and test runners.

;;; Code:

(use-package haskell-mode
  :ensure t
  :mode ("\\.hs\\'" . haskell-mode)
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . lsp-deferred))
  :config
  (setq haskell-process-type 'stack-ghci
        haskell-stylish-on-save t
        haskell-compile-cabal-build-command "stack build"))

(use-package ormolu
  :ensure t
  :hook (haskell-mode . ormolu-format-on-save-mode))

(use-package haskell-snippets :ensure t :after haskell-mode)

(defun +haskell/run-main ()
  "Compile and run the current Stack project."
  (interactive)
  (compile "stack run"))

(defun +haskell/test-project ()
  "Run Stack test for the current project."
  (interactive)
  (compile "stack test"))

(provide 'lang-haskell)
;;; 61-haskell.el ends here
