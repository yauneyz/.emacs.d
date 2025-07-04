;;; 50-lang-lsp.el --- LSP, DAP, major modes  -*- lexical-binding: t; -*-

;; ---------------------------- LSP, DAP --------------------------------------
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((typescript-mode js-mode clojure-mode python-mode rust-mode) . lsp-deferred)
  :config (lsp-enable-which-key-integration t)
  (evil-define-key 'normal 'global (kbd "<leader>l") lsp-command-map))

(setq lsp-pylsp-plugins-flake8-enabled t
      lsp-pylsp-plugins-flake8-ignore
      '("D100" "D101" "D102" "D103" "D104" "D105" "D107"))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom ((lsp-ui-doc-enable t)
           (lsp-ui-doc-position 'bottom)))

(use-package lsp-ivy)
(use-package lsp-treemacs :after lsp :config (lsp-treemacs-sync-mode 1))

;; DAP (Node example) ----------------------------------------------------------
(use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :hook ((js-mode typescript-mode) . dap-mode)
  :config
  (dap-auto-configure-mode)
  (require 'dap-ui) (dap-ui-mode 1)
  (require 'dap-node) (dap-node-setup)

  (dap-register-debug-template
   "Node :: Launch Current File"
   (list :type "node" :request "launch" :name "Launch"
         :program "${file}" :cwd "${workspaceFolder}"
         :runtimeExecutable "node"
         :console "integratedTerminal"
         :internalConsoleOptions "neverOpen"))

  (evil-define-key 'normal 'global
    (kbd "<leader>db") #'dap-breakpoint-toggle
    (kbd "<leader>dB") #'dap-breakpoint-condition
    (kbd "<leader>dd") #'dap-debug
    (kbd "<leader>dl") #'dap-debug-last
    (kbd "<leader>dc") #'dap-continue
    (kbd "<leader>di") #'dap-step-in
    (kbd "<leader>do") #'dap-step-out
    (kbd "<leader>dn") #'dap-next
    (kbd "<leader>dr") #'dap-restart-frame
    (kbd "<leader>dh") #'dap-hydra
    (kbd "<leader>dq") #'dap-disconnect))

;; ---------------------------- Major modes ------------------------------------
(use-package clojure-mode
  :mode "\\.clj[scx]?\\'"
  :hook (clojure-mode . lsp-deferred)
  :config (add-hook 'clojure-mode-hook #'paredit-mode))

(use-package cider
  :after clojure-mode
  :custom (cider-completion-system 'ivy)
  :config
  (setq cider-repl-display-help-banner nil)
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

(use-package python-mode :mode "\\.py\\'"  :hook (python-mode . lsp-deferred))
(use-package rust-mode   :mode "\\.rs\\'"  :hook (rust-mode   . lsp-deferred)
  :custom (lsp-rust-server 'rust-analyzer) (indent-tabs-mode nil))
(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode) ("\\.tsx\\'" . typescript-mode))
  :hook (typescript-mode . (lambda ()
                             (setq-local indent-tabs-mode nil
                                         tab-width 2
                                         typescript-indent-level 2))))

(use-package yaml-mode      :mode "\\.ya?ml\\'" :hook (yaml-mode . highlight-indent-guides-mode))
(use-package markdown-mode  :mode "\\.md\\'"   :hook (markdown-mode . visual-line-mode))

(provide '50-lang-lsp)
;;; 50-lang-lsp.el ends here
