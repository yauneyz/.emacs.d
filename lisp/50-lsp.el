;;; 50-lang-lsp.el --- LSP, DAP, major modes  -*- lexical-binding: t; -*-

;; ---------------------------- LSP, DAP --------------------------------------
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((typescript-mode js-mode clojure-mode python-mode rust-mode go-mode go-ts-mode) . lsp-deferred)
  :custom
  (lsp-idle-delay 0.2)
  (lsp-completion-provider :none)
  :config (lsp-enable-which-key-integration t)
  (evil-define-key 'normal 'global (kbd "<leader>l") lsp-command-map)

  ;; Go-specific LSP settings
  (setq lsp-go-use-gofumpt t
        lsp-go-staticcheck t
        lsp-semantic-tokens-enable t
        lsp-eldoc-enable-hover t
        lsp-headerline-breadcrumb-enable t)

  ;; Format and organize imports on save for Go buffers
  (dolist (hook '(go-mode-hook go-ts-mode-hook))
    (add-hook hook (lambda ()
                     (add-hook 'before-save-hook #'+go/lsp-format+imports nil t)))))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom ((lsp-ui-doc-enable t)
           (lsp-ui-doc-position 'bottom)
           (lsp-ui-sideline-show-code-actions t)
           (lsp-ui-sideline-show-diagnostics t)))

(use-package lsp-ivy)
(use-package lsp-treemacs :after lsp :config (lsp-treemacs-sync-mode 1))

;; DAP (Node example) ----------------------------------------------------------
(use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :hook ((js-mode typescript-mode go-mode go-ts-mode) . dap-mode)
  :config
  (dap-auto-configure-mode)
  (require 'dap-ui) (dap-ui-mode 1)
  (require 'dap-node) (dap-node-setup)

  ;; Go DAP (debugging with Delve)
  (require 'dap-dlv-go)  ;; Go adapter

  (dap-register-debug-template
   "Node :: Launch Current File"
   (list :type "node" :request "launch" :name "Launch"
         :program "${file}" :cwd "${workspaceFolder}"
         :runtimeExecutable "node"
         :console "integratedTerminal"
         :internalConsoleOptions "neverOpen"))

  ;; Go debug templates
  (dap-register-debug-template "Go: Launch main (go run .)"
    (list :type "go" :request "launch" :name "Go: Launch main"
          :mode "auto" :program "${workspaceFolder}"))
  (dap-register-debug-template "Go: Test current package"
    (list :type "go" :request "launch" :name "Go: Test pkg"
          :mode "test" :program "${workspaceFolder}"))

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
