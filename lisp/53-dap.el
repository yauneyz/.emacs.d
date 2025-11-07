(use-package dap-mode
  :after lsp-mode
  :commands (dap-debug dap-debug-edit-template)
  :hook
  ((go-mode go-ts-mode typescript-mode tsx-ts-mode python-mode rust-mode) . dap-mode)
  :init
  ;; Have dap auto-wire UI, sessions, tooltip eval, and controls
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  ;; Keep breakpoints across Emacs restarts
  (setq dap-breakpoints-file (expand-file-name "dap-breakpoints" user-emacs-directory))
  :config
  ;; Enable the UI minor modes once; hooks below will show/hide the windows
  (require 'dap-ui)
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1)
  (dap-auto-configure-mode 1)

  ;; --- Open/close panels automatically ---
  ;; When a session starts, pop open the layout and REPL
  (add-hook 'dap-session-created-hook
            (lambda (&rest _)
              (dap-ui-show-many-windows t)
              (dap-ui-repl)))

  ;; When a session ends, fold everything away
  (add-hook 'dap-terminated-hook
            (lambda (&rest _)
              (dap-ui-hide-many-windows)
              (dap-ui-controls-mode -1)))  ;; optional: hide the little inline controls

  (add-hook 'dap-session-created-hook
            (lambda (&rest _) (dap-ui-controls-mode 1))) ;; re-enable when a new session starts

  ;; Optional: automatically show the hydra the first time you stop on a breakpoint
  (defvar my/dap--hydra-shown nil)
  (add-hook 'dap-stopped-hook
            (lambda (arg)
              (unless my/dap--hydra-shown
                (setq my/dap--hydra-shown t)
                (call-interactively #'dap-hydra))))

  (add-hook 'dap-terminated-hook (lambda (&rest _) (setq my/dap--hydra-shown nil)))

  ;; ---------- Adapters & templates ----------
  ;; Node / TS
  (require 'dap-node)        ;; M-x dap-node-setup to install if needed
  ;; Tip: calling dap-node-setup on every startup is slow; run it manually if adapter is missing.
  (dap-register-debug-template
   "Node :: Launch Current File"
   (list :type "node" :request "launch" :name "Node :: Launch Current File"
         :program "${file}" :cwd "${workspaceFolder}"
         :runtimeExecutable "node"))
  ;; If you use ts-node, consider:
  ;; (list :type "node" :request "launch" :name "TS :: ts-node"
  ;;       :program "${workspaceFolder}/src/index.ts"
  ;;       :runtimeExecutable "node" :runtimeArgs ["--loader" "ts-node/esm"]
  ;;       :sourceMaps t)

  ;; Go / Delve
  (require 'dap-dlv-go)          ;; preferred require; uses Delve adapter
  ;; (M-x dap-go-setup) if you need to fetch the adapter
  (dap-register-debug-template
   "Go :: Run Main"
   (list :type "go" :request "launch" :name "Go :: Run Main"
         :mode "auto" :program "${workspaceFolder}/main" :cwd "${workspaceFolder}"))
  (dap-register-debug-template
   "Go :: Test Current Package"
   (list :type "go" :request "launch" :name "Go :: Test"
         :mode "test" :program "${workspaceFolder}" :cwd "${workspaceFolder}"))

  ;; Python (debugpy)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (dap-register-debug-template
   "Python :: Run current file"
   (list :type "python" :request "launch" :name "Python :: File"
         :program "${file}" :cwd "${workspaceFolder}" :justMyCode t))

  ;; Rust (CodeLLDB). Easiest is to drive cargo directly:
  (require 'dap-lldb) ;; ensure codelldb/lldb-vscode is installed
  (dap-register-debug-template
   "Rust :: Cargo Run"
   (list :type "lldb" :request "launch" :name "Rust :: Cargo Run"
         :cargo (list :args ["run"] :filterMode "rustc") ; build+run via cargo
         :cwd "${workspaceFolder}"))

  ;; ---------- Keybindings ----------
  (evil-define-key 'normal 'global (kbd "<leader>d") #'dap-hydra)

  ;; If you prefer the hydra *every* stop (not just first), use your original hook:
  ;; (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra)))


  ;; Make the hydra not

  (provide '53-dap)
