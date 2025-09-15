;;============== Clojure =============

(use-package clojure-mode
  :mode "\\.clj\\'"
  :hook (clojure-mode . lsp-deferred)
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

;; cider
(unless (package-installed-p 'cider)
  (package-install 'cider))

(use-package paredit)

(evil-define-key 'normal 'global (kbd "C-p") 'paredit-splice-sexp-killing-backward)

(use-package cider
  :after clojure-mode
  :custom (cider-completion-system 'ivy)
  :config
  (setq cider-repl-display-help-banner nil)
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

;; Enable paredit mode for Clojure buffers, CIDER mode and CIDER REPL buffers
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
;; Disable paredit in emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 0)))


(defun my-spy-and-slurp ()
  "Insert '(spy )', move point before the closing parenthesis, then call `paredit-forward-slurp-sexp`."
  (interactive)
  (insert "(spy )")
  (backward-char 1)  ;; Position point before the closing parenthesis
  (paredit-forward-slurp-sexp))

(global-set-key (kbd "C-S-p") 'my-spy-and-slurp)

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



;; ============== Misc =============

(use-package yaml-mode
  :mode "\\.yml\\'"
  :hook (yaml-mode . highlight-indent-guides-mode))

;; Markdown
(use-package markdown-mode
  :mode "\\.md\\'"
  :hook (markdown-mode . visual-line-mode))
