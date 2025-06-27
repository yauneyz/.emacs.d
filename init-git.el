;;; Code:

					;(setq debug-on-error t)


(defun install-if-necessary (package)
  "Install PACKAGE unless it is already installed."
  (unless (package-installed-p package)
    (package-install package)))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)

					;(when (memq window-system '(mac ns x))
					;  (exec-path-from-shell-initialize))

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Quelpa
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;; =========== UI Cleanup  ===================
(setq inhibit-startup-message t
      visible-bell t)

;; relative line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Command Log
(use-package command-log-mode
  :config
  (global-command-log-mode))

;; Don't show line numbers on some modes like terminal, shell, org
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; All the icons
(use-package all-the-icons)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)          ; Disable the menu bar

;; =========== Packages ===================

;;; No-littering
					;(use-package no-littering)

;; Evil
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  ;; (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-integration t)
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-undo-system 'undo-redo))

;; Evil-Collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Leader Key
(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'visual (kbd "SPC"))

;; Evil Surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; ;; Emacs snipe
;; (use-package evil-snipe
;;   :after evil
;;   :config
;;   (evil-snipe-mode +1)
;;   (evil-snipe-override-mode +1)
;;   (setq evil-snipe-scope 'visible
;;         evil-snipe-repeat-scope 'whole-buffer))

(use-package avy
  :ensure t
  :bind (("C-:"   . avy-goto-char)        ;; Jump to a character in the visible buffer
         ("C-'"   . avy-goto-char-2)      ;; Jump to a character using a two-character lookup
         ("M-g g" . avy-goto-line)        ;; Jump to a specific line
         ("M-g w" . avy-goto-word-0))     ;; Jump to the beginning of a word
  :config
  ;; When Avy is activated, highlight potential jump targets in the background.
  (setq avy-background t
        ;; Limit Avy to the current window (set to nil for all windows)
        avy-all-windows nil))


;; Which Key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; cider
(unless (package-installed-p 'cider)
  (package-install 'cider))

;; paredit
(use-package paredit)
;; :config
;; (show-paren-mode t)
;; :diminish)

;; Enable paredit mode for Clojure buffers, CIDER mode and CIDER REPL buffers
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
;; Disable paredit in emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 0)))

;; -----------------------------------------------------------------------------
;; Paredit custom keybinding: Bind Ctrl-p to paredit-splice-sexp-killing-backward
(evil-define-key 'normal 'global (kbd "C-p") 'paredit-splice-sexp-killing-backward)

(defun my-spy-and-slurp ()
  "Insert '(spy )', move point before the closing parenthesis, then call `paredit-forward-slurp-sexp`."
  (interactive)
  (insert "(spy )")
  (backward-char 1)  ;; Position point before the closing parenthesis
  (paredit-forward-slurp-sexp))

(global-set-key (kbd "C-S-p") 'my-spy-and-slurp)

;; Ensure Ivy is using fuzzy matching for all completions.
(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
(ivy-mode 1)

(defun my-counsel-projectile-find-file ()
  "Use counsel-projectile-find-file with true fuzzy matching.
Temporarily disable ivy-prescient-mode and force ivy--regex-fuzzy
to allow out-of-order matching like 'eve/mp' for 'events/map'."
  (interactive)
  (let ((ivy-prescient-mode nil)
        (ivy-re-builders-alist '((t . ivy--regex-fuzzy))))
    (counsel-projectile-find-file)))

;; Counsel
(use-package counsel
  :ensure t
  :bind (("C-x C-f"   . counsel-find-file)          ; default binding
         ("<leader>SPC" . my-counsel-projectile-find-file)) ; use our wrapper for Projectile
  :config
  (setq ivy-initial-inputs-alist nil))

;; NERD Commenter
(install-if-necessary 'evil-nerd-commenter)
;; (evilnc-default-hotkeys)
(evil-define-key '(normal visual) 'global (kbd "<leader>cc") 'evilnc-comment-or-uncomment-lines)
(evil-define-key '(normal visual) 'global (kbd "<leader>cu") 'evilnc-comment-or-uncomment-lines)

;; Helpful
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; Pulsar
(use-package pulsar
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1))

;; ELscreen
(use-package elscreen
  :config
  (elscreen-start))

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Bind counsel-M-x to C-t C-t
(global-set-key (kbd "C-a") 'counsel-M-x)

;; Counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; Swiper
(use-package swiper)

;; Ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-initial-inputs-alist nil) ; Don't start searches with ^
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t      . ivy--regex-fuzzy)))
  (ivy-mode 1))

;; Ivy Rich
(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1))

;; Flx for fuzzy matching
(use-package flx)
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

;; Also use prescient for sorting
(use-package prescient
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after (ivy prescient)
  :config
  (ivy-prescient-mode 1))

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom ((projectile-completion-system 'ivy))
  :init
  (setq projectile-project-search-path '("~/development/"))
  :config (projectile-mode)
  (evil-define-key 'normal 'global (kbd "<leader>p") 'projectile-command-map)
  (add-to-list 'projectile-globally-ignored-directories ".clj-kondo")
  (setq projectile-auto-discover t)   ;; discover projects at start-up
  (setq projectile-mode-line "Projectile")
  (setq projectile-sort-order 'recently-active)
  (setq projectile-generic-command "find . -type f -not -path '*/node_modules/*' -not -path '*/build/*' -not -path '*/.clj-kondo/*' -print0"))


;; Make sure we load our projects on startup
(with-eval-after-load 'projectile
  (projectile-discover-projects-in-search-path))

;; Counsel Projectile
(use-package counsel-projectile
  :after (projectile counsel)
  :config (counsel-projectile-mode)
  ;; :after ivy-prescient
  )

(use-package magit
  :ensure t
  :commands (magit-status magit-dispatch)
  :bind (("C-x g" . magit-status)       ;; Quickly open Magit status
         ("C-x M-g" . magit-dispatch))    ;; Magit command dispatcher
  :custom
  ;; Display Magit buffers in the same window (except for diffs)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  ;; Automatically save modified repository buffers without prompting before actions
  (magit-save-repository-buffers 'dontask)
  ;; Use Ivy (or your preferred completion framework) for Magit prompts
  (magit-completing-read-function #'ivy-completing-read)
  :config
  ;; Optional: Define your repository search path for Magit if you work in a common directory
  ;; (setq magit-repository-directories '(("~/projects" . 2)))
  (setq magit-repository-directories
	;; Turn each known-project into a (dir . depth) cons,
	;; so Magit will list them in “List repositories…”
	(mapcar (lambda (proj) (cons proj 1))
		projectile-known-projects))

  (global-auto-revert-mode +1)

  ;; Optional: Refine diffs to highlight intraline changes
  (setq magit-diff-refine-hunk t)
  ;; Optional: Uncomment the following if you use Forge to interact with GitHub, GitLab, etc.
  ;; (use-package forge
  ;;   :after magit)
  )

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-project-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

					;(use-package treemacs-magit
					;  :after (treemacs magit)
					;  :ensure t)

;; Command Log
(use-package command-log-mode)

;; ─────────────────────────────────────────────────────────
;; DAP (Debug Adapter Protocol) · Node.js
;; ─────────────────────────────────────────────────────────
(use-package dap-mode
  :after lsp-mode                     ;; dap sits on top of lsp-mode
  :commands dap-debug
  :hook ((js-mode        . dap-mode)  ;; enable DAP minor-mode automatically
         (typescript-mode . dap-mode)
         (dap-stopped     . (lambda (_) (dap-hydra)))) ; pop hydra on stop
  :config
  ;; Core UI helpers (locals, sessions, breakpoints, repl …)
  (dap-auto-configure-mode)
  ;; Optional fancier visuals
  (require 'dap-ui)
  (dap-ui-mode 1)

  ;; ── Node / JS adapter ─────────────────────────────────
  ;; dap ships with two Node back-ends:
  ;;   • “vscode-node-debug2” (classic, stable)
  ;;   • “js-debug” (newer, same engine VS Code uses today)
  ;; Pick ONE; js-debug is recommended in 2025.
  ;;
  (require 'dap-node)
  (dap-node-setup)          ;; Downloads js-debug if absent (≈ 40 MB once)

  ;; ── Templates you’ll see in `M-x dap-debug` selector ──
  (dap-register-debug-template
   "Node :: Launch Current File"
   (list :type "node"
         :request "launch"
         :name "Launch Current File"
         :program "${file}"
         :cwd "${workspaceFolder}"
         :runtimeExecutable "node"
         :console "integratedTerminal"
         :internalConsoleOptions "neverOpen"))

  (dap-register-debug-template
   "Node :: Attach to PID"
   (list :type "node"
         :request "attach"
         :name "Attach to Process"
         :processId "${command:pickProcess}"
         :cwd "${workspaceFolder}"))

  ;; ── Evil / Leader key helpers ─────────────────────────
  (evil-define-key 'normal 'global
    (kbd "<leader>db")  #'dap-breakpoint-toggle
    (kbd "<leader>dB")  #'dap-breakpoint-condition
    (kbd "<leader>dd")  #'dap-debug
    (kbd "<leader>dl")  #'dap-debug-last
    (kbd "<leader>dc")  #'dap-continue
    (kbd "<leader>di")  #'dap-step-in
    (kbd "<leader>do")  #'dap-step-out
    (kbd "<leader>dn")  #'dap-next
    (kbd "<leader>dr")  #'dap-restart-frame
    (kbd "<leader>dh")  #'dap-hydra
    (kbd "<leader>dq")  #'dap-disconnect)
  )


;; =========== Language Specific  ===================(

;; LSP
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred)
  :hook ((typescript-mode . lsp-deferred)   ; .ts/.tsx
         (js-mode        . lsp-deferred))   ; .js/.jsx
  :config
  (lsp-enable-which-key-integration t)
  (evil-define-key 'normal 'global (kbd "<leader>l") lsp-command-map))


;;; Jedi
					;(use-package lsp-jedi
					;  :ensure t)

;; configure pylsp
(setq lsp-pylsp-plugins-flake8-enabled t)
;; ignore docstring errors
(setq lsp-pylsp-plugins-flake8-ignore '("D100" "D101" "D102" "D103" "D104" "D105" "D107"))

;; LSP UI
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'bottom))

;; LSP Ivy
(use-package lsp-ivy)

;; LSP Treemacs
(use-package lsp-treemacs
  :after lsp
  :config
  (lsp-treemacs-sync-mode 1))

;; Header breadcrumbs
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; Clojure
(use-package clojure-mode
  :mode "\\.clj\\'"
  :hook (clojure-mode . lsp-deferred)
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

;; Cider
					; Use ivy for completion
(use-package cider
  :config
  (setq cider-completion-system 'ivy)
  (setq cider-repl-display-help-banner nil))

;; Hide the REPL window after connecting
(defun my/cider-hide-repl-window ()
  "Hide the cider REPL window after connecting."
  (let ((repl-window (get-buffer-window (cider-current-repl-buffer))))
    (when repl-window
      (delete-window repl-window))))

(add-hook 'cider-connected-hook 'my/cider-hide-repl-window)

;;; Format All
					;(use-package format-all
					;  :commands format-all-mode
					;  :hook (prog-mode . format-all-mode)
					;  :config
					;  (setq-default format-all-formatters
					;                ;;; Python
					;                '((python-mode "black")
					;                  ;;; Clojure
					;                  (clojure-mode "cljfmt")
					;                  ;;;Rust
					;                  (rust-mode "rustfmt")
					;                  ;;; Emacs Lisp
					;                  (emacs-lisp-mode "emacs-lisp-formatter"))))

;; Have lsp-format-buffer run on save in programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)))

;; Python
(use-package python-mode
  :mode "\\.py\\'"
  :hook (python-mode . lsp-deferred))

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . lsp-deferred)
  :config
  (setq lsp-rust-server 'rust-analyzer)
  (setq indent-tabs-mode nil))


;; Typescript
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

;; YAML w/ highlight-indent
(use-package yaml-mode
  :mode "\\.yml\\'"
  :hook (yaml-mode . highlight-indent-guides-mode))

;; Markdown
(use-package markdown-mode
  :mode "\\.md\\'"
  :hook (markdown-mode . visual-line-mode))

;; =========== Config  ===================

;; History saving
(setq history-length 25)
(savehist-mode 1)

;; Place Saving
(save-place-mode 1)

;; =========== Theme  ===================
					;(use-package doom-themes
					;  :ensure t
					;  :config
					;  ;;; Global settings (defaults)
					;  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
					;        doom-themes-enable-italic t) ; if nil, italics is universally disabled
					;  (load-theme 'doom-tokyo-night t)
					;  ;;(load-theme 'doom-rouge t)
					;  ;;(load-theme 'doom-dracula t)
					;  ;;(load-theme 'doom-snazzy t)
					;  ;;(load-theme 'doom-outrun-electric t)
					;)

;; =================== Theme and Appearance ===================

;; Install and configure ewal and ewal-doom-themes
					;(use-package ewal
					;  :quelpa (ewal :fetcher github :repo "ebanster/ewal"))

;; (use-package ewal
;;   :init (setq ewal-use-built-in-always-p nil
;;               ewal-use-built-in-on-failure-p t
;;               ewal-built-in-palette "sexy-gotham"))

(use-package ewal-doom-themes
  :quelpa (ewal-doom-themes :fetcher github :repo "tsc25/ewal-doom-themes")
  :config
 ;;; Set the theme to use ewal-doom-vibrant or your preferred theme
  ;; (load-theme 'ewal-doom-one t)
 ;;; (load-theme 'ewal-doom-vibrant t)
  ;; (load-theme 'doom-solarized-dark t)
					;(load-theme 'doom-feather-dark t)
  (load-theme 'doom-dark+ t)

 ;;; Enable bold and italic if desired
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
 ;;; Configure doom themes enhancements
  (doom-themes-org-config))     ;; Correct org-mode fontification

;; Let EMACS have transparent background
(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(alpha-background . 100)) ; 0-100, 100 = opaque
  (set-frame-parameter nil 'alpha-background 100))             ; current frame

(use-package ewal-evil-cursors
  :after ewal
  :config
  (ewal-evil-cursors-get-colors :apply t :spaceline t))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-icon t))

;; =========== Copilot  ===================
(install-if-necessary 'editorconfig)
(install-if-necessary 'jsonrpc)
(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package jsonrpc)

(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el")))
(add-hook 'prog-mode-hook
          (lambda ()
            (copilot-mode)
            ;; Unbind TAB in evil insert state to allow Copilot to use it
            (define-key evil-insert-state-map (kbd "TAB") nil)
            ;; Optionally, bind TAB to 'copilot-accept-completion'
            ;; (evil-define-key 'insert 'global (kbd "TAB") 'copilot-accept-completion)
            (setq copilot-indent-offset-warning-disable t)))

;; =========== Company  ===================

;; Company Mode - configured to use LSP
(use-package company
  :after (lsp-mode copilot)
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("C-l" . company-complete-selection)
              ("TAB" . copilot-accept-completion)
              ("<tab>" . copilot-accept-completion))
  (:map lsp-mode-map
        ("C-l" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; Company Box
(use-package company-box
  :hook (company-mode . company-box-mode))

;; =========== Font  ===================
(set-face-attribute 'default nil :font "Fira Code Retina" :height 120)

;; =========== Keybindings  ===================
;; In elisp-mode, C-c C-c will evaluate the buffer
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'eval-buffer)))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package windmove)

;; Evil keybindings for window movement
(evil-define-key 'normal 'global (kbd "C-h") 'windmove-left)
(evil-define-key 'normal 'global (kbd "C-j") 'windmove-down)
(evil-define-key 'normal 'global (kbd "C-k") 'windmove-up)
(evil-define-key 'normal 'global (kbd "C-l") 'windmove-right)

;; Keybindings for sexp movement using literal parentheses
(evil-define-key '(normal insert) 'global (kbd "C-c (") 'backward-sexp)
(evil-define-key '(normal insert) 'global (kbd "C-c )") 'forward-sexp)

;; Repeat for treemacs
(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map (kbd "C-h") 'windmove-left)
  (define-key treemacs-mode-map (kbd "C-j") 'windmove-down)
  (define-key treemacs-mode-map (kbd "C-k") 'windmove-up)
  (define-key treemacs-mode-map (kbd "C-l") 'windmove-right))

(global-set-key (kbd "C-M-h") 'help-command)
(global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)

;; paredit commands
;; (evil-define-key 'normal 'global (kbd "C-f") 'paredit-forward)
;; (evil-define-key 'normal 'global (kbd "C-b") 'paredit-backward)
;; (evil-define-key 'normal 'global (kbd "C-d") 'paredit-forward-down)
;; (evil-define-key 'normal 'global (kbd "C-u") 'paredit-backward-up)
;; (evil-define-key 'normal 'global (kbd "C-n") 'paredit-forward-up)
;; (evil-define-key 'normal 'global (kbd "C-p") 'paredit-backward-down)
					; Slurp and barf on parentheses and brackets

;; Turn off read-only mode
(read-only-mode 0)

(setq lsp-pylsp-plugins-autopep8-enabled t)

(defun org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (auto-fill-mode 0)
  (olivetti-mode 1)
  (define-key org-mode-map (kbd "C-<right>") 'org-metaright)
  (define-key org-mode-map (kbd "C-<left>") 'org-metaleft)
  (define-key org-mode-map (kbd "C-<up>") 'org-metaup)
  (define-key org-mode-map (kbd "C-<down>") 'org-metadown))

;; Org mode
(use-package org
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-ellipsis " ▼")
  (setq org-hide-emphasis-markers t)
					; set tab to be org mode tab
  (evil-define-key 'insert 'global (kbd "TAB") 'org-cycle))

;; Org hook to turn off evil auto-indent
;; Can't be in org-mode-setup or it somehow messes with other buffers
;;(add-hook 'org-mode-hook (lambda () (setq evil-auto-indent nil)))

;; Org-bullets
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "✸" "✿" "❀" "❁" "❂" "❃" "❄" "❅" "❆" "❇")))

(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
					;(set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))
  )

					; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(defun fountain-mode-setup ()
  (olivetti-mode 1)
  (visual-line-mode 1)
  (auto-fill-mode 0)
  (setq fountain-hide-emphasis-markup t)
  (setq fountain-hide-element t)
  (setq fountain-display-scene-numbers-in-margin t))

					; Fountain mode
(use-package fountain-mode
  :hook
  (fountain-mode . fountain-mode-setup)
  :config
  (setq copilot-mode 0)
  (setq company-mode 0)
  ;; Set the way we export, just using screenplain
  ;; To get it to work, just pip install screenplain
  (setq fountain-export-command-profiles '(("screenplain" . "screenplain -f pdf %b ../output/%B.pdf")))
  (which-function-mode 1))

(defun dispatch-tab-command ()
  "Dispatch <tab> to different functions based on the current buffer's major mode."
  (interactive)
  (cond
   ;; When in fountain-mode
   ((eq major-mode 'fountain-mode)
    (fountain-dwim))
   ;; Other cases
   (t
    (or (copilot-accept-completion)
					;(company-yasnippet-or-completion)
        (indent-for-tab-command)))))

;; Use snippets
(use-package yasnippet
  :config
  (yas-global-mode 1)
  ;; Bind C-y to yas-expand
  (define-key yas-minor-mode-map (kbd "C-j") 'yas-next-field-or-maybe-expand)
  (define-key yas-minor-mode-map (kbd "C-S-j") 'yas-prev-field))

(defun my/company-yasnippet-complete ()
  "Trigger company-yasnippet."
  (interactive)
  (let ((company-backends '(company-yasnippet)))
    (company-complete)))
(evil-define-key 'insert 'global (kbd "C-y") 'my/company-yasnippet-complete)

;; makes snippets work
(setq require-final-newline nil)

(defun my-yas-try-expanding-auto-snippets ()
  (when (and (boundp 'yas-minor-mode) yas-minor-mode)
    (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
      (yas-expand))))
(add-hook 'post-command-hook #'my-yas-try-expanding-auto-snippets)

;; Globally bind <tab> to the dispatcher function
(global-set-key (kbd "<tab>") 'dispatch-tab-command)
(evil-define-key 'insert 'global (kbd "<tab>") 'dispatch-tab-command)

					; Olivetti mode
(use-package olivetti
  :config
  (setq olivetti-body-width 80)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

;; Call olivetti mode on .txt
(add-hook 'text-mode-hook 'olivetti-mode)

;; --- New configuration for programming modes ---
(defun my/prog-olivetti-setup ()
  "Configure Olivetti in programming modes: set a 100-column width and disable visual margins."
  (setq-local olivetti-body-width 100)
  (setq-local olivetti-minimum-body-width 100)
  ;; Disable visual margins by setting the style to nil.
  ;; (See Olivetti’s documentation: setting `olivetti-style` to nil turns off any
  ;; extra side margins.)
  (setq-local olivetti-style nil)
  (olivetti-mode 1))

(add-hook 'prog-mode-hook #'my/prog-olivetti-setup)

					; Code folding in programming modes
(defun hs-minor-mode-setup ()
  (hs-minor-mode 1)
  (setq hs-hide-comments-when-hiding-all t))
(add-hook 'prog-mode-hook 'hs-minor-mode-setup)

;; =================== Evil command shortcuts ===================

;; Elscreen
(evil-define-key 'normal 'global (kbd "<leader>sc") 'elscreen-create)
(evil-define-key 'normal 'global (kbd "<leader>sk") 'elscreen-kill)
(evil-define-key 'normal 'global (kbd "<leader>sg") 'elscreen-goto)
(evil-define-key 'normal 'global (kbd "<leader>sp") 'elscreen-previous)
(evil-define-key 'normal 'global (kbd "<leader>sn") 'elscreen-next)
(evil-define-key 'normal 'global (kbd "<leader>sb") 'elscreen-find-and-goto-by-buffer)
(evil-define-key 'normal 'global (kbd "<leader>so") 'elscreen-toggle) ;; Jump to last screen

;; Elscreen with M-h and M-l
(evil-define-key 'normal 'global (kbd "M-h") 'elscreen-previous)
(evil-define-key 'normal 'global (kbd "M-l") 'elscreen-next)

;; Treemacs
(evil-define-key 'normal 'global (kbd "<leader>tt") 'treemacs)
(evil-define-key 'normal 'global (kbd "<leader>tf") 'treemacs-find-file)

;; Magit
(evil-define-key 'normal 'global (kbd "<leader>gs") 'magit-status)

;; Cider
(evil-define-key 'normal 'global (kbd "<leader>cj") 'cider-connect-cljs)
(evil-define-key 'normal 'global (kbd "<leader>cl") 'cider-eval-last-sexp)
(evil-define-key 'normal 'global (kbd "<leader>cb") 'cider-eval-buffer)
(evil-define-key 'normal 'global (kbd "<leader>cf") 'cider-format-buffer)
(evil-define-key 'normal 'global (kbd "<leader>cr") 'cider-restart)

;; Emacs lisp
(evil-define-key 'normal 'global (kbd "<leader>ee") 'eval-buffer)
(evil-define-key 'normal 'global (kbd "<leader>el") 'eval-last-sexp)

;; Repl
(evil-define-key 'normal 'global (kbd "<leader>nr") 'cider-switch-to-repl-buffer)
(evil-define-key 'normal 'global (kbd "<leader>nc") 'cider-switch-to-last-clojure-buffer)

;; Buffer control
(evil-define-key 'normal 'global (kbd "<leader>bs") 'counsel-switch-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-buffer)

;; File finding
(evil-define-key 'normal 'global (kbd "<leader>gg") 'counsel-projectile-rg)

;; LSP commands
(evil-define-key 'normal 'global (kbd "gd") 'lsp-find-definition)
(evil-define-key 'normal 'global (kbd "gr") 'lsp-find-references)
(evil-define-key 'normal 'global (kbd "gi") 'lsp-find-implementation)
(evil-define-key 'normal 'global (kbd "<leader>rn") 'lsp-rename)

;; Treemacs
(evil-define-key 'normal 'global (kbd "<leader>ts") 'lsp-treemacs-symbols)

;; Tooltips - glance, hover, etc
(evil-define-key 'normal 'global (kbd "<leader>lg") 'lsp-ui-doc-glance)
(evil-define-key 'normal 'global (kbd "<leader>ld") 'lsp-ui-doc-show)
(evil-define-key 'normal 'global (kbd "<leader>ls") 'lsp-signature-help)
(evil-define-key 'normal 'global (kbd "<leader>lh") 'lsp-describe-thing-at-point)

;; Help commands
(evil-define-key 'normal 'global (kbd "<leader>hf") 'counsel-describe-function)
(evil-define-key 'normal 'global (kbd "<leader>hv") 'counsel-describe-variable)
(evil-define-key 'normal 'global (kbd "<leader>hk") 'counsel-describe-key)

;; F3 is toggle fold
(evil-define-key 'normal 'global (kbd "<f3>") 'evil-toggle-fold)

;; Disable Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defun my-open-init-file ()
  "Open the init file without prompting about symbolic links."
  (interactive)
  (let ((vc-follow-symlinks t))
    (find-file user-init-file)))

;; Used to reload buffers after claudesync push
(defun revert-all-buffers ()
  "Revert all non-modified buffers associated with a file."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer :ignore-auto :noconfirm))))
  (message "All buffers reverted"))


(defun evil-select-inside-comment-block ()
  "Select everything inside the (comment ...) block under the cursor, excluding the 'comment' keyword itself, in Evil visual mode."
  (interactive)
  (when (and (bound-and-true-p evil-mode)
             (eq evil-state 'normal))
    (save-excursion
      ;; Navigate up the syntax tree to find the (comment ...) block
      (condition-case nil
          (progn
            ;; Move to the beginning of the current sexp
            (while (not (and (looking-at-p "(comment")
                             (eq (char-after (point)) ?\())
                        (not (bobp)))
              (backward-up-list))
            ;; Move past the 'comment' keyword and opening paren
            (let ((start (progn (forward-char 8) (point)))) ; Move forward over "(comment "
              ;; Move to the end of the comment block
              (forward-sexp)
              ;; Move back one char to avoid selecting the closing paren
              (let ((end (1- (point))))
                ;; Use Evil's visual selection
                (evil-visual-select start end))))
        (error (message "Not inside a (comment ...) block"))))))

(evil-define-key 'normal 'global (kbd "<leader>cv") 'evil-select-comment-block)

;; =========== Custom  Functions ===================

(defun my-open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(defun evil-select-inside-comment-block ()
  "Select everything inside the (comment ...) block under the cursor, excluding the 'comment' keyword itself, in Evil visual mode."
  (interactive)
  (when (and (bound-and-true-p evil-mode)
             (eq evil-state 'normal))
    (save-excursion
      ;; Navigate up the syntax tree to find the (comment ...) block
      (condition-case nil
          (progn
            ;; Move to the beginning of the current sexp
            (while (not (and (looking-at-p "(comment")
                             (eq (char-after (point)) ?\())
                        (not (bobp)))
              (backward-up-list))
            ;; Move past the 'comment' keyword and opening paren
            (let ((start (progn (forward-char 8) (point)))) ; Move forward over "(comment "
              ;; Move to the end of the comment block
              (forward-sexp)
              ;; Move back one char to avoid selecting the closing paren
              (let ((end (1- (point))))
                ;; Use Evil's visual selection
                (evil-visual-select start end))))
        (error (message "Not inside a (comment ...) block"))))))

(evil-define-key 'normal 'global (kbd "<leader>cv") 'evil-select-comment-block)

					;(setq debug-on-error t)
					;(setq lsp-log-io t)
