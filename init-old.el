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


(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)          ; Disable the menu bar

;; =========== Packages ===================



;; Ivy, Amx, Counsel, Swiper






;; Clojure

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

;; Rust


;; Typescript

;; YAML w/ highlight-indent

;; =========== Config  ===================

;; History saving



;; =========== Keybindings  ===================
;; In elisp-mode, C-c C-c will evaluate the buffer

;; Make ESC quit prompts





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
