;;; 60-completion.el --- company, minuet, snippets  -*- lexical-binding: t; -*-

;; Minuet AI ------------------------------------------------------------------
(use-package minuet
  :bind
  (("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
   ("M-I" . #'minuet-show-suggestion) ;; use overlay for completion
   ("C-c m" . #'minuet-configure-provider)
   :map minuet-active-mode-map
   ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
   ("M-P" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
   ("M-N" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
   ("C-TAB" . #'minuet-accept-suggestion) ;; accept whole completion
   ;; Accept the first line of completion, or N lines with a numeric-prefix:
   ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
   ("M-a" . #'minuet-accept-suggestion-line)
   ("M-e" . #'minuet-dismiss-suggestion))

  :init
  ;; if you want to enable auto suggestion.
  ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
  (add-hook 'minuet-active-mode-hook #'evil-normalize-keymaps)

  :config
  ;; You can use M-x minuet-configure-provider to interactively configure provider and model
  (setq minuet-provider 'openai-compatible)
  (plist-put minuet-openai-compatible-options
	     :end-point "http://localhost:11434/v1/chat/completions")
  (plist-put minuet-openai-compatible-options :api-key "OPENAI_API_KEY")
  ;; Must match the --served-model-name in vLLM
  (plist-put minuet-openai-compatible-options :model "qwen2.5-coder-14b")

  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 128))

;; ================ CORFU ========================
;; ----- LSP should provide CAPF, not Company
(with-eval-after-load 'lsp-mode
  (setq lsp-completion-provider :none))   ;; Corfu reads from CAPF


;; ----- Corfu UI
(use-package corfu
  :init
  (global-corfu-mode)                     ;; enable in all buffers
  :custom
  (corfu-auto t)                          ;; popup automatically
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 1)
  (corfu-preselect-first t)
  (corfu-cycle t)                         ;; TAB cycles candidates
  (corfu-quit-no-match 'separator)        ;; keep UI until you type a space/comma/etc.
  ;; nice in terminals too (M-x corfu-terminal-install if you use tty often)
  :config
  ;; Disable in org-mode to avoid interference
  (add-hook 'org-mode-hook (lambda () (corfu-mode -1)))
  (add-hook 'markdown-mode-hook (lambda () (corfu-mode -1)))
  (define-key corfu-map (kbd "RET") nil)
  (define-key corfu-map (kbd "C-m") nil)
  (define-key corfu-map (kbd "TAB") #'corfu-complete)
  )

;; NOTE - commented because we couldn't find the package
;; ;; nice docs-on-hover in the popup
;; (use-package corfu-popupinfo
;;   :after corfu
;;   :hook (corfu-mode . corfu-popupinfo-mode)
;;   :custom (corfu-popupinfo-delay 0.05))

;; better matching (VSCode-like fuzzy, out-of-order)
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

;; extra completion sources (dabbrev, files, etc.)
(use-package cape
  :init
  ;; Keep LSP at highest priority; add a few handy fallbacks after it.
  (add-to-list 'completion-at-point-functions #'cape-file   'append)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev 'append)
  (add-to-list 'completion-at-point-functions #'cape-keyword 'append))

;; pretty icons for candidates (requires a nerd font)
(use-package kind-icon
  :after corfu
  :custom (kind-icon-use-icons t)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; remember history
(savehist-mode 1)
(with-eval-after-load 'corfu
  (require 'corfu-history)
  (corfu-history-mode 1))

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  ;; Load personal snippets
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))  ;; personal snippets

  ;; Enable Yasnippet globally
  (yas-reload-all)
  (yas-global-mode 1))

(defun dispatch-tab-command ()
  "Context-aware <TAB>: Minuet, Tempel (manual), indent."
  (interactive)
  (cond
   ((eq major-mode 'fountain-mode) (fountain-dwim))
   (t
    (or (minuet-accept-suggestion)  ; your AI inline suggestion accept
	(yas-expand)
        (tempel-expand)             ; try a template at point (manual)
        (indent-for-tab-command)))))

;; (global-set-key (kbd "<tab>") #'dispatch-tab-command)
;; (evil-define-key 'insert 'global (kbd "<tab>") #'dispatch-tab-command)

(provide '60-completion)
;;; 60-completion.el ends here
