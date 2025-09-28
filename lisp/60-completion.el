;;; 60-completion.el --- company, minuet, snippets  -*- lexical-binding: t; -*-

;; Minuet AI ------------------------------------------------------------------
(use-package minuet
  :config
  (setq minuet-provider 'openai-compatible)
  (setq minuet-provider-options
        '((openai-compatible
           :end-point "http://127.0.0.1:11434/v1/chat/completions"
           :api-key "OPENAI_API_KEY"))))

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
  (add-hook 'org-mode-hook (lambda () (corfu-mode -1))))

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

;; YASnippet ------------------------------------------------------------------
(use-package yasnippet
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "C-j")   #'yas-next-field-or-maybe-expand)
  (define-key yas-minor-mode-map (kbd "C-S-j") #'yas-prev-field))

(defun my/yasnippet-complete ()
  "Complete using YASnippet via completion-at-point."
  (interactive)
  (yas-expand-from-trigger-key))
(evil-define-key 'insert 'global (kbd "C-y") #'my/yasnippet-complete)

;; Dispatcher TAB --------------------------------------------------------------
(defun dispatch-tab-command ()
  "Context-aware <TAB>: Minuet, YAS, indent."
  (interactive)
  (cond
   ((eq major-mode 'fountain-mode) (fountain-dwim))
   (t
    (or (minuet-accept-suggestion)
        (indent-for-tab-command)))))
(global-set-key (kbd "<tab>") #'dispatch-tab-command)
(evil-define-key 'insert 'global (kbd "<tab>") #'dispatch-tab-command)

(provide '60-completion)
;;; 60-completion.el ends here
