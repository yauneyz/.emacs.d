;;; 60-completion.el --- company, snippets  -*- lexical-binding: t; -*-

;; ;; ================ CORFU ========================
;; ;; ----- LSP should provide CAPF, not Company
;; (with-eval-after-load 'lsp-mode
;;   (setq lsp-completion-provider :none))   ;; Corfu reads from CAPF


;; ----- Corfu UI
(use-package corfu
  :init
  (global-corfu-mode)                     ;; enable in all buffers
  (corfu-popupinfo-mode)
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

  ;; Keys: RET accepts completion; S-RET inserts newline (ignoring Corfu)
  (define-key corfu-map (kbd "RET") #'corfu-insert)
  (define-key corfu-map (kbd "C-m") #'corfu-insert)

  (defun my/corfu-newline ()
    "Quit Corfu and insert a plain newline."
    (interactive)
    (corfu-quit)
    (reindent-then-newline-and-indent))

  (define-key corfu-map (kbd "S-<return>") #'my/corfu-newline)

  (define-key corfu-map (kbd "TAB") #'corfu-complete))

(use-package nerd-icons-corfu
  :after (corfu nerd-icons)
  :init
  ;; Remove kind-icon if you had added it
  (setq corfu-margin-formatters nil)
  ;; Add the font-glyph formatter (crisp, scales with text)
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


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
  "Context-aware <TAB>: Yasnippet, Tempel (manual), indent."
  (interactive)
  (cond
   ((eq major-mode 'fountain-mode) (fountain-dwim))
   (t
    (or (yas-expand)
        (tempel-expand)             ; try a template at point (manual)
        (indent-for-tab-command)))))

;; (global-set-key (kbd "<tab>") #'dispatch-tab-command)
;; (evil-define-key 'insert 'global (kbd "<tab>") #'dispatch-tab-command)



(provide '60-completion)
;;; 60-completion.el ends here
