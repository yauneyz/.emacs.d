;;; 70-keybindings.el --- SPC leader shortcuts, global binds -*- lexical-binding: t; -*-

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'eval-buffer)))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; Keybindings for sexp movement using literal parentheses
(evil-define-key '(normal insert) 'global (kbd "C-c (") 'backward-sexp)
(evil-define-key '(normal insert) 'global (kbd "C-c )") 'forward-sexp)

;; Buffer & file helpers ------------------------------------------------------
(evil-define-key 'normal 'global (kbd "<leader>bs") #'counsel-switch-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bk") #'kill-buffer)
(evil-define-key 'normal 'global (kbd "<leader>gg") #'counsel-projectile-rg)

;; Selection helpers --------------------------------------------------------
(evil-define-key 'normal 'global (kbd "<leader>cv") #'evil-select-inside-comment-block)

;; Project / Git / Treemacs ----------------------------------------------------
(evil-define-key 'normal 'global (kbd "<leader>gs") #'magit-status)
(evil-define-key 'normal 'global (kbd "<leader>tt") #'treemacs)
(evil-define-key 'normal 'global (kbd "<leader>tf") #'treemacs-find-file)
(evil-define-key 'normal 'global (kbd "<leader>ts") #'lsp-treemacs-symbols)


(with-eval-after-load 'evil
(evil-define-key 'normal 'global (kbd "<leader>p") 'projectile-command-map))


;; Clojure / REPL --------------------------------------------------------------
(evil-define-key 'normal 'global (kbd "<leader>cj") #'cider-connect-cljs)
(evil-define-key 'normal 'global (kbd "<leader>cl") #'cider-eval-last-sexp)
(evil-define-key 'normal 'global (kbd "<leader>cb") #'cider-eval-buffer)
(evil-define-key 'normal 'global (kbd "<leader>cf") #'cider-format-buffer)
(evil-define-key 'normal 'global (kbd "<leader>cr") #'cider-restart)
(evil-define-key 'normal 'global (kbd "<leader>nr") #'cider-switch-to-repl-buffer)
(evil-define-key 'normal 'global (kbd "<leader>nc") #'cider-switch-to-last-clojure-buffer)

;; LSP helpers -----------------------------------------------------------------
(evil-define-key 'normal 'global (kbd "gd") #'lsp-find-definition)
(evil-define-key 'normal 'global (kbd "gr") #'lsp-find-references)
(evil-define-key 'normal 'global (kbd "gi") #'lsp-find-implementation)
(evil-define-key 'normal 'global (kbd "<leader>rn") #'lsp-rename)
(evil-define-key 'normal 'global (kbd "<leader>lg") #'lsp-ui-doc-glance)
(evil-define-key 'normal 'global (kbd "<leader>ld") #'lsp-ui-doc-show)
(evil-define-key 'normal 'global (kbd "<leader>ls") #'lsp-signature-help)
(evil-define-key 'normal 'global (kbd "<leader>lh") #'lsp-describe-thing-at-point)

;; Helper describe-* -----------------------------------------------------------
(evil-define-key 'normal 'global (kbd "<leader>hf") #'counsel-describe-function)
(evil-define-key 'normal 'global (kbd "<leader>hv") #'counsel-describe-variable)
(evil-define-key 'normal 'global (kbd "<leader>hk") #'counsel-describe-key)

;; Window / screen management --------------------------------------------------
(use-package elscreen :config (elscreen-start))
(evil-define-key 'normal 'global (kbd "<leader>sc") #'elscreen-create)
(evil-define-key 'normal 'global (kbd "<leader>sk") #'elscreen-kill)
(evil-define-key 'normal 'global (kbd "<leader>sg") #'elscreen-goto)
(evil-define-key 'normal 'global (kbd "<leader>sp") #'elscreen-previous)
(evil-define-key 'normal 'global (kbd "<leader>sn") #'elscreen-next)
(evil-define-key 'normal 'global (kbd "<leader>sb") #'elscreen-find-and-goto-by-buffer)
(evil-define-key 'normal 'global (kbd "<leader>so") #'elscreen-toggle)

(evil-define-key 'normal 'global (kbd "M-h") #'elscreen-previous)
(evil-define-key 'normal 'global (kbd "M-l") #'elscreen-next)

;; Folds / misc ----------------------------------------------------------------
(evil-define-key 'normal 'global (kbd "<f3>") #'evil-toggle-fold)

;; Shell buffer toggles -------------------------------------------------------
(evil-define-key 'normal 'global (kbd "<leader>t0") (lambda () (interactive) (toggle-shell-buffer 0)))
(evil-define-key 'normal 'global (kbd "<leader>t1") (lambda () (interactive) (toggle-shell-buffer 1)))
(evil-define-key 'normal 'global (kbd "<leader>t2") (lambda () (interactive) (toggle-shell-buffer 2)))
(evil-define-key 'normal 'global (kbd "<leader>t3") (lambda () (interactive) (toggle-shell-buffer 3)))
(evil-define-key 'normal 'global (kbd "<leader>t4") (lambda () (interactive) (toggle-shell-buffer 4)))
(evil-define-key 'normal 'global (kbd "<leader>t5") (lambda () (interactive) (toggle-shell-buffer 5)))
(evil-define-key 'normal 'global (kbd "<leader>t6") (lambda () (interactive) (toggle-shell-buffer 6)))
(evil-define-key 'normal 'global (kbd "<leader>t7") (lambda () (interactive) (toggle-shell-buffer 7)))
(evil-define-key 'normal 'global (kbd "<leader>t8") (lambda () (interactive) (toggle-shell-buffer 8)))
(evil-define-key 'normal 'global (kbd "<leader>t9") (lambda () (interactive) (toggle-shell-buffer 9)))

;; Layout management ----------------------------------------------------------
(evil-define-key 'normal 'global (kbd "<leader>ls") #'layout-save)
(evil-define-key 'normal 'global (kbd "<leader>ll") #'layout-load)

;; Repeat for treemacs
(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map (kbd "C-h") 'windmove-left)
  (define-key treemacs-mode-map (kbd "C-j") 'windmove-down)
  (define-key treemacs-mode-map (kbd "C-k") 'windmove-up)
  (define-key treemacs-mode-map (kbd "C-l") 'windmove-right))

(global-set-key (kbd "C-M-h") 'help-command)
(global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)

(provide '70-keybindings)
;;; 70-keybindings.el ends here
