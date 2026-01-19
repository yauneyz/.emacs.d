;;; 55-clojure.el --- First-class Clojure tooling -*- lexical-binding: t; -*-

;;; Commentary:
;; Consolidates all Clojure / CIDER configuration so the core stack stays
;; maintainable. Builds on lsp-mode while prioritising CIDER when available.

;;; Code:


(use-package paredit
  :ensure t
  :bind (:map paredit-mode-map
              ("C-)" . paredit-forward-slurp-sexp)
              ("C-(" . paredit-backward-slurp-sexp))
  :config
  (evil-define-key '(normal insert) 'global (kbd "C-)") #'paredit-forward-slurp-sexp)
  (evil-define-key '(normal insert) 'global (kbd "C-(") #'paredit-backward-slurp-sexp))

(use-package clojure-mode
  :ensure t
  :mode (("\.clj\'"  . clojure-mode)
         ("\.cljs\'" . clojurescript-mode)
         ("\.cljc\'" . clojurec-mode))
  :hook (
         (clojure-mode . rainbow-delimiters-mode)
         (clojure-mode . clj-refactor-mode)
         (clojurescript-mode . rainbow-delimiters-mode)
         (clojurescript-mode . clj-refactor-mode)
         (clojurec-mode . rainbow-delimiters-mode)
         (clojurec-mode . clj-refactor-mode))
  :config
  (add-hook 'clojure-mode-hook
	    (lambda ()
	      (when (string-match-p "development\|dev" (or (getenv "NODE_ENV") "development"))
                (setq-local cider-auto-select-error-buffer t
			    cider-show-error-buffer t)))))

(use-package clj-refactor
  :ensure t
  :after clojure-mode
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (setq cljr-inject-dependencies-at-jack-in t))

(use-package flycheck-clj-kondo
  :ensure t
  :after flycheck)

(use-package cider
  :ensure t
  :after clojure-mode
  :custom
  (cider-completion-system 'ivy)
  (cider-eldoc-display-for-symbol-at-point nil)
  (cider-eldoc-display-context-dependent-info nil)
  (cider-eldoc-max-class-names-to-display 0)
  (cider-doc-auto-select-buffer nil)
  (cider-connection-timeout 10)
  (eldoc-idle-delay 1.0)
  (cider-test-show-report-on-success t)
  (cider-auto-select-test-report-buffer t)
  (cider-test-defining-forms '("deftest" "defspec"))
  (cider-repl-history-file "~/.emacs.d/cider-repl-history")
  (cider-repl-history-size 1000)
  (cider-repl-wrap-history t)
  (cider-show-error-buffer t)
  (cider-auto-select-error-buffer t)
  (cider-repl-display-help-banner nil)
  (cider-debug-use-overlays t)
  (cider-debug-prompt 'overlay)
  :config
  (add-hook 'cider-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-connected-hook
	    (lambda ()
	      (when (cider-repls 'cljs)
                (cider-eval-string-up-to-point
                 "(require '[test.dev.repl-utils :as repl] :reload) (repl/init!)"))))
  (add-hook 'cider-stacktrace-mode-hook (lambda () (setq-local truncate-lines nil))))

(evil-define-key 'normal 'global (kbd "C-p") #'paredit-splice-sexp-killing-backward)
(global-set-key (kbd "C-S-p") #'my-spy-and-slurp)
(global-set-key (kbd "C-M-S-p") #'my-spy-with-label)

(defun my-spy-and-slurp ()
  "Insert `(spy …)` and slurp the following sexp into it."
  (interactive)
  (insert "(spy )")
  (backward-char 1)
  (paredit-forward-slurp-sexp))


;; Use clojure-mode for EDN
(add-to-list 'auto-mode-alist '("\.edn\'" . clojure-mode))

(provide 'lang-clojure)
;;; 55-clojure.el ends here
