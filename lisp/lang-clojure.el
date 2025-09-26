;;; 55-clojure.el --- First-class Clojure tooling -*- lexical-binding: t; -*-

;;; Commentary:
;; Consolidates all Clojure / CIDER configuration so the core stack stays
;; maintainable. Builds on lsp-mode while prioritising CIDER when available.

;;; Code:

(declare-function paredit-forward-slurp-sexp "paredit")


(use-package paredit :ensure t)

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode))
  :hook ((clojure-mode . paredit-mode)
         (clojure-mode . rainbow-delimiters-mode)
         (clojure-mode . clj-refactor-mode)
         (clojurescript-mode . paredit-mode)
         (clojurescript-mode . rainbow-delimiters-mode)
         (clojurescript-mode . clj-refactor-mode)
         (clojurec-mode . paredit-mode)
         (clojurec-mode . rainbow-delimiters-mode)
         (clojurec-mode . clj-refactor-mode))
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (when (string-match-p "development\|dev" (or (getenv "NODE_ENV") "development"))
                (setq-local cider-auto-select-error-buffer t
                            cider-show-error-buffer t))
              (evil-define-key 'normal 'local (kbd "gd") #'cider-find-var)
              (evil-define-key 'normal 'local (kbd "gr") #'cider-find-references)
              (evil-define-key 'normal 'local (kbd "gi") #'cider-find-implementations)
              (evil-define-key 'normal 'local (kbd "<leader>rn") #'cider-rename-symbol-at-point)
              (evil-define-key 'normal 'local (kbd "<leader>lg") #'cider-doc)
              (evil-define-key 'normal 'local (kbd "<leader>ld") #'cider-doc)
              (evil-define-key 'normal 'local (kbd "<leader>ls") #'cider-eldoc)
              (evil-define-key 'normal 'local (kbd "<leader>lh") #'cider-describe-thing-at-point)
              (evil-define-key 'normal 'local (kbd "<leader>ts") #'cider-browse-ns))))

(use-package clj-refactor
  :ensure t
  :after clojure-mode
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m"))

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
  (cider-eldoc-ns-function nil)
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

(defun my-spy-with-label ()
  "Prompt for a label, then insert `(spy \"label\" …)` and slurp the next form."
  (interactive)
  (let ((label (read-string "Spy label: " "debug")))
    (insert (format "(spy \"%s\" )" label))
    (backward-char 1)
    (paredit-forward-slurp-sexp)))

(defun cider-test-run-ns-and-focus ()
  "Run tests for the current namespace and focus the report buffer."
  (interactive)
  (cider-test-run-ns-tests)
  (when-let ((buf (get-buffer "*cider-test-report*")))
    (pop-to-buffer buf)))

(defun cider-test-run-all-and-focus ()
  "Run all project tests and focus the report buffer."
  (interactive)
  (cider-test-run-all-tests)
  (when-let ((buf (get-buffer "*cider-test-report*")))
    (pop-to-buffer buf)))

(defun cider-repl-load-test-utils ()
  "Load project test utilities into the REPL session."
  (interactive)
  (cider-eval-string-up-to-point
   "(require '[test.dev.repl-utils :as repl] :reload)"))

(defun cider-repl-run-test-suite ()
  "Run a named `repl/run-test-suite` task from the REPL."
  (interactive)
  (let ((suite (completing-read "Test suite: " '("unit" "integration" "all"))))
    (cider-eval-string-up-to-point (format "(repl/run-test-suite :%s)" suite))))

(defun cider-scope-capture-last ()
  "Show the last scope capture execution point via Portal/Scope-Capture."
  (interactive)
  (cider-eval-string-up-to-point "(sc.api/last-ep)"))

(defun cider-scope-capture-ep-info ()
  "Inspect scope capture execution point details."
  (interactive)
  (cider-eval-string-up-to-point "(sc.api/ep-info)"))

(defun cider-toggle-spec-instrumentation ()
  "Toggle spec instrumentation helpers defined in `app.specs.validation`."
  (interactive)
  (cider-eval-string-up-to-point
   "(if (app.specs.validation/get-instrumented-functions)
      (app.specs.validation/uninstrument-all-functions!)
      (app.specs.validation/instrument-all-functions!))"))

(defun my-cider-connect-electron-renderer ()
  "Connect to the shadow-cljs REPL for the electron renderer."
  (interactive)
  (cider-connect-cljs
   `(:host "localhost"
     :port 8777
     :repl-type shadow
     :shadow-cljs-build ":renderer-dev"
     :project-dir ,default-directory)))

(evil-define-key 'normal 'global (kbd "<leader>c1") #'my-cider-connect-electron-renderer)



(provide 'lang-clojure)
;;; 55-clojure.el ends here
