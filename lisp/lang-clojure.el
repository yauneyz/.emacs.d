;;; 55-clojure.el --- First-class Clojure tooling -*- lexical-binding: t; -*-

;;; Commentary:
;; Consolidates all Clojure / CIDER configuration so the core stack stays
;; maintainable. Builds on lsp-mode while prioritising CIDER when available.

;;; Code:

(message "Loading lang-clojure.el...")
(condition-case err
    (progn
      (message "Starting Clojure configuration...")

(declare-function paredit-forward-slurp-sexp "paredit")


(use-package paredit :ensure t)

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode))
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
                            cider-show-error-buffer t))
              ;; (evil-define-key 'normal 'local (kbd "gd") #'cider-find-var)
              ;; (evil-define-key 'normal 'local (kbd "gr") #'cider-find-references)
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



      (message "Clojure configuration completed successfully"))
  (error
   (message "Error in lang-clojure.el: %s" err)
   (message "Error details: %S" err)))

(provide 'lang-clojure)

(use-package format-all
  :config
  (setq-default format-all-formatters
                '((clojure-mode . "cljfmt")
                  (clojurescript-mode . "cljfmt")
                  (clojurec-mode . "cljfmt")
                  (python-mode . "black")
                  (typescript-mode . "prettier")
                  (emacs-lisp-mode . emacs-lisp-format))))


;; FlowStorm & CiderStorm
(use-package cider-storm
  :vc (:url "https://github.com/flow-storm/cider-storm" :rev :newest))

(with-eval-after-load 'cider-storm
  ;; Call the command bound to a key in `cider-storm-map` (so we don't hardcode names)
  (defun +clj/cider-storm-dispatch (key)
    "Invoke the CiderStorm command bound to KEY (a string like \"s\", \"d\")."
    (let* ((cmd (and (boundp 'cider-storm-map)
                     (lookup-key cider-storm-map (kbd key)))))
      (if (commandp cmd)
          (call-interactively cmd)
        (user-error "No CiderStorm command bound to %s" key))))

  (defhydra +clj/storm-hydra (:color blue :hint nil)
    "
^Recording^           ^Debugging^             ^Jump/Inspect^        ^UI/Other^
────────────────────────────────────────────────────────────────────────────────
_t_: toggle rec       _d_: debug current fn   _j_: jump to fn       _s_: start UI
_l_: clear recs                            ^  _o_: open/timeline    _x_: stop UI
_h_: help                                   ^                      _q_: quit   _D_: DAP hydra
"
    ("t" (+clj/cider-storm-dispatch "t"))
    ("l" (+clj/cider-storm-dispatch "l"))
    ("h" (+clj/cider-storm-dispatch "h"))
    ("d" (+clj/cider-storm-dispatch "d"))
    ("j" (+clj/cider-storm-dispatch "j"))
    ("o" (+clj/cider-storm-dispatch "o"))
    ("s" (+clj/cider-storm-dispatch "s"))
    ("x" (+clj/cider-storm-dispatch "x"))
    ("D" dap-hydra :color red)
    ("q" nil :color blue))

  ;; Bind <leader>d only in Clojure-family modes (leaves global DAP bindings intact elsewhere)
  (dolist (hook '(clojure-mode-hook clojurescript-mode-hook clojurec-mode-hook))
    (add-hook hook
              (lambda ()
                (evil-define-key 'normal 'local
                  (kbd "<leader>d") #'+clj/storm-hydra/body)))))


;;; 55-clojure.el ends here
