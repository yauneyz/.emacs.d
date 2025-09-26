;;; lang-python.el --- Python language tooling -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern Python workflow leveraging lsp-mode (pyright + ruff), auto-formatting,
;; environment detection, and pytest helpers.

;;; Code:

(use-package python
  :ensure nil
  :mode "\\.py\\'"
  :init
  (setq python-shell-interpreter "python3"
        python-indent-guess-indent-offset-verbose nil)
  :hook (python-mode . (lambda ()
                         (setq-local fill-column 88
                                     python-indent-offset 4))))

;; Prefer lsp-mode integration; `eglot-ensure` fallback above handles no LSP.
(use-package lsp-pyright
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  :custom
  (lsp-pyright-typechecking-mode "strict")
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-use-library-code-for-types t))


(defun +python/ensure-ruff-lsp ()
  "Activate ruff-lsp alongside the main Python language server when available."
  (when (and (featurep 'lsp-mode)
             (executable-find "ruff-lsp"))
    ;; Trigger a manual workspace refresh so the add-on server attaches.
    (lsp-deferred)))

(add-hook 'python-mode-hook #'+python/ensure-ruff-lsp)

(use-package python-black
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim)
  :custom (python-black-extra-args '("--line-length" "88")))

(use-package py-isort
  :after python
  :hook (python-mode . (lambda ()
                         (add-hook 'before-save-hook #'py-isort-before-save nil t))))

(use-package pyvenv
  :after python
  :commands (pyvenv-activate pyvenv-deactivate)
  :config
  (pyvenv-mode 1))

(use-package pipenv
  :after python
  :if (executable-find "pipenv")
  :hook (python-mode . pipenv-mode))

(use-package poetry
  :ensure t
  :if (executable-find "poetry")
  :hook (python-mode . poetry-tracking-mode))

(use-package python-pytest
  :after python
  :commands (python-pytest-dispatch)
  :config
  (setq python-pytest-arguments '("-x")))

(use-package direnv
  :config (direnv-mode 1))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ruff-lsp"))
    :activation-fn (lsp-activate-on "python")
    :add-on? t
    :server-id 'ruff-lsp)))

(provide 'lang-python)
;;; 56-python.el ends here
