;;; lang-python.el --- Python language tooling -*- lexical-binding: t; -*-

;;; Commentary:
;; Share a project interpreter across Pyright, Flycheck, REPL, pytest and DAP.
;; Ruff supplies linting/code actions; Apheleia owns formatting on save.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)

;; --- Project environments ---

(defun +python/find-venv ()
  "Find the nearest usable .venv/venv, then an activated environment."
  (unless (file-remote-p default-directory)
    (let ((root (locate-dominating-file
                 default-directory
                 (lambda (dir)
                   (cl-some (lambda (name)
                              (file-executable-p
                               (expand-file-name (concat name "/bin/python") dir)))
                            '(".venv" "venv"))))))
      (cl-find-if
       (lambda (dir)
         (and dir (file-executable-p (expand-file-name "bin/python" dir))))
       (append (when root
                 (mapcar (lambda (name) (expand-file-name name root))
                         '(".venv" "venv")))
               (list (getenv "VIRTUAL_ENV")))))))

(defun +python/prepend-path (variable directories)
  "Prepend existing DIRECTORIES to path VARIABLE in this buffer."
  (let ((paths (delete-dups
                (append (cl-remove-if-not #'file-directory-p directories)
                        (split-string (or (getenv variable) "") path-separator t)))))
    (when paths
      (setenv variable (mapconcat #'identity paths path-separator)))))

(defun +python/setup-environment ()
  "Configure this buffer's Python tools without activating a global venv."
  (unless (file-remote-p default-directory)
    (setq-local process-environment (copy-sequence process-environment)
                exec-path (copy-sequence exec-path))
    (let* ((venv (+python/find-venv))
           (project (project-current nil))
           (root (or (and project (project-root project))
                     (and venv (file-name-directory venv))))
           (python (if venv (expand-file-name "bin/python" venv)
                     (or (executable-find "python3") "python3"))))
      (when venv
        (setenv "VIRTUAL_ENV" venv)
        (+python/prepend-path "PATH" (list (expand-file-name "bin" venv)))
        (setq-local exec-path (cons (expand-file-name "bin" venv)
                                    (delete (expand-file-name "bin" venv) exec-path)))
        ;; Wheel extensions need dlopen paths on NixOS, including libcuda.
        (when (getenv "NIX_LD_LIBRARY_PATH")
          (+python/prepend-path
           "LD_LIBRARY_PATH"
           (split-string (getenv "NIX_LD_LIBRARY_PATH") path-separator t)))
        (+python/prepend-path "LD_LIBRARY_PATH" '("/run/opengl-driver/lib"))
        ;; Interactive experiments should not reserve most of the GPU's RAM.
        (unless (getenv "XLA_PYTHON_CLIENT_PREALLOCATE")
          (setenv "XLA_PYTHON_CLIENT_PREALLOCATE" "false")))
      (when root
        (+python/prepend-path "PYTHONPATH" (list (expand-file-name "src" root)))
        (setq-local python-shell-buffer-name
                    (concat "Python:" (file-name-nondirectory
                                       (directory-file-name root)))))
      (setq-local python-shell-interpreter python
                  python-shell-virtualenv-root venv
                  lsp-pyright-python-executable-cmd python
                  lsp-pyright-python-search-functions
                  (list (lambda () python-shell-interpreter))
                  flycheck-python-mypy-python-executable python
                  flycheck-python-pycompile-executable python
                  dap-python-executable python
                  python-pytest-executable
                  (concat (shell-quote-argument python) " -m pytest")))))

(defun +python/setup ()
  "Prepare the Python environment before starting language servers."
  (setq-local fill-column 88 python-indent-offset 4)
  (+python/setup-environment)
  (require 'lsp-pyright)
  (require 'lsp-ruff)
  ;; Avoid competing pylsp/mypy/Black plugins in the system environment.
  (setq-local lsp-enabled-clients
              (if (file-remote-p default-directory)
                  '(pyright-remote pyright-tramp ruff-tramp)
                '(pyright ruff)))
  (lsp-deferred))

(use-package python
  :ensure nil
  :init
  (setq python-shell-interpreter "python3"
        python-indent-guess-indent-offset-verbose nil)
  :hook ((python-mode python-ts-mode) . +python/setup))

(use-package lsp-pyright
  :after lsp-mode
  :custom
  (lsp-pyright-type-checking-mode "strict")
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-disable-organize-imports t)
  (lsp-pyright-use-library-code-for-types t))

(use-package python-pytest
  :after python
  :commands (python-pytest-dispatch python-pytest-file python-pytest-function
                                    python-pytest-repeat)
  :hook (python-pytest-setup . +python/setup-environment)
  :config
  (setq python-pytest-arguments '("-x")))

(use-package direnv
  :config (direnv-mode 1))

;; --- Environment diagnostics ---

(defun +python/check-environment ()
  "Report imports, devices and a compiled JAX gradient using this buffer's Python."
  (interactive)
  (+python/setup-environment)
  (let ((buffer (get-buffer-create "*Python environment*")))
    (when (get-buffer-process buffer)
      (user-error "An environment check is already running"))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)) (erase-buffer)))
    (make-process
     :name "Python environment" :buffer buffer :noquery t
     :command
     (list python-shell-interpreter "-c"
           (concat "import sys, importlib.metadata as m\n"
                   "print('Python:', sys.executable, flush=True)\n"
                   "for name in ['jax', 'jaxlib', 'flax', 'optax', 'numpy', 'pytest', 'debugpy', 'ipython']:\n"
                   " try: print(name + ':', m.version(name), flush=True)\n"
                   " except m.PackageNotFoundError: print(name + ': MISSING', flush=True)\n"
                   "import jax, jax.numpy as jnp, flax.linen, optax\n"
                   "print('Devices:', jax.devices(), flush=True)\n"
                   "print('JIT gradient:', jax.jit(jax.grad(lambda x: jnp.sum(x*x)))(jnp.arange(4.)).block_until_ready(), flush=True)\n")))
    (display-buffer buffer)))

(provide 'lang-python)
;;; lang-python.el ends here
