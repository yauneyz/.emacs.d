;;; 57-go.el --- Go development helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern Go workflow with LSP (gopls), formatting, test hydra, and tree-sitter.

;;; Code:

(setq treesit-extra-load-path (list (expand-file-name "tree-sitter" user-emacs-directory)))

(require 'go-fns)
(require 'hydra)

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :init (setq gofmt-command "gofumpt")
  :hook (go-mode . lsp-deferred)
  :config
  (setq godoc-at-point-function nil
        go-guess-gopath-functions nil)
  (add-hook 'go-mode-hook
            (lambda ()
              (setq-local xref-backend-functions '(lsp--xref-backend)))))

(when (fboundp 'go-ts-mode)
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode)))

(use-package gotest
  :ensure t
  :commands (go-test-current-test go-test-current-file go-test-current-project))

(defhydra +go/hydra (:color blue :hint nil)
  "
^Build/Run^         ^Tests^           ^Debug^           ^Utils^
─────────────────────────────────────────────────────────────────
_b_: build          _t_: at point     _d_: debug        _g_: show compilation
_r_: run            _f_: file         _._: breakpoint   _x_: close compilation
_R_: re-run         _p_: project      _,_: continue     _q_: quit
_B_: build root     _k_: benchmark
_P_: test root      _c_: coverage
"
  ("b" +go/build)
  ("r" +go/run)
  ("R" +go/recompile)
  ("B" +go/build-from-root)
  ("P" +go/test-from-root)
  ("t" +go/test-at-point)
  ("f" +go/test-file)
  ("p" +go/test-project)
  ("k" +go/test-benchmark)
  ("c" +go/coverage-toggle)
  ("d" dap-debug)
  ("." dap-breakpoint-toggle)
  ("," dap-continue)
  ("g" (lambda () (interactive) (pop-to-buffer "*compilation*")) :color red)
  ("x" +go/dismiss-compilation :color red)
  ("q" nil))

(setq compilation-scroll-output 'first-error
      compilation-always-kill t
      compilation-ask-about-save nil)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . bottom) (slot . 0) (window-height . 0.33)))

(provide 'lang-go)
;;; 57-go.el ends here
