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
(evil-define-key 'normal 'global (kbd "<leader>bo") #'switch-to-buffer-other-window)
(evil-define-key 'normal 'global (kbd "<leader>bv") #'my/counsel-switch-buffer-right)
(evil-define-key 'normal 'global (kbd "<leader>bh") #'my/counsel-switch-buffer-below)
(evil-define-key 'normal 'global (kbd "<leader>bk") #'kill-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bf") #'format-all-buffer)
(evil-define-key 'normal 'global (kbd "<leader>gg") #'counsel-projectile-rg)

;; Selection helpers --------------------------------------------------------
(evil-define-key 'normal 'global (kbd "<leader>cv") #'evil-select-inside-comment-block)

;; Project / Git / Treemacs ----------------------------------------------------
(evil-define-key 'normal 'global (kbd "gs") #'magit-status)
(evil-define-key 'normal 'global (kbd "<leader>tt") #'treemacs)
(evil-define-key 'normal 'global (kbd "<leader>tf") #'treemacs-find-file)
(evil-define-key 'normal 'global (kbd "<leader>ts") #'lsp-treemacs-symbols)

(evil-define-key 'normal 'global (kbd "<leader>p") 'projectile-command-map)

;; Navigate between hunks (diff-hl and Magit both respect these)
(global-set-key (kbd "M-]") #'diff-hl-next-hunk)
(global-set-key (kbd "M-[") #'diff-hl-previous-hunk)

;; smerge conflict resolution --------------------------------------------------
(with-eval-after-load 'smerge-mode
  (evil-define-key 'normal smerge-mode-map (kbd "<leader>mn") #'smerge-next)
  (evil-define-key 'normal smerge-mode-map (kbd "<leader>mp") #'smerge-prev)
  (evil-define-key 'normal smerge-mode-map (kbd "<leader>mu") #'smerge-keep-upper)
  (evil-define-key 'normal smerge-mode-map (kbd "<leader>ml") #'smerge-keep-lower)
  (evil-define-key 'normal smerge-mode-map (kbd "<leader>ma") #'smerge-keep-all)
  (evil-define-key 'normal smerge-mode-map (kbd "<leader>mb") #'smerge-keep-base)
  (evil-define-key 'normal smerge-mode-map (kbd "<leader>mc") #'smerge-combine-with-next)
  (evil-define-key 'normal smerge-mode-map (kbd "<leader>mr") #'smerge-resolve))

;; Clojure / REPL --------------------------------------------------------------
;; Basic REPL operations
(evil-define-key 'normal 'global (kbd "<leader>cj") #'cider-connect-cljs)
(evil-define-key 'normal 'global (kbd "<leader>cl") #'cider-eval-last-sexp)
(evil-define-key 'normal 'global (kbd "<leader>cb") #'cider-eval-buffer)
(evil-define-key 'normal 'global (kbd "<leader>cf") #'cider-format-buffer)
(evil-define-key 'normal 'global (kbd "<leader>cr") #'cider-restart)
(evil-define-key 'normal 'global (kbd "<leader>nr") #'cider-switch-to-repl-buffer)
(evil-define-key 'normal 'global (kbd "<leader>nc") #'cider-switch-to-last-clojure-buffer)

;; Enhanced testing keybindings (using 'c' prefix to avoid conflicts)
(evil-define-key 'normal 'global (kbd "<leader>ct") #'cider-test-run-test)
(evil-define-key 'normal 'global (kbd "<leader>cn") #'cider-test-run-ns-and-focus)
(evil-define-key 'normal 'global (kbd "<leader>ca") #'cider-test-run-all-and-focus)
(evil-define-key 'normal 'global (kbd "<leader>cx") #'cider-test-rerun-tests)
(evil-define-key 'normal 'global (kbd "<leader>csr") #'cider-test-show-report)

;; REPL utilities and test integration (using 'cu' prefix)
(evil-define-key 'normal 'global (kbd "<leader>ce") #'cider-repl-run-test-suite)

;; Scope-capture debugging (using 'cp' prefix for "capture")
(evil-define-key 'normal 'global (kbd "<leader>cpl") #'cider-scope-capture-last)
(evil-define-key 'normal 'global (kbd "<leader>cpi") #'cider-scope-capture-ep-info)

;; Spec validation (using 'cs' prefix for "spec" - note cs is already used, using csv for "spec validation")
(evil-define-key 'normal 'global (kbd "<leader>csv") #'cider-toggle-spec-instrumentation)

;; Enhanced debugging (using 'cd' prefix for "debug")
(evil-define-key 'normal 'global (kbd "<leader>cdb") #'cider-debug-defun-at-point)
(evil-define-key 'normal 'global (kbd "<leader>cdc") #'cider-debug-continue)
(evil-define-key 'normal 'global (kbd "<leader>cdn") #'cider-debug-step-next)
(evil-define-key 'normal 'global (kbd "<leader>cdi") #'cider-debug-step-into)
(evil-define-key 'normal 'global (kbd "<leader>cdo") #'cider-debug-step-out)

;; LSP helpers -----------------------------------------------------------------
;; Prefer this: one key that works everywhere
(evil-define-key 'normal 'global (kbd "gd") #'xref-find-definitions)
(evil-define-key 'normal 'global (kbd "gr") #'xref-find-references)

(evil-define-key 'normal 'global (kbd "gi") #'lsp-find-implementation)
(evil-define-key 'normal 'global (kbd "<leader>rn") #'lsp-rename)
(evil-define-key 'normal 'global (kbd "<leader>lg") #'lsp-ui-doc-glance)
(evil-define-key 'normal 'global (kbd "<leader>lh") #'lsp-treemacs-call-hierarchy)
(evil-define-key 'normal 'global (kbd "<leader>ls") #'lsp-workspace-symbol)
(evil-define-key 'normal 'global (kbd "<leader>lf") #'lsp-format-buffer)
(evil-define-key 'normal 'global (kbd "<leader>lm") #'lsp-ui-imenu)


;; ELISP bindings --------------------------------------------------

;; Go development keybindings --------------------------------------------------
;; Note: <leader>gg = counsel-projectile-rg (grep), so Go menu is <leader>gm
(defun +go/set-keys ()
  "Set Go-specific keybindings for Go buffers."
  (let ((m (current-local-map)))
    (define-key m (kbd "<leader>gm") #'+go/hydra/body)
    (define-key m (kbd "<leader>gb") #'+go/build)
    (define-key m (kbd "<leader>gB") #'+go/build-from-root)
    (define-key m (kbd "<leader>gr") #'+go/run)
    (define-key m (kbd "<leader>gp") #'+go/test-project)
    (define-key m (kbd "<leader>gP") #'+go/test-from-root)
    (define-key m (kbd "<leader>gf") #'+go/test-file)
    (define-key m (kbd "<leader>gt") #'+go/test-at-point)
    (define-key m (kbd "<leader>gc") #'+go/coverage-toggle)
    (define-key m (kbd "<leader>g.") #'dap-breakpoint-toggle)
    (define-key m (kbd "<leader>gd") #'dap-debug)
    (define-key m (kbd "<f5>")    #'+go/recompile)))

;; Apply Go keybindings to both go-mode and go-ts-mode
(add-hook 'go-mode-hook     #'+go/set-keys)
(add-hook 'go-ts-mode-hook  #'+go/set-keys)

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

;; Outline-mode keybindings (vim-style z-leader) ------------------------------
(evil-define-key 'normal 'global (kbd "zo") #'outline-show-children)      ; expand one level
(evil-define-key 'normal 'global (kbd "zO") #'outline-show-subtree)       ; recursively expand all children
(evil-define-key 'normal 'global (kbd "zc") #'outline-hide-leaves)        ; collapse node one level
(evil-define-key 'normal 'global (kbd "zC") #'outline-hide-subtree)       ; collapse all children
(evil-define-key 'normal 'global (kbd "zR") #'outline-show-all)           ; expand all
(evil-define-key 'normal 'global (kbd "zM") #'outline-hide-body)          ; collapse all (show only headings)
(evil-define-key 'normal 'global (kbd "za") #'outline-toggle-children)    ; smart toggle

;; Outline navigation ----------------------------------------------------------
(evil-define-key 'normal 'global (kbd "zu") #'outline-up-heading)                ; up a level
(evil-define-key 'normal 'global (kbd "zj") #'outline-forward-same-level)        ; next at same level
(evil-define-key 'normal 'global (kbd "zk") #'outline-backward-same-level)       ; previous at same level
(evil-define-key 'normal 'global (kbd "zn") #'outline-next-visible-heading)      ; next heading (any level)
(evil-define-key 'normal 'global (kbd "zp") #'outline-previous-visible-heading)  ; previous heading (any level)

;; Shell buffer toggles -------------------------------------------------------
;; Quick access to last used shell buffer
(evil-define-key 'normal 'global (kbd "<leader><leader>t") #'toggle-last-shell-buffer)

;; Numbered shell buffers (with memory)
(evil-define-key 'normal 'global (kbd "<leader>t0") (lambda () (interactive) (toggle-shell-buffer-and-remember 0)))
(evil-define-key 'normal 'global (kbd "<leader>t1") (lambda () (interactive) (toggle-shell-buffer-and-remember 1)))
(evil-define-key 'normal 'global (kbd "<leader>t2") (lambda () (interactive) (toggle-shell-buffer-and-remember 2)))
(evil-define-key 'normal 'global (kbd "<leader>t3") (lambda () (interactive) (toggle-shell-buffer-and-remember 3)))
(evil-define-key 'normal 'global (kbd "<leader>t4") (lambda () (interactive) (toggle-shell-buffer-and-remember 4)))
(evil-define-key 'normal 'global (kbd "<leader>t5") (lambda () (interactive) (toggle-shell-buffer-and-remember 5)))
(evil-define-key 'normal 'global (kbd "<leader>t6") (lambda () (interactive) (toggle-shell-buffer-and-remember 6)))
(evil-define-key 'normal 'global (kbd "<leader>t7") (lambda () (interactive) (toggle-shell-buffer-and-remember 7)))
(evil-define-key 'normal 'global (kbd "<leader>t8") (lambda () (interactive) (toggle-shell-buffer-and-remember 8)))
(evil-define-key 'normal 'global (kbd "<leader>t9") (lambda () (interactive) (toggle-shell-buffer-and-remember 9)))

;; Shell buffer toggles (other window) ----------------------------------------
(evil-define-key 'normal 'global (kbd "<leader>to0") (lambda () (interactive) (toggle-shell-buffer-and-remember 0 t)))
(evil-define-key 'normal 'global (kbd "<leader>to1") (lambda () (interactive) (toggle-shell-buffer-and-remember 1 t)))
(evil-define-key 'normal 'global (kbd "<leader>to2") (lambda () (interactive) (toggle-shell-buffer-and-remember 2 t)))
(evil-define-key 'normal 'global (kbd "<leader>to3") (lambda () (interactive) (toggle-shell-buffer-and-remember 3 t)))
(evil-define-key 'normal 'global (kbd "<leader>to4") (lambda () (interactive) (toggle-shell-buffer-and-remember 4 t)))
(evil-define-key 'normal 'global (kbd "<leader>to5") (lambda () (interactive) (toggle-shell-buffer-and-remember 5 t)))
(evil-define-key 'normal 'global (kbd "<leader>to6") (lambda () (interactive) (toggle-shell-buffer-and-remember 6 t)))
(evil-define-key 'normal 'global (kbd "<leader>to7") (lambda () (interactive) (toggle-shell-buffer-and-remember 7 t)))
(evil-define-key 'normal 'global (kbd "<leader>to8") (lambda () (interactive) (toggle-shell-buffer-and-remember 8 t)))
(evil-define-key 'normal 'global (kbd "<leader>to9") (lambda () (interactive) (toggle-shell-buffer-and-remember 9 t)))

;; Layout management ----------------------------------------------------------
(evil-define-key 'normal 'global (kbd "<leader>ls") #'layout-save)
(evil-define-key 'normal 'global (kbd "<leader>ll") #'layout-load)

;; Global compilation buffer dismiss

(evil-define-key 'normal 'global (kbd "<leader>x") #'dismiss-popup-buffer)

;; Repeat for treemacs
(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map (kbd "C-h") 'windmove-left)
  (define-key treemacs-mode-map (kbd "C-j") 'windmove-down)
  (define-key treemacs-mode-map (kbd "C-k") 'windmove-up)
  (define-key treemacs-mode-map (kbd "C-l") 'windmove-right))

(global-set-key (kbd "C-M-h") 'help-command)
(global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)
(global-set-key (kbd "C-c o") 'toggle-command-log)
(global-set-key (kbd "C-x r r") 'reload-init)

;; Org-only color hydra binding in Evil visual state -------------------------
;; Defined here for consistency with other leader bindings. Ensures both
;; Evil and Org are loaded before binding, and only affects Org buffers.
(with-eval-after-load 'evil
  (add-hook 'org-mode-hook
            (lambda ()
              (evil-define-key 'visual org-mode-map
                (kbd "<leader>c") #'my/hydra-org-color/body))))


;; YAS Completion
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "<leader>yr") #'yas-reload-snippets)
  (evil-define-key 'insert 'global (kbd "C-;") #'yas-expand))
(with-eval-after-load 'yasnippet
  ;; Navigate snippet fields only while a snippet is active
  (define-key yas-keymap (kbd "C-j") #'yas-next-field)
  (define-key yas-keymap (kbd "C-k") #'yas-prev-field))


;; Unbind things I don't need
(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "C-k") nil))




(provide '70-keybindings)
;;; 70-keybindings.el ends here
