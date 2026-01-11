;;; --- Diffing stack: Magit + Delta + vdiff + diff-hl + smerge ---

;; Magit: unified, scrollable diffs + everything Git
(use-package magit
  :commands (magit-status magit-dispatch)
  :custom
  ((magit-display-buffer-function
    #'magit-display-buffer-fullframe-status-v1) ; take the full frame
   (magit-save-repository-buffers 'dontask)
   ;; make small changes pop
   (magit-diff-refine-hunk 'all)
   ;; more context around hunks
   (magit-diff-context-lines 5)
   ;; calmer by default
   (magit-diff-arguments '("--ignore-all-space")))
  :config
  ;; Auto-populate magit repository directories from project.el known projects
  (setq magit-repository-directories
        (mapcar (lambda (p) (cons p 1))
                (project-known-project-roots)))

  ;; --- helpers: always open a single, full-screen diff buffer ---
  (defun my/--select-window-showing-mode (mode)
    "Select the first window whose buffer major MODE matches."
    (catch 'found
      (dolist (w (window-list))
        (with-current-buffer (window-buffer w)
          (when (derived-mode-p mode)
            (select-window w) (throw 'found w))))
      nil))

  (defun my/magit-diff-all ()
    "Fullscreen view of ALL changes: HEAD..WORKTREE (staged + unstaged)."
    (interactive)
    (magit-diff-working-tree)                             ; HEAD..WORKTREE
    (my/--select-window-showing-mode 'magit-diff-mode)
    (magit-section-show-level-2-all)
    (delete-other-windows))

  (defun my/magit-diff-unstaged ()
    "Fullscreen view of UNSTAGED changes: INDEX..WORKTREE."
    (interactive)
    (magit-diff-unstaged)
    (my/--select-window-showing-mode 'magit-diff-mode)
    (magit-section-show-level-2-all)
    (delete-other-windows))

  (defun my/magit-diff-staged ()
    "Fullscreen view of STAGED changes: HEAD..INDEX."
    (interactive)
    (magit-diff-staged)
    (my/--select-window-showing-mode 'magit-diff-mode)
    (magit-section-show-level-2-all)
    (delete-other-windows))

  ;; Add entries to Magit’s dispatch popup for discoverability
  (with-eval-after-load 'transient
    (transient-append-suffix 'magit-dispatch "d"
      '("A" "Diff all (HEAD..WORKTREE)" my/magit-diff-all))
    (transient-append-suffix 'magit-dispatch "d"
      '("U" "Diff unstaged (INDEX..WORKTREE)" my/magit-diff-unstaged))
    (transient-append-suffix 'magit-dispatch "d"
      '("S" "Diff staged (HEAD..INDEX)"       my/magit-diff-staged))))

;; Pretty diffs inside Magit via git-delta (auto-enable if available)
(use-package magit-delta
  :if (executable-find "delta")
  :after magit
  :hook (magit-mode . magit-delta-mode))

;; Live gutter hints + Magit integration
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :config (global-diff-hl-mode 1))

;; vdiff: side-by-side diffs; keep around for focused reviews
(use-package vdiff
  :init
  (setq vdiff-diff-algorithm 'git)
  :config
  (setq vdiff-lock-scrolling t
        vdiff-disable-folding nil
        vdiff-fold-padding 6
        vdiff-min-fold-size 4
        vdiff-may-close-fold-on-point t
        vdiff-fold-string-function 'vdiff-fold-string-default
        vdiff-default-refinement-syntax-code "w"
        vdiff-auto-refine nil
        vdiff-subtraction-style 'full
        vdiff-subtraction-fill-char ?-))

(use-package vdiff-magit
  :after (vdiff magit)
  :commands (vdiff-magit-dwim vdiff-magit)
  :init
  (setq vdiff-magit-use-ediff-for-merges nil
        vdiff-magit-dwim-show-on-hunks  t
        vdiff-magit-show-stash-with-index t
        vdiff-magit-stage-is-2way nil)
  :config
  (define-key magit-mode-map (kbd "e") #'vdiff-magit-dwim)
  (define-key magit-mode-map (kbd "E") #'vdiff-magit)
  (with-eval-after-load 'transient
    (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
    (transient-suffix-put 'magit-dispatch "e" :command     #'vdiff-magit-dwim)
    (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
    (transient-suffix-put 'magit-dispatch "E" :command     #'vdiff-magit))
  (with-eval-after-load 'vdiff-magit
    (with-eval-after-load 'magit
      (define-key magit-mode-map (kbd "e") #'vdiff-magit-dwim)
      (define-key magit-mode-map (kbd "E") #'vdiff-magit)))

  ;; If evil-collection is installed, ensure we win after it:
  (with-eval-after-load 'evil-collection-magit
    (define-key magit-mode-map (kbd "e") #'vdiff-magit-dwim)
    (define-key magit-mode-map (kbd "E") #'vdiff-magit))
  )

;; smerge: auto-enable when conflict markers appear
(use-package smerge-mode
  :commands smerge-mode
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))))

;; When in magit status, turn on olivetti
(add-hook 'magit-status-mode-hook #'olivetti-mode)