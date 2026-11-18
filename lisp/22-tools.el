;;; 22-tools.el --- Tools  -*- lexical-binding: t; -*-

;; Deadgrep: Fast ripgrep interface
(use-package deadgrep
  :if (executable-find "rg")
  :bind (("C-M-s" . deadgrep))
  :config
  ;; Make deadgrep work with project.el
  (defun my/deadgrep-project-root ()
    "Get project root for deadgrep using project.el."
    (when-let ((project (project-current)))
      (project-root project)))

  (setq deadgrep-project-root-function #'my/deadgrep-project-root))

;; Color ripgrep (alternative)
(use-package color-rg
  :straight (color-rg :type git :host github :repo "manateelazycat/color-rg")
  :if (executable-find "rg")
  :commands color-rg-search-input)

;; Dirvish: Modern Dired replacement with rich previews
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :bind
  (("C-x C-j" . dired-jump)
   :map dirvish-mode-map
   ("a" . dirvish-quick-access)
   ("f" . dirvish-file-info-menu)
   ("y" . dirvish-yank-menu)
   ("N" . dirvish-narrow)
   ("^" . dirvish-history-last)
   ("h" . dirvish-history-jump)
   ("s" . dirvish-quicksort)
   ("v" . dirvish-vc-menu)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump))
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("d" "~/Downloads/" "Downloads")
     ("D" "~/development/" "Development")
     ("e" "~/.emacs.d/" "Emacs")
     ("t" "/tmp/" "Temp")))
  (dirvish-attributes
   '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (omit yank index)))
  (dirvish-header-line-format
   '(:left (path) :right (free-space)))
  (delete-by-moving-to-trash t)
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group")
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :config
  ;; Enable global auto-revert for all buffers
  (global-auto-revert-mode t)
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t
        auto-revert-use-notify nil
        auto-revert-interval 3))

;; Winner
(use-package winner
  :straight (:type built-in)
  :custom
  (winner-boring-buffers
   '("*Completions*"
     "*Compile-Log*"
     "*inferior-lisp*"
     "*Fuzzy Completions*"
     "*Apropos*"
     "*Help*"
     "*cvs*"
     "*Buffer List*"
     "*Ibuffer*"
     "*esh command on file*"))
  :config
  (winner-mode 1))


;; Which-key ------------------------------------------------------------------
(use-package which-key
  :diminish which-key-mode
  :custom ((which-key-separator " ")
           (which-key-prefix-prefix "+")
           (which-key-idle-delay 0.3))
  :config (which-key-mode 1))


(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command))

(use-package command-log-mode
  :defer t
  :commands (command-log-mode
             clm/open-command-log-buffer
             clm/close-command-log-buffer)
  :config
  (setq command-log-mode-open-log-turns-on-mode nil
        command-log-mode-window-size 60))

;; Pulsar
(use-package pulsar
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1))

;; Apheleia: Format on save
(use-package apheleia
  :custom
  ;; Enable format-on-save for all programming modes
  (apheleia-mode-alist
   '((python-mode . ruff)
     (python-ts-mode . ruff)
     (go-mode . gofmt)
     (go-ts-mode . gofmt)
     (rust-mode . rustfmt)
     (rust-ts-mode . rustfmt)
     (clojure-mode . zprint)
     (clojurescript-mode . zprint)
     (clojure-ts-mode . zprint)
     (clojurescript-ts-mode . zprint)
     (javascript-mode . prettier)
     (js-mode . prettier)
     (js2-mode . prettier)
     (js-ts-mode . prettier)
     (typescript-mode . prettier)
     (typescript-ts-mode . prettier)
     (tsx-ts-mode . prettier)
     (json-mode . prettier)
     (json-ts-mode . prettier)
     (yaml-mode . prettier)
     (yaml-ts-mode . prettier)
     (html-mode . prettier)
     (css-mode . prettier)
     (scss-mode . prettier)
     (web-mode . prettier)
     (markdown-mode . prettier)))
  :config
  (setf (alist-get 'zprint apheleia-formatters nil nil #'eq)
        '("zprint" "{:style :community}"))
  (apheleia-global-mode +1))

;; ELscreen
(use-package elscreen
  :config
  (elscreen-start))

;; Flycheck surfaces diagnostics globally so LSP errors stick around.
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; ======================== Vertico / Consult / Embark ========================

;; Vertico: Modern vertical completion UI
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize nil)  ; Keep window size fixed (don't shrink/grow)
  (vertico-count 15)    ; Always show 15 candidates
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-l" . vertico-insert)))

;; Marginalia: Rich annotations in the minibuffer
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;; Orderless: fuzzy matching for minibuffer + Corfu
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))
        orderless-matching-styles '(orderless-flex orderless-literal orderless-regexp)))

;; Consult: Consulting completing-read
(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x C-b" . consult-buffer)
         ("C-x C-f" . find-file)
         ("C-x p b" . consult-project-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s d" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line-multi)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (consult-narrow-key "<")
  (consult-project-function (lambda (_may-prompt) (project-root (project-current))))
  :config
  ;; Configure preview
  (setq consult-preview-key 'any)
  (consult-customize
   consult-ripgrep consult-grep
   consult-bookmark consult-recent-file
   :preview-key "M-."))

;; Embark: Context actions
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark + Consult integration
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ======================== Project.el Configuration ========================

(use-package project
  :straight (:type built-in)
  :custom
  (project-switch-commands 'project-find-file)
  :config
  ;; Add project search paths
  (setq project-vc-extra-root-markers '(".project" ".projectile" "go.mod" "Cargo.toml" "package.json"))

  (defun my/project--ignored-directories (path)
    "Return directories from PATH/.gitignore to skip during discovery."
    (let ((ignore-file (expand-file-name ".gitignore" path))
          ignores)
      (when (file-readable-p ignore-file)
        (with-temp-buffer
          (insert-file-contents ignore-file)
          (goto-char (point-min))
          (while (not (eobp))
            (let* ((raw (buffer-substring-no-properties (line-beginning-position)
                                                        (line-end-position)))
                   (line (string-trim raw)))
              (unless (or (string-empty-p line)
                          (string-prefix-p "#" line)
                          (string-prefix-p "!" line)
                          (string-match-p "/" line))
                (push (string-remove-suffix "/" line) ignores)))
            (forward-line 1))))
      ignores))

  ;; Discover projects (similar to projectile-discover)
  ;; Every git root under `my/project-recursive-roots' becomes a project.
  ;; Scanning stops at the first git root (no nested repos / submodules).
  ;; The walk is done by tools/git-roots (Rust, parallel), which Emacs builds
  ;; on demand with cargo; without cargo it falls back to an elisp walk.
  (defvar my/project-recursive-roots '("~/development")
    "Directories scanned recursively for git roots.")

  (defvar my/project-shallow-roots '("~/dotfiles" "~/.emacs.d")
    "Directories that are themselves projects, or whose direct children are.")

  (defvar my/project-scan-skip-dirs
    '(".git" "node_modules" "target" ".venv" "venv" "__pycache__" "dist"
      "build" ".cache" ".direnv" ".stack-work" "dist-newstyle" ".cpcache"
      ".gradle" ".idea" "straight" "elpa" "eln-cache" "celeba")
    "Directory names never descended into while scanning for projects.")

  (defvar my/git-roots-source-dir
    (expand-file-name "tools/git-roots" user-emacs-directory)
    "Cargo project for the git-roots scanner.")

  (defvar my/git-roots-target-dir
    (expand-file-name "var/git-roots" user-emacs-directory)
    "Cargo target dir for git-roots (kept out of the tracked tree).")

  (defun my/git-roots--binary ()
    (expand-file-name "release/git-roots" my/git-roots-target-dir))

  (defun my/git-roots--up-to-date-p ()
    "Non-nil if the built binary is newer than every source file."
    (let ((bin (my/git-roots--binary)))
      (and (file-executable-p bin)
           (seq-every-p
            (lambda (src) (file-newer-than-file-p bin src))
            (cons (expand-file-name "Cargo.toml" my/git-roots-source-dir)
                  (directory-files-recursively
                   (expand-file-name "src" my/git-roots-source-dir) "\\.rs\\'"))))))

  (defun my/git-roots-ensure (callback)
    "Call CALLBACK with the git-roots binary path, building it if needed.
CALLBACK gets nil when the binary is unavailable (no cargo, build failure)."
    (cond
     ((my/git-roots--up-to-date-p) (funcall callback (my/git-roots--binary)))
     ((not (and (executable-find "cargo")
                (file-exists-p (expand-file-name "Cargo.toml" my/git-roots-source-dir))))
      (funcall callback nil))
     (t
      (message "Building git-roots project scanner...")
      (let ((default-directory (file-name-as-directory my/git-roots-source-dir))
            (log (get-buffer-create "*git-roots-build*")))
        (with-current-buffer log (erase-buffer))
        (make-process
         :name "git-roots-build"
         :buffer log
         :noquery t
         :command (list "cargo" "build" "--release" "--quiet"
                        "--target-dir" my/git-roots-target-dir)
         :sentinel
         (lambda (proc _event)
           (unless (process-live-p proc)
             (if (and (zerop (process-exit-status proc))
                      (file-executable-p (my/git-roots--binary)))
                 (funcall callback (my/git-roots--binary))
               (message "git-roots build failed (see *git-roots-build*); using elisp scan")
               (funcall callback nil)))))))))

  (defun my/project--git-root-p (dir)
    "Non-nil if DIR contains a .git directory or file (worktrees, submodules)."
    (file-exists-p (expand-file-name ".git" dir)))

  (defun my/project--find-git-roots (dir)
    "Elisp fallback: return git roots below DIR, stopping at each root."
    (let ((roots '())
          (stack (list (file-name-as-directory (expand-file-name dir))))
          (seen (make-hash-table :test #'equal))
          (file-name-handler-alist nil))
      (while stack
        (let* ((d (pop stack))
               (true (file-truename d)))
          (unless (gethash true seen)
            (puthash true t seen)
            (if (my/project--git-root-p d)
                (push d roots)
              (dolist (ent (ignore-errors
                             (directory-files-and-attributes
                              d t directory-files-no-dot-files-regexp t)))
                (let ((type (file-attribute-type (cdr ent))))
                  (when (and (or (eq type t)
                                 (and (stringp type) (file-directory-p (car ent))))
                             (not (member (file-name-nondirectory (car ent))
                                          my/project-scan-skip-dirs)))
                    (push (file-name-as-directory (car ent)) stack))))))))
      (nreverse roots)))

  (defun my/project--remember-dirs (dirs &optional report)
    "Remember each git root in DIRS as a project and persist the list once.
DIRS are already known roots, so register them directly instead of going
through `project-current'.  STABLE keeps the existing recency order.
Paths reaching the same repo via symlinks collapse to the shortest one."
    (let ((by-true (make-hash-table :test #'equal)))
      (dolist (dir dirs)
        (let* ((key (file-truename (file-name-as-directory dir)))
               (cur (gethash key by-true)))
          (when (or (null cur) (< (length dir) (length cur)))
            (puthash key dir by-true))))
      (setq dirs (hash-table-values by-true)))
    (dolist (dir dirs)
      (project--remember-dir (file-name-as-directory dir) t t))
    (project--write-project-list)
    (when report
      (message "Remembered %d projects" (length dirs))))

  (defun my/project--shallow-dirs ()
    "Git roots in `my/project-shallow-roots' (the root itself or direct children)."
    (let (dirs)
      (dolist (path my/project-shallow-roots)
        (let ((expanded (expand-file-name path)))
          (when (file-directory-p expanded)
            (if (my/project--git-root-p expanded)
                (push expanded dirs)
              (dolist (child (directory-files expanded t directory-files-no-dot-files-regexp t))
                (when (and (file-directory-p child) (my/project--git-root-p child))
                  (push child dirs)))))))
      dirs))

  (defun my/add-known-projects (&optional report)
    "Remember every git root under `my/project-recursive-roots', plus shallow roots.
Runs asynchronously; with REPORT (interactive), message the count when done."
    (interactive (list t))
    (let ((roots (seq-filter #'file-directory-p
                             (mapcar #'expand-file-name my/project-recursive-roots)))
          (shallow (my/project--shallow-dirs)))
      (my/git-roots-ensure
       (lambda (bin)
         (if (not bin)
             (my/project--remember-dirs
              (nconc (mapcan #'my/project--find-git-roots roots) shallow) report)
           (let ((buf (generate-new-buffer " *git-roots*")))
             (make-process
              :name "git-roots"
              :buffer buf
              :noquery t
              :stderr (get-buffer-create " *git-roots-errors*")
              :command (append (list bin)
                               (mapcan (lambda (n) (list "--skip" n))
                                       my/project-scan-skip-dirs)
                               roots)
              :sentinel
              (lambda (proc _event)
                (unless (process-live-p proc)
                  (let ((dirs (with-current-buffer buf
                                (split-string (buffer-string) "\n" t))))
                    (kill-buffer buf)
                    (my/project--remember-dirs (nconc dirs shallow) report)))))))))))

  ;; Discover projects on startup (async; doesn't block init)
  (add-hook 'emacs-startup-hook #'my/add-known-projects)

  )


;; Normalize Treemacs paths so symlinked $HOME paths match project.el roots.
(defun my/treemacs--canonicalize-existing-path (path)
  "Return PATH in the canonical form Treemacs uses for workspace roots."
  (when path
    (cond
     ((file-remote-p path)
      (treemacs-canonical-path path))
     ((file-exists-p path)
      (treemacs-canonical-path (file-truename path)))
     (t
      (treemacs-canonical-path path)))))

(defun my/treemacs--with-canonical-buffer-paths (orig &rest args)
  "Run ORIG with canonical buffer-local file paths for Treemacs."
  (let ((buffer-file-name (my/treemacs--canonicalize-existing-path buffer-file-name))
        (default-directory (if default-directory
                               (my/treemacs--canonicalize-existing-path default-directory)
                             default-directory)))
    (apply orig args)))

(defun my/treemacs--canonicalize-path-arg (orig path &rest args)
  "Resolve PATH before ORIG compares it against Treemacs workspace roots."
  (apply orig (my/treemacs--canonicalize-existing-path path) args))

(defun my/treemacs--canonicalize-tag-follow-buffer-file (orig flat-index treemacs-window buffer-file project)
  "Resolve BUFFER-FILE before ORIG follows a tag in Treemacs."
  (funcall orig
           flat-index
           treemacs-window
           (my/treemacs--canonicalize-existing-path buffer-file)
           project))


;; Track the visited file so Treemacs can reveal it immediately after opening.
(defvar my/treemacs--last-file-buffer nil
  "Most recent file-visiting buffer seen before toggling Treemacs.")

(defun my/treemacs--remember-file-buffer (&rest _)
  "Store the current file-visiting buffer for later Treemacs reveal."
  (setq my/treemacs--last-file-buffer
        (let ((buf (window-buffer (selected-window))))
          (and (buffer-live-p buf)
               (buffer-file-name buf)
               buf))))

(defun my/treemacs--reveal-remembered-buffer (&rest _)
  "Reveal the previously remembered file when Treemacs becomes visible."
  (when (and (eq (treemacs-current-visibility) 'visible)
             (buffer-live-p my/treemacs--last-file-buffer))
    (with-current-buffer my/treemacs--last-file-buffer
      (treemacs-find-file)))
  (setq my/treemacs--last-file-buffer nil))

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    ;; Treemacs' python helpers clear PATH, so give them the full git path.
    (setq treemacs-git-executable                  (or (executable-find "git")
                                                       treemacs-git-executable)
          treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          ;; Child-frame prompts can lose keyboard focus on some window
          ;; manager/display setups, which makes Treemacs rename appear hung.
          treemacs-read-string-input               'from-minibuffer
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           25
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-project-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil)

    ;; Fix for "wrong-type-argument arrayp nil" error
    ;; Ensure workspace projects slot is initialized as empty list instead of nil
    (let ((projects-slot (cl-struct-slot-offset 'treemacs-workspace 'projects)))
      (dolist (ws treemacs--workspaces)
        (unless (treemacs-workspace->projects ws)
          (aset ws projects-slot '()))))

    (advice-add 'treemacs :around #'my/treemacs--with-canonical-buffer-paths)
    (advice-add 'treemacs-find-file :around #'my/treemacs--with-canonical-buffer-paths)
    (advice-add 'treemacs-find-tag :around #'my/treemacs--with-canonical-buffer-paths)
    (advice-add 'treemacs--follow :around #'my/treemacs--with-canonical-buffer-paths)
    (advice-add 'treemacs-find-file-node :around #'my/treemacs--canonicalize-path-arg)
    (advice-add 'treemacs--do-follow-tag :around #'my/treemacs--canonicalize-tag-follow-buffer-file)

    (advice-add 'treemacs :before #'my/treemacs--remember-file-buffer)
    (advice-add 'treemacs :after #'my/treemacs--reveal-remembered-buffer))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))


;; Use ripgrep

(use-package hydra :ensure t)
