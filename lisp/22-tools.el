
(use-package avy
  :defer t
  :ensure t
  :bind (("C-:"   . avy-goto-char)        ;; Jump to a character in the visible buffer
         ("C-'"   . avy-goto-char-2)      ;; Jump to a character using a two-character lookup
         ("M-g g" . avy-goto-line)        ;; Jump to a specific line
         ("M-g w" . avy-goto-word-0))     ;; Jump to the beginning of a word
  :config
  :custom
  (avy-timeout-seconds 0.3)
  (avy-style 'pre)
  ;; :custom-face
  ;; (avy-lead-face ((t (:background "#51afef" :foreground "#870000" :weight bold))))
  ;; When Avy is activated, highlight potential jump targets in the background.
  (setq avy-background t
        ;; Limit Avy to the current window (set to nil for all windows)
        avy-all-windows nil))

;; Crux
(use-package crux
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("C-x 4 t" . crux-transpose-windows)
   ("C-x K" . crux-kill-other-buffers)
   ("C-k" . crux-smart-kill-line))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(defun my-counsel-projectile-find-file ()
  "Use counsel-projectile-find-file with true fuzzy matching.
Temporarily disable ivy-prescient-mode and force ivy--regex-fuzzy
to allow out-of-order matching like 'eve/mp' for 'events/map'."
  (interactive)
  (let ((ivy-prescient-mode nil)
        (ivy-re-builders-alist '((t . ivy--regex-fuzzy))))
    (counsel-projectile-find-file)))


;; Color ripgrep
(use-package color-rg
  :straight (color-rg :type git :host github :repo "manateelazycat/color-rg")
  :if (executable-find "rg")
  :bind ("C-M-s" . color-rg-search-input))

;; Dired
(use-package dired
  :straight (:type built-in)
  :bind
  (("C-x C-j" . dired-jump))
  :custom
  ;; Always delete and copy recursively
  (dired-listing-switches "-lah")
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  ;; Quickly copy/move file in Dired
  (dired-dwim-target t)
  ;; Move files to trash when deleting
  (delete-by-moving-to-trash t)
  ;; Load the newest version of a file
  (load-prefer-newer t)
  ;; Detect external file changes and auto refresh file
  (auto-revert-use-notify nil)
  (auto-revert-interval 3) ; Auto revert every 3 sec
  :config
  ;; Enable global auto-revert
  (global-auto-revert-mode t)
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)
  :hook
  (dired-mode . (lambda ()
                  (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)
                  (local-set-key (kbd "RET") #'dired-find-alternate-file)
                  (local-set-key (kbd "^")
                                 (lambda () (interactive) (find-alternate-file ".."))))))

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
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; Pulsar
(use-package pulsar
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1))

;; ELscreen
(use-package elscreen
  :config
  (elscreen-start))

;; ;; Flycheck
;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package ivy :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done) ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line) ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line) ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line) ("C-d" . ivy-reverse-i-search-kill)
         ;; counsel
         ("C-x C-f" . counsel-find-file))
  :custom ((ivy-use-virtual-buffers t)
           (ivy-height 10) (ivy-wrap t)
           (ivy-count-format "【%d/%d】"))
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package counsel :after ivy
  :bind (("M-x"     . counsel-M-x)
	 ("<leader>SPC" . my-counsel-projectile-find-file)
         ("C-x b"   . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history)))

(use-package counsel
  :ensure t
  :bind (("C-x C-f"   . counsel-find-file)          ; default binding
         ) ; use our wrapper for Projectile
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package flx)                       ; faster fuzzy
(use-package prescient :config (prescient-persist-mode 1))
(use-package ivy-prescient :after (ivy prescient) :config (ivy-prescient-mode 1))

(use-package ivy-rich :after ivy :config (ivy-rich-mode 1))

;; ---------------------------- Projectile / Magit ----------------------------
(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom ((projectile-completion-system 'ivy))
  :init   
  (setq projectile-project-search-path '("~/development/"))
  :config
  (projectile-mode 1)
  ;; Set up the evil keybinding after both evil and projectile are loaded
  (add-to-list 'projectile-globally-ignored-directories ".clj-kondo")
  (setq projectile-auto-discover  t
	projectile-mode-line      "Projectile"
	projectile-sort-order     'recently-active
	projectile-generic-command
	"find . -type f -not -path '*/node_modules/*' -not -path '*/build/*' \
-not -path '*/.clj-kondo/*' -print0")
  )

;; Here to make sure projectile loads
(projectile-discover-projects-in-search-path)

(use-package counsel-projectile
  :after (projectile counsel)
  :config (counsel-projectile-mode 1))

(use-package magit
  :commands (magit-status magit-dispatch)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :custom ((magit-display-buffer-function
            #'magit-display-buffer-same-window-except-diff-v1)
           (magit-save-repository-buffers 'dontask)
           (magit-completing-read-function #'ivy-completing-read))
  :config
  (setq magit-repository-directories
        (mapcar (lambda (p) (cons p 1)) projectile-known-projects))
  (setq magit-diff-refine-hunk t))


;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
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
          treemacs-read-string-input               'from-child-frame
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
          treemacs-width                           35
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

    (treemacs-hide-gitignored-files-mode nil))
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

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))
