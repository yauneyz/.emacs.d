;;; 29-terminals.el --- vterm / shell helpers  -*- lexical-binding: t; -*-

;; Vterm: Modern terminal emulator
(use-package vterm
  :commands vterm
  :bind ((:map vterm-mode-map
               ("C-y" . vterm-yank)
               ("M-y" . vterm-yank-pop)
               ("C-q" . vterm-send-next-key)
               ("C-z" . nil)
               ("M-:" . nil)
               ("C-c C-t" . vterm-copy-mode)))
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm %s")
  (vterm-always-compile-module t)
  :config
  ;; Enable line number mode in vterm copy mode
  (add-hook 'vterm-copy-mode-hook
            (lambda ()
              (if vterm-copy-mode
                  (progn
                    (display-line-numbers-mode 1)
                    (evil-normal-state))
                (display-line-numbers-mode -1))))

  ;; Better vterm scrolling
  (setq vterm-scroll-to-bottom-on-output nil)
  (setq vterm-scroll-to-bottom-on-input t)

  ;; Enable vterm integration with Evil
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'emacs)))

(use-package shell-here
  :bind ("M-~" . shell-here)
  :config
  (when *sys/linux*
    (setq explicit-shell-file-name (executable-find "bash"))))

(use-package multi-term
  :straight (multi-term :type git :host github :repo "manateelazycat/multi-term")
  :commands (multi-term)
  :bind
  (("M-$" . multi-term)
   (:map dired-mode-map ("M-$" . multi-term)))
  :custom
  (multi-term-program (executable-find "bash"))
  (term-bind-key-alist
   '(("C-c C-c" . term-interrupt-subjob)
     ("C-c C-e" . term-send-esc)
     ("C-p" . previous-line)
     ("C-n" . next-line)
     ("C-m" . term-send-return)
     ("C-y" . term-paste)
     ("C-v" . scroll-up-command)
     ("M-v" . scroll-down-command)
     ("M-f" . term-send-forward-word)
     ("M-b" . term-send-backward-word)
     ("M-o" . term-send-backspace)
     ("M-p" . term-send-up)
     ("M-n" . term-send-down)
     ("M-M" . term-send-forward-kill-word)
     ("M-N" . term-send-backward-kill-word)
     ("<C-backspace>" . term-send-backward-kill-word)
     ("<M-backspace>" . term-send-backward-kill-word)
     ("M-r" . term-send-reverse-search-history)
     ("M-d" . term-send-delete-word)
     ("M-," . term-send-raw)
     ("M-." . comint-dynamic-complete))))

(use-package term-keys
  :straight (term-keys :type git :host github :repo "CyberShadow/term-keys")
  :if (not (display-graphic-p))
  :config (term-keys-mode t))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-variables
   '("PATH" "MANPATH"
     "OPENAI_API_KEY" "ANTHROPIC_API_KEY"
     "XAI_API_KEY" "DEEPSEEK_API_KEY"
     "OPENROUTER_API_KEY" "GEMINI_API_KEY"))
  :config
  (exec-path-from-shell-initialize))

(use-package ibuffer
  :straight (:type built-in)
  :bind ("C-x C-b" . ibuffer)
  :init
  (use-package ibuffer-vc
    :commands (ibuffer-vc-set-filter-groups-by-vc-root)
    :custom
    (ibuffer-vc-skip-if-remote 'nil))
  :custom
  (ibuffer-formats
   '((mark modified read-only locked " "
           (name 35 35 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename))))

(use-package recentf
  :straight (:type built-in)
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup "05:00am")
  (recentf-max-saved-items 200)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     "COMMIT_EDITMSG\\'")))
