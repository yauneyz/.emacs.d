;;; 20-ui.el --- Themes, modeline, icons, line-numbers  -*- lexical-binding: t; -*-

(use-package diminish)       ; hide minor-mode lighters

(setq inhibit-startup-message t
      inhibit-startup-screen  t
      visible-bell            t)

;; Relative line numbers ------------------------------------------------------
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(dolist (mode '(org-mode-hook term-mode-hook shell-mode-hook
			      treemacs-mode-hook eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Rainbow* helpers -----------------------------------------------------------
(use-package rainbow-mode   :hook (prog-mode . rainbow-mode))
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons)

;; --------------------------------------------------------------------------
;; Doom themes + modeline
;; --------------------------------------------------------------------------
(use-package doom-themes
  :custom-face (cursor ((t (:background "Red"))))
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (load-theme 'doom-dark+ t)
  (defun switch-theme ()
    "Interactively switch doom themes."
    (interactive)
    (when custom-enabled-themes
      (disable-theme (car custom-enabled-themes)))
    (call-interactively #'load-theme)))

(when (display-graphic-p)
  ;; transparent background
  (add-to-list 'default-frame-alist '(alpha-background . 100))
  (set-frame-parameter nil 'alpha-background 100))

(use-package doom-modeline
  :custom ((doom-modeline-minor-modes t)
           (doom-modeline-icon        t)
           (doom-modeline-major-mode-color-icon t)
           (doom-modeline-height 25)
           (doom-modeline-bar-width 3))
  :config (doom-modeline-mode 1))

;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)

;; Highlight lines
(global-hl-line-mode 1)

;; prettify symbols
(defun setup-prettify-symbols ()
  (setq prettify-symbols-alist
        '(("lambda"  . 955)
          ("delta"   . 120517)
          ("epsilon" . 120518)
          ("->" . 8594) ("<=" . 8804) (">=" . 8805)))
  (prettify-symbols-mode 1))
(global-prettify-symbols-mode 1)
(add-hook 'prog-mode-hook #'setup-prettify-symbols)
(add-hook 'org-mode-hook  #'setup-prettify-symbols)

(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

(setq initial-major-mode   'text-mode
      initial-scratch-message "Present Day, Present Time...\n")

(when (version<= "29.1" emacs-version)
  (pixel-scroll-precision-mode 1))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)

(provide 'ui)
;;; 20-ui.el ends here
