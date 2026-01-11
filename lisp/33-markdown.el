;;; 33-markdown.el --- Markdown editing with live preview  -*- lexical-binding: t; -*-

;; Markdown mode
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.Rmd\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown")
  (markdown-fontify-code-blocks-natively t)
  (markdown-enable-math t)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-fontify-whole-heading-line t)
  :config
  ;; Use visual-line-mode for nicer line wrapping
  (add-hook 'markdown-mode-hook 'visual-line-mode)

  ;; Use olivetti for comfortable writing
  (add-hook 'markdown-mode-hook 'olivetti-mode)

  ;; Keybindings for markdown commands
  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "C-c C-p") 'markdown-preview)
    (define-key markdown-mode-map (kbd "C-c C-l") 'markdown-insert-link)
    (define-key markdown-mode-map (kbd "C-c C-i") 'markdown-insert-image)))

;; Grip mode: Live GitHub-flavored markdown preview
(use-package grip-mode
  :commands grip-mode
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode))
  :custom
  (grip-preview-use-webkit t)
  :config
  ;; Use GitHub API for accurate rendering (optional: set GITHUB_TOKEN env var)
  (setq grip-update-after-change nil))  ; Manual refresh with C-c C-c

;; Impatient mode: Live browser preview
(use-package impatient-mode
  :commands (impatient-mode httpd-start httpd-stop)
  :config
  (defun my/markdown-impatient-preview ()
    "Start impatient-mode for markdown with automatic HTML conversion."
    (interactive)
    (unless (process-status "httpd")
      (httpd-start))
    (impatient-mode 1)
    (setq-local impatient-mode-delay 1)
    (message "Markdown preview available at http://localhost:8080/imp/"))

  ;; Keybinding for starting impatient preview
  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "C-c C-o") 'my/markdown-impatient-preview)))

;; Markdown xwidget: Inline webkit preview
(use-package markdown-xwidget
  :straight (markdown-xwidget :type git
                               :host github
                               :repo "cfclrk/markdown-xwidget")
  :if (and (fboundp 'xwidget-webkit-browse-url)
           (display-graphic-p))
  :commands (markdown-xwidget-preview-mode)
  :bind (:map markdown-mode-map
              ("C-c C-x" . markdown-xwidget-preview-mode))
  :custom
  (markdown-xwidget-command "pandoc")
  (markdown-xwidget-github-style-p t))

;; Markdown TOC: Generate table of contents
(use-package markdown-toc
  :commands (markdown-toc-generate-toc markdown-toc-refresh-toc)
  :bind (:map markdown-mode-command-map
              ("t" . markdown-toc-generate-toc)))

;; Edit code blocks in markdown with edit-indirect
(use-package edit-indirect
  :commands edit-indirect-region
  :bind (:map markdown-mode-map
              ("C-c '" . edit-indirect-region)))

;; Jinx: Fast spell checking using enchant
;; Only load if Emacs has module support AND pkg-config is available
(defvar my/jinx-available-p
  (and (functionp 'module-load)  ; Emacs has dynamic module support
       (or (executable-find "pkg-config")
           (executable-find "pkgconf")))
  "Whether jinx can be compiled and loaded.")

(use-package jinx
  :if my/jinx-available-p
  :commands (jinx-mode)
  :bind ([remap ispell-word] . jinx-correct)
  :custom
  (jinx-languages "en_US")
  :config
  ;; Use M-$ for spell checking (standard Emacs binding)
  (global-set-key (kbd "M-$") #'jinx-correct)
  (global-set-key (kbd "C-M-$") #'jinx-languages)

  ;; Additional keybindings for convenience
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global (kbd "z=") #'jinx-correct)))

;; Fallback to flyspell if jinx is not available
(unless my/jinx-available-p
  (when (not (functionp 'module-load))
    (message "Jinx unavailable: Emacs lacks dynamic module support"))
  (when (not (or (executable-find "pkg-config") (executable-find "pkgconf")))
    (message "Jinx unavailable: pkg-config not found"))

  (use-package flyspell
    :straight (:type built-in)
    :hook (text-mode . flyspell-mode)
    :bind ([remap ispell-word] . flyspell-correct-wrapper)
    :config
    ;; Use aspell if available
    (when (executable-find "aspell")
      (setq ispell-program-name "aspell")
      (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))

    ;; Vim-style binding
    (with-eval-after-load 'evil
      (evil-define-key 'normal 'global (kbd "z=") #'ispell-word))))

(provide '33-markdown)
;;; 33-markdown.el ends here
