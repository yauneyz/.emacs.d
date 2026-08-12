;;; 33-markdown.el --- Markdown editing with live preview  -*- lexical-binding: t; -*-

(defun my/markdown-compose-pretty-markers (limit)
  "Compose Markdown task markers up to LIMIT."
  (while (re-search-forward "^\\([[:space:]]*\\)\\(?:[-+*]\\|[0-9]+[.)]\\) \\(\\[[ xX]\\]\\)" limit t)
    (compose-region
     (match-beginning 2)
     (match-end 2)
     (if (string-match-p "[xX]" (match-string 2)) "☑" "☐")))
  nil)

(defun my/markdown-configure-prose-font ()
  "Use the best available variable-pitch font for prose buffers."
  (when (display-graphic-p)
    (catch 'configured
      (dolist (family '("Liberation Sans" "DejaVu Sans"))
        (when (find-font (font-spec :family family))
          (set-face-attribute 'variable-pitch nil :family family :height 1.08)
          (throw 'configured family))))))

(defun my/markdown-mode-setup ()
  "Use a prose-first display while preserving fixed-pitch code regions."
  (my/markdown-configure-prose-font)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (auto-fill-mode 0)
  (olivetti-mode 1)
  (display-line-numbers-mode -1)
  (setq-local line-spacing 0.12
              markdown-hide-urls t)
  (markdown-toggle-markup-hiding 1))

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
  (markdown-header-scaling t)
  (markdown-header-scaling-values '(1.55 1.35 1.18 1.08 1.0 1.0))
  (markdown-list-item-bullets '("•"))
  :custom-face
  (markdown-header-face ((t (:inherit variable-pitch :weight bold))))
  (markdown-header-delimiter-face ((t (:inherit shadow :weight normal))))
  (markdown-bold-face ((t (:inherit bold :weight bold))))
  (markdown-italic-face ((t (:inherit italic :slant italic))))
  (markdown-code-face ((t (:inherit fixed-pitch :extend t))))
  (markdown-inline-code-face ((t (:inherit (markdown-code-face font-lock-constant-face) :extend nil))))
  (markdown-pre-face ((t (:inherit (markdown-code-face font-lock-constant-face)))))
  (markdown-table-face ((t (:inherit fixed-pitch))))
  (markdown-language-keyword-face ((t (:inherit (fixed-pitch font-lock-type-face)))))
  (markdown-language-info-face ((t (:inherit (fixed-pitch font-lock-string-face)))))
  (markdown-metadata-key-face ((t (:inherit (fixed-pitch font-lock-variable-name-face)))))
  (markdown-metadata-value-face ((t (:inherit (fixed-pitch font-lock-string-face)))))
  (markdown-url-face ((t (:inherit (fixed-pitch link)))))
  :hook (markdown-mode . my/markdown-mode-setup)
  :config
  (font-lock-add-keywords
   'markdown-mode
   '((my/markdown-compose-pretty-markers))
   'append)

  (dolist (face '(markdown-code-face
                  markdown-inline-code-face
                  markdown-pre-face
                  markdown-table-face
                  markdown-language-keyword-face
                  markdown-language-info-face
                  markdown-metadata-key-face
                  markdown-metadata-value-face
                  markdown-url-face))
    (set-face-attribute face nil :family (face-attribute 'fixed-pitch :family)))

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
