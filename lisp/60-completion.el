;;; 60-completion.el --- company, copilot, snippets  -*- lexical-binding: t; -*-

;; Copilot --------------------------------------------------------------------
(install-if-necessary 'editorconfig)
(install-if-necessary 'jsonrpc)
(use-package editorconfig :config (editorconfig-mode 1))
(use-package jsonrpc)

(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el")))

;; Company --------------------------------------------------------------------
(use-package company
  :after (lsp-mode copilot)
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("C-l"   . company-complete-selection)
              ("TAB"   . copilot-accept-completion)
              ("<tab>" . copilot-accept-completion)
         :map lsp-mode-map
              ("C-l" . company-indent-or-complete-common))
  :custom ((company-minimum-prefix-length 1)
           (company-idle-delay 0.0)))

(use-package company-box :hook (company-mode . company-box-mode))

;; YASnippet ------------------------------------------------------------------
(use-package yasnippet
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "C-j")   #'yas-next-field-or-maybe-expand)
  (define-key yas-minor-mode-map (kbd "C-S-j") #'yas-prev-field))

(defun my/company-yasnippet-complete ()
  (interactive)
  (let ((company-backends '(company-yasnippet)))
    (company-complete)))
(evil-define-key 'insert 'global (kbd "C-y") #'my/company-yasnippet-complete)

;; Dispatcher TAB --------------------------------------------------------------
(defun dispatch-tab-command ()
  "Context-aware <TAB>: Copilot, YAS, indent."
  (interactive)
  (cond
   ((eq major-mode 'fountain-mode) (fountain-dwim))
   (t
    (or (copilot-accept-completion)
        (indent-for-tab-command)))))
(global-set-key (kbd "<tab>") #'dispatch-tab-command)
(evil-define-key 'insert 'global (kbd "<tab>") #'dispatch-tab-command)

(provide '60-completion)
;;; 60-completion.el ends here
