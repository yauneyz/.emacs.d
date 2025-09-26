;;; 58-typescript.el --- TypeScript/JavaScript tooling -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides TypeScript/JavaScript support with lsp-mode, formatting, linting,
;; test helpers, and tree-sitter remapping.

;;; Code:

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'"  . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :hook (typescript-mode . (lambda ()
                             (setq-local indent-tabs-mode nil
                                         tab-width 2
                                         typescript-indent-level 2))))

(when (and (fboundp 'typescript-ts-base-mode)
           (fboundp 'tsx-ts-mode))
  (add-to-list 'major-mode-remap-alist '(typescript-ts-base-mode . tsx-ts-mode)))

(use-package prettier
  :ensure t
  :hook ((typescript-mode . prettier-mode)
         (json-mode . prettier-mode))
  :custom (prettier-pre-warm-server t))

(use-package add-node-modules-path
  :hook (typescript-mode . add-node-modules-path))

(when (boundp 'tsx-ts-mode)
  (add-hook 'tsx-ts-mode-hook (lambda ()
                                (setq-local indent-tabs-mode nil
                                            tab-width 2)))
  (add-hook 'tsx-ts-mode-hook #'add-node-modules-path)
  (add-hook 'tsx-ts-mode-hook #'prettier-mode))

(use-package jest
  :after typescript-mode
  :commands (jest-file jest-project jest-single)
  :custom (jest-executable "npx jest"))

(provide 'lang-typescript)
;;; 58-typescript.el ends here
