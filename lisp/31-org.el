;; Olivetti mode

(use-package olivetti
  :config
  (setq olivetti-body-width 80)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

;; Call olivetti mode on .txt
(add-hook 'text-mode-hook 'olivetti-mode)

;; --- New configuration for programming modes ---
(defun my/prog-olivetti-setup ()
  "Configure Olivetti in programming modes: set a 100-column width and disable visual margins."
  (setq-local olivetti-body-width 100)
  (setq-local olivetti-minimum-body-width 100)
  ;; Disable visual margins by setting the style to nil.
  ;; (See Olivetti’s documentation: setting `olivetti-style` to nil turns off any
  ;; extra side margins.)
  (setq-local olivetti-style nil)
  (olivetti-mode 1))

(add-hook 'prog-mode-hook #'my/prog-olivetti-setup)

(defun org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (auto-fill-mode 0)
  (olivetti-mode 1)
  (define-key org-mode-map (kbd "C-<right>") 'org-metaright)
  (define-key org-mode-map (kbd "C-<left>") 'org-metaleft)
  (define-key org-mode-map (kbd "C-<up>") 'org-metaup)
  (define-key org-mode-map (kbd "C-<down>") 'org-metadown))

;; Org mode
(use-package org
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-ellipsis " ▼")
  (setq org-hide-emphasis-markers t)
					; set tab to be org mode tab
  (evil-define-key 'insert 'global (kbd "TAB") 'org-cycle))

;; Org hook to turn off evil auto-indent
;; Can't be in org-mode-setup or it somehow messes with other buffers
;;(add-hook 'org-mode-hook (lambda () (setq evil-auto-indent nil)))

;; Org-bullets
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "✸" "✿" "❀" "❁" "❂" "❃" "❄" "❅" "❆" "❇")))

(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
					;(set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))
  )

					; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Native code snippets
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)
