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
  (setq org-ellipsis " ▼"
	org-hide-emphasis-markers t
        org-startup-indented t
        org-image-actual-width '(300)
        org-cycle-separator-lines 0
        org-directory (expand-file-name "org" (getenv "HOME"))
        org-default-notes-file (expand-file-name "inbox.org" org-directory)
        org-agenda-files (list (expand-file-name "inbox.org" org-directory)
                               (expand-file-name "agenda" org-directory)
                               (expand-file-name "projects" org-directory))
        org-log-done 'time
        org-log-into-drawer t)

  (setq org-capture-templates
        '(("t" "Task" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  CREATED: %U\n  %a" :empty-lines 1)
          ("n" "Note" entry (file+headline org-default-notes-file "Notes")
           "* %?\n  %U\n  %a" :empty-lines 1)
          ("j" "Journal" entry (file+datetree (expand-file-name "journal.org" org-directory))
           "* %<%H:%M> %?" :empty-lines 1)))

  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT" ((org-agenda-overriding-header "Next Actions")))
            (todo "TODO" ((org-agenda-overriding-header "Backlog")))))))

					; set tab to be org mode tab
  ;; (evil-define-key 'insert 'global (kbd "TAB") #'org-cycle)

)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "✸" "✿" "❀" "❁" "❂" "❃" "❄" "❅" "❆" "❇")))

;; Org hook to turn off evil auto-indent
;; Can't be in org-mode-setup or it somehow messes with other buffers
;;(add-hook 'org-mode-hook (lambda () (setq evil-auto-indent nil)))

;; (use-package org-modern
;;   :after org
;;   :hook (org-mode . org-modern-mode)
;;   :config
;;   (setq org-modern-table nil
;;         org-modern-hide-stars "•"))

;; (use-package org-appear
;;   :after org
;;   :hook (org-mode . org-appear-mode)
;;   :config
;;   (setq org-appear-autolinks t
;;         org-appear-trigger 'manual))

(use-package org-super-agenda
  :after org
  :config
  (org-super-agenda-mode 1)
  (setq org-super-agenda-groups
        '((:name "Today" :time-grid t :scheduled today)
          (:name "Due Soon" :deadline future)
          (:name "Important" :priority "A")
          (:name "Waiting" :todo "WAIT"))))

(use-package org-roam
  :after org
  :ensure t
  :custom
  (org-roam-directory (file-truename (expand-file-name "roam" org-directory)))
  (org-roam-completion-everywhere t)
  :config
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))
  (org-roam-db-autosync-mode))

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
