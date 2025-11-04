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
  (diff-hl-mode -1)
  (define-key org-mode-map (kbd "C-<right>") 'org-metaright)
  (define-key org-mode-map (kbd "C-<left>") 'org-metaleft)
  (define-key org-mode-map (kbd "C-<up>") 'org-metaup)
  (define-key org-mode-map (kbd "C-<down>") 'org-metadown))

(defun my/olivetti-top-margin ()
  (setq header-line-format (propertize " " 'display '(height 2))))
(add-hook 'olivetti-mode-hook #'my/olivetti-top-margin)

;; Org mode
(use-package org
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-ellipsis " ▼"
	org-hide-emphasis-markers t
        org-startup-indented t
	org-link-descriptive t
        org-image-actual-width '(300)
        org-cycle-separator-lines 0
        org-directory (expand-file-name "org" (getenv "HOME"))
        org-default-notes-file (expand-file-name "inbox.org" org-directory)
        org-agenda-files (list (expand-file-name "inbox.org" org-directory)
                               (expand-file-name "agenda" org-directory)
                               (expand-file-name "projects" org-directory))
        org-log-done 'time
        org-log-into-drawer t)

  (add-hook 'org-mode-hook #'org-restart-font-lock)
  (org-link-set-parameters
   "color"
   :face  (lambda (path) (list :foreground path))
   :export (lambda (path desc backend)
             (pcase backend
               ('html  (format "<span style=\"color:%s;\">%s</span>" path (or desc "")))
               ('latex (format "\\textcolor{%s}{%s}" path (or desc "")))
               (_ (or desc "")))))

  (org-link-set-parameters
   "bg"
   :face  (lambda (path) (list :background path :extend t))
   :export (lambda (path desc backend)
             (pcase backend
               ('html  (format "<span style=\"background-color:%s;\">%s</span>" path (or desc "")))
               ('latex (format "\\colorbox{%s}{%s}" path (or desc "")))
               (_ (or desc "")))))

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

;; --- Persistent color markup and hydra --------------------------------------
;; Define inline link types that render with colored faces and export cleanly.
(with-eval-after-load 'org
  (org-link-set-parameters
   "color"
   :face  (lambda (path) `(:foreground ,path))
   :export (lambda (path desc backend)
             (pcase backend
               ('html  (format "<span style=\"color:%s;\">%s</span>" path (or desc "")))
               ('latex (format "\\textcolor{%s}{%s}" path (or desc "")))
               (_ (or desc "")))))

  (org-link-set-parameters
   "bg"
   :face  (lambda (path) `(:background ,path :extend t))
   :export (lambda (path desc backend)
             (pcase backend
               ('html  (format "<span style=\"background-color:%s;\">%s</span>" path (or desc "")))
               ('latex (format "\\colorbox{%s}{%s}" path (or desc "")))
               (_ (or desc ""))))))

(defun my/org--require-org ()
  (unless (derived-mode-p 'org-mode)
    (user-error "This color command only works in Org buffers")))

(defun my/org--wrap-region-as-link (type color)
  (interactive)
  (my/org--require-org)
  (unless (use-region-p) (user-error "Select a region first"))
  (let* ((beg (region-beginning))
         (end (region-end))
         (text (buffer-substring-no-properties beg end)))
    ;; NOTE: Org link descriptions cannot contain the literal "]]".
    (when (string-match-p "\\]\\]" text)
      (user-error "Selection contains \"]]\" which breaks Org link syntax; use a color block for long spans"))
    (delete-region beg end)
    (insert (format "[[%s:%s][%s]]" type color text))
    (deactivate-mark)))

(defun my/org-colorize-region (color)
  "Color foreground of region using [[color:COLOR][...]]."
  (interactive (list (read-string "Color name or #hex: ")))
  (my/org--wrap-region-as-link "color" color))

(defun my/org-bgcolorize-region (color)
  "Color background of region using [[bg:COLOR][...]]."
  (interactive (list (read-string "Background color or #hex: ")))
  (my/org--wrap-region-as-link "bg" color))

(defun my/org-uncolorize-at-point ()
  "If point is on a [[color:...]] or [[bg:...]] Org link, unwrap it."
  (interactive)
  (let* ((el (org-element-context)))
    (when (eq (org-element-type el) 'link)
      (let ((type (org-element-property :type el)))
        (when (member type '("color" "bg"))
          (let* ((beg (org-element-property :begin el))
                 (end (org-element-property :end el))
                 (contents (org-element-interpret-data (org-element-contents el))))
            (save-excursion
              (goto-char beg)
              (delete-region beg end)
              (insert contents))))))))

;; Color hydra opened from Evil visual state with <leader>c (Org buffers only).
(use-package hydra :ensure t)
(defhydra my/hydra-org-color (:hint nil :color blue)
  "
Color  _r_ red  _g_ green  _b_ blue  _y_ yellow  _o_ orange  _p_ purple  _c_ cyan  _m_ magenta  _k_ black  _w_ white  _t_ tan  _v_ violet
BG     _R_ red  _G_ green  _B_ blue  _Y_ yellow  _O_ orange  _P_ purple  _C_ cyan  _M_ magenta  _K_ black  _W_ white  _T_ tan  _V_ violet
Other  _x_ custom color     _X_ custom background     _u_ uncolor at point     _q_ quit
"
  ;; Foreground presets
  ("r" (my/org-colorize-region "red"))
  ("g" (my/org-colorize-region "green"))
  ("b" (my/org-colorize-region "blue"))
  ("y" (my/org-colorize-region "yellow"))
  ("o" (my/org-colorize-region "orange"))
  ("p" (my/org-colorize-region "purple"))
  ("c" (my/org-colorize-region "cyan"))
  ("m" (my/org-colorize-region "magenta"))
  ("k" (my/org-colorize-region "black"))
  ("w" (my/org-colorize-region "white"))
  ("t" (my/org-colorize-region "tan"))
  ("v" (my/org-colorize-region "violet"))
  ;; Background presets
  ("R" (my/org-bgcolorize-region "red"))
  ("G" (my/org-bgcolorize-region "green"))
  ("B" (my/org-bgcolorize-region "blue"))
  ("Y" (my/org-bgcolorize-region "yellow"))
  ("O" (my/org-bgcolorize-region "orange"))
  ("P" (my/org-bgcolorize-region "purple"))
  ("C" (my/org-bgcolorize-region "cyan"))
  ("M" (my/org-bgcolorize-region "magenta"))
  ("K" (my/org-bgcolorize-region "black"))
  ("W" (my/org-bgcolorize-region "white"))
  ("T" (my/org-bgcolorize-region "tan"))
  ("V" (my/org-bgcolorize-region "violet"))
  ;; Custom & utilities
  ("x" my/org-colorize-region)
  ("X" my/org-bgcolorize-region)
  ("u" my/org-uncolorize-at-point)
  ("q" nil "quit"))


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


(use-package anki-editor
  :ensure t
  :commands (anki-editor-mode anki-editor-push-notes)
  :custom
  ;; auto-create decks if missing (optional)
  (anki-editor-create-decks t))
