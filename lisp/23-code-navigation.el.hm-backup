
;; Bookmarks

;; Put bookmarks in a dedicated file
(setq bookmark-default-file "~/.emacs.d/bookmarks")

;; Save after each change so crashes can’t lose them
(setq bookmark-save-flag 1)

;; Other stuff

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
