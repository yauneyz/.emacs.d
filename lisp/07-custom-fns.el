;;; 07-custom-fns.el --- little helpers & advice  -*- lexical-binding: t; -*-

(defun my-open-init-file ()
  "Open init.el quickly."
  (interactive)
  (find-file user-init-file))

(defun reload-init ()
  "Reload the init.el file with debugging enabled (like --debug-init)."
  (interactive)
  (let ((debug-on-error t))
    (load-file user-init-file))
  (message "init.el reloaded with debugging"))

(defun revert-all-buffers ()
  "Revert every non-modified file buffer."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer :ignore-auto :noconfirm))))
  (message "All buffers reverted"))

(defun evil-select-inside-comment-block ()
  "Evil text-object: inside (comment …) block."
  (interactive)
  (when (and (bound-and-true-p evil-mode)
             (eq evil-state 'normal))
    (save-excursion
      (condition-case nil
          (progn
            (while (not (and (looking-at-p "(comment")
                             (eq (char-after (point)) ?\())
                        (not (bobp)))
              (backward-up-list))
            (let ((start (progn (forward-char 8) (point))))
              (forward-sexp)
              (let ((end (1- (point))))
                (evil-visual-select start end))))
        (error (message "Not inside a (comment ...) block"))))))


(defun my-counsel-projectile-find-file ()
  "Use counsel-projectile-find-file with true fuzzy matching.
Temporarily disable ivy-prescient-mode and force ivy--regex-fuzzy
to allow out-of-order matching like 'eve/mp' for 'events/map'."
  (interactive)
  (let ((ivy-prescient-mode nil)
        (ivy-re-builders-alist '((t . ivy--regex-fuzzy))))
    (counsel-projectile-find-file)))

(defun toggle-shell-buffer (&optional buffer-num other-window)
  "Toggle shell buffer for given BUFFER-NUM (defaults to 1).
If shell buffer doesn't exist, create it as eshell.
If buffer is visible in a window, switch that window to previous buffer.
If buffer is not visible, display it in current window.
If OTHER-WINDOW is non-nil, try to open in another window:
- If multiple windows exist, use an existing other window
- If only one window exists, split and use the new window"
  (interactive "p")
  (let* ((buffer-num (or buffer-num 1))
         (buffer-name (format "shell-buffer-%d" buffer-num))
         (shell-buffer (get-buffer buffer-name)))

    ;; Create buffer if it doesn't exist
    (unless shell-buffer
      (setq shell-buffer (get-buffer-create buffer-name))
      (with-current-buffer shell-buffer
        (eshell-mode)))

    ;; Find window displaying the shell buffer
    (let ((shell-window (get-buffer-window shell-buffer)))
      (if shell-window
          ;; Buffer is visible - switch to previous buffer in that window
          (with-selected-window shell-window
            (switch-to-prev-buffer))
        ;; Buffer is not visible - display it
        (if other-window
            ;; Open in another window
            (if (> (length (window-list)) 1)
                ;; Multiple windows exist - use another window
                (let ((other-win (next-window)))
                  (with-selected-window other-win
                    (switch-to-buffer shell-buffer)))
              ;; Only one window - split and use new window
              (progn
                (split-window-right)
                (other-window 1)
                (switch-to-buffer shell-buffer)))
          ;; Open in current window (original behavior)
          (switch-to-buffer shell-buffer))))))

(defun toggle-command-log ()
  "Toggle command-log-mode and its buffer.
If command-log-mode is enabled, disable it and close the buffer.
If command-log-mode is disabled, enable it and open the buffer."
  (interactive)
  (if (bound-and-true-p command-log-mode)
      ;; Command log mode is enabled - disable it and close buffer
      (progn
        (command-log-mode -1)
        (when (fboundp 'clm/close-command-log-buffer)
          (clm/close-command-log-buffer)))
    ;; Command log mode is disabled - enable it and open buffer
    (progn
      (command-log-mode 1)
      (when (fboundp 'clm/open-command-log-buffer)
        (clm/open-command-log-buffer)))))

;; Last toggled shell buffer tracking
(defvar last-toggled-shell-buffer-number 0
  "The number of the last toggled shell buffer.")

(defun toggle-shell-buffer-and-remember (buffer-number &optional other-window)
  "Toggle shell buffer BUFFER-NUMBER and remember it as the last used.
If OTHER-WINDOW is non-nil, open in other window."
  (setq last-toggled-shell-buffer-number buffer-number)
  (toggle-shell-buffer buffer-number other-window))

(defun toggle-last-shell-buffer ()
  "Toggle the last used shell buffer."
  (interactive)
  (toggle-shell-buffer last-toggled-shell-buffer-number))

(defun dismiss-popup-buffer (&optional names)
  "Close windows showing any buffer in NAMES.
If NAMES is nil, default to \"*compilation*\" and \"*ref*\"."
  (interactive)
  (dolist (name (or names '("*compilation*" "*ref*")))
    (when-let ((win (get-buffer-window name 'visible)))
      (delete-window win))))

;;;; --- Open the file-at-point in vdiff, based on Magit context ----
(defun my/magit-vdiff-open-current ()
  "Open the file at point in vdiff, using the current Magit diff context.
Works in magit-diff/status buffers without asking (no DWIM prompts)."
  (interactive)
  (require 'magit)
  (require 'vdiff-magit)
  (magit-with-toplevel
    (let* ((file (or (magit-current-file)
                     (user-error "No file at point")))
           (dt   (when (derived-mode-p 'magit-diff-mode)
                   (magit-diff-type))))
      (pcase dt
        ;; In a magit-diff buffer with a concrete type:
        ('unstaged  (vdiff-magit-show-unstaged file))
        ('staged    (vdiff-magit-show-staged   file))
        ('committed
         (let* ((range (car magit-refresh-args)) ; e.g. "A..B" or "A...B"
                (revs  (magit-ediff-compare--read-revisions range))
                (rev-a (nth 0 revs))
                (rev-b (nth 1 revs)))
           (vdiff-magit-compare rev-a rev-b file file)))
        (_
         ;; Not in a committed/staged/unstaged diff (e.g., status buffer):
         (vdiff-magit-show-working-tree file))))))

;;;; --- One-shot: show this file’s Magit diff, then immediately vdiff it ----
(defun my/vdiff-magit-this-file ()
  "Show `magit-diff-buffer-file` for the current file, then jump into vdiff."
  (interactive)
  (unless buffer-file-name
    (user-error "Not visiting a file"))
  (require 'magit)
  (require 'vdiff-magit)
  (unless (magit-toplevel)
    (user-error "Not inside a Git repository"))
  ;; Ensure Magit selects the diff window, then vdiff the file at point there.
  (let ((magit-display-buffer-noselect nil))
    (magit-diff-buffer-file))
  (call-interactively #'my/magit-vdiff-open-current))


;;;; Quit vdiff, keep only the working buffer (2-way “undo”)
(defun my/vdiff-quit-to-working ()
  "In a vdiff session, keep only the working tree buffer and exit vdiff.
Kills the other vdiff buffer(s), leaves a single window showing the
working file, and disables `vdiff-mode' in that buffer."
  (interactive)
  (unless (bound-and-true-p vdiff-mode)
    (user-error "Not in vdiff-mode"))

  (require 'seq)
  ;; Find all windows in this frame currently showing vdiff buffers
  (let* ((vdiff-wins (seq-filter
                      (lambda (w)
                        (with-current-buffer (window-buffer w)
                          (bound-and-true-p vdiff-mode)))
                      (window-list (selected-frame))))
         (vdiff-bufs (mapcar #'window-buffer vdiff-wins))
         ;; Heuristic: the *working* buffer is the one visiting a real file
         (work-buf   (or (seq-find (lambda (b)
                                     (with-current-buffer b
                                       (let ((f (buffer-file-name)))
                                         (and f (file-exists-p f)))))
                                   vdiff-bufs)
                         ;; Fallback: if nothing looks like a real file, keep current
                         (current-buffer))))

    ;; Kill all *other* vdiff buffers (HEAD/index/etc.)
    (dolist (b vdiff-bufs)
      (unless (eq b work-buf)
        (when (buffer-live-p b)
          (kill-buffer b))))

    ;; Show only the working buffer in one window
    (delete-other-windows)
    (switch-to-buffer work-buf)

    ;; Make sure we’re no longer in vdiff in the working buffer
    (when (bound-and-true-p vdiff-mode)
      (vdiff-mode -1))

    (message "vdiff closed; kept %s" (buffer-name work-buf))))

(defun yas-reload-snippets ()
  "Recompile and reload all Yasnippet snippets on the fly."
  (interactive)
  (when (featurep 'yasnippet)
    (yas-recompile-all)
    (yas-reload-all)
    (message "✅ Yasnippet snippets recompiled and reloaded.")))



(provide '07-custom-fns)
;;; 07-custom-fns.el ends here
