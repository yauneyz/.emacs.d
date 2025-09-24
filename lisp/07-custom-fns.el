;;; 07-custom-fns.el --- little helpers & advice  -*- lexical-binding: t; -*-

(defun my-open-init-file ()
  "Open init.el quickly."
  (interactive)
  (find-file user-init-file))

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

(provide '07-custom-fns)
;;; 07-custom-fns.el ends here

(defun dismiss-popup-buffer (&optional names)
  "Close windows showing any buffer in NAMES.
If NAMES is nil, default to \"*compilation*\" and \"*ref*\"."
  (interactive)
  (dolist (name (or names '("*compilation*" "*ref*")))
    (when-let ((win (get-buffer-window name 'visible)))
      (delete-window win))))
