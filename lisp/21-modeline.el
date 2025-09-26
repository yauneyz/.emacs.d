;;; 21-modeline.el --- A modern and clean modeline setup -*- lexical-binding: t; -*--*-

;;; Commentary:
;; This file contains the configuration for the modeline, using doom-modeline
;; to provide a clean and efficient status bar.

;;; Code:

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35
        doom-modeline-bar-width 5
        doom-modeline-persp-name nil
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count t
        doom-modeline-vcs-max-length 20)

  (setq doom-modeline-segments '(,(propertize "⚡" 'face 'font-lock-keyword-face)
                                     window-number
                                     buffer-info
                                     remote-host
                                     matches
                                     persp-name
                                     t
                                     misc-info
                                     vcs
                                     +cider/modeline-status
                                     macro-recording
                                     major-mode
                                     process
                                     flycheck
                                     lsp
                                     dap
                                     minor-modes)))

;; =============================================================================
;; Cider Status Modeline Integration
;; =============================================================================

(defface cider-modeline-connected
  '((t (:foreground "#a6e22e" :weight bold)))
  "Face for connected Cider status."
  :group 'cider)

(defface cider-modeline-disconnected
  '((t (:foreground "#f92672" :weight bold)))
  "Face for disconnected Cider status."
  :group 'cider)

(defface cider-modeline-connecting
  '((t (:foreground "#e6db74" :weight bold)))
  "Face for connecting Cider status."
  :group 'cider)

(defvar +cider/connection-status 'disconnected
  "Current Cider connection status.")

(defvar +cider/last-eval-time nil
  "Time of last evaluation.")

(defvar +cider/eval-count 0
  "Count of evaluations in this session.")

(defun +cider/update-status ()
  "Update Cider connection status."
  (setq +cider/connection-status
        (cond
         ((not (bound-and-true-p cider-mode)) 'not-clojure)
         ((cider-connected-p) 'connected)
         ((and (bound-and-true-p cider-mode) (cider--connection-info)) 'connecting)
         (t 'disconnected))))

(defun +cider/modeline-status ()
  "Generate modeline status string for Cider."
  (when (derived-mode-p 'clojure-mode)
    (+cider/update-status)
    (let* ((status +cider/connection-status)
           (repl-type (when (eq status 'connected)
                        (when-let ((repl (cider-current-repl)))
                          (cider-repl-type repl))))
           (nrepl-port (when (cider-connected-p) (plist-get (cider--gather-connect-params) :port)))
           (face (pcase status
                   ('connected 'cider-modeline-connected)
                   ('connecting 'cider-modeline-connecting)
                   (_ 'cider-modeline-disconnected)))
           (icon (pcase status
                   ('connected "⚡")
                   ('connecting "⏳")
                   ('disconnected "⚫")
                   ('not-clojure "")))
           (text (pcase status
                   ('connected (format "CIDER:%s" (upcase (symbol-name repl-type))))
                   ('connecting "CIDER:CONN")
                   ('disconnected "CIDER:OFF")
                   ('not-clojure ""))))

      (when (not (eq status 'not-clojure))
        (concat
         " "
         (propertize (concat icon text)
                     'face face
                     'help-echo (pcase status
                                  ('connected
                                   (format "Cider connected (%s REPL on port %s)\nEvals: %d\nLast eval: %s\nClick to switch to REPL"
                                           repl-type nrepl-port +cider/eval-count
                                           (if +cider/last-eval-time
                                               (format-time-string "%H:%M:%S" +cider/last-eval-time)
                                             "never")))
                                  ('connecting "Cider connecting...\nClick to view connection buffer")
                                  ('disconnected "Cider disconnected\nClick to connect"))
                     'mouse-face 'mode-line-highlight
                     'local-map (let ((map (make-sparse-keymap)))
                                  (define-key map [mode-line mouse-1] #'+cider/modeline-click-handler)
                                  map))
         " ")))))

(defun +cider/modeline-click-handler ()
  "Handle clicks on Cider modeline status."
  (interactive)
  (pcase +cider/connection-status
    ('connected (cider-switch-to-repl-buffer))
    ('connecting (pop-to-buffer "*cider-repl localhost*"))
    ('disconnected (cider-connect-clj&cljs))))

(provide '21-modeline)
;;; 21-modeline.el ends here
