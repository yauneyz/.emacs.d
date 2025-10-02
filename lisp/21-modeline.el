;;; 21-modeline.el --- A modern and clean modeline setup -*- lexical-binding: t; -*--*-


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
         ((and (fboundp 'cider-connected-p) (cider-connected-p)) 'connected)
         ((and (bound-and-true-p cider-mode) (fboundp 'cider--connection-info) (cider--connection-info)) 'connecting)
         (t 'disconnected))))

(defun +cider/modeline-status ()
  "Generate modeline status string for Cider."
  (condition-case err
      (if (derived-mode-p 'clojure-mode 'clojurescript-mode 'clojurec-mode)
          (progn
            (+cider/update-status)
            (let* ((status +cider/connection-status)
                   (repl-type (when (eq status 'connected)
                                (condition-case nil
                                    (when-let ((repl (cider-current-repl)))
                                      (cider-repl-type repl))
                                  (error nil))))
                   (nrepl-port (when (eq status 'connected)
                                 (condition-case nil
                                     (plist-get (cider--gather-connect-params) :port)
                                   (error nil))))
                   (face (pcase status
                           ('connected 'cider-modeline-connected)
                           ('connecting 'cider-modeline-connecting)
                           (_ 'cider-modeline-disconnected)))
                   (icon (pcase status
                           ('connected "⚡")
                           ('connecting "⏳")
                           ('disconnected "⚫")))
                   (text (pcase status
                           ('connected (if repl-type
                                           (format "CIDER:%s" (upcase (symbol-name repl-type)))
                                         "CIDER:CONNECTED"))
                           ('connecting "CIDER:CONN")
                           ('disconnected "CIDER:OFF"))))

              (concat
               " "
               (propertize (concat icon text " [STATUS:" (symbol-name status) "]")
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
               " ")))
        "")
    (error (format " [CIDER-ERROR: %s] " err))))

(defun +cider/modeline-click-handler ()
  "Handle clicks on Cider modeline status."
  (interactive)
  (pcase +cider/connection-status
    ('connected (cider-switch-to-repl-buffer))
    ('connecting (pop-to-buffer "*cider-repl localhost*"))
    ('disconnected (cider-connect-clj&cljs))))

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

  ;; Define custom cider segment
  (doom-modeline-def-segment cider-status
    "Display CIDER connection status in the modeline."
    (+cider/modeline-status))

  ;; Add cider-status to the main modeline
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp cider-status minor-modes input-method indent-info buffer-encoding major-mode process vcs check time)))


(provide '21-modeline)
;;; 21-modeline.el ends here
