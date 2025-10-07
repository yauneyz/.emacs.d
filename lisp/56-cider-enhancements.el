;;; 56-cider-enhancements.el --- Enhanced Cider integration and status -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhanced Cider integration that maximizes the use of all Cider features,
;; provides excellent status visibility in the modeline, and creates a
;; seamless Clojure development experience.

;;; Code:



;; =============================================================================
;; Enhanced Cider Configuration
;; =============================================================================

;; Enhanced REPL configuration
(setq cider-repl-history-file "~/.emacs.d/.cider-repl-history"
      cider-repl-history-size 1000
      cider-repl-wrap-history t
      cider-repl-history-separator "\n---\n"
      cider-repl-pop-to-buffer-on-connect nil
      cider-repl-display-help-banner nil
      cider-repl-buffer-size-limit 10000)

;; Enhanced evaluation settings (optimized for performance)
(setq cider-show-error-buffer t
      cider-auto-select-error-buffer nil ; Don't auto-select to reduce UI churn
      cider-show-eval-spinner nil ; Disable spinner to reduce overhead
      cider-overlays-use-font-lock t
      cider-prompt-for-symbol nil
      cider-doc-display-file nil) ; Don't display doc files automatically

;; Enhanced debugging settings
(setq cider-debug-use-overlays t
      cider-debug-prompt 'overlay
      cider-debug-print-level 10
      cider-debug-print-length 10)

;; Enhanced completion settings
(setq cider-completion-annotations-include-ns t
      cider-annotate-completion-candidates t
      cider-completion-annotations-alist
      '(("class" "c")
        ("field" "f")
        ("function" "ƒ")
        ("keyword" "k")
        ("local" "l")
        ("macro" "m")
        ("method" "M")
        ("namespace" "n")
        ("protocol" "p")
        ("protocol-function" "pf")
        ("record" "r")
        ("special-form" "s")
        ("static-field" "sf")
        ("static-method" "sm")
        ("type" "t")
        ("var" "v")))

;; Enhanced test integration
(setq cider-test-show-report-on-success t
      cider-auto-select-test-report-buffer t
      cider-test-defining-forms '("deftest" "defspec" "defcheck")
      cider-test-fail-fast nil)

;; =============================================================================
;; Enhanced REPL Functions
;; =============================================================================

(defun +cider/enhanced-jack-in ()
  "Enhanced jack-in with better project detection and options."
  (interactive)
  (let* ((project-type (cond
                        ((file-exists-p "shadow-cljs.edn") 'shadow-cljs)
                        ((file-exists-p "deps.edn") 'deps)
                        ((file-exists-p "project.clj") 'lein)
                        (t 'generic)))
         (repl-type (completing-read "REPL type: "
                                     (pcase project-type
                                       ('shadow-cljs '("shadow-cljs" "shadow-node"))
                                       (_ '("clj" "cljs")))
                                     nil t)))

    (message "Starting %s REPL for %s project..." repl-type project-type)

    (pcase (list project-type repl-type)
      ('(shadow-cljs "shadow-cljs") (cider-jack-in-cljs '(:cljs-repl-type shadow)))
      ('(shadow-cljs "shadow-node") (cider-jack-in-cljs '(:cljs-repl-type shadow-node)))
      ('(_ "cljs") (cider-jack-in-cljs))
      (_ (cider-jack-in-clj)))))

(defun +cider/switch-to-repl-and-clear ()
  "Switch to REPL buffer and clear it."
  (interactive)
  (cider-switch-to-repl-buffer)
  (cider-repl-clear-buffer))

;; =============================================================================
;; Enhanced Testing Functions
;; =============================================================================

(defun +cider/test-current-and-focus ()
  "Run current test and focus on report buffer."
  (interactive)
  (cider-test-run-test)
  (when-let ((buf (get-buffer (cider-test-report-buffer))))
    (pop-to-buffer buf)))

(defun +cider/test-ns-and-focus ()
  "Run namespace tests and focus on report buffer."
  (interactive)
  (cider-test-run-ns-tests)
  (when-let ((buf (get-buffer (cider-test-report-buffer))))
    (pop-to-buffer buf)))

(defun +cider/inspect-last-result-in-buffer ()
  "Inspect last result in a dedicated buffer."
  (interactive)
  (cider-inspect-last-result)
  (when-let ((buf (get-buffer "*cider-inspect*")))
    (with-current-buffer buf
      (rename-buffer (format "*cider-inspect-%s*" (format-time-string "%H:%M:%S"))))
    (pop-to-buffer buf)))

(defun +cider/apropos-documentation ()
  "Search documentation instead of just names."
  (interactive)
  (cider-apropos-documentation))

(defun +cider/pretty-print-stacktrace ()
  "Pretty print the last stacktrace."
  (interactive)
  (when-let ((buf (get-buffer "*cider-error*")))
    (with-current-buffer buf
      (cider-stacktrace-toggle-all))))

(defun +cider/copy-last-error ()
  "Copy last error to clipboard."
  (interactive)
  (when-let ((buf (get-buffer "*cider-error*")))
    (with-current-buffer buf
      (kill-ring-save (point-min) (point-max))
      (message "Error copied to clipboard"))))

;; Initialize status tracking
(+cider/update-status)

(provide '56-cider-enhancements)
;;; 56-cider-enhancements.el ends here
