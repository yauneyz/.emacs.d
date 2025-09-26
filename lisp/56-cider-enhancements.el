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
      cider-repl-pop-to-buffer-on-connect 'display-only
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

(defun +cider/eval-and-replace ()
  "Evaluate last sexp and replace it with the result."
  (interactive)
  (let ((last-sexp (cider-last-sexp)))
    (cider-eval-last-sexp-and-replace)))

(defun +cider/eval-buffer-up-to-point ()
  "Evaluate buffer from beginning up to point."
  (interactive)
  (cider-eval-region (point-min) (point)))

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

(defun +cider/rerun-failed-tests ()
  "Rerun only failed tests."
  (interactive)
  (cider-test-rerun-failed-tests))

(defun +cider/test-with-profile ()
  "Run tests with a specific Leiningen profile."
  (interactive)
  (let ((profile (read-string "Profile (empty for default): ")))
    (if (string-empty-p profile)
        (cider-test-run-project-tests)
      (let ((cider-lein-parameters (concat cider-lein-parameters " with-profile " profile)))
        (cider-test-run-project-tests)))))

;; =============================================================================
;; Enhanced Debugging Functions
;; =============================================================================

(defun +cider/debug-instrument-defun ()
  "Instrument function at point for debugging."
  (interactive)
  (cider-eval-defun-at-point '(16))) ; C-u C-u prefix

(defun +cider/debug-locals ()
  "Show local variables in debug context."
  (interactive)
  (when cider--debug-mode
    (cider-debug-mode-send-reply ":locals")))

;; =============================================================================
;; Enhanced Documentation and Inspection
;; =============================================================================

(defun +cider/doc-and-source ()
  "Show both documentation and source for symbol at point."
  (interactive)
  (cider-doc)
  (cider-find-var))

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

;; =============================================================================
;; Enhanced Refactoring Integration
;; =============================================================================

(defun +cider/extract-to-def ()
  "Extract expression to def."
  (interactive)
  (if (fboundp 'cljr-extract-def)
      (cljr-extract-def)
    (message "clj-refactor not available")))

(defun +cider/inline-symbol ()
  "Inline symbol at point."
  (interactive)
  (if (fboundp 'cljr-inline-symbol)
      (cljr-inline-symbol)
    (message "clj-refactor not available")))

;; =============================================================================
;; Enhanced Error Handling
;; =============================================================================

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

;; =============================================================================
;; Enhanced REPL Interaction
;; =============================================================================

(defun +cider/send-to-repl ()
  "Send current form to REPL without evaluating."
  (interactive)
  (let ((form (cider-sexp-at-point)))
    (cider-switch-to-repl-buffer)
    (goto-char (point-max))
    (insert form)))

(defun +cider/repl-set-ns-to-current ()
  "Set REPL namespace to current buffer's namespace."
  (interactive)
  (cider-repl-set-ns (cider-current-ns)))

(defun +cider/load-current-ns-in-repl ()
  "Load current namespace in REPL."
  (interactive)
  (cider-load-buffer)
  (cider-repl-set-ns (cider-current-ns)))

;; =============================================================================
;; Cider Hydra for Complete Control
;; =============================================================================

(defhydra +cider/hydra (:color blue :hint nil)
  "
^REPL^              ^Eval^            ^Test^            ^Debug^
─────────────────────────────────────────────────────────────────
_rr_: jack-in       _el_: last sexp   _tt_: current     _db_: instrument
_rR_: enhanced      _eb_: buffer      _tn_: namespace   _dl_: show locals
_rs_: switch        _er_: and replace _tp_: project     _ds_: stacktrace
_rc_: clear         _eu_: up to point _tf_: failed      ^
_rq_: quit          _es_: send to repl _tw_: with profile^
^                   ^                 _tr_: rerun       ^
^Doc/Inspect^       ^Navigation^      ^Refactor^        ^Utils^
─────────────────────────────────────────────────────────────────
_dd_: doc           _gg_: definition  _re_: extract def _ss_: status
_ds_: doc + source  _gr_: references  _ri_: inline      _si_: info
_di_: inspect       _gn_: namespace   _rn_: rename      _sp_: profile

^                   ^                 ^                 ^
^Format^            ^Error^           ^Load^            ^Control^
─────────────────────────────────────────────────────────────────
_ff_: format        _ee_: show error  _ll_: load buffer _q_: quit
_fi_: indent        _ec_: copy error  _ln_: load ns     _?_: help
_fa_: align         _ep_: pretty trace _lf_: load file  ^
"
  ;; REPL
  ("rr" cider-jack-in)
  ("rR" +cider/enhanced-jack-in)
  ("rs" cider-switch-to-repl-buffer)
  ("rc" +cider/switch-to-repl-and-clear)
  ("rq" cider-quit)

  ;; Eval
  ("el" cider-eval-last-sexp)
  ("eb" cider-eval-buffer)
  ("er" +cider/eval-and-replace)
  ("eu" +cider/eval-buffer-up-to-point)
  ("es" +cider/send-to-repl)

  ;; Test
  ("tt" +cider/test-current-and-focus)
  ("tn" +cider/test-ns-and-focus)
  ("tp" cider-test-run-project-tests)
  ("tf" +cider/rerun-failed-tests)
  ("tw" +cider/test-with-profile)
  ("tr" cider-test-rerun-tests)

  ;; Debug
  ("db" +cider/debug-instrument-defun)
  ("dl" +cider/debug-locals)
  ("ds" +cider/pretty-print-stacktrace)

  ;; Documentation/Inspect
  ("dd" cider-doc)
  ("ds" +cider/doc-and-source)
  ("di" +cider/inspect-last-result-in-buffer)
  ("da" +cider/apropos-documentation)


  ;; Navigation
  ("gg" cider-find-var)
  ("gr" cider-xref-fn-refs)
  ("gn" cider-find-ns)
  ("gb" cider-pop-back)

  ;; Refactor (if clj-refactor available)
  ("re" +cider/extract-to-def)
  ("ri" +cider/inline-symbol)
  ("rn" cider-rename-symbol)

  ;; Format
  ("ff" cider-format-buffer)
  ("fi" cider-format-defun)
  ("fa" cider-format-region)

  ;; Error
  ("ee" cider-selector)
  ("ec" +cider/copy-last-error)
  ("ep" +cider/pretty-print-stacktrace)

  ;; Load
  ("ll" cider-load-buffer)
  ("ln" +cider/load-current-ns-in-repl)
  ("lf" cider-load-file)

  ;; Utils
  ("ss" +bridge/status)
  ("si" +bridge/debug-info)
  ("sp" cider-profile-toggle)
  ("sq" +bridge/cider-disconnect-all)

  ;; Control
  ("?" (lambda () (interactive) (describe-function '+cider/hydra/body)) :color red)
  ("q" nil))

;; =============================================================================
;; Auto-completion Enhancements
;; =============================================================================

(defun +cider/company-complete-or-indent ()
  "Complete at point or indent line."
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
    (indent-for-tab-command)))

;; =============================================================================
;; Keybinding Integration
;; =============================================================================

(defun +cider/setup-enhanced-keys ()
  "Set up enhanced Cider keybindings."
  (when (derived-mode-p 'clojure-mode)
    (local-set-key (kbd "<f12>") #'+cider/hydra/body)
    (local-set-key (kbd "C-c C-h") #'+cider/hydra/body)
    (local-set-key (kbd "C-c C-t t") #'+cider/test-current-and-focus)
    (local-set-key (kbd "C-c C-t n") #'+cider/test-ns-and-focus)
    (local-set-key (kbd "C-c C-d s") #'+cider/doc-and-source)
    (local-set-key (kbd "C-c C-r") #'+cider/enhanced-jack-in)))

(add-hook 'cider-mode-hook #'+cider/setup-enhanced-keys)

;; =============================================================================
;; Session Management
;; =============================================================================

(defun +cider/save-session ()
  "Save current Cider session state."
  (interactive)
  (when (cider-connected-p)
    (let ((session-file (expand-file-name ".cider-session" (project-root (project-current)))))
      (with-temp-file session-file
        (insert (format ";;; Cider Session - %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
        (insert (format ";; Project: %s\n" (project-root (project-current))))
        (insert (format ";; REPL Type: %s\n"
                        (if-let ((repl (cider-current-repl)))
                            (cider-repl-type repl)
                          "unknown")))
        (insert (format ";; Namespace: %s\n" (cider-current-ns)))
        (insert ";; Add session restoration code here\n"))
      (message "Session saved to %s" session-file))))


;; Initialize status tracking
(+cider/update-status)

(provide '56-cider-enhancements)
;;; 56-cider-enhancements.el ends here