;;; 56-cider-enhancements.el --- Enhanced Cider integration and status -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhanced Cider integration that maximizes the use of all Cider features,
;; provides excellent status visibility in the modeline, and creates a
;; seamless Clojure development experience.

;;; Code:

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
    ('disconnected (+bridge/cider-enhanced-connect))))

;; Add to modeline
(add-to-list 'mode-line-misc-info '(:eval (+cider/modeline-status)) t)

;; Update status on connection changes
(add-hook 'cider-connected-hook #'+cider/update-status)
(add-hook 'cider-disconnected-hook #'+cider/update-status)

;; Track evaluations
(defun +cider/track-evaluation (&rest _)
  "Track Cider evaluations for status display."
  (setq +cider/last-eval-time (current-time)
        +cider/eval-count (1+ +cider/eval-count)))

(advice-add 'cider-eval-last-sexp :after #'+cider/track-evaluation)
(advice-add 'cider-eval-buffer :after #'+cider/track-evaluation)

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
_da_: apropos       _gb_: back        _dc_: cache setup _sq_: quit all
_dm_: manual docs   ^                 _dS_: cache status^
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
  ("dm" +cider/fetch-clojuredocs-manually)
  ("dc" +cider/smart-cache-setup)
  ("dS" +cider/cache-status)

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

;; =============================================================================
;; ClojureDocs-specific Optimizations
;; =============================================================================

;; Disable automatic ClojureDocs fetching in eldoc contexts
(defun +cider/disable-clojuredocs-in-eldoc ()
  "Disable ClojureDocs fetching in eldoc to prevent hangs."
  (when (derived-mode-p 'clojure-mode)
    ;; Override eldoc functions to avoid ClojureDocs requests
    (setq-local eldoc-documentation-functions
                (remove 'cider-eldoc eldoc-documentation-functions))
    ;; Use a minimal eldoc function instead
    (add-hook 'eldoc-documentation-functions
              (lambda () nil) ; Return nil to disable documentation
              nil t)))

;; Apply ClojureDocs optimization on Clojure mode
(add-hook 'clojure-mode-hook #'+cider/disable-clojuredocs-in-eldoc)

;; =============================================================================
;; ClojureDocs Cache Debugging and Management
;; =============================================================================

(defun +cider/cache-status ()
  "Check ClojureDocs cache status and locations."
  (interactive)
  (let ((cache-locations (list
                         (expand-file-name "~/.cache/orchard/")
                         (expand-file-name "~/.cache/orchard/clojuredocs/")
                         (expand-file-name "~/.orchard/")
                         (expand-file-name "~/.clojure/")
                         (expand-file-name "~/.m2/repository/cider/orchard/")))
        (results '()))

    (dolist (location cache-locations)
      (push (cons location (file-exists-p location)) results))

    (with-current-buffer (get-buffer-create "*cider-cache-debug*")
      (erase-buffer)
      (insert "=== ClojureDocs Cache Status ===\n\n")

      (insert "Expected cache locations:\n")
      (dolist (result results)
        (insert (format "  %s: %s\n"
                       (car result)
                       (if (cdr result) "EXISTS" "MISSING"))))

      (insert "\nCider connection status:\n")
      (insert (format "  Connected: %s\n" (if (cider-connected-p) "YES" "NO")))
      (when (cider-connected-p)
        (insert (format "  REPL type: %s\n"
                        (if-let ((repl (cider-current-repl)))
                            (cider-repl-type repl)
                          "unknown"))))

      (insert "\nRecent ClojureDocs requests:\n")
      (insert "  (Track manually by watching messages buffer)\n")

      (pop-to-buffer (current-buffer)))

    (message "Cache status displayed in *cider-cache-debug* buffer")))

(defun +cider/create-cache-directories ()
  "Create missing ClojureDocs cache directories."
  (interactive)
  (let ((cache-dirs (list
                    (expand-file-name "~/.cache/orchard/")
                    (expand-file-name "~/.cache/orchard/clojuredocs/"))))

    (dolist (dir cache-dirs)
      (unless (file-exists-p dir)
        (make-directory dir t)
        (message "Created cache directory: %s" dir)))

    (message "Cache directories setup complete")
    (+cider/cache-status)))

(defun +cider/force-cache-refresh ()
  "Force refresh of ClojureDocs cache with error handling."
  (interactive)
  (if (cider-connected-p)
      (progn
        (message "Refreshing ClojureDocs cache...")
        (condition-case err
            (progn
              (cider-clojuredocs-refresh-cache)
              (message "ClojureDocs cache refresh completed"))
          (error
           (message "Cache refresh failed: %s" (error-message-string err))
           (message "Try: M-x +cider/create-cache-directories first"))))
    (message "Cider not connected - connect REPL first")))

;; Manual documentation commands for when you actually need docs
(defun +cider/fetch-clojuredocs-manually ()
  "Manually fetch ClojureDocs for symbol at point with cache info."
  (interactive)
  (if (cider-connected-p)
      (let ((symbol (cider-symbol-at-point)))
        (when symbol
          (message "Fetching ClojureDocs for %s... (check cache first)" symbol)
          (let ((start-time (current-time)))
            (condition-case err
                (progn
                  (cider-clojuredocs symbol)
                  (message "ClojureDocs fetched for %s in %.2f seconds"
                          symbol
                          (float-time (time-subtract (current-time) start-time))))
              (error
               (message "Failed to fetch ClojureDocs for %s: %s"
                       symbol (error-message-string err)))))))
    (message "Cider not connected")))

(defun +cider/batch-cache-common-symbols ()
  "Pre-cache documentation for common Clojure symbols."
  (interactive)
  (if (cider-connected-p)
      (let ((common-symbols '("map" "filter" "reduce" "assoc" "dissoc" "conj" "cons"
                             "first" "rest" "last" "count" "get" "get-in" "update"
                             "update-in" "merge" "into" "apply" "comp" "partial"
                             "defn" "defmacro" "let" "if-let" "when" "when-let")))
        (message "Pre-caching documentation for %d common symbols..." (length common-symbols))
        (let ((cached 0) (failed 0) (start-time (current-time)))

          (dolist (symbol common-symbols)
            (condition-case err
                (progn
                  (cider-sync-request:clojuredocs-lookup "clojure.core" symbol)
                  (setq cached (1+ cached))
                  (message "Cached: %s (%d/%d)" symbol cached (length common-symbols)))
              (error
               (setq failed (1+ failed))
               (message "Failed: %s - %s" symbol (error-message-string err)))))

          (message "Batch caching completed: %d cached, %d failed in %.2f seconds"
                  cached failed
                  (float-time (time-subtract (current-time) start-time)))))
    (message "Cider not connected - connect REPL first")))

(defun +cider/smart-cache-setup ()
  "Smart setup of ClojureDocs cache system."
  (interactive)
  (message "Setting up ClojureDocs cache system...")

  ;; Step 1: Create directories
  (+cider/create-cache-directories)

  ;; Step 2: Check connection
  (unless (cider-connected-p)
    (message "Please connect to Cider REPL first: M-x cider-jack-in")
    (user-error "Cider not connected"))

  ;; Step 3: Force refresh
  (condition-case err
      (progn
        (+cider/force-cache-refresh)
        (sit-for 2) ; Wait for refresh to complete

        ;; Step 4: Pre-cache common symbols
        (+cider/batch-cache-common-symbols)

        (message "ClojureDocs cache setup completed successfully!"))
    (error
     (message "Cache setup failed: %s" (error-message-string err))
     (message "Check network connection and try again"))))

;; =============================================================================
;; Enhanced Monitoring and Modeline Integration
;; =============================================================================

(defvar +cider/cache-monitoring-enabled nil
  "Whether cache monitoring is enabled.")

(defvar +cider/last-cache-check-time nil
  "Time of last cache check.")

(defun +cider/monitor-cache-performance ()
  "Monitor ClojureDocs cache performance and display in modeline."
  (when (and (derived-mode-p 'clojure-mode) +cider/cache-monitoring-enabled)
    (setq +cider/last-cache-check-time (current-time))
    ;; Add cache info to existing modeline status
    (let ((cache-exists (file-exists-p (expand-file-name "~/.cache/orchard/clojuredocs/"))))
      (when (not cache-exists)
        (message "ClojureDocs cache missing - run M-x +cider/smart-cache-setup")))))

(defun +cider/toggle-cache-monitoring ()
  "Toggle ClojureDocs cache monitoring."
  (interactive)
  (setq +cider/cache-monitoring-enabled (not +cider/cache-monitoring-enabled))
  (message "ClojureDocs cache monitoring %s"
          (if +cider/cache-monitoring-enabled "enabled" "disabled")))

;; Enhanced modeline integration
(defun +cider/enhanced-modeline-status ()
  "Enhanced modeline status with cache info."
  (when (derived-mode-p 'clojure-mode)
    (+cider/update-status)
    (let* ((base-status (+cider/modeline-status))
           (cache-exists (file-exists-p (expand-file-name "~/.cache/orchard/clojuredocs/")))
           (cache-indicator (if cache-exists "📚" "❌")))

      (when base-status
        (concat base-status
                (propertize cache-indicator
                           'help-echo (format "ClojureDocs cache: %s\nClick for cache management"
                                            (if cache-exists "Available" "Missing"))
                           'mouse-face 'mode-line-highlight
                           'local-map (let ((map (make-sparse-keymap)))
                                        (define-key map [mode-line mouse-1] #'+cider/cache-status)
                                        map)))))))

;; Replace the original modeline function
(setq mode-line-misc-info
      (remove '(:eval (+cider/modeline-status)) mode-line-misc-info))
(add-to-list 'mode-line-misc-info '(:eval (+cider/enhanced-modeline-status)) t)

;; Add debugging keymap
(defvar +cider/debug-map (make-sparse-keymap)
  "Keymap for Cider debugging commands.")

(define-key +cider/debug-map (kbd "c s") #'+cider/cache-status)
(define-key +cider/debug-map (kbd "c c") #'+cider/smart-cache-setup)
(define-key +cider/debug-map (kbd "c r") #'+cider/force-cache-refresh)
(define-key +cider/debug-map (kbd "c m") #'+cider/toggle-cache-monitoring)
(define-key +cider/debug-map (kbd "c b") #'+cider/batch-cache-common-symbols)

;; Initialize status tracking
(+cider/update-status)

(provide '56-cider-enhancements)
;;; 56-cider-enhancements.el ends here