;;; 54-lang-debugging.el --- Universal debugging system -*- lexical-binding: t; -*-

;;; Commentary:
;; Universal debugging interface that works with both Cider (for Clojure)
;; and DAP (Debug Adapter Protocol) for other languages, providing consistent
;; debugging operations across all programming languages.

;;; Code:

(require 'hydra)

;; =============================================================================
;; Debug State Management
;; =============================================================================

(defvar +debug/session-active nil
  "Whether a debug session is currently active.")

(defvar +debug/breakpoints (make-hash-table :test 'equal)
  "Hash table storing breakpoint information.")

(defvar +debug/current-backend nil
  "Currently active debugging backend.")

;; =============================================================================
;; Debug Backend Detection
;; =============================================================================

(defun +debug/get-backend ()
  "Get the appropriate debugging backend for current language."
  (pcase major-mode
    ((or 'clojure-mode 'clojurescript-mode 'clojurec-mode)
     (if (and (bound-and-true-p cider-mode) (cider-connected-p))
         'cider
       'none))
    ((or 'go-mode 'go-ts-mode)
     (if (bound-and-true-p dap-mode)
         'dap-go
       'none))
    ('python-mode
     (if (bound-and-true-p dap-mode)
         'dap-python
       'none))
    ('rust-mode
     (if (bound-and-true-p dap-mode)
         'dap-rust
       'none))
    ('java-mode
     (if (bound-and-true-p dap-mode)
         'dap-java
       'none))
    (_ 'none)))

(defun +debug/backend-available? ()
  "Check if debugging backend is available."
  (not (eq (+debug/get-backend) 'none)))

(defun +debug/session-active? ()
  "Check if debug session is active."
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ('cider (and (cider-connected-p) cider--debug-mode))
      ((or 'dap-go 'dap-python 'dap-rust 'dap-java)
       (and (bound-and-true-p dap-mode) (dap--cur-session)))
      (_ nil))))

;; =============================================================================
;; Universal Debug Operations
;; =============================================================================

(defun +debug/start ()
  "Start debugging session."
  (interactive)
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ('cider (+debug/cider-start))
      ('dap-go (+debug/dap-go-start))
      ('dap-python (+debug/dap-python-start))
      ('dap-rust (+debug/dap-rust-start))
      ('dap-java (+debug/dap-java-start))
      ('none (message "No debugging backend available for %s" major-mode)))
    (setq +debug/current-backend backend)))

(defun +debug/stop ()
  "Stop debugging session."
  (interactive)
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ('cider (+debug/cider-stop))
      ((or 'dap-go 'dap-python 'dap-rust 'dap-java) (+debug/dap-stop))
      (_ (message "No active debug session")))
    (setq +debug/session-active nil +debug/current-backend nil)))

(defun +debug/toggle-breakpoint ()
  "Toggle breakpoint at current line."
  (interactive)
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ('cider (+debug/cider-toggle-breakpoint))
      ((or 'dap-go 'dap-python 'dap-rust 'dap-java) (+debug/dap-toggle-breakpoint))
      (_ (message "Breakpoints not supported for %s" major-mode)))))

(defun +debug/continue ()
  "Continue execution."
  (interactive)
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ('cider (cider-debug-defun-at-point))  ; Cider doesn't have explicit continue
      ((or 'dap-go 'dap-python 'dap-rust 'dap-java) (dap-continue))
      (_ (message "No active debug session")))))

(defun +debug/step-next ()
  "Step to next line."
  (interactive)
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ('cider (message "Use 'n' in Cider debug prompt"))
      ((or 'dap-go 'dap-python 'dap-rust 'dap-java) (dap-next))
      (_ (message "No active debug session")))))

(defun +debug/step-into ()
  "Step into function."
  (interactive)
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ('cider (message "Use 'i' in Cider debug prompt"))
      ((or 'dap-go 'dap-python 'dap-rust 'dap-java) (dap-step-in))
      (_ (message "No active debug session")))))

(defun +debug/step-out ()
  "Step out of current function."
  (interactive)
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ('cider (message "Use 'o' in Cider debug prompt"))
      ((or 'dap-go 'dap-python 'dap-rust 'dap-java) (dap-step-out))
      (_ (message "No active debug session")))))

;; =============================================================================
;; Cider Debug Functions
;; =============================================================================

(defun +debug/cider-start ()
  "Start Cider debugging."
  (if (cider-connected-p)
      (progn
        (cider-debug-defun-at-point)
        (setq +debug/session-active t)
        (message "Cider debug session started"))
    (message "Cider not connected. Connect REPL first.")))

(defun +debug/cider-stop ()
  "Stop Cider debugging."
  (when (bound-and-true-p cider--debug-mode)
    (cider--debug-quit)
    (setq +debug/session-active nil)
    (message "Cider debug session stopped")))

(defun +debug/cider-toggle-breakpoint ()
  "Toggle Cider breakpoint."
  (if (cider-connected-p)
      (let ((line (line-number-at-pos))
            (file buffer-file-name))
        (save-excursion
          (beginning-of-line)
          (if (looking-at ".*#dbg.*")
              (progn
                (replace-regexp-in-region "#dbg " "" (line-beginning-position) (line-end-position))
                (message "Breakpoint removed at line %d" line))
            (progn
              (skip-chars-forward " \t")
              (insert "#dbg ")
              (message "Breakpoint set at line %d" line)))))
    (message "Cider not connected")))

;; =============================================================================
;; DAP Debug Functions
;; =============================================================================

(defun +debug/dap-go-start ()
  "Start Go debugging with DAP."
  (if (and (bound-and-true-p dap-mode) (executable-find "dlv"))
      (progn
        (dap-debug
         (list :type "go"
               :request "launch"
               :name "Launch Go Program"
               :mode "auto"
               :program (buffer-file-name)))
        (setq +debug/session-active t)
        (message "Go debug session started"))
    (message "DAP not available or dlv (delve) not installed")))

(defun +debug/dap-python-start ()
  "Start Python debugging with DAP."
  (if (bound-and-true-p dap-mode)
      (progn
        (dap-debug
         (list :type "python"
               :request "launch"
               :name "Launch Python Program"
               :program (buffer-file-name)))
        (setq +debug/session-active t)
        (message "Python debug session started"))
    (message "DAP not available for Python")))

(defun +debug/dap-rust-start ()
  "Start Rust debugging with DAP."
  (if (bound-and-true-p dap-mode)
      (progn
        (dap-debug
         (list :type "rust"
               :request "launch"
               :name "Launch Rust Program"))
        (setq +debug/session-active t)
        (message "Rust debug session started"))
    (message "DAP not available for Rust")))

(defun +debug/dap-java-start ()
  "Start Java debugging with DAP."
  (if (bound-and-true-p dap-mode)
      (progn
        (dap-java-debug
         (list :type "java"
               :request "launch"
               :name "Launch Java Program"))
        (setq +debug/session-active t)
        (message "Java debug session started"))
    (message "DAP not available for Java")))

(defun +debug/dap-stop ()
  "Stop DAP debugging session."
  (when (dap--cur-session)
    (dap-disconnect (dap--cur-session))
    (setq +debug/session-active nil)
    (message "Debug session stopped")))

(defun +debug/dap-toggle-breakpoint ()
  "Toggle DAP breakpoint at current line."
  (if (bound-and-true-p dap-mode)
      (dap-breakpoint-toggle)
    (message "DAP mode not active")))

;; =============================================================================
;; Debug Information and Inspection
;; =============================================================================

(defun +debug/inspect-variable ()
  "Inspect variable at point."
  (interactive)
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ('cider (+debug/cider-inspect-variable))
      ((or 'dap-go 'dap-python 'dap-rust 'dap-java) (+debug/dap-inspect-variable))
      (_ (message "Variable inspection not available")))))

(defun +debug/cider-inspect-variable ()
  "Inspect variable using Cider."
  (if (cider-connected-p)
      (cider-inspect-last-result)
    (message "Cider not connected")))

(defun +debug/dap-inspect-variable ()
  "Inspect variable using DAP."
  (if (bound-and-true-p dap-mode)
      (dap-ui-inspect-thing-at-point)
    (message "DAP not available")))

(defun +debug/show-locals ()
  "Show local variables."
  (interactive)
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ('cider (message "Use 'l' in Cider debug prompt to show locals"))
      ((or 'dap-go 'dap-python 'dap-rust 'dap-java) (dap-ui-locals))
      (_ (message "Local variables display not available")))))

(defun +debug/show-stack ()
  "Show call stack."
  (interactive)
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ('cider (cider-stacktrace-mode))
      ((or 'dap-go 'dap-python 'dap-rust 'dap-java) (dap-ui-sessions))
      (_ (message "Stack trace not available")))))

;; =============================================================================
;; Debug Session Management
;; =============================================================================

(defun +debug/status ()
  "Show debug session status."
  (interactive)
  (let ((backend (+debug/get-backend))
        (active (+debug/session-active?)))
    (message "Debug backend: %s | Session: %s"
             (if (eq backend 'none) "None" backend)
             (if active "Active" "Inactive"))))

(defun +debug/list-breakpoints ()
  "List all breakpoints."
  (interactive)
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ('cider (message "Cider breakpoints are #dbg forms in code"))
      ((or 'dap-go 'dap-python 'dap-rust 'dap-java) (dap-ui-breakpoints))
      (_ (message "No breakpoints available")))))

(defun +debug/clear-all-breakpoints ()
  "Clear all breakpoints."
  (interactive)
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ('cider (message "Remove #dbg forms manually from code"))
      ((or 'dap-go 'dap-python 'dap-rust 'dap-java) (dap-breakpoint-delete-all))
      (_ (message "No breakpoints to clear")))))

;; =============================================================================
;; Debug REPL and Evaluation
;; =============================================================================

(defun +debug/eval-expression ()
  "Evaluate expression in debug context."
  (interactive)
  (let ((backend (+debug/get-backend))
        (expr (read-string "Expression: ")))
    (pcase backend
      ('cider (cider-eval-string expr))
      ((or 'dap-go 'dap-python 'dap-rust 'dap-java) (dap-eval expr))
      (_ (message "Expression evaluation not available")))))

(defun +debug/open-repl ()
  "Open debug REPL."
  (interactive)
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ('cider (cider-switch-to-repl-buffer))
      ((or 'dap-go 'dap-python 'dap-rust 'dap-java) (dap-ui-repl))
      (_ (message "Debug REPL not available")))))

;; =============================================================================
;; Debug Hydra
;; =============================================================================

(defhydra +debug/hydra (:color pink :hint nil)
  "
^Session^           ^Stepping^        ^Breakpoints^     ^Inspection^
─────────────────────────────────────────────────────────────────
_s_: start          _n_: step next    _b_: toggle       _i_: inspect var
_S_: stop           _N_: step into    _B_: list all     _l_: show locals
_r_: restart        _o_: step out     _C_: clear all    _k_: show stack
_c_: continue       _f_: finish       ^                 ^
^                   ^                 ^                 ^Evaluation^
^REPL^              ^Windows^         ^Navigation^      ─────────────────
_R_: open repl      _w_: layout       _u_: up frame     _e_: eval expr
_E_: eval last      _W_: reset ui     _d_: down frame   _?_: status
^                   ^                 ^                 ^
^Utils^             ^                 ^                 ^Control^
─────────────────────────────────────────────────────────────────
_L_: toggle logs    ^                 ^                 _q_: quit
_T_: toggle traces  ^                 ^                 _Q_: quit all
"
  ;; Session management
  ("s" +debug/start)
  ("S" +debug/stop)
  ("r" +debug/restart)
  ("c" +debug/continue)

  ;; Stepping
  ("n" +debug/step-next)
  ("N" +debug/step-into)
  ("o" +debug/step-out)
  ("f" +debug/finish-function)

  ;; Breakpoints
  ("b" +debug/toggle-breakpoint)
  ("B" +debug/list-breakpoints)
  ("C" +debug/clear-all-breakpoints)

  ;; Inspection
  ("i" +debug/inspect-variable)
  ("l" +debug/show-locals)
  ("k" +debug/show-stack)

  ;; REPL and Evaluation
  ("R" +debug/open-repl)
  ("E" +debug/eval-last-sexp)
  ("e" +debug/eval-expression)

  ;; Windows and UI
  ("w" +debug/setup-windows)
  ("W" +debug/reset-ui)

  ;; Navigation
  ("u" +debug/up-frame)
  ("d" +debug/down-frame)

  ;; Utils
  ("L" +debug/toggle-logs)
  ("T" +debug/toggle-traces)
  ("?" +debug/status :color blue)

  ;; Control
  ("q" nil :color blue)
  ("Q" +debug/quit-all :color blue))

;; Placeholder functions for advanced features
(defun +debug/restart ()
  "Restart debug session."
  (interactive)
  (+debug/stop)
  (+debug/start))

(defun +debug/finish-function ()
  "Continue until current function returns."
  (interactive)
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ('cider (message "Use 'c' in Cider debug prompt"))
      ((or 'dap-go 'dap-python 'dap-rust 'dap-java) (dap-continue))
      (_ (message "Not supported")))))

(defun +debug/eval-last-sexp ()
  "Evaluate last s-expression in debug context."
  (interactive)
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ('cider (cider-eval-last-sexp))
      (_ (+debug/eval-expression)))))

(defun +debug/setup-windows ()
  "Set up debug window layout."
  (interactive)
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ('cider (message "Cider debug uses overlays in source buffer"))
      ((or 'dap-go 'dap-python 'dap-rust 'dap-java) (dap-ui-mode 1))
      (_ (message "Debug UI not available")))))

(defun +debug/reset-ui ()
  "Reset debug UI."
  (interactive)
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ((or 'dap-go 'dap-python 'dap-rust 'dap-java) (dap-ui-mode -1))
      (_ (message "Debug UI reset not needed")))))

(defun +debug/up-frame ()
  "Move up one frame in call stack."
  (interactive)
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ('cider (message "Use debug prompt navigation"))
      ((or 'dap-go 'dap-python 'dap-rust 'dap-java) (message "Use stack window"))
      (_ (message "Frame navigation not available")))))

(defun +debug/down-frame ()
  "Move down one frame in call stack."
  (interactive)
  (let ((backend (+debug/get-backend)))
    (pcase backend
      ('cider (message "Use debug prompt navigation"))
      ((or 'dap-go 'dap-python 'dap-rust 'dap-java) (message "Use stack window"))
      (_ (message "Frame navigation not available")))))

(defun +debug/toggle-logs ()
  "Toggle debug logging."
  (interactive)
  (message "Debug logging toggle not yet implemented"))

(defun +debug/toggle-traces ()
  "Toggle debug traces."
  (interactive)
  (message "Debug tracing toggle not yet implemented"))

(defun +debug/quit-all ()
  "Quit all debug sessions."
  (interactive)
  (+debug/stop)
  (when (bound-and-true-p dap-ui-mode)
    (dap-ui-mode -1))
  (message "All debug sessions terminated"))

;; =============================================================================
;; Integration with Main Development Hydra
;; =============================================================================

;; Make debug functions available to main dev hydra
(defun +debug/start-session ()
  "Start debug session and open debug hydra."
  (interactive)
  (+debug/start)
  (when (+debug/session-active?)
    (+debug/hydra/body)))

;; Auto-setup for DAP modes
(add-hook 'dap-mode-hook
          (lambda ()
            (local-set-key (kbd "<f9>") #'+debug/toggle-breakpoint)
            (local-set-key (kbd "<f5>") #'+debug/start)
            (local-set-key (kbd "<f10>") #'+debug/step-next)
            (local-set-key (kbd "<f11>") #'+debug/step-into)))

(provide '54-lang-debugging)
;;; 54-lang-debugging.el ends here