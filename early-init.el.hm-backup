;; Runs before `init.el` (Emacs 27+) so GC + UI settings kick in ASAP.

;; Speed up startup -----------------------------------------------------------
(setq gc-cons-threshold          100000000   ; 100 MB
      package-enable-at-startup  nil)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Simplify the frame before themes / modeline load
(menu-bar-mode -1)
(push '(tool-bar-lines . 0)        default-frame-alist)
(push '(vertical-scroll-bars)      default-frame-alist)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0)      default-frame-alist))

(provide 'early-init)
