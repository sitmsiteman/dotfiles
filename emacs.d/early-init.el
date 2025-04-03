(setq gc-cons-threshold most-positive-fixnum)

;; Restore to normal value after startup (e.g. 50MB)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 50 1024 1024))))

(setq package-enable-at-startup nil)

(setq load-prefer-newer t)

;; Setups
(setq frame-title-format "%b - Emacs")
(setq visible-bell nil
      ring-bell-function #'ignore)

;; Frame setups
(tool-bar-mode -1)
(tab-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 0)

;; Numbering Lines
(setq line-number-display-limit nil)
(global-display-line-numbers-mode t)
(size-indication-mode t)
(column-number-mode t)

;; Window options
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)
(setq pixel-scroll-precision-mode t)
; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
;; Deprecated
;; (defalias 'yes-or-no #'y-or-n-p)


(setq inhibit-startup-screen t)

;; Load themes early to avoid flickering during startup (you need a built-in theme, though)
(load-theme 'modus-operandi t)

;; tweak native compilation settings
(setq native-comp-speed 2)

(provide 'early-init)
