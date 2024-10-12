(setq gc-cons-threshold (* 50 1000 1000))

(eval-when-compile
  (require 'use-package))

(setq load-prefer-newer t)

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(setq native-comp-deferred-compilation-deny-list '())
(setq native-comp-async-report-warnings-errors nil)

(custom-set-variables
 '(column-number-mode t)
 '(custom-safe-themes '(default))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(pdf-tools multi-vterm vterm treesit-auto rust-mode company-ghci company-go company-coq proof-general company-anaconda company-quickhelp company ivy paredit quack which-key undo-tree no-littering exec-path-from-shell git-timemachine magit delight auto-compile geiser-racket acme-theme))
 '(quack-programs
   '("chezscheme" "chicken-csi" "chez" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi"))
 '(tab-bar-mode t)
 '(tool-bar-mode nil))

;; Font setups for Windows-NT / *nix

(if (eq system-type 'windows-nt)
 (custom-set-faces
 '(default ((t (:family "CaskaydiaMono NFM" :foundry "outline" :slant normal :weight regular :height 120 :width normal))))
 '(fixed-pitch ((t (:family "CaskaydiaMono NFM" :foundry "outline" :slant normal :weight regular :height 120 :width normal))))
 '(variable-pitch ((t (:family "CaskaydiaMono NFM" :foundry "outline" :slant normal :weight regular :height 120 :width normal)))))
 (set-face-attribute 'default nil :family "CaskaydiaMono Nerd Font" :height 120)
 (set-face-attribute 'fixed-pitch nil :family "CaskaydiaMono Nerd Font" :height 120)
 (set-face-attribute 'variable-pitch nil :family "CaskaydiaMono Nerd Font" :height 120)
 (set-fontset-font "fontset-default" 'hangul (font-spec :family "Noto Sans CJK KR" :height 120))
)

;; utf-8
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(set-language-environment      "UTF-8")
(prefer-coding-system          'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system   'utf-8)
(set-default-coding-systems    'utf-8)
(set-file-name-coding-system   'utf-8)
(set-keyboard-coding-system    'utf-8)
(set-selection-coding-system   'utf-8)
(set-terminal-coding-system    'utf-8)
(setq locale-coding-system     'utf-8)

;; Korean inputs
(setq default-input-method "korean-hangul")

;; Setups
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(tab-bar-mode t)
(setq frame-title-format "%b - Emacs")
(setq visible-bell 1)

;; Scheme program
(setq scheme-program-name "chezscheme")

;; Numbering Lines
;; (global-display-line-numbers-mode t)
(column-number-mode t)
;; (size-indication-mode t)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
;; Deprecated
;; (defalias 'yes-or-no #'y-or-n-p)

;; Set major mode from file name
(setq-default major-mode
              (lambda ()
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

;; Window options
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Backups
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)

;; Tabs configuration
(setq indent-tabs-mode nil)

;; quit Emacs directly even if there are running processes
(setq confirm-kill-processes nil)

;; load elisp
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; General coding style.
(setq-default show-trailing-whitespace t)
(setq whitespace-style '(trailing lines space-before-tab)
      whitespace-line-column 80)
(global-whitespace-mode 1)
(global-font-lock-mode 1)

;; Lisp mode
(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)

;; OpenBSD KNF for C/C++
(require 'openbsd-knf-style)
(c-add-style "OpenBSD" openbsd-knf-style)
(setq c-default-style '((c-mode . "OpenBSD")))

;; Packages
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package geiser-racket
  :ensure t)

(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package delight
  :ensure t)

(use-package ielm
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package git-timemachine
  :ensure t
  :bind (("s-g" . git-timemachine)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package no-littering
  :ensure t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "autosave") t))))

(use-package undo-tree
  :ensure t
  :delight undo-tree-mode
  :config
  (global-undo-tree-mode t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package which-key
  :ensure t
  :delight which-key-mode
  :config
  (which-key-mode 1))

(use-package quack
  :ensure t
  :config
  (setq quack-fontify-style 'emacs)
  (setq quack-default-program "chezscheme")
  (add-hook 'scheme-mode-hook #'quack-mode))

(use-package paredit
  :ensure t
  :delight paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'inferior-lisp-mode-hook #'paredit-mode)
  (add-hook 'inferior-scheme-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojurescript-mode-hook #'paredit-mode)
  (add-hook 'clojurec-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'geiser-repl-mode-hook #'paredit-mode)
  ;; (define-key paredit-mode-map (kbd "M-j") 'paredit-newline)
  (define-key paredit-mode-map (kbd "RET") nil))

(use-package ivy
  :ensure t
  :delight ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package company
  :ensure t
  :delight company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
        ("C-n". company-select-next)
        ("C-p". company-select-previous)
        ("M-<". company-select-first)
        ("M->". company-select-last)))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode))

(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook #'anaconda-mode))

(use-package company-anaconda
  :ensure t
  :config
  (add-hook 'python-mode-hook #'company-anaconda-mode))

(use-package proof-general
  :ensure t)

(use-package company-coq
  :ensure t
  :config
  (add-hook 'coq-mode-hook #'company-coq-mode))

(use-package company-go
  :ensure t
  :config
  (add-hook 'go-mode-hook #'company-go-mode))

(use-package company-ghci
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'company-ghci-mode))

(use-package rust-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package eglot
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'common-lisp-mode-hook 'eglot-ensure)
  (add-hook 'lisp-mode-hook 'eglot-ensure)
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure))

(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; Packages for non-windows systems

(when (not (eq system-type 'windows-nt))
    (use-package vterm
      :ensure t
      :commands vterm
      :config
      (setq vterm-max-scrollback 10000))
    (use-package multi-vterm
      :ensure t))

;; pdf-tools needs some tweak for windows...

(if (eq system-type 'windows-nt)
    (use-package pdf-tools
      :ensure t
      :init
      (setenv "PATH" (concat "C:\\msys64\\mingw64\\bin" ";" (getenv "PATH")))
      :mode  ("\\.pdf\\'" . pdf-view-mode)
      :custom
      (pdf-info-epdfinfo-program "C:\\msys64\\mingw64\\bin\\epdfinfo.exe")
      (pdf-tools-install :no-query)
      )
      (use-package pdf-tools
      :ensure t
      :mode  ("\\.pdf\\'" . pdf-view-mode)
      :custom
      (pdf-tools-install :no-query)))

;; (use-package pretty-mode
;;   :ensure t
;;   :config
;;   (add-hook 'scheme-mode-hook #'pretty-mode)
;;   (add-hook 'lisp-mode-hook #'pretty-mode)
;;   (add-hook 'emacs-lisp-mode-hook #'pretty-mode)
;;   (add-hook 'inferior-scheme-mode-hook #'pretty-mode)
;;   (global-pretty-mode 1)
;;   (global-prettify-symbols-mode t))

;; Themes
;; (use-package acme-theme
;;   :ensure t
;;   :config
;;   (load-theme 'acme t)
;;   (setq acme-theme-black-fg t)
;;   (let ((fg (if acme-theme-black-fg "#000000" "#444444"))
;; 	(bg "#FFFFEA")
;; 	(acme-blue-light "#E1FAFF"))
;;     (custom-theme-set-faces 'acme
;;       `(tab-bar     ((nil (:foreground ,fg :background ,acme-blue-light
;; 	                      :box (:line-width -1)))))
;; 	 `(tab-bar-tab ((nil (:foreground ,fg :weight bold :background ,acme-blue-light))))
;; 	 `(tab-bar-tab-inactive ((nil (:foreground ,fg :weight normal :background ,acme-blue-light))))
;; 	 `(tab-line    ((nil (:foreground ,fg :background ,acme-blue-light
;; 	                      :box (:line-width -1)))))
;; 	 `(tab-line-tab          ((nil (:foreground ,fg :background ,acme-blue-light))))
;; 	 `(tab-line-tab-current  ((nil (:foreground ,fg :weight bold :background ,acme-blue-light))))
;; 	 `(tab-line-tab-inactive ((nil (:foreground ,fg :weight normal :background ,acme-blue-light))))
;; 	 `(tab-line-highlight   ((nil (:foreground ,fg :weight normal :background ,acme-blue-light
;; 	                                :box (:line-width (0 . 1)))))) ; mouseover
;; 	 `(tab-line-tab-modified  ((nil (:foreground ,fg :slant italic :background ,acme-blue-light))))))
;;   (enable-theme 'acme))

(setq gc-cons-threshold (* 2 1000 1000))
