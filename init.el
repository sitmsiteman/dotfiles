(setq gc-cons-threshold (* 50 1000 1000))

(eval-when-compile
  (require 'use-package))


(require 'package) 
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" default))
 '(geiser-chez-binary "chezscheme")
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(exec-path-from-shell git-timemachine magit pyvenv no-littering solarized-theme treesit-auto haskell-mode rust-mode eglot esup use-package auto-compile sicp proof-general geiser-guile geiser-mit geiser-chez geiser geiser-racket zenburn-theme company-quickhelp company scheme-complete counsel ivy quack which-key rainbow-delimiters undo-tree pretty-mode paredit smart-compile))
 '(quack-default-program "chezscheme")
 '(quack-programs
   '("chezscheme" "chicken-csi" "chez" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; set Korean font (Noto Serif CJK KR)
;; (set-face-attribute 'default nil :font "Monospace" :height 120)
;; (set-fontset-font "fontset-default" 'hangul (font-spec :family "Noto Sans CJK KR" :height 120))

(setq inhibit-startup-screen t)
;; (tool-bar-mode -1)
(set-language-environment "UTF-8")
(setq default-input-method "korean-hangul")
(set-default-coding-systems 'utf-8)

;; Enable line numbering by default
(global-display-line-numbers-mode t)

;; Tabs mode
(setq indent-tabs-mode nil)

;; quit Emacs directly even if there are running processes
(setq confirm-kill-processes nil)

;; Miscellaneous options
(setq-default major-mode
              (lambda ()	     ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(defalias 'yes-or-no #'y-or-n-p)

(setq load-prefer-newer t)

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;    (load-theme 'solarized-zenburn t) 
;;     (if (display-graphic-p) 
;;      (enable-theme 'solarized-zenburn)
;;      (enable-theme 'solarized-zenburn)))

(use-package lisp-mode
  :config
  (defun bozhidar-visit-ielm ()
    "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
    (interactive)
    (crux-start-or-switch-to 'ielm "*ielm*"))

  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'bozhidar-visit-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer))

(use-package ielm
  :config
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))

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
	`((".*" ,(no-littering-expand-var-file-name "autosave") t)))
  )

(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))
  
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t))
  

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package quack
  :hook
  scheme-mode
  :init
  (setq quack-fontify-style 'emacs))

(use-package pretty-mode
  :ensure t
  :hook
  scheme-mode
  :config
  (global-pretty-mode 1)
  (global-prettify-symbols-mode t))


(use-package rainbow-delimiters
  :defer t
  :init
  (progn
    (add-hook 'scheme-mode 'rainbow-delimiters-mode 1)
    ))

 ;; (use-package paredit
 ;;   :ensure t
 ;;   :hook
 ;;   (emacs-lisp-mode . paredit-mode) ; Elisp buffers.
 ;;   (lisp-mode . paredit-mode) ; Common Lisp buffers.
 ;;   (lisp-interaction-mode . paredit-mode) ; Scratch buffers.
 ;;   (ielm-mode-hook . paredit-mode) ; ELM buffers.
 ;;   (eval-expression-minibuffer-setup . paredit-mode)
 ;;   :bind
 ;;   (:map paredit-mode-map ("<return>" . my/paredit-RET))
 ;;   :config
 ;;   (defun my/paredit-RET ()
 ;;     "Wraps `paredit-RET' to provide a sensible minibuffer experience" (interactive)
 ;;     (cond ((minibufferp)
 ;; 	    (read--expression-try-read))
 ;; 	   ((and (eq major-mode 'inferior-emacs-lisp-mode) (string-prefix-p "*ielm*" (buffer-name)))
 ;; 	    (ielm-return))
 ;; 	   (t (paredit-RET)))))

(use-package paredit
  :ensure t
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
  (define-key paredit-mode-map (kbd "RET") nil)
  (define-key paredit-mode-map (kbd "C-j") 'paredit-newline)
)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))
 
(add-hook 'after-init-hook 'global-company-mode)

(use-package company
  :ensure
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
  ;; Quickhelp may incorrectly place tooltip towards end of buffer
  ;; See: https://github.com/expez/company-quickhelp/issues/72
  :ensure t
  :config
  (company-quickhelp-mode)
  )

;; setup eglot

(use-package rust-mode
  :ensure t
  )
(use-package haskell-mode
  :ensure t
  )

(use-package eglot
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'scheme-mode-hook 'eglot-ensure)
  (add-hook 'common-lisp-mode-hook 'eglot-ensure)
  (add-hook 'lisp-mode-hook 'eglot-ensure)
  (add-hook 'emacs-lisp-mode-hook 'eglot-ensure)
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  )

;treesit

(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(use-package smart-compile
  :ensure t
  )

;; scheme, lisp stuffs

;; Replace "sbcl" with the path to your implementation

(setq scheme-program-name "chezscheme")

;; Enable line numbering by default
(global-display-line-numbers-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Miscellaneous options
(setq-default major-mode
              (lambda ()	     ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq gc-cons-threshold (* 2 1000 1000))
