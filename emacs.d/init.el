(setq gc-cons-threshold (* 50 1000 1000))

(setq load-prefer-newer t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t) 

(use-package el-patch)

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "autosave") t))))

(setq native-comp-deferred-compilation-deny-list '())
(setq native-comp-async-report-warnings-errors nil)

(custom-set-variables
 '(column-number-mode t)
 '(custom-safe-themes '(default))
 '(inhibit-startup-screen t)
 '(tab-bar-mode nil)
 '(tool-bar-mode nil))

;; Font setups for Windows-NT / *nix

(when (display-graphic-p)
(if (eq system-type 'windows-nt)
    (progn
      (setenv "PATH" (concat "C:\\Program Files (x86)\\GnuWin32\\bin" ";" (getenv "PATH")))
       (custom-set-faces
	'(default ((t (:family "DejaVu Sans Mono" :foundry "outline"
		:slant normal :weight regular :height 120 :width normal))))
	'(fixed-pitch ((t (:family "DejaVu Sans Mono" :foundry "outline"
		:slant normal :weight regular :height 120 :width normal))))
	'(variable-pitch ((t (:family "DejaVu Sans Mono" :foundry "outline"
	        :slant normal :weight regular :height 120 :width normal))))))
 (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 120)
 (set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono" :height 120)
 (set-face-attribute 'variable-pitch nil :family "DejaVu Sans Mono" :height 120)
 (set-fontset-font "fontset-default" 'hangul (font-spec :family "Noto Sans Mono CJK KR" :height 120))
))

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
(global-unset-key (kbd "<Hangul>"))
(global-set-key (kbd "<Hangul>") 'toggle-input-method)

;; Setups
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tab-bar-mode t)
(fringe-mode 0)
(setq frame-title-format "%b - Emacs")
(setq visible-bell nil
      ring-bell-function #'ignore)

;; Define my-mono-face-set function.
;; If arg is nil, then do nothing.
;; If arg is other than nil, then make emacs (almost) monochrome.

(defun my-mono-face-set (arg)
  (when arg
    ;; disable color
    (global-font-lock-mode -1) ;; Disable syntax highlighting everywhere
    (mapc #'disable-theme custom-enabled-themes) ;; Remove all themes
    (setq frame-background-mode nil) ;; Prevent Emacs from auto-detecting a color scheme
    (setq term-default-bg-color nil)
    (setq term-default-fg-color nil)

    ;; Override faces to ensure they're plain
    (custom-set-faces
     '(default ((((type tty))) (t (:foreground "black" :background "white"))))
     '(font-lock-builtin-face ((t (:foreground "black"))))
     '(font-lock-comment-face ((t (:foreground "black"))))
     '(font-lock-constant-face ((t (:foreground "black"))))
     '(font-lock-function-name-face ((t (:foreground "black"))))
     '(font-lock-keyword-face ((t (:foreground "black"))))
     '(font-lock-string-face ((t (:foreground "black"))))
     '(font-lock-type-face ((t (:foreground "black"))))
     '(font-lock-variable-name-face ((t (:foreground "black"))))
     '(minibuffer-prompt ((t (:foreground "black" :background "white")))))))

(my-mono-face-set nil)

;; Numbering Lines
(setq line-number-display-limit nil)
(global-display-line-numbers-mode t)
(column-number-mode t)
(size-indication-mode t)

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
(setq frame-inhibit-implied-resize t)
(setq pixel-scroll-precision-mode t)
; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Backups
(make-directory "~/.emacs_backups/" t)
(make-directory "~/.emacs_autosave/" t)
(make-directory "~/.emacs.d/undo/" t)
(setq backup-by-copying t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs_autosave/" t)))
(setq backup-directory-alist '(("." . "~/.emacs_backups/")))
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)

;; Sentences have a single space between them.
(setq sentence-end-double-space nil)

;; require newline at the end of the file
(setq require-final-newline t)

;; Tabs configuration
(setq indent-tabs-mode nil)

;; quit Emacs directly even if there are running processes
(setq confirm-kill-processes nil)

;; General coding style.
(global-whitespace-mode 1)
(global-font-lock-mode 1)

;; text replaces the selection if the selection is active
(delete-selection-mode 1)

;; Text-mode options
;; shortcut for greek transliteration
(define-key text-mode-map (kbd "C-c C-r") #'fill-region)
(define-key text-mode-map (kbd "C-c C-e")
            (lambda ()
	      (interactive)
              (insert "ē")))
(define-key text-mode-map (kbd "C-c C-o")
            (lambda ()
	      (interactive)
              (insert "ō")))

;; Define abbreviation
;; 
;; (add-hook 'text-mode-hook
;;           (lambda ()
;;             (abbrev-mode 1) ;; Enable abbrev-mode in text-mode
;; 	    (define-abbrev text-mode-abbrev-table "'e" "ē")
;;             (define-abbrev text-mode-abbrev-table "'o" "ō")))


;; Lisp mode
(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)

;; Packages

(use-package modus-themes
  :demand t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs nil)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi :no-confirm)
  :bind ("<f5>" . modus-themes-toggle))

(use-package dtrt-indent
  :commands (dtrt-indent-global-mode
             dtrt-indent-mode
             dtrt-indent-adapt
             dtrt-indent-undo
             dtrt-indent-diagnosis
             dtrt-indent-highlight)
  :config
  (dtrt-indent-global-mode))

(use-package racket-mode
  :config
  (add-hook 'scheme-mode-hook #'racket-mode)
  (add-hook 'racket-mode-hook #'racket-xp-mode)
  (add-to-list 'auto-mode-alist '("\\.scm\\'" . racket-mode))
  (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode)))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package delight)

(use-package ielm)

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package git-timemachine
  :bind (("s-g" . git-timemachine)))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package undo-fu
  :delight undo-fu-mode
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package undo-fu-session
  :after
  undo-fu
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :custom
  (undo-fu-session-directory (expand-file-name "~/.emacs.d/undo")))

(undo-fu-session-global-mode)

(use-package which-key  
  :delight which-key-mode
  :config
  (which-key-mode 1))

(use-package paredit
  :delight paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'paredit-mode)
  (add-hook 'racket-mode-hook #'paredit-mode)
  (add-hook 'racket-repl-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'inferior-lisp-mode-hook #'paredit-mode)
  (add-hook 'inferior-scheme-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojurescript-mode-hook #'paredit-mode)
  (add-hook 'clojurec-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  ;; (define-key paredit-mode-map (kbd "M-j") 'paredit-newline)
  (define-key paredit-mode-map (kbd "RET") nil))

(use-package ivy
  :delight ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
      '((ivy-switch-buffer . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package ggtags
  :config
  (add-hook 'c-ts-mode-hook
            (lambda ()
              (when (derived-mode-p 'c-ts-mode 'c++-ts-mode 'java-mode)
		(ggtags-mode 1)))))

(defun knf-style-wip()
  `(((node-is ")") parent-bol 0)
    ((match nil "argument_list" nil 1 1)
     parent-bol c-ts-mode-indent-offset)
    ((parent-is "argument_list") prev-sibling 0)
    ((match nil "parameter_list" nil 1 1)
     parent-bol c-ts-mode-indent-offset)
    ((parent-is "parameter_list") prev-sibling 0)
    ,@(alist-get 'bsd (c-ts-mode--indent-styles 'c))))

(use-package c-ts-mode
  :if (treesit-language-available-p 'c)
  :custom
  (c-ts-mode-indent-offset 8)
  (c-ts-mode-indent-style #'bsd)
  :init
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))

(use-package company
  :delight company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  :custom
  (setq company-idle-delay 0) ;; how long to wait until popup
  (setq company-minimum-prefix-length 3)
  (add-to-list 'company-backends 'company-yasnippet)
  :bind
  (:map company-active-map
        ("C-n". company-select-next)
        ("C-p". company-select-previous)
        ("M-<". company-select-first)
        ("M->". company-select-last)))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode))

(use-package anaconda-mode
  :config
  (add-hook 'python-ts-mode-hook #'anaconda-mode))

(use-package company-anaconda
  :config
  (add-to-list 'company-backends '(company-anaconda :with company-capf)))

(use-package proof-general)

(use-package company-coq
  :config
  (add-hook 'coq-mode-hook #'company-coq-mode))

(use-package company-go
  :config
  (add-hook 'go-ts-mode-hook #'company-go-mode))

(use-package company-ghci  
  :config
  (add-hook 'haskell-ts-mode-hook #'company-ghci-mode))

(use-package haskell-mode)

(use-package j-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode)))

(use-package eglot
  :straight (:type built-in)
  :config
  ;; make sure trailing white spaces are only shown in programming mode.
  ;; I don't use emacs to edit my mails, so it is sufficient (atm).
  (add-hook 'eglot--managed-mode-hook (lambda () (setq show-trailing-whitespace t)))
  (setq whitespace-style '(trailing lines space-before-tab)
	whitespace-line-column 80)
  (add-hook 'rust-ts-mode-hook 'eglot-ensure)
  (add-hook 'c-ts-mode-hook 'eglot-ensure)
  (add-hook 'c++-ts-mode-hook 'eglot-ensure)
  (add-hook 'go-ts-mode-hook 'eglot-ensure)
  (add-hook 'common-lisp-mode-hook 'eglot-ensure)
  (add-hook 'lisp-mode-hook 'eglot-ensure)
  (add-hook 'haskell-ts-mode 'eglot-ensure)
  (add-hook 'python-ts-mode-hook 'eglot-ensure)
  ;; Racket language server
  (add-to-list 'eglot-server-programs '(racket-mode . ("racket" "-l" "racket-langserver")))
  (add-hook 'racket-mode-hook 'eglot-ensure))

(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(use-package diogenes
  :straight (diogenes :host github
		      :repo "nitardus/diogenes.el")
  :init
  (setq diogenes-path (expand-file-name "~/diogenes"))
  ;; Prebuilt binary doesn't have grc.lsj.logeion.xml files so I don't use it.
  (setq diogenes-preferred-lsj-file "grc.lsj.xml")
  :config
  ;; Use Gentium Plus fonts in diogenes browser mode.
  (defun my-diogenes-greek-face ()
   (interactive)
   (setq buffer-face-mode-face '(:family "Gentium Plus" :height 140))
   (buffer-face-mode))
  
  (add-hook 'diogenes-browser-mode-hook 'my-diogenes-greek-face)
  (add-hook 'diogenes-lookup-mode-hook 'my-diogenes-greek-face)
  (add-hook 'diogenes-analysis-mode-hook 'my-diogenes-greek-face)
  (add-hook 'diogenes-search-mode-hook 'my-diogenes-greek-face)
  (add-hook 'diogenes-corpus-mode-hook 'my-diogenes-greek-face)
  (add-hook 'diogenes-corpus-edit-mode-hook 'my-diogenes-greek-face)
  
  :bind (("C-c C-d g" . diogenes)
	 (:map diogenes-browser-mode-map
	       (("C-c C-d w" . diogenes-parse-greek))))

  :commands (diogenes-ad-to-ol
             diogenes-ol-to-ad
             diogenes-utf8-to-beta
             diogenes-beta-to-utf8))

(use-package org
  :straight (:type built-in)
  :bind
  (("C-c C--" . org-mark-ring-goto)))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Notes")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

(use-package olivetti
  :config
  (setq olivetti-style 'fancy)
  :custom
  (olivetti-body-width 130))

(use-package auto-olivetti
  :straight (auto-olivetti :host sourcehut
			   :repo "ashton314/auto-olivetti")
  :config
  (auto-olivetti-mode)
  :custom
  (auto-olivetti-enabled-modes
   '(text-mode diogenes-browser-mode diogenes-search-mode
	       diogenes-corpus-mode diogenes-corpus-edit-mode
	       diogenes-analysis-mode diogenes-lookup-mode)))

;; Packages for only non-windows systems

(when (not (eq system-type 'windows-nt))
    (use-package vterm
      :commands vterm
      :config
      (setq vterm-max-scrollback 10000))
    (use-package multi-vterm
      :requires vterm))

;; pdf-tools needs some tweak for windows...

(if (eq system-type 'windows-nt)
    (use-package pdf-tools
      :init
      (setenv "PATH" (concat "C:\\msys64\\mingw64\\bin" ";" (getenv "PATH")))
      :mode  ("\\.pdf\\'" . pdf-view-mode)
      :custom
      (pdf-info-epdfinfo-program "C:\\msys64\\mingw64\\bin\\epdfinfo.exe")
      (pdf-tools-install :no-query)
      )
      (use-package pdf-tools
      :mode  ("\\.pdf\\'" . pdf-view-mode)
      :custom
      (pdf-tools-install :no-query)))

;; Unused Packages
;; (use-package pretty-mode
;;   :config
;;   (add-hook 'scheme-mode-hook #'pretty-mode)
;;   (add-hook 'lisp-mode-hook #'pretty-mode)
;;   (add-hook 'emacs-lisp-mode-hook #'pretty-mode)
;;   (add-hook 'inferior-scheme-mode-hook #'pretty-mode)
;;   (global-pretty-mode 1)
;;   (global-prettify-symbols-mode t))
;;
;; (use-package yasnippet
;;   :config
;;   (use-package yasnippet-snippets
;;     )
;;   (yas-reload-all)
;;   (yas-global-mode 1)
;;   (global-set-key (kbd "C-c y") 'company-yasnippet))
;;
;; (use-package quack
;;   :straight (:host  github
;; 	     :repo "sitmsiteman/quack"
;; 	     :branch "testing")
;;   :config
;; (custom-set-variables
;; '(quack-programs
;;    '("chez-scheme" "chezscheme" "chicken-csi" "chez" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi"))
;;  '(quack-smart-open-paren-p nil)
;;  )
;;   (setq quack-programs '("chezscheme" "chicken-csi" "chez" "bigloo" "csi"
;; 			      "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -"
;; 			      "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket"
;; 			      "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi"))
;;   ;; Scheme program
;;   (setq my-quack-chez-executables (list "chez" "chezscheme" "scheme"
;;   					"chez-scheme" "petite"))
;;   (defun my-quack-find-chez-executables (my-quack-executables)
;;     (cl-find-if #'executable-find my-quack-executables))
;;   (setq quack-default-program
;; 	(my-quack-find-chez-executables my-quack-chez-executables))
;;   (setq quack-fontify-style 'emacs))

;; Themes
;; (use-package acme-theme  
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
