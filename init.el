;; Better defaults
(setq-default user-full-name "Dylan DiGeronimo"				; Set user
	      user-mail-address "dylandigeronimo1@gmail.com"
	      frame-title-format '("%b")				; Set window title to file name
	      inhibit-startup-screen t					; Hide startup screen and start on scratch buffer
	      backup-directory-alist '(("." . "~/.emacs.d/backups")))	; Save backups to a single location rather than leaving them in the dir of the original

;; Global modes and settings
(menu-bar-mode -1)					; Hide all the bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq display-line-numbers-type 'relative)		; Use Vim-style relative line numbers, helps with count-prefixed commands
(add-hook 'prog-mode-hook 'display-line-numbers-mode)	; Activate line numbers
(show-paren-mode t)					; Highlight matching parenthesis
(global-visual-line-mode t)				; Nice line-wrapping
(electric-pair-mode t)					; Automatically complete delimiter pairs
(global-hl-line-mode 1)					; Highlight the line the cursor is on for ease of finding
(add-hook 'prog-mode-hook 'hs-minor-mode)		; Enable basic code folding without any packages
(fset 'yes-or-no-p 'y-or-n-p)				; Replace all yes/no promepts with y/n

;; Emacs 29 scroll mode
(if (>= emacs-major-version 29)
    (pixel-scroll-precision-mode))

;; Transparent titlebar - enable on Macs
(add-to-list 'default-frame-alist	
             '(ns-transparent-titlebar . t))

(add-to-list 'default-frame-alist
             '(ns-appearance . light)) ;; or dark - depending on your theme

;; WSL-specific setup
(when (and (eq system-type 'gnu/linux)
	   (getenv "WSLENV"))

  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
	(cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program cmd-exe
	    browse-url-generic-args cmd-args
	    browse-url-browser-function 'browse-url-generic
	    search-web-default-browser 'browse-url-generic))))

;; Customize scratch buffer message with the output of a shell command
(setq scratch-message-program "fortune | cowsay -f cheese")
(defun custom-scratch ()
  (with-temp-buffer
    (lisp-mode)
    (insert (shell-command-to-string scratch-message-program))
    (comment-region (point-max) (point-min))
    (buffer-string)))
(setq initial-scratch-message (custom-scratch))

;; Toggle the time and battery when Emacs is in fullscreen and blocks the system time and battery
(defun enable-time-and-battery ()
  (interactive)
  (display-time-mode)
  (display-battery-mode))

(defun fullscreen-time-and-battery (frame)
  (let ((fullscreen (frame-parameter frame 'fullscreen)))
    (when (memq fullscreen '(fullscreen fullboth))
      (enable-time-and-battery))))

(add-hook 'window-size-change-functions 'fullscreen-time-and-battery)

;; Package.el stuff
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Package setups

;; TODO:
;; Delight/diminish
;; Flycheck mode
;; Avy
;; Add binding for org store link and org insert link
;; Add new org elpa
;; more go support
;;   improve completion with either lsp-mode + gopls or co-complete plus gocode(?)
;;   lsp-ui?
;; Toggle scratch message program based on whether programs are installed

(use-package exec-path-from-shell	; Setup exec-path-from-shell to fix Mac $PATH issues
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-initialize))

(use-package org
  :ensure t
  :defer t
  :mode ("\\.org\\'" . org-mode) 
  :interpreter ("org" . org-mode)
  :init
  (setq org-agenda-restore-windows-after-quit 't))

(use-package magit
  :ensure t
  :defer t)

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package projectile
  :ensure t
  :defer t
  :init
  (projectile-mode +1))

(use-package ivy
  :ensure t
  :defer 0.1
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package counsel
  :after ivy
  :ensure t
  :bind ("C-s" . counsel-grep-or-swiper)
  ("C-r" . counsel-grep-or-swiper-backward)
  :config
  (setq ivy-initial-inputs-alist nil) ; Don't automatically start searches with ^ (first character matching)
  (counsel-mode)
  (setq counsel-org-headline-display-tags t
	counsel-org-headline-display-todo t)) ; Show org tags and todos in counsel searches 

(use-package ivy-rich
  :after (:all ivy counsel)
  :ensure t
  :custom
  (ivy-virtual-abbreviate 'full
			  ivy-rich-switch-buffer-align-virtual-buffer t
			  ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode 1))

(use-package swiper
  :after ivy
  :ensure t)

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :interpreter ("go" . go-mode)
  :init
  (setq gofmt-command "goimports") ; Use goimports instead of gofmt (includes gofmt functionality)
  (defun my-go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save)
    (setq tab-width 4))
  (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package yasnippet
  :ensure t
  :config
  (setq
   yas-verbosity 1
   yas-wrap-around-region 1)
  (yas-reload-all)
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package company
  :after yasnippet
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook #'company-mode))
  :config
  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
	      '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

;; Evil mode setup inc. leader bindings, better undo/working redo w/
;; undo-tree, support for different menus and major modes, and
;; emulations of my favorite Vim plugins
 (use-package evil-leader
  :ensure t
  :init
  (setq evil-want-keybinding nil) ; Required for evil-collection, must be loaded before evil and evil-leader
  :config
  (evil-leader/set-leader "<SPC>") ; Set leader to space
  ;; w - window commands
  (evil-leader/set-key "w" 'evil-window-map)
  ;; f - file commands
  (evil-leader/set-key "f f" 'find-file)
  (evil-leader/set-key "f d" 'dired-jump)
  (evil-leader/set-key "f r" 'counsel-recentf)
  ;; b - buffer commands
  (evil-leader/set-key "b k" 'kill-buffer)
  (evil-leader/set-key "b l" 'list-buffers)
  (evil-leader/set-key "b b" 'ivy-switch-buffer)
  (evil-leader/set-key "b w" 'ivy-switch-buffer-other-window)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "b s" 'eval-buffer)
  (evil-leader/set-key "b r" 'counsel-buffer-or-recentf)
  ;; g - git commands
  (evil-leader/set-key "g m" 'magit-file-dispatch)
  (evil-leader/set-key "g s" 'magit-status)
  (evil-leader/set-key "g d" 'magit-diff-buffer-file)
  (evil-leader/set-key "g l" 'magit-log)
  (evil-leader/set-key "g g" 'counsel-git-grep)
  (evil-leader/set-key "g L" 'counsel-git-log)
  ;; p - projectile commands
  (evil-leader/set-key "p" 'projectile-command-map)
  ;; i - ivy/counsel/swiper commands unrelated to other categories
  (evil-leader/set-key "i i" 'ivy-resume)
  (evil-leader/set-key "i f" 'counsel-imenu)
  (evil-leader/set-key "i r" 'counsel-evil-registers)
  (evil-leader/set-key "i m" 'counsel-evil-marks)
  (evil-leader/set-key "i a" 'counsel-ag)
  ;; y - yasnippet
  (evil-leader/set-key "y i" 'yas-insert-snippet))


(use-package evil
  :after evil-leader
  :ensure t
  :config
  (evil-mode 1)
  (global-evil-leader-mode 1))

(use-package evil-collection ; Adds vim keys to different major modes
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package undo-tree ; Use vim-style undo, plus enables C-r redo in evil mode
  :after evil
  :ensure t
  :init
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1)
  (setq evil-want-fine-undo t))

(use-package evil-org ; Adds vim keys to org-mode (not included within evil-collection)
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-commentary ; Emulation of tpope's commentary.vim
  :after evil
  :ensure t
  :config
  (evil-commentary-mode 1))

(use-package evil-surround ; Emulation of tpope's surround.vim
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-goggles ; Emulation of vim-highlightedyank + additional visual hints
  :after evil
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;; Finally, keep custom variables in a seperate file that git will ignore
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
