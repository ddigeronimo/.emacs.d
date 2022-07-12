;; Better defaults
(setq-default user-full-name "Dylan DiGeronimo"				; Set user
	      user-mail-address "dylandigeronimo1@gmail.com"
	      frame-title-format '("%b")				; Set window title to file name
	      inhibit-startup-screen t					; Hide startup screen and start on scratch buffer
	      backup-directory-alist '(("." . "~/.emacs.d/backups")))	; Save backups to a single location rather than leaving them in the dir of the original

;; Global modes
(menu-bar-mode -1)			; Hide all the bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(add-hook 'text-mode-hook 'linum-mode)	; Activate line numbers
(add-hook 'prog-mode-hook 'linum-mode)
(show-paren-mode t)			; Highlight matching parenthesis
(global-visual-line-mode t)		; Nice line-wrapping
(electric-pair-mode t)			; Automatically complete delimiter pairs
(fset 'yes-or-no-p 'y-or-n-p)		; Replace all yes/no promepts with y/n
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
;; helm/ivy/some completion framework
;; go support
;; replace vim packages:
;;   surround
;;   git-gutter (maybe magit handles this?)
;;   highlighted yank
;;   easymotion (maybe avy instead)?
;;   easyalign?

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
  :interpreter ("org" . org-mode))

(use-package magit
  :ensure t
  :defer t)

(use-package which-key
  :ensure t
  :defer t
  :init
  (which-key-mode))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1))

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
  ;; b - buffer commands
  (evil-leader/set-key "b k" 'kill-buffer)
  (evil-leader/set-key "b l" 'list-buffers)
  (evil-leader/set-key "b b" 'switch-to-buffer)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "b s" 'eval-buffer)
  ;; g - git commands
  (evil-leader/set-key "g" 'magit)
  ;; p - projectile commands
  (evil-leader/set-key "p" 'projectile-command-map))

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

(use-package evil-commentary ; Emulation of tpope's vim commentary
  :after evil
  :ensure t
  :config
  (evil-commentary-mode 1))

;; Finally, keep custom variables in a seperate file that git will ignore
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
