;; Better defaults
(setq user-full-name "Dylan DiGeronimo" ; Set user
      user-mail-address "dylandigeronimo1@gmail.com"
      frame-title-format '("%b")      ; Set window title to file name
      inhibit-startup-screen t	; Hide startup screen and start on scratch buffer
      show-paren-mode t	; Highlight matching parenthesis
      global-visual-line-mode t	; Nice line-wrapping
      backup-directory-alist '(("." . "~/.emacs.d/backups"))) ; Save backups to a single location rather than leaving them in the dir of the original

(setq-default cursor-type 'box ; Set cursor to bar style
	      cursor-in-non-selected-windows nil) ; Hide cursor in non-active windows
(menu-bar-mode -1)			; Hide all the bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(electric-pair-mode t)		        ; Automatically complete delimiter pairs
(fset 'yes-or-no-p 'y-or-n-p)		; Replace all yes/no promepts with y/n

;; Transparent titlebar - enable on Macs
(add-to-list 'default-frame-alist	
             '(ns-transparent-titlebar . t))

(add-to-list 'default-frame-alist
             '(ns-appearance . light)) ;; or dark - depending on your theme

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
;; projectile
;; go support
;; general.el?
;; replace vim packages:
;;   surround
;;   commentary
;;   git-gutter (maybe magit handles this?)
;;   highlighted yank
;;   easymotion (maybe avy instead)?

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

;; Evil mode setup inc. leader bindings, evil-collection, undo-tree, and org support
(use-package evil-leader
  :ensure t
  :init
  (setq evil-want-keybinding nil) ; Required for evil-collection, must be loaded before evil and evil-leader
  :config
  (evil-leader/set-leader "<SPC>") ; Set leader to space
  ;; w - window commands
  (evil-leader/set-key "w v" 'evil-window-vsplit)
  (evil-leader/set-key "w s" 'evil-window-split)
  (evil-leader/set-key "w h" 'evil-window-left)
  (evil-leader/set-key "w j" 'evil-window-down)
  (evil-leader/set-key "w k" 'evil-window-up)
  (evil-leader/set-key "w l" 'evil-window-right)
  (evil-leader/set-key "w <" 'evil-window-decrease-width)
  (evil-leader/set-key "w >" 'evil-window-increase-width)
  (evil-leader/set-key "w q" 'evil-window-delete)
  (evil-leader/set-key "w o" 'delete-other-windows)
  ;; f - file commands
  (evil-leader/set-key "f f" 'find-file)
  (evil-leader/set-key "f d" 'dired)
  ;; b - buffer commands
  (evil-leader/set-key "b k" 'kill-buffer)
  (evil-leader/set-key "b l" 'list-buffers)
  (evil-leader/set-key "b b" 'switch-to-buffer)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "b s" 'eval-buffer)
  ;; g - git commands
  (evil-leader/set-key "g" 'magit))

(use-package evil
  :after evil-leader
  :ensure t
  :init
  :config
  (evil-mode 1)
  (global-evil-leader-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package undo-tree
  :after evil
  :ensure t
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1)
  (setq evil-want-fine-undo t))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Finally, keep custom variables in a seperate file that git will ignore
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
