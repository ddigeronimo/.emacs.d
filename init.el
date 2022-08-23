;; TODO:
;; Stop LSP from removing yasnippet from company completion menu (eval order means company-capf doesn't have yasnippet added?)
;; Why does a frame on my 3rd display not update visually unless the mouse moves?
;; Avy
;; Ace window?
;; Add bindings for org store link and org insert link?

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
(setq custom-safe-themes t)				; Don't ask whether a theme is safe prior to evaluating it


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
	    search-web-default-browser 'browse-url-generic)))

  ;; When running Emacs in WSLg, use powershell to paste from the Windows clipboard
  ;; If dos2unix is unavailable, it can be replaced with "tr -d '\r'" or "sed -e 's\r//g'"
  (defun wsl-paste ()
    (interactive)
    (insert (shell-command-to-string "powershell.exe Get-Clipboard | dos2unix | tr -d '\n'")))
  (global-set-key (kbd "C-S-v") 'wsl-paste)

  ;; Pipe the selected text to clip.exe, putting it on the Windows clipboard
  (defun wsl-copy (start end)
    (interactive "r")
    (if (use-region-p)
	(let ((text (buffer-substring-no-properties start end)))
	  (shell-command (concat "echo '" text "' | clip.exe")))))
  (global-set-key (kbd "C-S-c") 'wsl-copy))

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

;; Scale up buffer and mode line fonts for larger screen, make the frame, and then reset for smaller main screen
(defun make-large-frame ()
   (interactive)
  (set-face-attribute 'default t :height 140)
  (set-face-attribute 'mode-line t :height 140)
  (make-frame)
  (set-face-attribute 'default t :height 110)
  (set-face-attribute 'mode-line t :height 110))
(global-set-key (kbd "C-S-n") 'make-large-frame) ; TODO: Needs a binding that isn't overwritten by evil mode

;; Package.el stuff
(require 'package)
(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "http://elpa.nongnu.org/nongnu/"))
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

(use-package exec-path-from-shell	; Setup exec-path-from-shell to fix Mac $PATH issues
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-initialize))

(use-package delight
  :ensure t
  :config
  (delight '((visual-line-mode nil t)
	     (hs-minor-mode nil "hideshow")
	     (eldoc-mode nil "eldoc"))))

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

(use-package forge
  :after magit)

(use-package which-key
  :ensure t
  :delight which-key-mode
  :init
  (which-key-mode))

(use-package projectile
  :ensure t
  :defer t
  :delight '(:eval (concat " P[" (projectile-project-name) "]"))
  :init
  (projectile-mode +1))

(use-package ivy
  :ensure t
  :defer 0.1
  :delight
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package counsel
  :after ivy
  :ensure t
  :delight
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

(use-package vterm
  :ensure t
  :config
  (setq vterm-timer-delay 0.05))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(use-package know-your-http-well
  :requires restclient)

(use-package company-restclient
  :requires restclient)

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :interpreter ("go" . go-mode)
  :init
  (defun my-go-mode-hook ()
    (setq tab-width 4))
  (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package yasnippet
  :ensure t
  :delight yas-minor-mode
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
  :delight company-mode
  :init
  (add-hook 'prog-mode-hook #'company-mode)
  (add-hook 'restclient-mode-hook #'company-mode)
  :config
  (add-to-list 'company-backends 'company-restclient)
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

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :init
  (add-hook 'go-mode-hook #'lsp-deferred)
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(use-package lsp-ui
  :ensure t
  :init (setq lsp-ui-doc-show-with-cursor 't))

(use-package paredit
  :defer t
  :init
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

;; Evil mode setup inc. leader bindings, better undo/working redo w/
;; undo-tree, support for different menus and major modes, and
;; emulations of my favorite Vim plugins
 (use-package evil-leader
  :ensure t
  :init
  (setq evil-want-keybinding nil) ; Required for evil-collection, must be loaded before evil and evil-leader
  :config
  ;; Set leader to space
  (evil-leader/set-leader "<SPC>")
  ;; NOTE - l is reserved for LSP related commands, defined in evil's config below
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
  (evil-leader/set-key "g b" 'magit-blame)
  ;; p - projectile commands
  (evil-leader/set-key "p" 'projectile-command-map)
  ;; i - ivy/counsel/swiper commands unrelated to other categories
  (evil-leader/set-key "i i" 'ivy-resume)
  (evil-leader/set-key "i f" 'counsel-imenu)
  (evil-leader/set-key "i r" 'counsel-evil-registers)
  (evil-leader/set-key "i m" 'counsel-evil-marks)
  (evil-leader/set-key "i a" 'counsel-ag)
  ;; y - yasnippet commands
  (evil-leader/set-key "y i" 'yas-insert-snippet)
  ;; ` - launch vterm
  (evil-leader/set-key "`" 'vterm)
  ;; n - new?
  (evil-leader/set-key "n f" 'make-frame)
  (evil-leader/set-key "n l" 'make-large-frame))

(use-package evil
  :after evil-leader
  :ensure t
  :config
  (evil-mode 1)
  (global-evil-leader-mode 1)
  (evil-define-key 'normal lsp-mode-map (kbd "<SPC> l") lsp-command-map)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (evil-set-initial-state 'magit-status-mode 'emacs)
  (evil-set-initial-state 'forge-topic-mode 'emacs))

(use-package evil-collection ; Adds vim keys to different major modes
  :after evil
  :ensure t
  :delight
  :config
  (evil-collection-init))

(use-package undo-tree ; Use vim-style undo, plus enables C-r redo in evil mode
  :after evil
  :ensure t
  :delight
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
  :delight
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
  :delight
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-paredit ; Adds vim keys to paredit-mode (not included within evil-collection)
  :requires paredit
  :init (add-hook 'paredit-mode-hook 'evil-paredit-mode))

;; Finally, keep custom variables in a seperate file that git will ignore
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
