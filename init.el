;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ____                                ;;
;;   _________  ____ ___  / __/_  ______ ___  ____ ___________ ;;
;;  / ___/ __ \/ __ `__ \/ /_/ / / / __ `__ \/ __ `/ ___/ ___/ ;;
;; / /__/ /_/ / / / / / / __/ /_/ / / / / / / /_/ / /__(__  )  ;;
;; \___/\____/_/ /_/ /_/_/  \__, /_/ /_/ /_/\__,_/\___/____/   ;;
;;                         /____/  			       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Comfymacs: They said Emacs could be anything, so I made it C O M F Y
;; My goals in this configuration are two-fold:
;; - Create the comfiest, most-modern UI possible, inspired by Spacemacs, Doom, and trendy modern editors like VSCode and Atom.
;; - Hone Emacs into the most powerful editor tool for me, based around my preference for non-modal editing *GASP*. 

;; TODO:
;; 1) Leader keys with Hydra?
;; 2) Setup Projectile
;; 3) Finish JS setup
;; 4) Fix Python setup
;; 5) Maybe copy J. Blanchard's format buffer function? (In rust setup page)

;; Better defaults
(setq user-full-name "Dylan DiGeronimo" ; Set user
      user-mail-address "dylandigeronimo1@gmail.com"
      frame-title-format '("%b")      ; Set window title to file name
      inhibit-startup-screen t	; Hide startup screen and start on scratch buffer
      show-paren-mode t	; Highlight matching parenthesis
      global-visual-line-mode t	; Nice line-wrapping
      make-backup-files nil)  ; Stop Emacs from creating backup files

(setq-default cursor-type 'bar ; Set cursor to bar style
	      cursor-in-non-selected-windows nil) ; Hide cursor in non-active windows
(menu-bar-mode -1)			; Hide all the bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(electric-pair-mode t)		        ; Automatically complete delimiter pairs
(fset 'yes-or-no-p 'y-or-n-p)		; Replace all yes/no promepts with y/n

;; Replace the message in scratch with a quote from fortune
(defun start-with-fortune ()
  (with-temp-buffer
    (lisp-mode)
    (insert (shell-command-to-string scratch-message-program))
    (comment-region (point-max) (point-min))
    (buffer-string)))
(setq scratch-message-program "fortune | cowsay")
(setq initial-scratch-message (start-with-fortune))

;; Rebind undo-tree-undo to M-/
(global-set-key (kbd "M-/") 'undo-tree-redo)

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
(use-package org			; Time to be studious
  :ensure t
  :mode ("\\.org\\'" . org-mode) 
  :interpreter ("org" . org-mode))

(use-package magit 			; Git good son
  :ensure t)

(use-package helm			; Take the helm and find everything
  :ensure t
  :delight helm-mode
  :init
  (require 'helm-config)
  (helm-mode)
  :bind
  (("M-x" . helm-M-x)
   ("<menu>" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-buffers-list)))

;; (use-package powerline			; With great powerline comes great visibility
;;   :ensure t
;;   :config
;;   (powerline-default-theme))

(use-package company 			; We finish each other's sandwiches
  :ensure t
  :defer t
  :init
  (global-company-mode)
  :config
  (progn
    ;; Use company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map))
  :bind ("C-;" . company-complete-common))

(use-package company-erlang		; It's quite trivial
  :ensure t
  :config
  (add-hook 'erlang-mode-hook #'company-erlang-init))

(defun my-irony-mode-hook ()
       (define-key irony-mode-map [remap completion-at-point]
	 'irony-completion-at-point-async)
       (define-key irony-mode-map [remap complete-symbol]
	 'irony-completion-at-point-async))

(use-package irony			; Can you C the irony?
  :ensure t
  :config
  (progn
    (use-package company-irony
      :ensure t
      :config
      (add-to-list 'company-backends 'company-irony))
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

(use-package flycheck 			; Checking... on the fly
  :ensure t
  :init
  (global-flycheck-mode 1)
  :config
    (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)
    (flycheck-status-emoji-mode)
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))))

(use-package neotree 			; Keep on climbing and you'll find something
  :ensure t
  :bind
  (([f8] . neotree-toggle))
  :config
  (setq neo-theme 'icons))

(use-package which-key			; For when your memory is as bad as mine
  :ensure t
  :defer t
  :init
  (which-key-mode)
  :delight which-key-mode)

(use-package rainbow-mode 		; ALL THE HUES
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode)
  :delight)

(use-package yasnippet			; YAS QUEEN
  :ensure t
  :defer t
  :init
  (yas-global-mode))

(use-package yasnippet-snippets		; What's a yas without snippets?
  :ensure t)

(use-package delight			; Hiding labor est. 2018
  :ensure t)

(use-package elpy 			; Hisssss
  :ensure t
  ;; :mode ("\\.py\\'" . elpy-mode)
  ;; :interpreter ("python" . elpy-mode)
  )

(use-package tuareg 			; Deserted dunes welcome weary feet
  :ensure t
  :mode ("\\.ml\\'" . tuareg-mode)
  :interpreter ("ocaml" . tuareg-mode))

(use-package all-the-icons		; Every last one of 'em
  :ensure t
  :defer t)

(use-package evil			; Ugh
  :ensure t
  :init
  (setq evil-default-state 'emacs)
  (setq-default evil-emacs-state-cursor '("purple" bar))
  ;; (evil-mode 1)
  )

(use-package web-mode 			; HTML but better
  :ensure t
  :mode ("\\.html\\'" . web-mode) 
  :interpreter ("html" . web-mode))

(use-package comment-dwim-2 		; Commenting: this time, it's personal
  :ensure t
  :bind
  (("M-;" . comment-dwim-2)))

(use-package expand-region		; Selection à la Jetbrains
  :ensure t
  :bind
  (("C-=" . er/expand-region)))

(use-package paredit			; Sluuuuuurp BAAAAARF
  :ensure t)

(use-package rainbow-delimiters		; For when code looks like (((((this)))))
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package js2-mode			; Trendy Javascript stuff incoming
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("javascript" . js2-mode))

(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(use-package js2-refactor
  :ensure t)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; Required to make xref-js2 work
(use-package ag
  :ensure t)

(use-package xref-js2
  :ensure t
  :init)

;; js-mode binds M-. which conflicts with xref-js2
  (define-key js-mode-map (kbd "M-.") nil)
  (add-hook 'js2-mode-hook (lambda ()
			     (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(use-package company-tern
  :ensure t)

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook #'tern-mode)
;; Disable tern completion keybindings
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

(use-package pdf-tools			; Bite me, Adobe
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :interpreter ("pdf" . pdf-view-mode))

(use-package rust-mode			; R
  :ensure t
  :mode ("\\.rs\\'" . rust-mode))

(use-package cargo			; U
  :ensure t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package flycheck-rust		; S
  :ensure t)

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(use-package racer			; T
  :ensure t)

(use-package nlinum			; Getting line numbers on lock
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'nlinum-mode))

(use-package nlinum-hl
  :ensure t)

;; (use-package flycheck-pos-tip-mode
;;   :ensure t)
;; (use-package flycheck-status-emoji-mode
;;   :ensure t)

;; use-package is giving me trouble with doom-modeline so this is a temporary fix
(doom-modeline-init)

;; Elpy setup
(elpy-enable)

;; Merlin setup
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
 (when (and opam-share (file-directory-p opam-share))
  (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
  (autoload 'merlin-mode "merlin" nil t nil)
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t)))
;; Make company aware of Merlin
(with-eval-after-load 'company
  (add-to-list 'company-backends 'merlin-company-backend))

;; Trigger paredit in lisp modes
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)

;; Additional Rust setup
(setq racer-cmd "~/.cargo/bin/racer") ;; racer binary PATH
(setq racer-rust-src-path "/home/dylan/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src") ;; rust source code PATH
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

;; Finally, keep custom variables in a seperate file that git will ignore
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
