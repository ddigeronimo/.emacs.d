;; My goals in this Emacs configuration are two-fold:
;; 1) I want to create the comfiest, most-modern UI possible (inspired by Spacemacs, Doom, and VSCode) without sacrificing quality and convienience.
;; 2) I want to hone Emacs into the perfect tool for me, customizing it to fit my editing style and workflow.

;; TODO:
;  1) Start thinking about pushing back RSI --> Maybe God-mode?
;  2) Setup Projectile
;  3) Finish moving setup to use-package declarations
;  4) Setup JS error checking using ESlint


;; Better defaults
(setq user-full-name "Dylan DiGeronimo"	; Set user
      user-mail-address "dylandigeronimo1@gmail.com"
      frame-title-format '("%b")	; Set window title to file name
      inhibit-startup-screen t		; Hide startup screen and start on scratch buffer
      global-linum-mode t		; Line numbering
      show-paren-mode t			; Highlight matching parenthesis
      global-visual-line-mode t		; Nice line-wrapping
      make-backup-files nil             ; Stop Emacs from creating backup files
      )
(setq-default cursor-type 'bar)		; Set cursor to bar style
(setq-default cursor-in-non-selected-windows nil) ; Hide cursor in non-active windows
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
(setq scratch-message-program "fortune")
(setq initial-scratch-message (start-with-fortune))

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
  ;; :mode "\\.org\\'"
  ;; :interpreter "org"
  )

(use-package magit 			; Git good son
  :ensure t)

(use-package helm			; Take the helm and find everything
  :ensure t
  :delight helm-mode 
  :init
  (require 'helm-config)
  :config
  (helm-mode 1)
  :bind
  (("M-x" . helm-M-x)
   ("<menu>" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-buffers-list)))

(use-package powerline			; With great powerline comes great visibility
  :ensure t
  :config
  (powerline-default-theme))

(use-package company 			; We finish each other's sandwiches
  :ensure t
  :config
  (global-company-mode))

(use-package flycheck 			; Checking... on the fly
  :ensure t
  :config
  (global-flycheck-mode 1)
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)
    (flycheck-status-emoji-mode)
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))))

(use-package irony			; Can you C the irony?
  :ensure t
  ;; :mode "\\.c\\'"
  ;; :mode "\\.h\\'"
  ;; :interpreter "c"
  ;; :mode "\\.cpp\\'"
  ;; :interpreter "c++"
  ;; :config
  ;; (irony-cdb-autosetup-compile-options)
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package neotree 			; Keep on climbing and you'll find something
  :ensure t
  :bind
  (([f8] . neotree-toggle))
  :config
  (setq neo-theme 'icons))

(use-package which-key			; For when your memory is as bad as mine
  :ensure t
  :config
  (which-key-mode)
  :delight which-key-mode)

(use-package rainbow-mode 		; ALL THE HUES
  :ensure t)

(use-package yasnippet			; YAS QUEEN
  :ensure t
  :config
  (yas-global-mode))

(use-package yasnippet-snippets		; What's a yas without snippets?
  :ensure t)

(use-package elpy 			; Hisssss
  :ensure t
  ;; :mode "\\.py\\'"
  ;; :interpreter "Python"
  :config
  (elpy-enable))

(use-package tuareg 			; Deserted dunes welcome weary feet
  :ensure t
  ;; :mode "\\.ml\\'"
  ;; :interpreter "ocaml"
  )

(use-package all-the-icons		; Every last one of 'em
  :ensure t)

(use-package sublimity			; For that extra cozy feeling
  :defer t)

(use-package company-erlang		; It's quite trivial
  :ensure t
  :config
  (add-hook 'erlang-mode-hook #'company-erlang-init))

(use-package evil			; Because sometimes you're going to have to hand your computer to someone who uses Vim
  :defer t)

(use-package web-mode 			; HTML but better
  :ensure t
  ;; :mode "\\.html\\'"
  ;; :interpreter "html"
  :config
  (add-hook 'html-mode-hook 'web-mode)
  )

(use-package comment-dwim-2 		; This time, it's personal
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
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package delight			; Hiding labor est. 2018
  :ensure t)

;; (use-package flycheck-pos-tip-mode
;;   :ensure t)
;; (use-package flycheck-status-emoji-mode
;;   :ensure t)
;; (use-package pdf-tools
;;   :ensure t)

;; Rainbow mode setup
(add-hook 'css-mode-hook 'my-css-mode-hook)
(defun my-css-mode-hook ()
  (rainbow-mode 1))

;; Elpy setup
;;(elpy-enable)

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

;; Sublimity Mode
;; (require 'sublimity-scroll)
;;(require 'sublimity-map)
;;(require 'sublimity-attractive)
;; (sublimity-mode 1)
;; (setq sublimity-scroll-weight 4
;;       sublimity-scroll-drift-length 6)

;; Bind expand-region
;;(global-set-key (kbd "C-=") 'er/expand-region)

;; Trigger paredit in lisp modes
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)

;; Finally, keep custom variables in a seperate file that git will ignore
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
