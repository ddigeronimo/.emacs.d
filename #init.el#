;;; package --- Summary
;; I had to type that to fix an error - disregard it.

;;; Commentary:
;; My goals in this Emacs configuration are two-fold:
;; 1) I want to create the comfiest, most-modern UI possible (inspired by Spacemacs, Doom, and VSCode) without sacrificing quality and convienience.
;; 2) I want to hone Emacs into the perfect tool for me, customizing it to fit my editing style and workflow.

;; TODO:
;; 1) Setup JS error checking using ESlint
;; 2) Setup Projectile
;; 3) Work more with Magit


;;; Code:

;; This is me
(setq user-full-name "Dylan DiGeronimo"
      user-mail-address "dylandigeronimo1@gmail.com")

;; Hide startup screen and start on scratch buffer
(setq inhibit-startup-screen t)

;; Replace the message in scratch with a quote from fortune
(defun start-with-fortune ()
  (with-temp-buffer
    (lisp-mode)
    (insert (shell-command-to-string scratch-message-program))
    (comment-region (point-max) (point-min))
    (buffer-string)))

(setq scratch-message-program "fortune")
(setq initial-scratch-message (start-with-fortune))

;; Hide all the bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Line numbering
(global-linum-mode 1)

;; Set window title to file name
(setq-default frame-title-format '("%b"))

;; Set cursor to bar style
(setq-default cursor-type 'bar)

;; When the cursor is positioned over a parenthesis, highlight the matching parenthesis
(show-paren-mode 1)

;; Whenever a delimeter (parenthesis, brackets, etc) is inserted, complete the pair
(electric-pair-mode 1)

;; Ensure that line wrapping looks nice
(global-visual-line-mode t)

;; Stop Emacs from creating backup files
(setq make-backup-files nil)

;; Replace all yes/no prompts with y/n
(fset 'yes-or-no-p 'y-or-n-p)

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

;; use-package declarations
(use-package org ; Time to be studious
  :ensure t)
(use-package magit ; Git good son
  :ensure t)
(use-package helm ; Take the helm and find everything
  :ensure t)
(use-package powerline ; With great powerline comes great visibility
  :ensure t)
(use-package company ; We finish each other's sandwiches
  :ensure t)
(use-package flycheck ; Checking... on the fly
  :ensure t)
(use-package irony ; Can you C the irony?
  :ensure t)
(use-package neotree ; Keep on climbing and you'll find something
  :ensure t)
(use-package which-key ; For when your memory is as bad as mine
  :ensure t)
(use-package rainbow-mode ; ALL THE HUES
  :ensure t)
(use-package yasnippet ; YAS QUEEN
  :ensure t)
(use-package yasnippet-snippets ; What's a yas without snippets?
  :ensure t)
(use-package elpy ; Hisssss
  :ensure t)
(use-package tuareg ; Deserted dunes welcome weary feet
  :ensure t)
(use-package all-the-icons ; Every last one of 'em
  :ensure t)
(use-package sublimity ; For that extra cozy feeling
  :ensure t)
(use-package company-erlang ; It's quite trivial
  :ensure t)
(use-package evil ; Because sometimes you're going to have to hand your computer to someone who uses Vim
  :ensure t)
(use-package web-mode ; HTML but better
  :ensure t)
(use-package comment-dwim-2 ; This time, it's personal
  :ensure t)
(use-package expand-region ; Selection à la Jetbrains
  :ensure t)

;; (use-package flycheck-pos-tip-mode
;;   :ensure t)
;; (use-package flycheck-status-emoji-mode
;;   :ensure t)
;; (use-package pdf-tools
;;   :ensure t)

;; Helm setup/keybindings
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "<menu>") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;; Setup modeline
(powerline-default-theme)
;; (zerodark-setup-modeline-format)

;; Neotree
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme 'icons)

;; Company setup
(add-hook 'after-init-hook 'global-company-mode)

;; Irony setup
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Flycheck setup
;; flycheck-pos-tip-mode enables Flycheck errors to become popups, ensure that you install flyckeck-pos-tip first
;; flycheck-status-emoji-mode replaces 'FlyC' with an emoji on the mode line, ensure that you install flycheck-status-emoji first
(global-flycheck-mode 1)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode)
  (flycheck-status-emoji-mode))

;; Which-key setup
(which-key-mode)

;; Rainbow mode setup
(add-hook 'css-mode-hook 'my-css-mode-hook)
(defun my-css-mode-hook ()
  (rainbow-mode 1))

;; Yasnippet setup
(yas-global-mode 1)

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

;; Sublimity Mode
(require 'sublimity-scroll)
;; (require 'sublimity-map)
;; (require 'sublimity-attractive)
(sublimity-mode 1)
(setq sublimity-scroll-weight 10
      sublimity-scroll-drift-length 5)

;; Setup Company support for Erlang
(add-hook 'erlang-mode-hook #'company-erlang-init)

;; Use web-mode instead of html-mode
(add-hook 'html-mode-hook 'web-mode)

;; Bind comment-dwim-2
(global-set-key (kbd "M-;") 'comment-dwim-2)

;; Bind expand-region
(global-set-key (kbd "C-=") 'er/expand-region)


;; Keep custom variables in a seperate file that git will ignore
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
