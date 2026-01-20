(setq inhibit-startup-message t
      visible-bell t
      column-number-mode t
      scroll-step 1
      tool-bar-mode -1
      menu-bar-mode -1
      scroll-bar-mode -1)

(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

(setq backup-directory-alist `(("." . "~/.saves")))

(set-face-attribute 'default nil :height 175)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-display-line-numbers-mode t)

(rassq-delete-all 'change-log-mode auto-mode-alist)

(setq-default indent-tabs-mode nil)

(setq package-list '(use-package))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("gnu-devel" . "https://elpa.gnu.org/devel/")))

(tool-bar-mode -1)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-display-line-numbers-mode t)
 '(menu-bar-mode -1)
 '(package-selected-packages
   '(docker docker-mode hcl-mode rsjx rsjx-mode js2-mode yaml-mode terraform terraform-mode json-mode rainbow-delimiters web-mode cider clojure-mode clojure exec-path-from-shell multiple-cursors lsp-tailwindcss vterm compat magit-version magit yasnippet-snippets yasnippet flycheck tree-sitter-langs tree-sitter modus-themes solo-jazz-theme company company-mode use-package tide projectile ace-jump-mode paredit prettier-js rjsx-mode solarized-theme))
 '(tool-bar-mode -1)
 '(warning-suppress-log-types '((use-package) (use-package) (use-package)))
 '(warning-suppress-types '((use-package) (use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Menlo" :foundry "nil" :slant normal :weight regular :height 140 :width normal)))))

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(global-hl-line-mode t)

(use-package ido-completing-read+
  :ensure t
  :config
  ;; This enables ido in all contexts where it could be useful, not just
  ;; for selecting buffer and file names
  (ido-mode t)
  (ido-everywhere t)
  ;; This allows partial matches, e.g. "uzh" will match "Ustad Zakir Hussain"
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  ;; Includes buffer names of recently opened files, even if they're not open now.
  (setq ido-use-virtual-buffers t)
  :diminish nil)

(use-package smex
  :ensure t
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  :bind ("M-x" . smex))

(use-package paredit
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  :config
  (show-paren-mode t)
  (define-key paredit-mode-map (kbd "RET") nil)
  :bind (("M-[" . paredit-wrap-square)
         ("M-{" . paredit-wrap-curly))
  :diminish nil)

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi t))

(use-package ace-jump-mode
  :ensure t
  :bind ("C-x SPC" . ace-jump-mode))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.3)
  (setq tab-always-indent 'complete)
  (global-company-mode t))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change)))

(use-package web-mode
  :ensure t
  :after (flycheck company)
  :mode (("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" . web-mode))
  :config
  (company-mode +1)
  (eldoc-mode +1))

(use-package prettier-js
  :ensure t
  :hook (web-mode css-mode)
  :config
  (setq prettier-js-args '("--print-width" "120")))

(use-package tide
  :ensure t
  :after (web-mode company flycheck)
  :hook ((web-mode . tide-setup)
         (web-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (global-set-key (kbd "C-;") 'yas-expand)
  :bind
  (:map yas-minor-mode-map
        ("TAB" . nil)
        ("<tab>" . nil)))

(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet))

(use-package magit
  :ensure t
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

(use-package vterm
  :ensure t
  :config
  (vterm-send-string "source ~/.bashrc"))

(use-package multiple-cursors
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t
  :pin melpa
  :config
  (setq cider-repl-display-help-banner nil))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package json-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package rjsx-mode
  :ensure t)

(use-package hcl-mode
  :ensure t)

(use-package docker
  :ensure t)

(defun clerk-show ()
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     "(nextjournal.clerk/clear-cache!)")
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

(define-key clojure-mode-map (kbd "<M-return>") 'clerk-show)
