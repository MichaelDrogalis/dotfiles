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

(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))

(defalias 'yes-or-no-p 'y-or-n-p)

(global-display-line-numbers-mode t)

(setq package-list '(use-package))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("gnu-devel" . "https://elpa.gnu.org/devel/")))

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
   '(multiple-cursors lsp-tailwindcss vterm compat magit-version magit yasnippet-snippets yasnippet flycheck tree-sitter-langs tree-sitter modus-themes solo-jazz-theme company company-mode use-package tide projectile ace-jump-mode paredit prettier-js rjsx-mode solarized-theme))
 '(tool-bar-mode -1))
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
  :bind (("M-[" . paredit-wrap-square)
         ("M-{" . paredit-wrap-curly))
  :diminish nil)

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-operandi t))

(use-package ace-jump-mode
  :ensure t
  :bind ("C-x SPC" . ace-jump-mode))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.3)
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
  :hook (web-mode css-mode))

(use-package tide
  :ensure t
  :after (web-mode company flycheck)
  :hook ((web-mode . tide-setup)
         (web-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet))

(use-package magit
  :ensure t
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

(use-package vterm
y  :ensure t
  :config
  (vterm-send-string "source ~/.bashrc"))

(use-package multiple-cursors
  :ensure t)
