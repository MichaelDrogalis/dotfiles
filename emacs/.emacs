(setq inhibit-startup-message t)
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)
(setq visible-bell t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq transient-mark-mode t)
(setq c-basic-offset 4)
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse t)
(setq js-indent-level 2)
(setq cider-repl-display-help-banner nil)
(setq backup-directory-alist `(("." . "~/.backup-emacs")))
(setq org-log-done t)
(setq org-agenda-files '("~/org"))

(require 'package)

;;; Packages
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

;;; Faces

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (projectile cider json-mode rjsx-mode web-mode sass-mode paredit ensime scala-mode erlang yaml-mode auto-complete parent-mode auto-complete-clang-async go-mode dockerfile-mode clojure-mode clojure-mode-extra-font-locking ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(show-paren-mode t)
(global-linum-mode t)
(global-hl-line-mode t)
(display-time-mode t)
(global-auto-complete-mode t)
(ido-mode t)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)

(define-key global-map (kbd "C-x SPC") 'ace-jump-mode)

(set-face-background 'hl-line "color-194")
