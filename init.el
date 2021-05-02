(require 'package)

(setq gc-cons-threshold (* 100 1024 1024))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-list
      '(fzf
	undo-tree
	visual-regexp
	ryo-modal
	paredit
	twilight-bright-theme
	linum-relative
	which-key
	projectile
	ido
	racket-mode
	magit
	company
	direx
	helm
	treemacs
	treemacs-evil
	evil
	evil-exchange
	zones
	expand-region
	counsel))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;UI
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq column-number-mode t)
(setq visible-bell t)
(display-line-numbers-mode)
(setq display-line-numbers 'relative)

(use-package evil-surround
  :load-path "vendor/evil-surround"
  :hook ((after-init . global-evil-surround-mode)))

(use-package expand-region
  :ensure t)

(use-package paredit
  :load-path "vendor/paredit"
  :diminish paredit-mode
  :config
  (bind-keys :map paredit-mode-map
		     ("{" . paredit-open-curly)
		     ("}" . paredit-close-curly))
  :hook ((emacs-lisp-mode lisp-mode racket-mode scheme-mode) . paredit-mode))

(use-package projectile
  :ensure t)

(use-package company
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.1)
  (setq company-tooltip-align-anotations t)
  :hook
  ((racket-mode . company-mode)
   (racket-repl-mode . company-mode)))

(use-package racket-xp-mode
  :hook racket-mode)

(use-package evil
  :config
  (evil-mode)
  (bind-keys ("C-c C-k" . evil-leader-prefix-map))
  (bind-keys :map evil-normal-state-map
	     ("C" . comment-box)
	     ("m" . er/expand-region))
  (bind-keys :map evil-normal-state-map
	     :prefix "SPC"
	     :prefix-map evil-leader-prefix-map
	     ("x" . evil-exchange))
  (bind-keys :map evil-visual-state-map
	     :prefix "SPC"
	     :prefix-map evil-leader-prefix-map
	     ("x" . evil-exchange))
  )


(use-package treemacs
  :ensure t
  :defer t
  :config (bind-keys ("º" . treemacs))
  )

(use-package treemacs-evil
  :ensure t
  :init (setq treemacs-no-png-images t))


(use-package smex
  :init
  (global-set-key (kbd "M-x") 'smex))

(use-package which-key
  :init
  (which-key-mode))

(use-package twilight-bright-theme
  :when (display-graphic-p)
  :load-path "vendor/twilight-bright-theme"
  :config (load-theme 'twilight-bright t))

(use-package phi-search
  :bind (("C-s" . phi-search)
         ("C-r" . phi-search-backward)))
