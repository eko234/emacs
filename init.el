;; Could it be possible to really emulate kakoune with zones?

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
	kakoune
	evil-paredit
	which-key
	visual-regexp-steroids
	projectile
	ido
	racket-mode
	magit
	company
	company-fuzzy
	direx
	general
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
(show-paren-mode)

(use-package evil-surround			       
  :ensure t
  :hook ((after-init . global-evil-surround-mode))
  :config
  (global-evil-surround-mode 1)) 

(use-package general
  :ensure t)

(use-package evil
  :config
  (evil-mode)
  ;; (general-nmap
  ;;  :prefix "spc"
  ;;  :prefix-map 'evil-leader-prefix-map
  ;;  )
  (setq evil-cross-lines t)
  (setq evil-move-cursor-back nil)
  (bind-keys ("C-c C-9" . evil-leader-prefix-map))
  (bind-keys :map evil-normal-state-map
	     ("m" . er/expand-region)
	     ("M-h" . paredit-backward)
	     ("M-j" . paredit-forward-down)
	     ("M-k" . paredit-backward-up)
	     ("M-l" . paredit-forward)
	     ("M-H" . paredit-backward-slurp-sexp)
	     ("M-J" . paredit-forward-slurp-sexp)
	     ("M-K" . paredit-forward-barf-sexp)
	     ("M-L" . paredit-backward-barf-sexp))
  (bind-keys :map evil-visual-state-map
	     :prefix "SPC"
	     :prefix-map evil-leader-prefix-map
	     ("c" . comment-line)
	     ("C" . comment-box))
  (bind-keys :map evil-normal-state-map
	     :prefix "SPC"
	     :prefix-map evil-leader-prefix-map
	     ("w" . save-buffer)
	     ("SPC" . set-mark-command)
	     ("C" . comment-box)
	     ("c" . comment-line)
	     ("x" . evil-exchange)))

;; This overrides the default mark-in-region with a prettier-looking one,
;; and provides a couple extra commands
(use-package visual-regexp-steroids
  :ensure t)

(use-package visual-regexp
  :ensure t
  :config
  (bind-keys :map evil-normal-state-map
	     ("s" . vr/query-replace)
	     ("?" . vr/replace)
	     ("M-/" . vr/query-replace)))

					;Emacs incremental search doesn't work with multiple cursors, but this fixes that
(use-package phi-search
  :bind (("C-s" . phi-search)
	 ("C-r" . phi-search-backward)))

;; Probably the first thing you'd miss is undo and redo, which requires an extra package
;; to work like it does in kakoune (and almost every other editor).
;; (use-package undo-tree
;;   :config
;;   (global-undo-tree-mode)
;;   :ryo
;;   ("u" undo-tree-undo)
;;   ("U" unda-tree-redo)
;;   ("SPC u" undo-tree-visualize)
;;   :bind (:map undo-tree-visualizer-mode-map
;;               ("h" . undo-tree-visualize-switch-branch-left)
;;               ("j" . undo-tree-visualize-redo)
;;               ("k" . undo-tree-visualize-undo)
;;               ("l" . undo-tree-visualize-switch-branch-right)))

(use-package expand-region
  :ensure t)

(use-package evil-paredit
  :ensure t)

(use-package paredit
  :load-path "vendor/paredit"
  :diminish paredit-mode
  :config
  (bind-keys :map paredit-mode-map
	     ("{" . paredit-open-curly)
	     ("}" . paredit-close-curly))
  (evil-paredit-mode)
  :hook ((emacs-lisp-mode lisp-mode racket-mode scheme-mode) . paredit-mode))

(use-package projectile
  :ensure t)

(use-package company
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.1)
  (setq company-tooltip-align-anotations t)
  :hook
  ((emacs-lisp-mode . company-mode)
   (racket-mode . company-mode)
   (racket-repl-mode . company-mode)))

(use-package company-fuzzy
  :init (global-company-fuzzy-mode 1))

(use-package racket-xp-mode
  :hook racket-mode)

(use-package treemacs
  :ensure t
  :defer t
  :config (bind-keys ("º" . treemacs)))

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
