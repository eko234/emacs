;; META
;; t transpose
;; % query replace
;; s-o list of regex matching lines
;; C-u C-s regex search
;; F3 record macr F4 save macro / aply macro
;; you can cycle the macro ring

(setq gc-cons-threshold (* 100 1024 1024))
(require 'package)
(add-to-list 'package-archives
;;            '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
 	     '("melpa" . "http://melpa.org/packages/")
 	     )

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

; List 
(setq package-list
      '( elm-mode
	 haskell-mode
	 which-key
	 move-text
	 company
	 direx
	 smex
	 popwin
	 ace-window
	 dsvn
	 hasky-stack
	 browse-kill-ring
	 treemacs
	 sly
	 ace-window
	 smyx-theme
	 ido
	 fzf
	 rainbow-delimiters
	 swiper
	 use-package
	 dashboard
	 projectile
	 undo-tree
	 anzu
	 magit
	 linum-relative 
	 ;; Evil Packages
	 evil
	 evil-mc
	 treemacs-evil
	 evil-multiedit
	 evil-surround
	 evil-numbers
	 evil-magit
	 evil-visualstar
	 ))

;; Activate all the packages
(package-initialize)
; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(load-theme 'gruvbox t)

;; Requires
(require 'haskell-mode)
(require 'popwin)
(require 'hydra)
(require 'linum-relative)
(require 'magit)
;; Evil Requires
(require 'evil)
(require 'evil-mc)
(require 'treemacs-evil)
(require 'evil-multiedit)
(require 'evil-surround)
(require 'evil-numbers)
(require 'evil-magit)

;; interface
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-anzu-mode +1)
(global-undo-tree-mode)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Evil things
(setq evil-ex-visual-char-range t)
(setq global-evil-surround-mode 1)
(evil-multiedit-default-keybinds)
(evil-mode 1)
(global-evil-mc-mode 1)
(evil-define-key 'visual evil-mc-key-map
  "A" #'evil-mc-make-cursor-in-visual-selection-end
  "I" #'evil-mc-make-cursor-in-visual-selection-beg)
(add-to-list 'evil-emacs-state-modes 'haskell-interactive-mode)
(add-to-list 'evil-emacs-state-modes 'haskell-error-mode)
(add-to-list 'evil-insert-state-modes 'dashboard-mode)
(global-evil-visualstar-mode)
;; Dashboard
(setq dashboard-startup-banner nil)
(setq dashboard-items '((recents . 5)
			(projects . 5)
			(agenda . 10)
 			))
(setq treemacs-no-png-images t)
(setq org-agenda-files '("~/org/tasks.org" "~/org/casita.org"))
(dashboard-setup-startup-hook)
(setq dashboard-footer-messages '("Thanks Richard Stallman"))
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq column-number-mode t)
(set-face-foreground 'highlight nil)
(global-hl-line-mode 1)
(which-key-mode)
(popwin-mode)
(setq aw-dispatch-always t)
(setq company-tooltip-align-annotations t)
(move-text-default-bindings)
(ido-mode t)
(put 'upcase-region 'disabled nil)
(setq visible-bell 1)
(setq split-width-threshold nil) ;;for vertical split.
;;(setq split-width-threshold 1 ) ;;for horizontal split.
(add-to-list 'default-frame-alist
	     '(vertical-scroll-bars . nil))
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq company-show-numbers t)
(setq company-idle-delay nil)
(setq company-minimum-prefix-length 1)
(setq-default left-margin-width 1 right-margin-width 1)
(show-paren-mode t)
(set-face-italic-p 'italic nil)
(setq scroll-margin 4)
;; remember to add this thing to fix fucked parens in modes like this haskell-indentation-common-electric-command
(add-hook 'prog-mode-hook 'linum-relative-mode)
(setq linum-update-current t)
(setq linum-relative-current-symbol "")

;; Org
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; Python
(add-hook 'python-mode-hook 'company-mode)
(setq python-shell-interpreter "python3")
(setq python-indent-offset 2)

;; Elm
(add-hook 'elm-mode-hook 'company-mode)

;; Haskell
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'company-mode)
;;(add-hook 'haskell-interactive-mode-hook 'company-mode)
(setq haskell-doc-prettify-types t)
(setq haskell-doc-show-global-types t)
(setq haskell-doc-show-user-defined t)
(define-key haskell-mode-map (kbd "C-c C-d") 'show-doc)

;; Show haskell documentation

(defun thing-or-region ()
  (interactive)
  (if (region-active-p)
      (buffer-substring (region-beginning) (region-end))
    (thing-at-point 'word)
    )
  )

(defun show-info ()
  (interactive)
  (let ((input_ (thing-or-region)))
    (with-current-buffer (haskell-interactive-buffer)
      (haskell-interactive-mode-run-expr (concat ":i " input_))
      )
    ))

(defun set-show-mem-and-time ()
  (interactive)
  (with-current-buffer (haskell-interactive-buffer)
    (haskell-interactive-mode-run-expr ":set +s")
      )
    )

(defun show-type ()
  (interactive)
  (let ((input_ (thing-or-region)))
    (with-current-buffer (haskell-interactive-buffer)
      (haskell-interactive-mode-run-expr (concat ":t " input_))
      )
    ))

(defun show-doc ()
  (interactive)
  (let ((input_ (thing-or-region)))
    (with-current-buffer (haskell-interactive-buffer)
      (haskell-interactive-mode-run-expr (concat ":doc " input_ ))
      )
    )
  )

;; ELisp
;; Lisp
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(setq inferior-lisp-program "sbcl")

;; MY CUSTOM BINDS
(define-prefix-command 'custom-mapl)
(define-key custom-mapl (kbd "c") 'company-mode)
(define-key custom-mapl (kbd "o") 'comment-region)
(define-key custom-mapl (kbd "O") 'uncomment-region)
(define-key custom-mapl (kbd "k") 'kill-buffer)
(define-key custom-mapl (kbd "<") 'undo-tree-undo)
(define-key custom-mapl (kbd "z") 'undo-tree-redo)
(define-key custom-mapl (kbd "Z") 'undo-tree-visualize)
(define-key custom-mapl (kbd "g") 'keyboard-quit)
(define-key custom-mapl (kbd "w") 'save-buffer)
(define-key custom-mapl (kbd "b") 'buffer-menu )
(define-key custom-mapl (kbd "s") 'hasky-stack-execute)
(define-key custom-mapl (kbd "S") 'flx-isearch-forward)
(define-key custom-mapl (kbd "SPC") 'company-capf)
(define-key custom-mapl (kbd "j") 'haskell-mode-jump-to-def)
(define-key custom-mapl (kbd "a") 'ace-window)
(define-key custom-mapl (kbd "x") 'other-window)
(define-key custom-mapl (kbd "r") 'browse-kill-ring)
(define-key custom-mapl (kbd "t") 'treemacs)
(define-key custom-mapl (kbd "f") 'fzf-directory)
(define-key custom-mapl (kbd "d") 'dired)
(define-key custom-mapl (kbd "m") 'magit)
(define-key custom-mapl (kbd "q q q") 'kill-emacs)
(define-key custom-mapl (kbd "q r c") 'copy-rectangle-as-kill)
(define-key custom-mapl (kbd "q r y") 'yank-rectangle)
(define-key custom-mapl (kbd "u") 'delete-other-windows)
(define-key custom-mapl (kbd "1") 'evil-mc-make-and-goto-prev-match)
(define-key custom-mapl (kbd "2") 'evil-mc-make-and-goto-next-match)
(global-set-key (kbd "º") custom-mapl)
;; My "leader"
(define-key evil-normal-state-map (kbd "SPC") 'custom-mapl)
(defhydra brutus (evil-normal-state-map "ñ")
  "BRRRUUUUUMMMM"
  ("+" evil-numbers/inc-at-pt "increaso pawa")
  ("-" evil-numbers/dec-at-pt "decreaso pawa")
  )
;; moves
(global-set-key (kbd "M-¡") 'move-text-down)
(global-set-key (kbd "M-¿") 'move-text-up)
(global-set-key (kbd "<f11>") 'shrink-window-horizontally)
(global-set-key (kbd "<f10>") 'enlarge-window-horizontally )
(global-set-key (kbd "<f9>") 'shrink-window)
(global-set-key (kbd "<f12>") 'enlarge-window)
(global-set-key (kbd "<f8>") 'menu-bar-open)

;; Globals
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)
(push '(direx:direx-mode :position left :width 50 :dedicated t)
      popwin:special-display-config)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)
