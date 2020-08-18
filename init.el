(require 'package)
(add-to-list 'package-archives
;;             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
	     '("melpa" . "http://melpa.org/packages/")
	     )

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

; List the packages you want
(setq package-list
      '( elm-mode
	 haskell-mode
	 which-key
	 move-text
	 expand-region
	 direx
	 smex
	 popwin
	 simple-modeline
	 ace-window
	 dsvn
	 hasky-stack
	 browse-kill-ring
	 treemacs
	 ))


;; as a result of the tortuous process to realize that emacs is the best environment for haskell
;; i record here that one must set a hie.yaml file in the root of a project for it to actually work
;; also its necesary to install haskell ide engine and haskell language server
;; and also install lsp ui lsp mode and lsp haskell


					; activate all the packages
(package-initialize)

					; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

					; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'smyx t)

(let ((filename "~/.emacs.d/startup.txt"))
  (when (file-exists-p filename)
    (setq initial-buffer-choice filename)))


(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
    (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
    ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
    ;; and `package-pinned-packages`. Most users will not need or want to do this.
    ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
    )



;; Global requires
(require 'multiple-cursors)
(require 'elm-mode)
(require 'ido)
(require 'direx)
(require 'popwin)
(require 'vc-svn)
(require 'ace-window)
(require 'expand-region)
(require 'color)
(require 'cl)
(require 'dsvn)
(require 'view)

;; interface
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
;;(display-time-mode 1)
(setq column-number-mode t)
(set-face-foreground 'highlight nil)
(global-hl-line-mode 1)
(which-key-mode)
(popwin-mode)
(setq aw-dispatch-always t)
(setq company-tooltip-align-annotations t)
(move-text-default-bindings)
(ido-mode t)
;;(projectile-mode +1)
(simple-modeline-mode)
(put 'upcase-region 'disabled nil)
(setq visible-bell 1)
;;(setq split-width-threshold nil) ;;for vertical split.
(setq split-width-threshold 1 ) ;;for horizontal split.
;;(setq inhibit-splash-screen t)
;;(setq inhibit-startup-message t)
(add-to-list 'default-frame-alist
	     '(vertical-scroll-bars . nil))
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq display-line-numbers-current-absolute t)
;;(global-display-line-numbers-mode)
(setq company-show-numbers t)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
;(set-window-margins () 3 3)
(setq-default left-margin-width 1 right-margin-width 1)
(show-paren-mode t)
;; remember to add this thing to fix fucked parens in modes like this haskell-indentation-common-electric-command

;; HOOKS

;; Rust

(setq racer-rust-src-path "/home/juicyjouissance/rust-src/rust/src")
(add-hook 'rust-mode-hook 'company-mode)

;; Python
(add-hook 'python-mode-hook 'company-mode)
(setq python-shell-interpreter "python3")
(setq python-indent-offset 2)

;; Haskell

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'company-mode)
(add-hook 'haskell-interactive-mode-hook 'company-mode)

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




;; Elixir
(add-hook 'elixir-mode-hook 'alchemist-mode)
(add-hook 'alchemist-mode-hook 'company-mode)

;; ELisp
(add-hook 'emacs-lisp-mode-hook 'company-mode)

;; Projectile
;;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;; MY CUSTOM BINDS
(define-prefix-command 'custom-mapl)
(define-key custom-mapl (kbd "c") 'company-mode)
(define-key custom-mapl (kbd "b") 'buffer-menu )
(define-key custom-mapl (kbd "e") 'mc/edit-lines)
(define-key custom-mapl (kbd ">") 'mc/mark-next-like-this)
(define-key custom-mapl (kbd "<") 'mc/mark-previous-like-this)
(define-key custom-mapl (kbd "l") 'mc/mark-all-like-this)
(define-key custom-mapl (kbd "s") 'hasky-stack-execute)
(define-key custom-mapl (kbd "SPC") 'company-capf)
(define-key custom-mapl (kbd "j") 'haskell-mode-jump-to-def)
(define-key custom-mapl (kbd "a") 'ace-window)
(define-key custom-mapl (kbd "x") 'other-window)
(define-key custom-mapl (kbd "r") 'browse-kill-ring)
(define-key custom-mapl (kbd "t") 'treemacs)
(define-key custom-mapl (kbd "f") 'fzf-directory)
(define-key custom-mapl (kbd "d") 'dired)
(define-key custom-mapl (kbd "m") 'magit)
(define-key custom-mapl (kbd "q q") 'kill-emacs)




(global-set-key (kbd "C-z") custom-mapl)
(global-set-key (kbd "º") custom-mapl)

(global-set-key (kbd "ç") 'insert-lambda)

(defun insert-lambda ()
  (interactive)
  (insert "\\")
)
 
;; EMACS HAS MODAL EDITING INTEGRATED DUDE
(global-set-key (kbd "<f1>") 'view-mode)
(define-key view-mode-map (kbd "b") 'buffer-menu)
(define-key view-mode-map (kbd ">") 'mc/mark-next-like-this )
(define-key view-mode-map (kbd "<") 'mc/mark-previous-like-this )
(define-key view-mode-map (kbd "l") 'mc/mark-all-like-this)
(define-key view-mode-map (kbd "a") 'ace-window)
(define-key view-mode-map (kbd "x") 'other-window)
(define-key view-mode-map (kbd "m") 'mc/edit-lines)
(define-key view-mode-map (kbd "w") 'save-buffer)
(define-key view-mode-map (kbd "e") 'er/expand-region)
(define-key view-mode-map (kbd "C-<left>") 'shrink-window-horizontally)
(define-key view-mode-map (kbd "C-<right>") 'enlarge-window-horizontally)
(define-key view-mode-map (kbd "C-<down>") 'shrink-window)
(define-key view-mode-map (kbd "C-<up>") 'enlarge-window)


;; Globals
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)
(setq markdown-preview-stylesheets (list "http://thomasf.github.io/solarized-css/solarized-light.min.css"))
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)
(push '(direx:direx-mode :position left :width 50 :dedicated t)
      popwin:special-display-config)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#5f5f5f" "#ff4b4b" "#a1db00" "#fce94f" "#5fafd7" "#d18aff" "#afd7ff" "#ffffff"])
 '(custom-safe-themes
   (quote
    ("a6fc75241bcc7ce6f68dcfd0de2d4c4bd804d0f8cd3a9f08c3a07654160e9abe" "e61752b5a3af12be08e99d076aedadd76052137560b7e684a8be2f8d2958edc3" "7675ffd2f5cb01a7aab53bcdd702fa019b56c764900f2eea0f74ccfc8e854386" "ae65ccecdcc9eb29ec29172e1bfb6cadbe68108e1c0334f3ae52414097c501d2" "ca2e59377dc1ecee2a1069ec7126b453fa1198fed946304abb9a5b8c7ad5404d" "25f81851315ee76bd43cb551767861d24d2450d07e8e3ca412d09adbe28f5f98" "d4f8fcc20d4b44bf5796196dbeabec42078c2ddb16dcb6ec145a1c610e0842f3" "afd761c9b0f52ac19764b99d7a4d871fc329f7392dfc6cd29710e8209c691477" "5903c5f26edd1ef3f9555a2864982b24f2980f096aacb9da0b4b5ccd47962233" "16ab866312f1bd47d1304b303145f339eac46bbc8d655c9bfa423b957aa23cc9" "d9766ec8d1dca35ad9392d212da441517800028b585d187f08ea44ce1c84ebdf" "5d75f9080a171ccf5508ce033e31dbf5cc8aa19292a7e0ce8071f024c6bcad2a" "bd82c92996136fdacbb4ae672785506b8d1d1d511df90a502674a51808ecc89f" default)))
 '(fci-rule-color "#3a3a3a")
 '(hl-sexp-background-color "#121212")
 '(notmuch-search-line-faces
   (quote
    (("unread" :foreground "#aeee00")
     ("flagged" :foreground "#0a9dff")
     ("deleted" :foreground "#ff2c4b" :bold t))))
 '(package-selected-packages
   (quote
    (white-sand-theme which-key treemacs subatomic256-theme subatomic-theme srcery-theme sourcerer-theme smyx-theme smex slime simple-modeline seti-theme reykjavik-theme purple-haze-theme popwin popup pkg-info peacock-theme paper-theme northcode-theme nofrils-acme-theme noctilux-theme multiple-cursors move-text moe-theme minsk-theme metalheart-theme material-theme magit jazz-theme helm-core hasky-stack haskell-mode fzf expand-region elm-mode dsvn direx company browse-kill-ring badwolf-theme avk-emacs-themes)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
