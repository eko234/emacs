;; some useful stuff
;; META
;; t transpose
;; h mark paragraph
;; % query replace
;; s-o list of regex matching lines
;; x h select all\
;; C-u C-s regex search


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
	 multiple-cursors
	 company
	 direx
	 smex
	 popwin
	 simple-modeline
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
	 hydra
	 rainbow-delimiters
	 swiper
	 use-package
	 dashboard
	 projectile
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
;;(require 'vc-svn)
(require 'ace-window)
(require 'expand-region)
(require 'color)
(require 'cl)
(require 'dsvn)
(require 'view)
;;(require 'org)
;;(require 'dashboard)
(require 'projectile)
;;(require 'org)
;;(require 'use-package)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))


;; interface

(setq dashboard-items '((recents . 5)
			(projects . 5)
			(agenda . 10)
 			))
(custom-set-variables
 '(org-agenda-files (quote ("~/org/tasks.org" "~/org/casita.org"))))


(dashboard-setup-startup-hook)



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
(simple-modeline-mode)
(put 'upcase-region 'disabled nil)
(setq visible-bell 1)
(setq split-width-threshold nil) ;;for vertical split.
;;(setq split-width-threshold 1 ) ;;for horizontal split.

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


;; Org

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; Rust

(setq racer-rust-src-path "/home/juicyjouissance/rust-src/rust/src")
(add-hook 'rust-mode-hook 'company-mode)

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




;; Elixir
;;(add-hook 'elixir-mode-hook 'alchemist-mode)
;;(add-hook 'alchemist-mode-hook 'company-mode)

;; ELisp
;; Lisp
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
	 

(setq inferior-lisp-program "sbcl")



;; Scheme
(setq scheme-program-name "guile")
;; This hook lets you use your theme colours instead of quack's ones.
;; use run-scheme to bring repl,

(defun scheme-mode-quack-hook ()
  (require 'quack)
  (setq quack-fontify-style 'emacs)
  (setq quack-default-program "guile"))


(add-hook 'scheme-mode-hook 'scheme-mode-quack-hook 'rainbow-delimiters)



;; Projectile
;;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)




;; MY CUSTOM BINDS
(define-prefix-command 'custom-mapl)
(define-key custom-mapl (kbd "c") 'company-mode)
(define-key custom-mapl (kbd "o") 'comment-region)
(define-key custom-mapl (kbd "k") 'kill-buffer)
(define-key custom-mapl (kbd "z") 'undo)
(define-key custom-mapl (kbd "g") 'keyboard-quit)
(define-key custom-mapl (kbd "w") 'save-buffer)
(define-key custom-mapl (kbd "b") 'buffer-menu )

(define-key custom-mapl (kbd "e") 'mc/edit-lines)
(define-key custom-mapl (kbd "1") 'mc/mark-previous-like-this)
(define-key custom-mapl (kbd "2") 'mc/mark-next-like-this)
(define-key custom-mapl (kbd "3") 'er/expand-region)
(define-key custom-mapl (kbd "l") 'mc/mark-all-like-this)

(define-key custom-mapl (kbd "s") 'hasky-stack-execute)
(define-key custom-mapl (kbd "S") 'flx-isearch-forward)

;;(define-key custom-mapl (kbd "SPC") 'company-capf)
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


(global-set-key (kbd "C-z") custom-mapl)


(global-set-key (kbd "º") custom-mapl)

(global-set-key (kbd "ç") 'insert-lambda)

(defun insert-lambda ()
  (interactive)
  (insert "\\")
)

;; Inset moves
;; (global-set-key (kbd "C-j") 'previous-line )
;; (global-set-key (kbd "C-k") 'next-line )
;; (global-set-key (kbd "C-h") 'backward-char)
;; (global-set-key (kbd "C-l") 'forward-char)
;; (global-set-key (kbd "C-n") 'backward-word)
;; (global-set-key (kbd "C-.") 'forward-word)
;; (global-set-key (kbd "C-m") 'backward-sentence)
;; (global-set-key (kbd "C-,") 'forward-sentence)

;; Hydras

(defhydra nav (global-map "ESC SPC")
  "nav"
  ("j" previous-line "nl")
  ("k" next-line  "pl")
  ("h" backward-char "bc")
  ("l" forward-char "fc")
  ("n" backward-word "bw")
  ("." forward-word "fw")
  ("m" backward-sentence "bs")
  ("," forward-sentence "fs")
  ("SPC" set-mark-command "ma")
  ("g" keyboard-quit "kq")
  ("i" nil "eh")
  )







;; EMACS HAS MODAL EDITING INTEGRATED DUDE ... SORTA
;; (global-set-key (kbd "<f1>") 'view-mode)
;; (define-key view-mode-map (kbd "h") 'next-line )
;; (define-key view-mode-map (kbd "a") 'ace-window)
;; (define-key view-mode-map (kbd "x") 'other-window)
;; (define-key view-mode-map (kbd "w") 'save-buffer)
;; (define-key view-mode-map (kbd "C-<left>") 'shrink-window-horizontally)
;; (define-key view-mode-map (kbd "C-<right>") 'enlarge-window-horizontally)
;; (define-key view-mode-map (kbd "C-<down>") 'shrink-window)
;; (define-key view-mode-map (kbd "C-<up>") 'enlarge-window)


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

