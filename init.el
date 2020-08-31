;; some useful stuff
;; Meslo LGL
;; META
;; t transpose
;; h mark paragraph
;; % query replace
;; s-o list of regex matching lines
;; x h select all\
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


; List the packages you want
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
	 treemacs-evil
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
	 evil
	 evil-mc
	 ))








					; activate all the packages
(package-initialize)

					; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

					; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(load-theme 'smyx t)

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
;;(require 'multiple-cursors)
;;(require 'elm-mode)
(require 'haskell-mode)
;;(require 'ido)
;;(require 'direx)
(require 'popwin)
;;(require 'vc-svn)
;;(require 'ace-window)
;;(require 'expand-region)
;;(require 'color)
;;(require 'cl)
;;(require 'dsvn)
;;(require 'view)
;;(require 'org)
;;(require 'dashboard)
;;(require 'projectile)
;;(require 'use-package)
(require 'evil)
(require 'evil-mc)
;;(powerline-default-theme)
(require 'treemacs-evil)
;;(powerline-nano-theme)


(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))



;; interface

(evil-mode 1)
(global-anzu-mode +1)
(global-undo-tree-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq dashboard-items '((recents . 5)
			(projects . 5)
			(agenda . 10)
 			))
(global-evil-mc-mode 1)
(setq org-agenda-files '("~/org/tasks.org" "~/org/casita.org"))

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
(put 'upcase-region 'disabled nil)
(setq visible-bell 1)
(setq split-width-threshold nil) ;;for vertical split.
;;(setq split-width-threshold 1 ) ;;for horizontal split.


(add-to-list 'default-frame-alist
	     '(vertical-scroll-bars . nil))
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq display-line-numbers-current-absolute t)
(setq company-show-numbers t)
(setq company-idle-delay nil)
(setq company-minimum-prefix-length 1)
(setq-default left-margin-width 1 right-margin-width 1)
(show-paren-mode t)
(set-face-italic-p 'italic nil)


;; special-evil-buffers
(add-to-list 'evil-emacs-state-modes 'haskell-interactive-mode)
(add-to-list 'evil-emacs-state-modes 'haskell-error-mode)
;; remember to add this thing to fix fucked parens in modes like this haskell-indentation-common-electric-command


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
;;(setq haskell-interactive-popup-errors nil)
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

;;(global-set-key (kbd "C-SPC") 'evil-normal-state)
(global-set-key (kbd "º") custom-mapl)
(global-set-key (kbd "ç") 'insert-lambda)

(defun insert-lambda ()
  (interactive)
  (insert "\\")
)



;; moves
(global-set-key (kbd "<f11>") 'shrink-window-horizontally)
(global-set-key (kbd "<f10>") 'enlarge-window-horizontally )
(global-set-key (kbd "<f9>") 'shrink-window)
(global-set-key (kbd "<f12>") 'enlarge-window)
(global-set-key (kbd "<f8>") 'menu-bar-open)
(global-set-key (kbd "M-<f11>") 'move-text-up)
(global-set-key (kbd "M-<f12>") 'move-text-down)

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



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(background-color "#202020")
 '(background-mode dark)
 '(cursor-color "#cccccc")
 '(custom-safe-themes
   (quote
    ("1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "1de8de5dddd3c5138e134696180868490c4fc86daf9780895d8fc728449805f3" "a6fc75241bcc7ce6f68dcfd0de2d4c4bd804d0f8cd3a9f08c3a07654160e9abe" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "4c460925984441cad09c74e042fa9d26f4b35320e04d6fb8a265d1a61c9f5c45" "9ef81da35ce99a4c7155db7d46e4f8c20a51860d6879cf082e3ed1c5222c17d3" "b89ae2d35d2e18e4286c8be8aaecb41022c1a306070f64a66fd114310ade88aa" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "cae81b048b8bccb7308cdcb4a91e085b3c959401e74a0f125e7c5b173b916bf9" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "d5a878172795c45441efcd84b20a14f553e7e96366a163f742b95d65a3f55d71" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "7c0495f3973b9f79251205995ccccca41262b41a86553f81efe71c0dc3a50f43" "ffac21ab88a0f4603969a24b96993bd73a13fe0989db7ed76d94c305891fad64" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-color "#1fb3b3")
 '(fci-rule-color "#151515")
 '(foreground-color "#cccccc")
 '(gnus-logo-colors (quote ("#2fdbde" "#c0c0c0")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#D08770"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#A3BE8C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#4f5b66"))
 '(linum-format " %7i ")
 '(objed-cursor-color "#BF616A")
 '(package-selected-packages
   (quote
    (treemacs-evil smart-mode-line which-key use-package treemacs swiper sublime-themes smyx-theme smex sly rainbow-delimiters projectile powerline popwin popup noctilux-theme names multiple-cursors move-text magit kaolin-themes hasky-stack haskell-mode gruvbox-theme fzf flx evil-multiedit evil-mc elm-mode dsvn doom-themes direx dashboard company browse-kill-ring base16-theme avk-emacs-themes anzu alect-themes)))
 '(pdf-view-midnight-colors (cons "#c0c5ce" "#2b303b"))
 '(pos-tip-background-color "#191F26")
 '(pos-tip-foreground-color "#d4d4d6")
 '(rustic-ansi-faces
   ["#2b303b" "#BF616A" "#A3BE8C" "#ECBE7B" "#8FA1B3" "#c678dd" "#46D9FF" "#c0c5ce"])
 '(vc-annotate-background "#2b303b")
 '(vc-annotate-color-map
   (list
    (cons 20 "#A3BE8C")
    (cons 40 "#bbbe86")
    (cons 60 "#d3be80")
    (cons 80 "#ECBE7B")
    (cons 100 "#e2ab77")
    (cons 120 "#d99973")
    (cons 140 "#D08770")
    (cons 160 "#cc8294")
    (cons 180 "#c97db8")
    (cons 200 "#c678dd")
    (cons 220 "#c370b6")
    (cons 240 "#c16890")
    (cons 260 "#BF616A")
    (cons 280 "#a35f69")
    (cons 300 "#875e68")
    (cons 320 "#6b5c67")
    (cons 340 "#65737E")
    (cons 360 "#65737E")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
