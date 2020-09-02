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
	 evil-multiedit
	 linum-relative 
	 evil-surround
	 evil-numbers
	 magit
	 ))



;; activate all the packages
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(load-theme 'gruvbox t)

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
(require 'haskell-mode)
(require 'popwin)
(require 'evil)
(require 'evil-mc)
(require 'treemacs-evil)
(require 'evil-multiedit)
(require 'hydra)
(require 'linum-relative)
(require 'evil-surround)
(require 'all-the-icons)
(require 'evil-numbers)
(require 'magit)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))


;; interface
(setq global-evil-surround-mode 1)
(evil-multiedit-default-keybinds)
(evil-mode 1)
(global-anzu-mode +1)
(global-undo-tree-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
;; dashboard
(setq dashboard-startup-banner nil)
(setq dashboard-items '((recents . 5)
			(projects . 5)
			(agenda . 10)
 			))
(setq treemacs-no-png-images t)
(global-evil-mc-mode 1)
(setq org-agenda-files '("~/org/tasks.org" "~/org/casita.org"))
(dashboard-setup-startup-hook)
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
(evil-define-key 'visual evil-mc-key-map
  "A" #'evil-mc-make-cursor-in-visual-selection-end
  "I" #'evil-mc-make-cursor-in-visual-selection-beg)
(setq scroll-margin 4)
(add-to-list 'evil-emacs-state-modes 'haskell-interactive-mode)
(add-to-list 'evil-emacs-state-modes 'haskell-error-mode)
(add-to-list 'evil-insert-state-modes 'dashboard-mode)
;; remember to add this thing to fix fucked parens in modes like this haskell-indentation-common-electric-command
(add-hook 'prog-mode-hook 'linum-relative-mode)


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
(define-key custom-mapl (kbd "1") 'evil-mc-make-and-goto-prev-match)
(define-key custom-mapl (kbd "2") 'evil-mc-make-and-goto-next-match)
;;(global-set-key (kbd "C-SPC") 'evil-normal-state)
(global-set-key (kbd "º") custom-mapl)
(global-set-key (kbd "ç") 'insert-lambda)

(defun insert-lambda ()
  (interactive)
  (insert "\\")
)

;; my "leader"


(defhydra leader (evil-normal-state-map "SPC")
  "BRRRUUUUUMMMM"
  ("t" treemacs "tree :V")
  ("b" buffer-menu "buffer menu")
  ("f" fzf-directory "find filerinos :o")
  ("+" evil-numbers/inc-at-pt "increaso pawa")
  ("-" evil-numbers/dec-at-pt "decreaso pawa")
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
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes
   '("d88049c628f3a8a92f9e46982d3e891867e4991de2b3a714f29f9f5eb91638c1" "42ec9eaa86da5f052feed0e35b578681015b9e21ab7b5377a5a34ea9a0a9e1b9" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "bd82c92996136fdacbb4ae672785506b8d1d1d511df90a502674a51808ecc89f" "5d75f9080a171ccf5508ce033e31dbf5cc8aa19292a7e0ce8071f024c6bcad2a" "d9766ec8d1dca35ad9392d212da441517800028b585d187f08ea44ce1c84ebdf" "cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2" "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae" "ffba0482d3548c9494e84c1324d527f73ea4e43fff8dfd0e48faa8fc6d5c2bc7" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "3d4df186126c347e002c8366d32016948068d2e9198c496093a96775cc3b3eaa" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "a3b6a3708c6692674196266aad1cb19188a6da7b4f961e1369a68f06577afa16" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "587938eeeaefd2b2f68a0970e02985246a28c02c1c140cb0943d2b6909c47261" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "4f01c1df1d203787560a67c1b295423174fd49934deb5e6789abd1e61dba9552" "b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "d5a878172795c45441efcd84b20a14f553e7e96366a163f742b95d65a3f55d71" "74ba9ed7161a26bfe04580279b8cad163c00b802f54c574bfa5d924b99daa4b9" "8d7684de9abb5a770fbfd72a14506d6b4add9a7d30942c6285f020d41d76e0fa" "188fed85e53a774ae62e09ec95d58bb8f54932b3fd77223101d036e3564f9206" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" "a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "e1d09f1b2afc2fed6feb1d672be5ec6ae61f84e058cb757689edb669be926896" "123a8dabd1a0eff6e0c48a03dc6fb2c5e03ebc7062ba531543dfbce587e86f2a" "b89ae2d35d2e18e4286c8be8aaecb41022c1a306070f64a66fd114310ade88aa" "4c460925984441cad09c74e042fa9d26f4b35320e04d6fb8a265d1a61c9f5c45" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" default))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(emms-mode-line-icon-color "#1ba1a1")
 '(fci-rule-color "#151515")
 '(gnus-logo-colors '("#4c8383" "#bababa") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1ba1a1\",
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
\"###########.######\" };") t)
 '(jdee-db-active-breakpoint-face-colors (cons "#969896" "#444444"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#969896" "#282a2e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#969896" "#282a2e"))
 '(nrepl-message-colors
   '("#ee11dd" "#8584ae" "#b4f5fe" "#4c406d" "#ffe000" "#ffa500" "#ffa500" "#DC8CC3"))
 '(objed-cursor-color "#282a2e")
 '(package-selected-packages
   '(colorless-themes avk-emacs-themes zerodark-theme zweilight-theme almost-mono-themes alect-themes abyss-theme doom-themes all-the-icons which-key use-package treemacs-evil swiper smyx-theme smex sly rainbow-delimiters projectile popwin move-text linum-relative hasky-stack haskell-mode gruvbox-theme fzf evil-surround evil-multiedit evil-mc elm-mode dsvn direx dashboard company browse-kill-ring anzu))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#1d2021"))
 '(rustic-ansi-faces
   ["#ffffff" "#282a2e" "#282a2e" "#282a2e" "#282a2e" "#282a2e" "#282a2e" "#282a2e"])
 '(vc-annotate-background "#ffffff")
 '(vc-annotate-color-map
   (list
    (cons 20 "#282a2e")
    (cons 40 "#282a2e")
    (cons 60 "#282a2e")
    (cons 80 "#282a2e")
    (cons 100 "#282a2e")
    (cons 120 "#282a2e")
    (cons 140 "#282a2e")
    (cons 160 "#282a2e")
    (cons 180 "#282a2e")
    (cons 200 "#282a2e")
    (cons 220 "#282a2e")
    (cons 240 "#282a2e")
    (cons 260 "#282a2e")
    (cons 280 "#282a2e")
    (cons 300 "#282a2e")
    (cons 320 "#282a2e")
    (cons 340 "#c5c8c6")
    (cons 360 "#c5c8c6")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
