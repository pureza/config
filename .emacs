;; Fullscreen mode
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))


; Make the buffer in the first window the buffer in the second and
; vice versa.  Broken assumes two windows only.
(defun my-swap-buffers ()
  "Swap the buffers in the window and the next other window."
  (interactive)
  (let ((my-orig-win-buffer (buffer-name)))
    (other-window 1)
    (let ((my-other-win-buffer (buffer-name)))
      (switch-to-buffer my-orig-win-buffer)
      (other-window 1)
      (switch-to-buffer my-other-win-buffer)))
  (other-window 1))


;; Load path
(add-to-list 'load-path "~/.emacs.d")

(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t)
(tool-bar-mode nil)
(bar-cursor-mode t)
(column-number-mode t)
(setq display-time-24hr-format t)
(display-time)
(setq scroll-step 1) ; Scroll one line at a time
(set-scroll-bar-mode 'right)
(setq make-backup-files nil) 
(global-hl-line-mode t)
(shell-command-completion-mode t)
(show-paren-mode t)
(tabbar-mode t)
(transient-mark-mode t)
(fullscreen)


;; Keys
(global-set-key [f11] 'fullscreen)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key "\C-cg" 'grep)
(global-set-key "\C-cr" 'remember)
(global-set-key "\C-cw" 'my-swap-buffers)


; ido
(require 'ido)
(ido-mode t)


;; occur-mode
(global-set-key "\C-co" 'occur)
(define-key occur-mode-map "n" 'next-error-no-select)
(define-key occur-mode-map "p" 'previous-error-no-select)


;; Ruby!
(require 'inf-ruby)


;; Python
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")


;; yasnippet
(require 'yasnippet-bundle)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")


;; GIT
(setq load-path (cons (expand-file-name 
		       "/usr/share/doc/git-core/contrib/emacs") load-path))
(require 'vc-git)
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
(require 'git)
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)


;; Add comment keywords to progmodes
(mapcar (lambda (mode)
	  (font-lock-add-keywords mode
				  '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\)" 1 font-lock-warning-face prepend))))
	'(c-mode python-mode ruby-mode))


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-layout-window-sizes (quote (("left3" (0.24561403508771928 . 0.6333333333333333) (0.24561403508771928 . 0.18333333333333332) (0.24561403508771928 . 0.16666666666666666)) ("left8" (0.23391812865497075 . 0.26666666666666666) (0.23391812865497075 . 0.25) (0.23391812865497075 . 0.26666666666666666) (0.23391812865497075 . 0.2)))))
 '(ecb-options-version "2.32")
 '(ecb-tip-of-the-day nil)
 '(org-agenda-files (quote ("~/organizer.org"))))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(company-pseudo-tooltip-selection-face ((t (:inherit company-pseudo-tooltip-face :background "#ff6600"))))
 '(diff-added ((t (:inherit diff-changed :background "#EEFFEE" :foreground "#009900"))))
 '(diff-changed ((nil (:background "grey95"))))
 '(diff-context ((((class color grayscale) (min-colors 88)) (:inherit shadow :foreground "#333333"))))
 '(diff-file-header ((((class color) (min-colors 88) (background light)) (:weight bold))))
 '(diff-header ((((class color) (min-colors 88) (background light)) (:foreground "#3333FF"))))
 '(diff-hunk-header ((t (:background "#eeeeee" :weight bold))))
 '(diff-indicator-added ((t (:inherit diff-added :weight bold))))
 '(diff-indicator-removed ((t (:inherit diff-removed :weight bold))))
 '(diff-removed ((t (:inherit diff-changed :background "#FFEEEE" :foreground "#990000"))))
 '(ecb-default-general-face ((((class color) (background light)) (:height 100 :family "microsoft-tahoma"))))
 '(ecb-default-highlight-face ((((class color) (background light)) (:inherit ecb-default-general-face :background "cornflower blue" :foreground "yellow" :family "microsoft-tahoma"))))
 '(ecb-tag-header-face ((((class color) (background light)) (:background "SeaGreen1"))))
 '(ecb-tree-highlight-face ((((class color) (background light)) (:inherit (ecb-default-general-face highlight) :height 1.0))))
 '(file-name-shadow ((t (:inherit shadow :foreground "grey80"))))
 '(fixed-pitch ((t nil)))
 '(flyspell-duplicate ((t (:foreground "Gold3" :underline t))))
 '(flyspell-incorrect ((t (:foreground "OrangeRed" :underline t))))
 '(font-latex-verbatim-face ((((class color) (background light)) (:inherit monotype-courier\ new :foreground "SaddleBrown"))))
 '(font-lock-comment-face ((t (:foreground "firebrick" :slant oblique :height 1.0))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "Blue" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "Purple" :weight bold))))
 '(font-lock-string-face ((t (:foreground "firebrick" :height 1.0))))
 '(font-lock-type-face ((t (:foreground "#0a0" :weight bold))))
 '(font-lock-warning-face ((((class color) (min-colors 88) (background light)) (:background "yellow" :foreground "Red1" :slant normal :weight extra-bold))))
 '(fringe ((((class color) (background light)) (:background "grey96"))))
 '(highlight ((((class color) (min-colors 88) (background light)) (:background "#FAFABF"))))
 '(margin-face ((t (:background "red"))) t)
 '(minibuffer-prompt ((t (:foreground "dark blue"))))
 '(mode-line ((t (:background "#FFBB44" :foreground "black" :box (:line-width 3 :color "#FFBB44") :height 99 :family "microsoft-tahoma"))))
 '(mode-line-buffer-id ((t (:foreground "#990000" :slant italic :weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88) (background light)) (:inherit mode-line :background "RoyalBlue4" :foreground "white" :box (:line-width 2 :color "RoyalBlue4")))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey80" :foreground "grey20" :box (:line-width 3 :color "grey80")))))
 '(nxml-comment-content-face ((t (:inherit font-lock-comment-face))))
 '(org-special-keyword ((((class color) (min-colors 16) (background light)) (:foreground "#66aa00"))))
 '(pesche-tab ((t (:background "red"))))
 '(py-XXX-tag-face ((t (:background "yellow" :foreground "#f00"))) t)
 '(py-builtins-face ((t (:inherit font-lock-keyword-face :weight normal))) t)
 '(py-pseudo-keyword-face ((t (:inherit font-lock-keyword-face :weight normal))) t)
 '(semantic-dirty-token-face ((((class color) (background light)) (:background "gray96"))))
 '(semantic-unmatched-syntax-face ((((class color) (background light)) nil)))
 '(show-ws-spaces ((((class color)) nil)))
 '(show-ws-tabs ((((class color)) (:inherit trailing-whitespace))))
 '(show-ws-unbr-spaces ((((class color)) nil)))
 '(speedbar-directory-face ((((class color) (background light)) (:inherit speedbar-file-face :foreground "blue4"))))
 '(speedbar-file-face ((((class color) (background light)) (:foreground "cyan4" :family "microsoft-tahoma"))))
 '(speedbar-highlight-face ((((class color) (background light)) (:inherit speedbar-file-face :background "green"))))
 '(speedbar-selected-face ((((class color) (background light)) (:inherit speedbar-file-face :foreground "red" :underline t))))
 '(speedbar-tag-face ((((class color) (background light)) (:inherit speedbar-file-face :foreground "brown"))))
 '(tabbar-button ((t (:inherit tabbar-default :foreground "dark red"))))
 '(tabbar-button-highlight ((t (:inherit tabbar-default :background "white" :box (:line-width 2 :color "white")))))
 '(tabbar-default ((t (:inherit variable-pitch :background "gray90" :foreground "gray50" :box (:line-width 3 :color "gray90") :height 99))))
 '(tabbar-highlight ((t (:underline t))))
 '(tabbar-selected ((t (:inherit tabbar-default :foreground "blue" :weight bold))))
 '(tabbar-separator ((t (:inherit tabbar-default))))
 '(tabbar-unselected ((t (:inherit tabbar-default))))
 '(table-cell-face ((t (:foreground "#0000aa" :inverse-video nil))))
 '(tool-bar ((default (:foreground "black" :box (:line-width 1 :style released-button))) (((type x w32 mac) (class color)) (:background "grey75"))))
 '(trailing-whitespace ((((class color) (background light)) (:background "#ffcccc"))))
 '(trex-unicode-face ((t nil)))
 '(variable-pitch ((t (:height 105 :family "microsoft-tahoma"))))
 '(widget-documentation ((((class color) (background light)) (:inherit custom-documentation :foreground "dark green"))))
 '(woman-addition ((t (:inherit default :foreground "orange"))))
 '(woman-bold ((((min-colors 88) (background light)) (:inherit default :foreground "blue1" :weight bold))))
 '(woman-italic ((((min-colors 88) (background light)) (:inherit default :foreground "red1" :underline t :slant italic))))
 '(woman-unknown ((((background light)) (:inherit default :foreground "brown")))))



