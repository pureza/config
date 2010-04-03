(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(require 'bar-cursor)
(bar-cursor-mode t)
(column-number-mode t)
(transient-mark-mode t)
(global-hl-line-mode t)
(show-paren-mode t)
(global-linum-mode 1)


(setq inhibit-startup-message t
      display-time-24hr-format t
      require-final-newline t
      make-backup-files nil)


(setq-default show-trailing-whitespace t)
(fset 'yes-or-no-p 'y-or-n-p)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'show-trailing-whitespace t)
(set-default 'imenu-auto-rescan t)

(display-time)


;; ido
(require 'ido)
(setq ido-case-fold  t
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-enable-last-directory-history t
      ido-ignore-buffers '("\\` " ".*Completion" "^\*Ido" "^\*compilation" "^\*"))
(ido-mode t)


;; color-theme
(require 'color-theme)
(load-library "color-theme-chocolate-rain.el")
(color-theme-chocolate-rain)


;; diff
(setq diff-switches "-u")
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))


;; autopair
(require 'autopair)
(autopair-global-mode)


;; yasnippet
(add-to-list 'load-path "~/.emacs.d/elisp/yasnippet-0.6.1c")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/elisp/yasnippet-0.6.1c/snippets")


;; Flymake
(require 'flymake)
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate yellow")


;; Company mode
(add-to-list 'load-path "~/.emacs.d/elisp/company-mode")
(autoload 'company-mode "company" nil t)


;; Hitting delete will delete region and selecting a region and then
;; press a character will replace region with that character.
(pending-delete-mode 1)


;; Scroll smooth
(setq scroll-step 1)
(require 'smooth-scrolling)


;; saveplace
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)


;; cua-mode
(cua-mode)

(set-face-foreground 'font-lock-preprocessor-face "#ff9933")

(run-with-idle-timer 0.01 nil 'fullscreen)


(setq custom-file "~/.emacs.d/emacs-custom.el")

(provide 'my-misc)
