(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(fullscreen)

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

(require 'bar-cursor)
(bar-cursor-mode t)

(column-number-mode t)
(transient-mark-mode t)
(global-hl-line-mode t)
(show-paren-mode t)

;; ido
(require 'ido)
(setq ido-case-fold  t
      ido-enable-flex-matching t
      ido-enable-last-directory-history t
      ido-ignore-buffers '("\\` " ".*Completion" "^\*Ido" "^\*compilation" "^\*"))
(ido-mode t)

;; color-theme
(require 'color-theme)
(load-library "color-theme-chocolate-rain.el")
(color-theme-chocolate-rain)

;; GPG
(require 'epa nil)
(epa-file-enable)

;; diff
;; Default to unified diffs
(setq diff-switches "-u")
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))


(set-face-foreground 'font-lock-preprocessor-face "#ff9933")

(setq custom-file "~/.emacs.d/emacs-custom.el")

(provide 'my-misc)
