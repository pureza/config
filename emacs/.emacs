;; QERL stuff
(defun qerl-test ()
  (interactive)
  (setq compilation-finish-functions
	'((lambda (buffer string)
	    (setq compilation-finish-functions nil)
	    (when (string-match "finished" string)
	      (shell-command "cd ~/qerl/qemu/sparc-softmmu && ./qemu-system-sparc -kernel ~/qerl/examples/hello.prom -nographic -d in_asm")))))
  (compile "cd ~/qerl/qemu && make"))
(global-set-key [f6] 'qerl-test)
(setq compile-command "cd ~/qerl/qemu && make")


;; Indent the entire buffer
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))


;; Create TAGS for a given directory tree
(defun make-tags (dir)
  (interactive "D")
  (shell-command (format "cd %s && rm -f TAGS && find -iname '*.[ch]' | xargs etags -a" dir))
  (message "TAGS created"))


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
(tool-bar-mode -1)
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
(global-set-key [f5] 'compile)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key "\C-ci" 'indent-buffer)
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
(add-hook 'ruby-mode-hook
          (lambda()
	    (require 'inf-ruby)
	    (require 'ruby-electric)
	    (setq ruby-indent-level 4)))

;; Python
(add-hook 'python-mode-hook
	  (lambda ()
	    (require 'pymacs)
	    (pymacs-load "ropemacs" "rope-")))

;; C
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-set-style "stroustrup")
	    (c-set-offset 'case-label '+)
	    (setq indent-tabs-mode nil)))

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


;; Keep customizations in a different file
(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)
