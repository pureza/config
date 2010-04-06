(global-set-key [f11] 'fullscreen)
(global-set-key "\C-cg" 'grep)
(global-set-key "\C-cw" 'delete-trailing-whitespace)
(global-set-key "\C-cf" 'cleanup-buffer)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-7") 'comment-or-uncomment-region)
(global-set-key (kbd "M-n") 'open-line-below)
(global-set-key (kbd "<f7>") 'toggle-menu-bar-mode-from-frame)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; tabkey2
(require 'tabkey2)
(tabkey2-mode)


;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)

;; occur-mode
(global-set-key "\C-co" 'occur)
(define-key occur-mode-map "n" 'next-error-no-select)
(define-key occur-mode-map "p" 'previous-error-no-select)


;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(windmove-default-keybindings 'meta)


;; Fast go to .emacs.
(global-set-key (kbd "<f12>") (lambda ()
                                (interactive)
                                (find-file "~/.emacs.d/my-misc.el")))

(provide 'my-keys)
