(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (company-mode)
            (font-lock-watchwords)
            (font-lock-numbers)
            (c-toggle-auto-state 1)
            (c-toggle-hungry-state 1)))

(provide 'my-elisp)
