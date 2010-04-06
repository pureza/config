(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (company-mode)
            (font-lock-watchwords)
            (font-lock-numbers)))

(provide 'my-elisp)
