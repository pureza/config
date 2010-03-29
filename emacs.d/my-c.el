(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-watchwords)
            (font-lock-numbers)
            (c-set-style "stroustrup"))

(provide 'my-c))
