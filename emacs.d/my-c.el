(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-watchwords)
            (font-lock-numbers)
            (c-toggle-auto-state 1)
            (c-toggle-hungry-state 1)
            (c-set-style "stroustrup"))

(provide 'my-c))
