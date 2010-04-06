(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))
(add-hook 'nxml-mode-hook (lambda ()
                            (company-mode)
                            (define-key nxml-mode-map (kbd ">") 'nxml-balanced-close-start-tag-inline)))


(provide 'my-xml)
