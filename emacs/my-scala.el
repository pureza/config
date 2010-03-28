;; Scala
(add-to-list 'load-path "~/.emacs.d/elisp/scala-mode")
(autoload 'scala-mode "scala-mode")
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))

(add-hook 'scala-mode-hook
          (lambda ()
            (autoload 'maven-mode "maven-mode" nil t)))

(provide 'my-scala)
