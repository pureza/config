(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(autoload #'espresso-mode "espresso" "Start espresso-mode" t)

;; Javascript indentation
;; see http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion
        ;; I like to indent case and labels to half of the tab width
         (back-to-indentation)
        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))


(defun my-js2-mode-hook ()
  (require 'js2-highlight-vars)
  (require 'espresso)
  (font-lock-watchwords)
  (font-lock-numbers)
  (c-toggle-auto-state 1)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (company-mode)
  (set (make-local-variable 'company-backends) '(company-dabbrev-code))
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode)))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

(provide 'my-js)
