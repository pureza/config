;; Fullscreen mode
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))


(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (require 'imenu)
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))


;; Run whatever program I'm writing...
(defvar run-command "./a.out"
  "Default command for running applications")


(defun run-app (arg)
  "Executes the application. If called with a prefix argument, asks the user
for the running command. Otherwise, reuses the last one"
  (interactive "P")
  (let ((command (if arg
                     (read-from-minibuffer "Run command: " run-command)
                   run-command)))
    (setq run-command command)
    (shell-command run-command)))


(defun run-after-compile (arg)
  "Compiles and run the application. If called with a prefix argument,
asks the user for the compile and run command. Otherwise, reuses the last
ones"
  (interactive "P")
  (let ((run-fn (lambda (buffer string)
                  (setq compilation-finish-functions nil)
                  (when (string-match "finished" string)
                    (call-interactively 'run-app)))))
    (add-hook 'compilation-finish-functions run-fn)
    (if arg
        (call-interactively 'compile)
      (compile compile-command))))


(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun font-lock-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\)"
          1 font-lock-warning-face prepend))))

(defun font-lock-numbers ()
  (font-lock-add-keywords
   nil
   '(("\\<\\(-?[0-9]+\.[0-9]+\\)" 1 font-lock-preprocessor-face)
     ("\\<\\(-?[0-9]+\\)" 1 font-lock-preprocessor-face))))


(provide 'my-defun)
