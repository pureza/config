;; QERL stuff
(define-minor-mode qerl-mode
  "QERL mode"
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " QERL"
  :keymap '(([f6] . qerl-test))

  (defun qerl-test ()
    (interactive)
    (setq compilation-finish-functions
          '((lambda (buffer string)
              (setq compilation-finish-functions nil)
              (when (string-match "finished" string)
                (let ((shell-buffer (get-buffer-create "*qemu-system-sparc*")))
                  (pop-to-buffer shell-buffer)
                  (shell-command "cd ~/qerl/qemu/sparc-softmmu && ./qemu-system-sparc -kernel ~/qerl/image.flashbz.debug_uart -nographic -d in_asm &" shell-buffer))))))
    (compile "cd ~/qerl/qemu && make"))
  (setq compile-command "cd ~/qerl/qemu && make"))


;; Enable QERL mode for files inside ~/qerl/qemu
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (string-match "/qerl/qemu" (buffer-file-name))
              (qerl-mode))))

(provide 'my-qerl)
