;;; ensime-test.el --- Regression tests for ENSIME
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.


(require 'ensime)

(defvar ensime-testing-buffer "*ensime-tests*"
  "Contains the output of all the tests. Also tracks all the testing-specific
   buffer-local variables.")

(defvar ensime-test-queue '()
  "The queue of tests yet to be run.")
(make-variable-buffer-local 'ensime-test-queue)

(defvar ensime-async-handler-stack '()
  "Asynchronous event handlers waiting for signals. See 'ensime-test-sig'.")
(make-variable-buffer-local 'ensime-async-handler-stack)

(defvar ensime-shared-test-state '()
  "A state dump for anyone who wants to use it. Useful for async tests.")
(make-variable-buffer-local 'ensime-shared-test-state)

(defvar ensime-test-env-classpath '("/home/aemon/lib/scala/lib/scala-library.jar")
  "Hard-code a classpath for testing purposes. Not great.")


(defun ensime-create-file (file-name contents)
  "Create file named file-name. Write contents to the file. Return file's name."
  (with-temp-file file-name
    (insert contents))
  file-name)


(defmacro* ensime-with-tmp-file ((name prefix contents) &rest body)
  "Create temporary file with given prefix. Bind the file to 
   name and evaluate body."
  `(let ((,name (make-temp-file ,prefix)))
     (with-temp-file ,name
       (insert ,contents))
     (unwind-protect
	 (progn ,@body)
       (delete-file ,name))))



(defun ensime-create-tmp-project (src-files)
  "Create a temporary project directory. Populate with config, source files. Return 
   a plist describing the project. Note: Delete such projects with 
   ensime-cleanup-tmp-project."
  (let* ((root-dir (file-name-as-directory (make-temp-file "ensime_test_proj_" t)))
	 (conf-file (ensime-create-file 
		     (concat root-dir ".ensime")
		     (format "%S" (list :source '("src")
					:project-package "com.test"
					:classpath ensime-test-env-classpath
					:test-mode t
					))))
	 (src-dir (file-name-as-directory (concat root-dir "src"))))
    (mkdir src-dir)
    (let* ((proj '())
	   (src-file-names 
	    (mapcar
	     (lambda (f) (ensime-create-file 
			  (concat src-dir (plist-get f :name))
			  (plist-get f :contents)))
	     src-files)))
      (setq proj (plist-put proj :src-files src-file-names))
      (setq proj (plist-put proj :root-dir root-dir))
      (setq proj (plist-put proj :conf-file conf-file))
      (setq proj (plist-put proj :src-dir src-dir))
      proj
      )))

(defvar ensime-tmp-project-hello-world
  `((:name 
     "hello_world.scala"
     :contents ,(concat 
		 "package com.helloworld\n"
		 "class HelloWorld{\n"
		 "}\n"
		 "object HelloWorld {\n"
		 "def main(args: Array[String]) = {\n"
		 "Console.println(\"Hello, world!\")\n"
		 "}\n"
		 "def foo(a:Int, b:Int):Int = {\n"
		 "a + b"
		 "}\n"
		 "}\n"
		 )
     )))

(defun ensime-cleanup-tmp-project (proj)
  "Destroy a temporary project directory, kill all buffers visiting
   source files in the project."
  (let ((src-files (plist-get proj :src-files))
	(root-dir (plist-get proj :root-dir)))
    (dolist (f src-files)
      (find-file f)
      (kill-buffer nil))
    ;; a bit of paranoia..
    (if (and root-dir (integerp (string-match "^/tmp/" root-dir)))
	;; ..before we wipe away the project dir
	(shell-command (format "rm -rf %S" root-dir)))))

(defun ensime-kill-all-ensime-servers ()
  "Kill all inferior ensime server buffers."
  (dolist (b (buffer-list))
    (if (string-match "^\\*inferior-ensime-server" (buffer-name b))
	(kill-buffer b))))

(defmacro ensime-test-var-put (var val)
  "Helper for writing to shared testing state."
  `(with-current-buffer ensime-testing-buffer
     (setq ensime-shared-test-state 
	   (plist-put ensime-shared-test-state ,var ,val))))

(defmacro ensime-test-var-get (var)
  "Helper for reading from shared testing state."
  `(with-current-buffer ensime-testing-buffer
     (plist-get ensime-shared-test-state ,var)))

(defun ensime-test-sig (event value)
  "Driver for asynchonous tests. This function is invoked from ensime core,
   signaling events to events handlers installed by asynchronous tests."
  (when (buffer-live-p (get-buffer ensime-testing-buffer))
    (with-current-buffer ensime-testing-buffer
      (when (not (null ensime-async-handler-stack))
	(let* ((ensime-prefer-noninteractive t)
	       (handler (car ensime-async-handler-stack))
	       (handler-event (plist-get handler :event)))
	  (when (equal event handler-event)
	    (let ((handler-func (plist-get handler :func))
		  (is-last (plist-get handler :is-last)))
	      (pop ensime-async-handler-stack)
	      (funcall handler-func value)
	      (when is-last
		(pop ensime-test-queue)
		(ensime-run-next-test)))))))))


(defun ensime-output-test-result (title result)
  "Helper for writing results to testing buffer."
  (with-current-buffer ensime-testing-buffer
    (if (equal result t)
	(insert ".")
      (insert (format "\n%s:\n%s\n" title result)))))


(defmacro ensime-test-suite (&rest tests)
  "Define a sequence of tests to execute. 
   Tests may be synchronous or asynchronous."
  `(progn
     (switch-to-buffer ensime-testing-buffer)
     (setq ensime-test-queue (list ,@tests))
     (ensime-run-next-test)))


(defmacro ensime-test (title &rest body)
  "Define a synchronous test."
  `(list :title ,title :async nil 
	 :func (lambda () 
		 (ensime-test-run-with-handlers
		  ,title
		  ,@body))))


(defmacro ensime-test-run-with-handlers (context &rest body)
  "Evaluate body in the context of an error handler. Handle errors by
   writing to the testing output buffer."
  `(condition-case signal
       (progn
	 ,@body
	 (ensime-output-test-result ,context t))
     (error
      (ensime-output-test-result 
       ,context
       (format "Assertion failed at '%s': %s" ,context signal)))))


(defmacro* ensime-async-test (title trigger &rest handlers)
  "Define an asynchronous test."
  (let* ((last-handler (car (last handlers)))
	 (handler-structs 
	  (mapcar
	   (lambda (h)
	     (let* ((head (car h))
		    (evt (car head))
		    (val-sym (cadr head))
		    (func-body (cadr h))
		    (func `(lambda (,val-sym)
			     (ensime-test-run-with-handlers
			      ,title
			      ,func-body))))
	       (list
		:event evt
		:val-sym val-sym
		:func func
		:is-last (equal h last-handler)
		)))
	   handlers))
	 (trigger-func 
	  `(lambda ()
	     (ensime-test-run-with-handlers
	      ,title
	      ,trigger))))
    `(list :title ,title :async t 
	   :trigger ,trigger-func
	   :handlers ',handler-structs
	   )))

;;(message "%S" (macroexpand '(ensime-async-test "blarg" (do-this-thing) ((:evt-a val) val) ((:evt-b val) val))))


(defun ensime-run-next-test ()
  "Run the next test from the test queue."
  (with-current-buffer ensime-testing-buffer
    (if ensime-test-queue
	(let ((ensime-prefer-noninteractive t)
	      (test (car ensime-test-queue)))
	  (setq ensime-shared-test-state '())
	  (setq ensime-async-handler-stack '())
	  (if (plist-get test :async)

	      ;; Asynchronous test
	      (let ((handlers (reverse (plist-get test :handlers))))
		(dolist (h handlers)
		  (push h ensime-async-handler-stack))
		(funcall (plist-get test :trigger)))

	    ;; Synchronous test
	    (progn
	      (pop ensime-test-queue)
	      (funcall (plist-get test :func))
	      (ensime-run-next-test))))
      (insert "\nFinished."))))


(defmacro ensime-assert (pred)
  `(let ((val ,pred))
     (with-current-buffer ensime-testing-buffer
       (if (not val)
	   (error (format "Expected truth of %s." ',pred))))))


(defmacro ensime-assert-equal (a b)
  `(let ((val-a ,a)
	 (val-b ,b))
     (with-current-buffer ensime-testing-buffer
       (if (equal val-a val-b) t
	 (error (format "Expected %s to equal %s" ',a ',b))))))


(defun ensime-run-tests ()
  "Run all regression tests for ensime-mode."
  (interactive)

  (ensime-test-suite



   (ensime-test 
    "Test loading a simple config."
    (ensime-with-tmp-file 
     (file "ensime_test_conf_" 
	   (format "%S"
		   '( :server-cmd 
		      "bin/server.sh"
		      :classpath ("hello" "world")
		      )))
     (let ((conf (ensime-load-config file)))
       (ensime-assert (equal (plist-get conf :server-cmd) "bin/server.sh"))
       (ensime-assert (equal (plist-get conf :classpath) '("hello" "world")))
       (ensime-assert (equal (plist-get conf :root-dir) 
			     (expand-file-name (file-name-directory file)))))))




   (ensime-test 
    "Test loading a broken(syntactically) config file."
    (ensime-with-tmp-file 
     (file "ensime_test_conf_" "(lkjsdfkjskfjs")
     (let ((conf (ensime-load-config file)))
       (ensime-assert (null conf)))))




   (ensime-test 
    "Test specifying classpath as function."
    (ensime-with-tmp-file 
     (file "ensime_test_conf_"
	   (format "%S" '( :server-cmd 
			   "bin/server.sh"
			   :classpath gen-class-path)))
     (let* ((gen-class-path #'(lambda (conf) (list "one" "two" "three")))
	    (conf (ensime-load-config file)))
       (ensime-assert (equal (plist-get conf :classpath) '("one" "two" "three"))))))




   (ensime-async-test 
    "Load and compile 'hello world'."
    (let* ((proj (ensime-create-tmp-project
		  ensime-tmp-project-hello-world))
	   (src-files (plist-get proj :src-files)))
      (ensime-test-var-put :proj proj)
      (find-file (car src-files))
      (ensime))

    ((:connected connection-info)
     (ensime-assert (stringp (plist-get connection-info :version))))

    ((:full-typecheck-finished val)
     (let ((proj (ensime-test-var-get :proj)))
       (ensime-assert-equal val '(:notes ()))
       (ensime-cleanup-tmp-project proj)
       (ensime-kill-all-ensime-servers)
       ))
    )




   (ensime-async-test 
    "Get package info for com.helloworld."
    (let* ((proj (ensime-create-tmp-project
		  ensime-tmp-project-hello-world))
	   (src-files (plist-get proj :src-files)))
      (ensime-test-var-put :proj proj)
      (find-file (car src-files))
      (ensime))

    ((:connected connection-info))

    ((:full-typecheck-finished val)
     (let ((proj (ensime-test-var-get :proj)))
       (ensime-assert-equal val '(:notes ()))
       (let ((info (ensime-rpc-inspect-package-by-path
		    "com.helloworld")))
	 (ensime-assert (not (null info)))
	 (ensime-assert-equal (ensime-package-full-name info) "com.helloworld")
	 (ensime-assert-equal 1 (length (ensime-package-members info)))
	 )
       (ensime-cleanup-tmp-project proj)
       (ensime-kill-all-ensime-servers)
       ))
    )



   (ensime-async-test 
    "Verify re-typecheck on save-buffer."
    (let* ((proj (ensime-create-tmp-project
		  ensime-tmp-project-hello-world))
	   (src-files (plist-get proj :src-files)))
      (ensime-test-var-put :proj proj)
      (find-file (car src-files))
      (ensime))

    ((:connected connection-info))

    ((:full-typecheck-finished val)
     (let* ((proj (ensime-test-var-get :proj))
	    (src-files (plist-get proj :src-files))
	    (notes (plist-get val :notes)))
       (ensime-assert-equal (length notes) 0)
       (find-file (car src-files))
       (goto-char (point-min))
       (insert "lksdjfldkjf ")

       ;; save-buffer should trigger a recheck...
       (save-buffer)
       ))

    ((:full-typecheck-finished val)
     (let ((proj (ensime-test-var-get :proj))
	   (notes (plist-get val :notes)))
       (ensime-assert (> (length notes) 0))
       (ensime-cleanup-tmp-project proj)
       (ensime-kill-all-ensime-servers)
       ))
    )


   (ensime-async-test 
    "Test get symbol info at point."
    (let* ((proj (ensime-create-tmp-project
		  ensime-tmp-project-hello-world))
	   (src-files (plist-get proj :src-files)))
      (ensime-test-var-put :proj proj)
      (find-file (car src-files))
      (ensime))

    ((:connected connection-info))

    ((:full-typecheck-finished val)
     (let* ((proj (ensime-test-var-get :proj))
	    (src-files (plist-get proj :src-files)))
       (ensime-assert-equal val '(:notes ()))
       ;; Set cursor to symbol in method body..
       (find-file (car src-files))
       (goto-char 163)
       (let* ((info (ensime-rpc-symbol-at-point))
	      (pos (ensime-symbol-decl-pos info)))
	 ;; New position should be at formal parameter...
	 (ensime-assert-equal (ensime-pos-offset pos) 141)
	 )
       (ensime-cleanup-tmp-project proj)
       (ensime-kill-all-ensime-servers)
       ))
    )


   ))

(provide 'ensime-test)