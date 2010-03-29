;;; -*-Emacs-Lisp-*-
;;;
;;; Author: Bryan Kyle <bryan.kyle@gmail.com>
;;; Date: 2009-08-21
;;;
;;; Maven Minor Mode - run maven from within emacs.
;;;
;;; Overview:
;;;
;;; maven-mode is a minor mode for that helps emacs work with maven.  maven-mode
;;; provides the following features:
;;;
;;;   * automatically locates the nearest pom.xml
;;;   * automatically configure compile-command for running maven
;;;   * allows you to quickly navigate to your pom.xml using M-x find-pom-file
;;;
;;;
;;; Usage:
;;;
;;; (0) modify .emacs to load maven-mode.
;;;     for example :
;;;
;;;     (autoload 'maven-mode "maven-mode" nil t)
;;;
;;; (1) open a file that is within a maven project and enable the mode 
;;;     by typing: M-x maven-mode <RET>.
;;;
;;; (2a) compile, test, package, install your maven project by using the 
;;;      standard compile (M-x compile <RET> <RET>) and recompile (M-x recompile
;;;      <RET>) functions.
;;;
;;; (2b) open the pom.xml by typing M-x find-pom-file <RET>
;;;



(define-minor-mode maven-mode 
  "Toggle Maven mode.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Maven mode is enabled, compilation is performed
by \"mvn\"."

  :init-value nil
  :lighter " Maven"
 
  (if maven-mode
	  (progn
		(maven-mode-configure-variables)
		(add-hook 'after-save-hook 'maven-mode-after-save-hook))
	(progn
	  (remove-hook 'after-save-hook 'maven-mode-after-save-hook))))

(defun maven-mode-after-save-hook ()
  (maven-mode-configure-variables))

(defun maven-mode-configure-variables ()
  "Configures buffer-local variables for use with other commands."
  (let ((pom (maven-find-pom buffer-file-name)))
	(if pom
		(setq compile-command (format "mvn -f %s compile" pom)))))

	
(defun maven-find-pom (path)
  "Finds the nearest pom to the given path."

  (when (not path)
	(setq path buffer-file-name))
  
  (unless (not path)
	(let ((current-path (if (file-directory-p (file-name-as-directory path))
							(file-name-as-directory path)
						  (file-name-directory path)))
		  (next-path nil)
		  (found-p nil))
	  (catch 'done
		(while (not found-p)
		  (let ((files (directory-files current-path t))
				(next-path nil))
			(dolist (file files)
			  (cond
			   ((equal "pom.xml" (file-name-nondirectory file))
				(setq found-p t)
				(throw 'done t))
			   ((and (equal ".." (file-name-nondirectory file))
					 (not (equal current-path (file-truename file))))
				(setq next-path (file-truename file)))))
			(if (not next-path)
				(throw 'done t))
			(setq current-path next-path)
			(setq next-path nil))))
	  (cond
	   (found-p
		(expand-file-name (concat (file-name-as-directory current-path) "pom.xml")))
	   (t
		nil)))))
  
(defun find-pom-file ()
  "Finds the nearest pom to the current file."
  (interactive)
  (let ((pom (maven-find-pom buffer-file-name)))
	(if pom (find-file pom)
	  (error "Couldn't find pom.xml"))))

