(require :asdf)
(require :uiop)

(defvar *basedir* (car (directory (directory-namestring *load-truename*))))
(pushnew *basedir* asdf:*central-registry*)

(defun make-fasl ()
  (compile-file (merge-pathnames "lazyk.lisp" *basedir*)))

(defun run-lazyk (file)
  (asdf:oos :load-op :lazyk)
  (uiop:symbol-call :lazyk :run-file file))

(defun %test ()
  (if (find-package :ql)
      (run-lazyk-test)
      (uiop:run-program (list (car sb-ext:*posix-argv*)
			      "--quit" "--load"
			      (uiop:native-namestring *load-truename*)
			      "%test")
			:output t)))

(defun run-lazyk-test ()
  (uiop:symbol-call :ql :quickload "fiveam")
  (asdf:oos :test-op :lazyk.test))

(defun show-usage ()
  (format t "usage: sbcl --script run-lazyk.lisp <command> [arg]~%~%")
  (format t "commands:~%")
  (format t "  run FILENAME  : run Lazy K program file~%")
  (format t "  make          : make lazyk.fasl~%")
  (format t "  test          : run tests for lazyk.lisp (requires quicklisp)~%"))

(let* ((argv (cdr sb-ext:*posix-argv*))
       (cmd (car argv)))
  (cond ((string= cmd "run")
	 (if (null (cdr argv))
	     (show-usage)
	     (run-lazyk (cadr argv))))
	((string= cmd "make") (make-fasl))
	((string= cmd "test") (%test))
	((string= cmd "%test") (run-lazyk-test))
	(t (show-usage))))
