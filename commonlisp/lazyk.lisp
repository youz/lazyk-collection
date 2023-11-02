(declaim (optimize (speed 3) (safety 0)))

(defpackage #:lazyk
  (:use #:common-lisp)
  (:export #:parse #:expr #:expr-type #:expr-apply #:expr-eval
	   #:expr-to-string #:churchnum-to-int #:run #:run-file))

(in-package :lazyk)


;;; expressions
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct expr type (arg1 nil) (arg2 nil))
  (defmacro expr (type &optional a1 a2)
    `(make-expr :type ,type :arg1 ,a1 :arg2 ,a2)))
(defmacro is-a (e tsym) `(eq (expr-type ,e) ,tsym))

(defvar *s* (expr :s))
(defvar *k* (expr :k))
(defvar *i* (expr :i))
(defvar *iota* (expr :iota))
(defvar *true* *k*)
(defvar *false* (expr :false))
(defvar *inc* (expr :inc))
(defvar *num0* (expr :num 0))
(defvar *cnums*
  (let ((a (make-array 257)))
    (dotimes (i 257)
      (setf (aref a i) (expr :cn i)))
    a))

(defun expr-apply (e1 e2)
  (expr :app e1 e2))

(defun expr-to-string (e)
  (with-slots (type arg1 arg2) e
    (case type
      ((:app) (format nil "(~A ~A)" (expr-to-string arg1) (expr-to-string arg2)))
      ((:s1 :k1 :i1) (format nil "(~A ~A)" type (expr-to-string arg1)))
      ((:s2) (format nil "(S2 ~A ~A)" (expr-to-string arg1) (expr-to-string arg2)))
      ((:cn :num) (format nil "~A#~A" type arg1))
      ((:cn1) (format nil "(CN1#~A ~A)" arg1 (expr-to-string arg2)))
      (t (symbol-name type)))))


;;; evaluator

(defun expr-eval (e)
  (labels
      ((drop-i1 (e)
	 (loop for cur = e then (expr-arg1 cur)
	       if (not (is-a cur :i1)) return cur))
       (eval-primitive (e)
	 (let ((lhs (expr-arg1 e))
	       (rhs (drop-i1 (expr-arg2 e))))
	   (setf (expr-arg1 e) nil
		 (expr-arg2 e) nil)
	   (case (expr-type lhs)
	     (:false
	      (setf (expr-type e) :i))
	     (:i
	      (setf (expr-type e) :i1
		    (expr-arg1 e) rhs))
	     (:k
	      (setf (expr-type e) :k1
		    (expr-arg1 e) rhs))
	     (:k1
	      (setf (expr-type e) :i1
		    (expr-arg1 e) (expr-arg1 lhs)))
	     (:s
	      (setf (expr-type e) :s1
		    (expr-arg1 e) rhs))
	     (:s1
	      (setf (expr-type e) :s2
		    (expr-arg1 e) (expr-arg1 lhs)
		    (expr-arg2 e) rhs))
	     (:s2
	      (setf (expr-arg1 e) (expr-apply (expr-arg1 lhs) rhs)
		    (expr-arg2 e) (expr-apply (expr-arg2 lhs) rhs)))
	     (:iota
	      (setf (expr-arg1 e) (expr-apply rhs *s*)
		    (expr-arg2 e) *k*))
	     (:read
	      (let* ((reader (expr-arg1 lhs))
		     (ch (funcall reader))
		     (readnext (expr :read reader)))
		(setf (expr-arg1 e) (expr-apply rhs ch)
		      (expr-arg2 e) readnext
		      (expr-type lhs) :s2
		      (expr-arg1 lhs) (expr :s2 *i* (expr :k1 ch))
		      (expr-arg2 lhs) (expr :k1 readnext))))
	     (:cn
	      (setf (expr-type e) :cn1
		    (expr-arg1 e) (expr-arg1 lhs)
		    (expr-arg2 e) rhs))
	     (:cn1
	      (if (and (is-a (expr-arg2 lhs) :inc)
		       (is-a rhs :num))
		  (setf (expr-type e) :num
			(expr-arg1 e) (+ (expr-arg1 lhs) (expr-arg1 rhs)))
		  (let ((f (expr-arg2 lhs))
			(x rhs))
		    (dotimes (i (expr-arg1 lhs))
		      (setq x (expr-apply f x)))
		    (setf (expr-type e) (expr-type x)
			  (expr-arg1 e) (expr-arg1 x)
			  (expr-arg2 e) (expr-arg2 x)))))
	     (:inc
	      (unless (is-a rhs :num)
		(setq rhs (expr-eval rhs)))
	      (if (is-a rhs :num)
		  (setf (expr-type e) :num
			(expr-arg1 e) (1+ (expr-arg1 rhs)))
		  (error "invalid output format (cannot aplly inc to #<~A>)" (expr-type rhs))))
	     (:num
	      (error "invalid output format (cannnot apply num)"))
	     (t
	      (error "unexpected stat: lhs=#<~A> rhs=#<~A>"
		     (expr-type lhs) (expr-type rhs)))))))
    ;; main loop
    (let ((stack nil)
	  (cur e))
      (loop
	 (setq cur (drop-i1 cur))
	 (loop while (is-a cur :app)
	       do (push cur stack)
		  (setq cur (drop-i1 (expr-arg1 cur))))
	 (when (null stack)
	   (return-from expr-eval cur))
	 (let ((prev (pop stack)))
	   (setf (expr-arg1 prev) cur
		 cur prev))
	 (eval-primitive cur)))))

(defun churchnum-to-int (e)
  (let ((n (expr-eval (expr-apply (expr-apply e *inc*) *num0*))))
    (unless (is-a n :num)
      (error "invalid output format (result was not a number: #<~A>)" (expr-type n)))
    (expr-arg1 n)))


;;; io

(defun expr-car (e)
  (expr-apply e *true*))

(defun expr-cdr (e)
  (expr-apply e *false*))

(defun make-output-buffer ()
  (make-array 0 :element-type '(unsigned-byte 8)
		:adjustable t :fill-pointer 0))

(defstruct iobuffer
  (input *standard-input* :type stream)
  (output *standard-output* :type stream)
  (inbuf nil :type list)
  (eof nil :type boolean)
  (outbuf  (make-output-buffer)
	   :type (vector (unsigned-byte 8))))

(defun flush-output (iobuf)
  (when (= 0 (length (iobuffer-outbuf iobuf)))
    (return-from flush-output))
  (with-slots (output outbuf) iobuf
    (let ((s (sb-ext:octets-to-string outbuf)))
      (write-string s output)
      (force-output output))
    (setf outbuf (make-output-buffer))
    t))

(defun writec (iobuf c)
  (vector-push-extend c (iobuffer-outbuf iobuf))
  (when (= c 10)
    (flush-output iobuf))
  c)

(defun print-list (iobuf lst)
  (loop for c = (churchnum-to-int (expr-car lst))
	do (if (< c 256)
	       (writec iobuf c)
	       (progn
		 (flush-output iobuf)
		 (return-from print-list (- c 256))))
	   (setq lst (expr-cdr lst))))

(defun make-reader (iobuf)
  (flet ((readc ()
	   (with-slots (input inbuf eof) iobuf
	     (when (and (null inbuf) (not eof))
	       (flush-output iobuf)
	       (multiple-value-bind (l missinglf) (read-line input nil nil)
		 (if l
		     (setf inbuf (concatenate 'list (sb-ext:string-to-octets l)
					      (if (not missinglf) (list 10))))
		    (setf eof t))))
	     (aref *cnums* (if (null inbuf) 256 (pop inbuf))))))
    (expr :read #'readc)))


;;; parser
(defun parse (src)
  (labels
      ((next-char (in)
	 (loop for c = (read-char in nil :eof)
	       do (case c
		    ((#\Tab #\Newline #\Return #\Space) t)
		    (#\# (read-line in))
		    (t (return-from next-char c)))))
       (safe-unread-char (c in)
	 (when (characterp c)
	   (unread-char c in)))
       (read-cc (in closing-char)
	 (let ((c (next-char in)))
	   (when (eq c closing-char)
	     (return-from read-cc *i*))
	   (safe-unread-char c in))
	 (loop for e = (read-one in nil) then (expr-apply e (read-one in nil))
	       do (let ((c (next-char in)))
		    (when (eq c closing-char)
		      (return-from read-cc e))
		    (safe-unread-char c in))))
       (read-one (in i-is-iota)
	 (let ((c (next-char in)))
	   (case c
	     (#\` (expr-apply (read-one in nil) (read-one in nil)))
	     (#\* (expr-apply (read-one in t) (read-one in t)))
	     (#\( (read-cc in #\)))
	     ((#\S #\s) *s*)
	     ((#\K #\k) *k*)
	     (#\I *i*)
	     (#\i (if i-is-iota *iota* *i*))
	     ((#\0 #\1) (read-jot in c))
	     (:eof (error "unexpected EOF"))
	     (t (error "unexpected char: '~A'" c)))))
       (read-jot (in c0)
	 (let ((c c0)
	       (e *i*))
	   (loop
	      (case c
		(#\0 (setq e (expr-apply (expr-apply e *s*) *k*)))
		(#\1 (setq e (expr-apply *s* (expr-apply *k* e))))
		(t (safe-unread-char c in)
		 (return-from read-jot e)))
	      (setq c (next-char in))))))
    (with-input-from-string (in src)
      (read-cc in :eof))))

(defun read-from-file (file)
  (with-output-to-string (os)
    (with-open-file (s file :direction :input)
      (loop for l = (read-line s nil nil)
	    while l do (write-line l os)))))


;;; entry point

(defun run (src &key (input *standard-input*) (output *standard-output*))
  (handler-case
      (let* ((expr (parse src))
	     (iobuf (make-iobuffer :input input :output output))
	     (reader (make-reader iobuf)))
	(prog1 (print-list iobuf (expr-apply expr reader))
	  (flush-output iobuf)))
    (error (c) (format t "ERROR: ~A" c))))

(defun run-file (srcpath &key (input *standard-input*) (output *standard-output*))
  (run (read-from-file srcpath) :input input :output output))

