#!/usr/bin/env gosh

(use gauche.uvector)
(use gauche.collection)


;;; expressions

(define-macro (expr t :optional (a1 #f) (a2 #f)) `(list ,t ,a1 ,a2))
(define-macro (isa? e t) `(eq? ,t (car ,e)))
(define expr-type car)
(define expr-arg1 cadr)
(define expr-arg2 caddr)

(define-constant *s* (expr :s))
(define-constant *k* (expr :k))
(define-constant *i* (expr :i))
(define-constant *iota* (expr :iota))
(define-constant *false* (expr :false))
(define-constant *true* *k*)
(define-constant *cnums* (map-to <vector> (^i (expr :cn i)) (iota 257)))
(define-constant *inc* (expr :inc))
(define-constant *num0* (expr :num 0))

(define-inline (expr-apply e1 e2)
  (expr :app e1 e2))

(define (expr->string e)
  (let* ((t (expr-type e))
	 (ts (string-copy (symbol->string t) 1)))
    (case t
      ((:app) #"(~(expr->string (expr-arg1 e)) ~(expr->string (expr-arg2 e)))")
      ((:s1 :k1 :i1) #"(~t ~(expr->string (expr-arg1 e)))")
      ((:s2) #"((~t ~(expr->string (expr-arg1 e))) ~(expr->string (expr-arg2 e)))")
      ((:cn :num) #"t#~(expr->arg1 e)")
      ((:cn1) #"(cn1#~(expr->arg1 e) ~(expr->string (expr-arg2 e)))")
      (else ts))))


;;; evaluator

(define (expr-drop-i1 e)
  (if (isa? e :i1)
      (expr-drop-i1 (expr-arg1 e))
      e))

(define (expr-eval e)
  (let ((stack ())
	(cur e))
    (let/cc return
      (while #t
	(set! cur (expr-drop-i1 cur))
	(while (isa? cur :app)
	  (push! stack cur)
	  (set! cur (expr-drop-i1 (expr-arg1 cur))))
	(when (null? stack)
	  (return cur))
	(let ((prev (pop! stack)))
	  (set! (expr-arg1 prev) cur)
	  (set! cur prev))
	(expr-eval-primitive cur)))))

(define (expr-eval-primitive e)
  (let ((lhs (expr-arg1 e))
	(rhs (expr-drop-i1 (expr-arg2 e))))
    (set! (expr-arg1 e) #f)
    (set! (expr-arg2 e) #f)
    (case (expr-type lhs)
      [(:false)
       (set! (expr-type e) :i)]
      [(:i)
       (set! (expr-type e) :i1)
       (set! (expr-arg1 e) rhs)]
      [(:k)
       (set! (expr-type e) :k1)
       (set! (expr-arg1 e) rhs)]
      [(:k1)
       (set! (expr-type e) :i1)
       (set! (expr-arg1 e) (expr-arg1 lhs))]
      [(:s)
       (set! (expr-type e) :s1)
       (set! (expr-arg1 e) rhs)]
      [(:s1)
       (set! (expr-type e) :s2)
       (set! (expr-arg1 e) (expr-arg1 lhs))
       (set! (expr-arg2 e) rhs)]
      [(:s2)
       (set! (expr-arg1 e) (expr-apply (expr-arg1 lhs) rhs))
       (set! (expr-arg2 e) (expr-apply (expr-arg2 lhs) rhs))]
      [(:iota)
       (set! (expr-arg1 e) (expr-apply rhs *s*))
       (set! (expr-arg2 e) *k*)]
      [(:read)
       (let ((ch (readc))
	     (readnext (expr :read)))
	 (set! (expr-arg1 e) (expr-apply rhs ch))
	 (set! (expr-arg2 e) readnext)
	 (set! (expr-type lhs) :s2)
	 (set! (expr-arg1 lhs) (expr :s2 *i* (expr :k1 ch)))
	 (set! (expr-arg2 lhs) (expr :k1 readnext)))]
      [(:cn)
       (set! (expr-type e) :cn1)
       (set! (expr-arg1 e) (expr-arg1 lhs))
       (set! (expr-arg2 e) rhs)]
      [(:cn1)
       (if (and (eq? (expr-arg2 lhs) *inc*)
		(isa? rhs :num))
	   (begin
	     (set! (expr-type e) :num)
	     (set! (expr-arg1 e) (+ (expr-arg1 lhs) (expr-arg1 rhs))))
	   (let ((f (expr-arg2 lhs))
		 (x rhs))
	     (dotimes (i (expr-arg1 lhs))
	       (set! x (expr-apply f x)))
	     (set! (expr-type e) (expr-type x))
	     (set! (expr-arg1 e) (expr-arg1 x))
	     (set! (expr-arg2 e) (expr-arg2 x))))]
      [(:inc)
       (unless (isa? rhs :num)
	 (set! rhs (expr-eval rhs)))
       (if (isa? rhs :num)
	   (begin
	     (set! (expr-type e) :num)
	     (set! (expr-arg1 e) (+ 1 (expr-arg1 rhs))))
	   (error "invalid output format: cannot aplly inc to " rhs))]
      [(:num) (error "invalid output format: cannnot apply num")]
      [else   (errorf "unexpected state: lhs=~S rhs=~S"
		      (expr-type lhs) (expr-type rhs))])))

(define (churchnum-to-int ch)
  (let1 n (expr-eval (expr-apply (expr-apply ch *inc*) *num0*))
    (if (isa? n :num)
	(expr-arg1 n)
	(error "invalid output format: result was not a number: " n))))


;;; io

(define (readc)
  (let1 b (read-byte)
    (if (eof-object? b)
	(~ *cnums* 256)
	(~ *cnums* b))))

(define (expr-car lst) (expr-eval (expr-apply lst *true*)))
(define (expr-cdr lst) (expr-eval (expr-apply lst *false*)))
(define (print-list lst)
  (let1 c (churchnum-to-int (expr-car lst))
    (cond [(< c 256)
	   (write-byte c)
	   (print-list (expr-cdr lst))]
	  [else
	   (flush)
	   (- c 256)])))


;;; parser

(define (parse src)
  (let ((in (open-input-string src))
	(unread ()))

    (define (next-char)
      (if (pair? unread)
	  (pop! unread)
	  (let loop ()
	    (let1 c (read-char in)
	      (cond [(eq? c #\#) (read-line in) (loop)]
		    [(eof-object? c) c]
		    [(char-whitespace? c) (loop)]
		    [else c])))))
    
    (define (unread-char c) (push! unread c) #t)
    
    (define (read-cc closing-char)
      (let1 c (next-char)
	(if (eq? c closing-char)
	    *i*
	    (begin
	      (unread-char c)
	      (let loop ((e (read-one #f))
			 (c (next-char)))
		(if (eq? c closing-char)
		    e
		    (begin
		      (unread-char c)
		      (loop (expr-apply e (read-one #f))
			    (next-char)))))))))
    
    (define (read-one i-is-iota)
      (let1 c (next-char)
	(when (eof-object? c)
	  (error "unexpected EOF"))
	(case c
	  [(#\`) (expr-apply (read-one #f) (read-one #f))]
	  [(#\*) (expr-apply (read-one #t) (read-one #t))]
	  [(#\() (read-cc #\))]
	  [(#\S #\s) *s*]
	  [(#\K #\k) *k*]
	  [(#\I) *i*]
	  [(#\i) (if i-is-iota *iota* *i*)]
	  [(#\0 #\1) (read-jot c)]
	  [else (errorf "unexpected char '~A'" c)])))
    
    (define (read-jot c0)
      (let loop ((c c0) (e *i*))
	(case c
	  [(#\0) (loop (next-char) (expr-apply (expr-apply e *s*) *k*))]
	  [(#\1) (loop (next-char) (expr-apply *s* (expr-apply *k* e)))]
	  [else (push! unread c) e])))

    (read-cc (eof-object))))


;;; entry point

(define (run src)
  (let ((prog (parse src)))
    (print-list (expr-apply prog (expr :read)))))

(define (run-file srcpath)
  (let ((src (call-with-input-file srcpath port->string
	       :if-does-not-exist :error
	       :encoding "utf-8")))
    (run src)))


(define (usage progname)
  (format #t "usage: gosh ~A src.lazyk~%" progname)
  (exit 2))

(define (main args)
  (if (null? (cdr args))
      (usage (car args))
      (exit (run-file (cadr args)))))
