
(defpackage #:lazyk.test
  (:use #:common-lisp #:fiveam)
  (:import-from #:lazyk
		#:parse #:expr #:expr-apply #:expr-eval
		#:expr-to-string #:churchnum-to-int
		#:expr-car #:expr-cdr #:print-list
		#:*s* #:*k* #:*i* #:*iota* #:*true* #:*false*
		#:*cnums* #:*inc* #:*num0*))

(in-package :lazyk.test)

(def-suite lazyk)

(defun parse-test (s) (expr-to-string (parse s)))
(def-suite* parser :in lazyk)
(test single-token
      (is (string= "S" (parse-test "s")))
      (is (string= "K" (parse-test "k")))
      (is (string= "I" (parse-test "i"))))
(test combinator-calculus-style
      (is (string= "((S K) K)" (parse-test "SKK")))
      (is (string= "(S (K K))" (parse-test "S(KK)"))))
(test unlambda-style
      (is (string= "((S K) K)" (parse-test "``skk")))
      (is (string= "(S (K K))" (parse-test "`s`kk"))))
(test iota-style
      (is (string= "(IOTA ((IOTA IOTA) IOTA))" (parse-test "*i**iii"))))
(test jot-style
      (is (string= "(S (K ((I S) K)))" (parse-test "01"))))
(test mixed-style
      (is (string= "((I (IOTA I)) I)" (parse-test "`i*iIi")))
      (is (string= "(S (((S K) ((S (K ((I S) K))) IOTA)) I))"
		   (parse-test "`s(SK*01ii)"))))
(test comments
      (is (string= "(S (K K))" (parse-test " S # l1
  (K # l2

 K # l4
 ) # l5"))))
(test empty
      (is (string= "I" (parse-test "")))
      (is (string= "((K I) I)" (parse-test "K()()"))))
(test syntax-error
      (signals simple-error (parse-test "SNK"))
      (signals simple-error (parse-test "(SK"))
      (signals simple-error (parse-test "``sk"))
      (signals simple-error (parse-test "*i")))


(def-suite* combinator :in lazyk)
(test apply-ski
  (let ((n1 (expr :num 1))
	(n2 (expr :num 2))
	(n3 (expr :num 3)))
    (is (eq n1 (expr-eval (expr-apply *i* n1))))
    (is (eq n1 (expr-eval (expr-apply (expr-apply *k* n1) n2))))
    (is (eq n2 (expr-eval (expr-apply (expr-apply (expr-apply (expr-apply *k* *k*) n1) n2) n3))))
    (is (eq n1 (expr-eval (expr-apply (expr-apply (expr-apply *s* *k*) *k*) n1))))))
(test apply-false
  (is (eq *k* (expr-eval (expr-apply (expr-apply *false* *s*) *k*)))))
(test apply-iota
  (is (eq *s* (expr-eval (expr-apply *iota* *k*))))
  (is (eq *k* (expr-eval (expr-apply *iota* *false*)))))


(def-suite* primitive-church-nums :in lazyk)
(test *cnums*   
  (dolist (i '(0 1 128 255 256))
    (is (= i (churchnum-to-int (aref *cnums* i))))))
(test pow
  (let ((cn2 (aref *cnums* 2))
	(cn3 (aref *cnums* 3)))
    (is (= 8 (churchnum-to-int (expr-apply cn3 cn2))))
    (is (= 64 (churchnum-to-int (expr-apply cn2 (expr-apply cn3 cn2)))))
    (is (= 256 (churchnum-to-int (expr-apply (expr-apply cn3 cn2) cn2))))))


(def-suite* composed-church-nums :in lazyk)
(test cn0-1
  (is (= 0 (churchnum-to-int (expr-apply *k* *i*))))
  (is (= 1 (churchnum-to-int *i*))))
(test succ&pow
  (let* ((succ (parse "S(S(KS)K)"))
	 (cn2 (expr-apply succ *i*))
	 (cn3 (expr-apply succ cn2)))
    (is (= 2 (churchnum-to-int cn2)))
    (is (= 3 (churchnum-to-int cn3)))
    (is (= 8 (churchnum-to-int (expr-apply cn3 cn2))))
    (is (= 64 (churchnum-to-int (expr-apply cn2 (expr-apply cn3 cn2)))))
    (is (= 256 (churchnum-to-int (expr-apply (expr-apply cn3 cn2) cn2))))))


(defmacro with-string-io (instr &body body)
  `(let ((*standard-input* (make-string-input-stream ,instr))
	 (*standard-output* (make-string-output-stream)))
     (progn
       (let ((#0=#:result (progn ,@body)))
	 (values #0# (get-output-stream-string *standard-output*))))))

(def-suite* i/o :in lazyk)
(test input
  (let* ((instr "bar\n")
	 (expects (append (map 'list #'char-code instr) '(256))))
    (with-string-io instr
      (let ((l (lazyk::make-reader (lazyk::make-iobuffer))))
	(dotimes (i (length expects))
	  (is (= (nth i expects) (churchnum-to-int (expr-car l))))
	  (setq l (expr-cdr l)))))))
(test output
  (let* (($cons (parse "S(S(KS)(S(KK)(S(KS)(S(K(SI))K))))(KK)"))
	 (lst (reduce (lambda (n a) (expr-apply (expr-apply $cons (aref *cnums* n)) a))
		      '(65 10 66 67 256)
		      :initial-value (expr :cn 256)
		      :from-end t)))
    (is (equal '(0 "A
BC")
	       (multiple-value-list
		(with-string-io ""
		  (lazyk::print-list (lazyk::make-iobuffer) lst)))))))


(def-suite* run-program :in lazyk)

(defun run-test (src stdin)
  (multiple-value-list (with-string-io stdin (lazyk:run src))))

(test cc-style
  ;; echo
  (is (equal '(0 #0="asdf
qwer") (run-test "SKK" #0#)))
  ;; cdr
  (is (equal '(0 "jkl") (run-test "SI(K(KI))" "hjkl")))
  ;; ((lambda (input) (* 8 (car input))) '(33))
  (is (equal '(8 "") (run-test "11111110001111111000111100111110001111111000111100111111000111111111000001111111000111100111001111111000111100111111000111100111111110001111111000111100111110001110011111110001111111000111100111110001110011111111100000111111100011111110001111001111100011100111111111000001111111000111111111000001111001110011110011110011100" "!"))))

