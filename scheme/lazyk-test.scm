(use gauche.test)
(use gauche.uvector)

(add-load-path "." :relative)
(load "lazyk")
(define (main _))

(test-start "LazyK")

(test-section "parser")
(let1 p (.$ expr->string parse)
  (test* "single S" "s" (p "s"))
  (test* "single K" "k" (p "k"))
  (test* "single I" "i" (p "i"))
  (test* "CC style 1" "((s k) k)" (p "SKK"))
  (test* "CC style 2" "(s (k k))" (p "S(KK)"))
  (test* "Unlambda style 1" "((s k) k)" (p "``skk"))
  (test* "Unlambda style 2" "(s (k k))" (p "`s`kk"))
  (test* "Iota style" "(iota ((iota iota) iota))" (p "*i**iii"))
  (test* "Mixed i" "((i (iota i)) i)" (p "`i*iIi"))
  (test* "Jot style" "(s (k ((i s) k)))" (p "01"))
  (test* "Mixed style" "(s (((s k) ((s (k ((i s) k))) iota)) i))" (p "`s(SK*01ii)"))
  (test* "spaces & comments" "(s (k k))" (p " S # l1\n  (K # l2\n\n K # l4\n ) # l5"))
  (test* "blank 1" "i" (p ""))
  (test* "blank 2" "((k i) i)" (p "K()()"))

  (test* "invalid char" (test-error <error> "unexpected char 'N'") (p "SNK"))
  (test* "eof error" (test-error <error> "unexpected EOF") (p "(SK"))
  (test* "eof error" (test-error <error> "unexpected EOF") (p "``sk"))
  (test* "eof error" (test-error <error> "unexpected EOF") (p "*i")))
  
  
(test-section "combinator")
(let ((n1 (expr :num 1))
      (n2 (expr :num 2))
      (n3 (expr :num 3)))
  (test* "(i 1)" n1 (expr-eval (expr-apply *i* n1)))
  (test* "(k 1 2)" n1 (expr-eval (expr-apply (expr-apply *k* n1) n2)))
  (test* "(k k 1 2 3)" n2 (expr-eval (expr-apply (expr-apply (expr-apply (expr-apply *k* *k*) n1) n2) n3)))
  (test* "(s k k 1)" n1 (expr-eval (expr-apply (expr-apply (expr-apply *s* *k*) *k*) n1)))
  (test* "(false 1 2)" n2 (expr-eval (expr-apply (expr-apply *false* n1) n2)))
  (test* "(iota k)" *s* (expr-eval (expr-apply *iota* *k*)))
  (test* "(iota false)" *k* (expr-eval (expr-apply *iota* *false*))))


(test-section "primitive church nums")
(dolist (i '(0 1 128 255 256))
  (test* #"cn#~i" i (churchnum-to-int (~ *cnums* i))))
(let ((cn2 (~ *cnums* 2))
      (cn3 (~ *cnums* 3)))
  (test* "(cn#3 cn#2)" 8 (churchnum-to-int (expr-apply cn3 cn2)))
  (test* "(cn#2 (cn#3 cn#2))" 64 (churchnum-to-int (expr-apply cn2 (expr-apply cn3 cn2))))
  (test* "((cn#3 cn#2) cn#2)" 256 (churchnum-to-int (expr-apply (expr-apply cn3 cn2) cn2))))


(test-section "composed church nums")
(test* "cn#0 as (k i)" 0 (churchnum-to-int (expr-apply *k* *i*)))
(test* "cn#1 as i" 1 (churchnum-to-int *i*))
(let* ((succ (parse "S(S(KS)K)"))
       (cn2 (expr-apply succ *i*))
       (cn3 (expr-apply succ cn2)))
  (test* "cn#2 as (succ i)" 2 (churchnum-to-int cn2))
  (test* "cn#3 as (succ cn#2)" 3 (churchnum-to-int cn3))
  (test* "(cn#3 cn#2)" 8 (churchnum-to-int (expr-apply cn3 cn2)))
  (test* "(cn#2 (cn#3 cn#2))" 64 (churchnum-to-int (expr-apply cn2 (expr-apply cn3 cn2))))
  (test* "((cn#3 cn#2) cn#2)" 256 (churchnum-to-int (expr-apply (expr-apply cn3 cn2) cn2))))


(test-section "input")
(let* ((input "bar\n")
       (expects (append (u8vector->list (string->u8vector input)) '(256))))
  (test-log "in = ~S" input)
  (with-input-from-string input
    (^()
      (let loop ((i 0) (l (expr :read)))
	(test* #"in[~i]" (~ expects i) (churchnum-to-int (expr-car l)))
	(when (< i (string-length input))
	  (loop (+ i 1) (expr-cdr l)))))))


(test-section "output")
(let* (($cons (parse "S(S(KS)(S(KK)(S(KS)(S(K(SI))K))))(KK)"))
       (lst (fold-right (^(n a) (expr-apply (expr-apply $cons (expr :cn n)) a))
			(expr :cn 256) '(65 10 66 67 256))))
  (test* "print-list" "A\nBC" (with-output-to-string (^() (print-list lst)))))


(test-section "run")
(define (run-test src input)
  (let ((in (open-input-string input))
	(out (open-output-string)))
    (list (with-input-from-port in
	    (^() (with-output-to-port out
		   (^() (run src)))))
	  (get-output-string out))))

(test* "echo (SKK)" '(0 "asdf\nqwer") (run-test "SKK" "asdf\nqwer"))
(test* "cdr (SI(K(KI)))" '(0 "jkl") (run-test "SI(K(KI))" "hjkl"))
(test* "8 * 33" '(8 "") (run-test "11111110001111111000111100111110001111111000111100111111000111111111000001111111000111100111001111111000111100111111000111100111111110001111111000111100111110001110011111110001111111000111100111110001110011111111100000111111100011111110001111001111100011100111111111000001111111000111111111000001111001110011110011110011100" "!"))

(exit (test-end))

