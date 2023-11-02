
let qcheck t msg label expect f a =
  let test () = Alcotest.check t msg expect (f a) in
  Alcotest.test_case label `Quick test

let qcheck_fail msg label fmsg f a =
  let thunk _ = let _ = f a in () in
  let test () = Alcotest.check_raises msg (Failure fmsg) thunk in
  Alcotest.test_case label `Quick test

let parser_tests =
  let sapp f a = "(" ^ f ^ " " ^ a ^ ")" in
  let tparse = Lazyk.parse sapp "s" "k" "i" "iota" in
  let check label expect src =
    qcheck Alcotest.string "same string" label expect tparse src in
  let fails label fmsg src =
    qcheck_fail "fails" label fmsg tparse src in
  ("parser", [
      check "single s" "s" "S";
      check "single k" "k" "K";
      check "single i" "i" "I";
      check "CC style 1" "((s k) k)" "SKK";
      check "CC style 2" "(s (k k))" "S(KK)";
      check "Unlambda style 1" "((s k) k)" "``skk";
      check "Unlambda style 2" "(s (k k))" "`s`kk";
      check "Iota style" "(iota ((iota iota) iota))" "*i**iii";
      check "Mixed i" "((i (iota i)) i)" "`i*iIi";
      check "Jot style" "(s (k ((i s) k)))" "01";
      check "Mixed style" "(s (((s k) ((s (k ((i s) k))) iota)) i))" "`s(SK*01ii)";
      check "spaces & comments" "(s (k k))" " S # l1\n  (K # l2\n\n K # l4\n ) # l5";
      check "blank 1" "i" "";
      check "blank 2" "((k i) i)" "k()()";
      
      fails "invald char" "unexpected token: 'N'" "SNK";
      fails "eof error" "unexpected EOF" "(SK";
      fails "eof error" "unexpected EOF" "``sk";
      fails "eof error" "unexpected EOF" "*i";
  ])

let combinator_tests =
  let check label f =
    qcheck Alcotest.bool "to be true" label true (fun _ -> f ()) () in
  let open Lazyk in
  ("combinator", [
     check "i" (fun _ -> num0 == eval (apply i num0));
     check "k" (fun _ -> ctrue == eval (apply (apply k ctrue) cfalse));
     check "s" (fun _ -> num0 == eval (apply (apply (apply s k) k) num0));
     check "false" (fun _ -> num0 == eval (apply (apply cfalse s) num0));
     check "iota true" (fun _ -> s == eval (apply iota ctrue));
     check "iota false" (fun _ -> k == eval (apply iota cfalse));
  ])

let cnum_tests =
  let cn2i = Lazyk.int_of_cnum in
  let check label expect e =
    qcheck Alcotest.int "same int" label expect cn2i e in
  let fails label fmsg e =
    qcheck_fail "fails" label fmsg cn2i e in
  let app = Lazyk.apply in
  let cn2 = Lazyk.cnums.(2) in
  let cn3 = Lazyk.cnums.(3) in
  ("church num", [
     check "false -> 0" 0 Lazyk.cfalse;
     check "i -> 1" 1 Lazyk.i;
     check "cnums.(2)" 2 cn2;
     check "cnums.(3)" 3 cn3;
     check "cnums.(256)" 256 Lazyk.cnums.(256);
     check "2^3 = 8" 8 (app cn3 cn2);
     check "2^3^2 = 64" 64 (app cn2 (app cn3 cn2));
     check "2^(2^3) = 256" 256 (app (app cn3 cn2) cn2);
     fails "not a cnum" "invalid output format" Lazyk.s;
     fails "not a cnum" "invalid output format (attempted to apply num)" Lazyk.num0;
  ])


let input_tests =
  let car = fun x -> Lazyk.(apply x ctrue) in
  let cdr = fun x -> Lazyk.(apply x cfalse) in
  let cn2i = Lazyk.int_of_cnum in
  let buf = Buffer.create(1) in
  let (r, _) = Lazyk.mem_io (Bytes.of_string "abc\n") buf in
  let check label expect e =
    qcheck Alcotest.int "same int" label expect cn2i e in
  ("input", [
     check "a" 97 (car r);
     check "b" 98 (car (cdr r));
     check "c" 99 (car (cdr (cdr r)));
     check "\\n" 10 (car (cdr (cdr (cdr r))));
     check "eof" 256 (car (cdr (cdr (cdr (cdr r)))));
   ])
  

let parse src = Lazyk.(parse apply s k i iota) src

let output_tests =
  let cons = parse "S(S(KS)(S(KK)(S(KS)(S(K(SI))K))))(KK)" in
  let l = List.fold_right (fun c a -> Lazyk.(apply (apply cons cnums.(c)) a)) [65; 10; 66; 67; 256] Lazyk.num0 in
  let buf = Buffer.create 10 in
  let (_, w) = Lazyk.mem_io Bytes.empty buf in
  let _ = Lazyk.print_list w l in
  let bs = Buffer.to_bytes buf in
  ("output", [
     qcheck Alcotest.string "same string" "print_list" "A\nBC" Bytes.to_string bs
   ])


let run_str src stdin_str () =
  let ib = String.to_bytes stdin_str in
  let (r, ob) = Lazyk.run_lazyk_memio src ib in
  let ostr = Bytes.to_string ob in
  match r with
  | Ok(exitcode) -> Printf.sprintf "%d:%s" exitcode ostr
  | Error(e) -> ostr ^ "\n" ^ (Printexc.to_string e)

let program_tests =
  let check label expect src stdin_str =
    qcheck Alcotest.string "same string" label expect (run_str src stdin_str) () in
  ("program", [
     check "echo" "0:asdf\nqwer" "SKK" "asdf\nqwer";
     check "cdr" "0:jkl" "SI`k`ki" "hjkl";
     check "8*33" "8:" "11111110001111111000111100111110001111111000111100111111000111111111000001111111000111100111001111111000111100111111000111100111111110001111111000111100111110001110011111110001111111000111100111110001110011111111100000111111100011111110001111001111100011100111111111000001111111000111111111000001111001110011110011110011100" "!";
   ])
  
let () =
  Alcotest.run "Lazy K" [
      parser_tests;
      combinator_tests;
      cnum_tests;
      input_tests;
      output_tests;
      program_tests;
    ]
