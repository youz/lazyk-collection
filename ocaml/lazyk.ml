
type expr =
  | L of expr Lazy.t
  | F of (expr -> expr)
  | N of int

let rec eval = function
  | L(l) -> eval (Lazy.force l)
  | e -> e

let eval_app lhs rhs =
  match eval lhs with
  | F(f) -> f rhs
  | N(_) -> failwith "invalid output format (attempted to apply num)"
  | _ -> failwith "unreachable code"

let apply f a = L(lazy (eval_app f a))

let s = F(fun x -> F(fun y -> F(fun z -> apply (apply x z) (apply y z))))
let k = F(fun x -> F(fun _ -> x))
let i = F(fun x -> x)
let iota = F(fun x -> apply (apply x s) k)
let ctrue = k
let cfalse = F(fun _ -> i)
let inc = F(fun e ->
              match eval e with
              | N(n) -> N(n + 1)
              | _ -> failwith "invalid output format"
            )

let num0 = N(0)
let succ = apply s (apply (apply s (apply k s)) k)
let cnums = Seq.iterate (apply succ) cfalse |> Seq.take 257 |> Array.of_seq

let int_of_cnum cn =
  match eval (apply (apply cn inc) num0) with
  | N(n) -> n
  | _ -> failwith "invalid output format"

let rec make_reader r initial_arg =
  let f () =
    let (c, next_arg) = r initial_arg in
    let cn = cnums.(c) in
    F(fun a -> apply (apply a cn) (make_reader r next_arg)) in
  L(lazy (f ()))

let chan_io ic oc =
  let reader () =
    flush oc;
    let b = try input_byte ic with End_of_file -> 256 in
    (b, ()) in
  let writer c =
    output_byte oc c;
    if c = 10 then
      flush oc in
  (make_reader reader (), writer)

let mem_io in_bytes out_buffer =
  let ilen = Bytes.length in_bytes in
  let reader i =
    if i = ilen then (256, i)
    else (Bytes.get_uint8 in_bytes i, i + 1) in
  let writer c =
    Buffer.add_int8 out_buffer c in
  (make_reader reader 0, writer)

let print_list writer =
  let rec loop l =
    let i = int_of_cnum (apply l ctrue) in
    if i >= 256 then i - 256
    else (writer i; loop (apply l cfalse)) in
  loop


let parse app s k i iota src =
  let rec skip_line = function
    | [] -> []
    | '\n' :: t -> t
    | _ :: t -> skip_line t in
  let isspace c =
    let i = Char.code c in
    (i >= 9 && i <= 13) || i = 32 in
  let rec skip_ws = function
    | [] -> []
    | (h :: t) as l ->
       if isspace h then skip_ws t
       else if h = '#' then skip_line t |> skip_ws
       else l in
  let rec read_cc l closingchar =
    let rec loop l acc =
      match skip_ws l, closingchar with
      | [], None -> (acc, [])
      | [], Some(_) -> failwith "unexpected EOF"
      | h :: t, Some(c) when h = c -> (acc, t)
      | l, _ ->
         let (e, r) = read_one l false in
         loop r (if acc == i then e else app acc e) in
    loop l i
  and read_one l i_is_iota =
    match skip_ws l with
    | [] -> failwith "unexpected EOF"
    | '`' :: t ->
       let (f, t) = read_one t false in
       let (a, t) = read_one t false in
       (app f a, t)
    | '*' :: t ->
       let (f, t) = read_one t true in
       let (a, t) = read_one t true in
       (app f a, t)
    | '(' :: r -> read_cc r (Some(')'))
    | (h :: t) as l -> (
      match h with
      | 'S' | 's' -> (s, t)
      | 'K' | 'k' -> (k, t)
      | 'I' -> (i, t)
      | 'i' -> ((if i_is_iota then iota else i), t)
      | '0' | '1' -> read_jot l
      | u -> failwith (Printf.sprintf "unexpected token: '%c'" u)
    )
  and read_jot l =
    let rec loop l e =
      match skip_ws l with
      | '0' :: t -> loop t (app (app e s) k)
      | '1' :: t -> loop t (app s (app k e))
      | r -> (e, r) in
    loop l i in
  let (e, _) = read_cc (String.to_seq src |> List.of_seq) None in
  e


let run_lazyk srcstr r w =
  try
    let prog = parse apply s k i iota srcstr in
    let reuslt = print_list w (apply prog r) in
    Ok(reuslt)
  with e ->
    Error(e)

let run_lazyk_chan srcstr ic oc =
  let (r, w) = chan_io ic oc in
  let result = run_lazyk srcstr r w in
  flush oc;
  result

(* for test *)
let run_lazyk_memio srcstr in_bytes =
  let outbuf = Buffer.create 1024 in
  let (r, w) = mem_io in_bytes outbuf in
  let result = run_lazyk srcstr r w in
  (result, Buffer.to_bytes outbuf)
