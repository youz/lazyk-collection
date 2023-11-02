
let showhelp () =
  print_string ("usage: " ^ Filename.basename Sys.argv.(0) ^ " <source.lazy>\n");
  exit 1

let read_file filename =
  let ic = open_in filename in
  let size = in_channel_length ic in
  let result = really_input_string ic size in
  close_in ic;
  result

let run src =
  match Lazyk.run_lazyk_chan src stdin stdout with
  | Ok(ec) -> exit ec
  | Error(e) ->
     prerr_newline ();
     Printexc.to_string e |> prerr_endline;
     exit 1

let main () =
  if Array.length Sys.argv != 2 then
    showhelp ()
  else
    match Sys.argv.(1) with
    | "--help" | "-h" -> showhelp ()
    | f -> read_file f |> run

let () = main ()
