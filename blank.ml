open Core

(* Get the input filename *)
let get_filename =
    try Some (Array.get (Sys.get_argv ()) 1)
    with Invalid_argument _ -> None

let () =
    match get_filename with
    | Some filename ->
        let tokens = Parse.scan (In_channel.read_all filename) in
          List.iter tokens ~f:(fun token -> Parse.print_token token)
    | None -> print_endline "usage: blank FILENAME"
