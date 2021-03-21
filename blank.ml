open Core

(* Get the input filename *)
let get_filename =
    try Some (Array.get (Sys.get_argv ()) 1)
    with Invalid_argument _ -> None

let () =
    match get_filename with
    | Some filename -> Parse.scan (In_channel.read_all filename)
    | None -> print_endline "usage: blank FILENAME"
