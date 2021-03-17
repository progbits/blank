open Core

(* Lexical tokens *)
type token =
    | Space
    | Tab
    | LineFeed

(* Get the input filename *)
let get_filename =
    try Some (Array.get (Sys.get_argv ()) 1)
    with Invalid_argument _ -> None

let read_lines file =
    In_channel.read_lines file

let print_line line =
    print_endline line

let () =
    match get_filename with
    | Some filename ->
        let lines = read_lines filename in
            List.iter lines ~f:print_line
    | None -> print_endline "usage: blank FILENAME"
