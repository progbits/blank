open Core

(* Lexical tokens *)
type token =
    | Space
    | Tab
    | LineFeed
    | EOF

    (* Print the string representation of a token *)
let print_token token =
    match token with
    | Space -> print_endline "Space"
    | Tab -> print_endline "Tab"
    | LineFeed -> print_endline "LineFeed"
    | EOF -> print_endline "EOF"

(* Get the input filename *)
let get_filename =
    try Some (Array.get (Sys.get_argv ()) 1)
    with Invalid_argument _ -> None

let read_lines file =
    In_channel.read_lines file

(* Convert a char to a token*)
let char_to_token c = 
    match c with 
    | ' ' -> Space
    | '\t' -> Tab
    | '\n' -> LineFeed
    | _ -> raise (Invalid_argument "unexpected character")

(* Scan tokens from a line *)
let scan_line line = 
    let chars = List.init (String.length line) ~f:(String.get line) in
        List.map chars ~f:(fun c -> char_to_token c)

(* Scan a list of tokens from a list of lines *)
let scan lines = 
    let tokens = List.concat (List.map lines ~f:scan_line) in
        List.iter tokens ~f:(fun token -> print_token token)

let () =
    match get_filename with
    | Some filename -> scan (read_lines filename)
    | None -> print_endline "usage: blank FILENAME"
