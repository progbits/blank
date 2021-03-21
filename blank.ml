open Core

(* Lexical tokens *)
type token =
    | Space
    | Tab
    | LineFeed
    | EOF

(* Stack manipulation instructions *)
type stack_manipulation =
    | Push of int 
    | Duplicate
    | Swap
    | Discard

(* Arithmetic instructions *)
type arithmetic = 
    | Addtion of int * int
    | Subtraction of int * int
    | Multiplication of int * int
    | Division of int * int
    | Modulo of int * int

(* Heap access instructions *)
type heap_acces =
    | Store
    | Retrieve

(* Flow control instructions *)
type flow_control =
    | Mark of int
    | Call of int
    | UnconditionalJump of int
    | JumpZero of int
    | JumpNegative of int
    | EndSubroutine
    | EndProgram

(* IO instructions *)
type io = 
    | OutputCharacter
    | OutputNumber
    | ReadCharacter
    | ReadNumber

(* Instruction modification parameters *)
type imp =
    | StackManipulation of stack_manipulation
    | Arithmetic of arithmetic
    | HeapAccess of heap_acces
    | FlowControl of flow_control
    | IO of io

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

(* Convert a char to a token*)
let char_to_token c = 
    match c with 
    | ' ' -> Space
    | '\t' -> Tab
    | '\n' -> LineFeed
    | _ -> raise (Invalid_argument "unexpected character")

(* Scan a list of tokens from a list of lines *)
let scan input =
    let chars = List.init (String.length input) ~f:(String.get input) in
    let tokens = List.map chars ~f:(fun c -> char_to_token c) in
        List.iter tokens ~f:(fun token -> print_token token)

let () =
    match get_filename with
    | Some filename -> scan (In_channel.read_all filename)
    | None -> print_endline "usage: blank FILENAME"
