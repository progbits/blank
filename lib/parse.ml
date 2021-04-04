open Core

(* Lexical tokens *)
type token = Space | Tab | LineFeed | EOF

(* Stack manipulation instructions *)
type stack_manipulation = Push of int | Duplicate | Swap | Discard

(* Arithmetic instructions *)
type arithmetic = Addtion | Subtraction | Multiplication | Division | Modulo

(* Heap access instructions *)
type heap_acces = Store | Retrieve

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
type io = OutputCharacter | OutputNumber | ReadCharacter | ReadNumber

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

let print_imp instruction =
  match instruction with
  | StackManipulation _ -> print_endline "StackManipulation"
  | Arithmetic _ -> print_endline "Arithmetic"
  | HeapAccess _ -> print_endline "HeapAccess"
  | FlowControl _ -> print_endline "FlowControl"
  | IO _ -> print_endline "IO"

(* Convert a char to a token *)
let char_to_token c =
  match c with
  | ' ' -> Space
  | '\t' -> Tab
  | '\n' -> LineFeed
  | _ -> raise (Invalid_argument "unexpected character")

(* Scan a list of tokens from a string *)
let scan input =
  let chars = List.init (String.length input) ~f:(String.get input) in
  let tokens = List.map chars ~f:(fun c -> char_to_token c) in
  Stream.of_list tokens

(* Parse a number *)
let rec parse_number tokens =
  match Stream.next tokens with
  | LineFeed -> (0, 0)
  | Space ->
      let result = parse_number tokens in
      let pos = snd result in
      (fst result, pos + 1)
  | Tab ->
      let result = parse_number tokens in
      let pos = snd result in
      (fst result + (1 lsl pos), pos + 1)
  | _ -> raise (Invalid_argument "unexpected token")

(* Parse Stack Manipulation Instruction Modification Parameter*)
let parse_stack_imp tokens =
  match Stream.next tokens with
  | Space -> Push (fst (parse_number tokens))
  | LineFeed -> (
    match Stream.next tokens with
    | Space -> Duplicate
    | Tab -> Swap
    | LineFeed -> Discard
    | _ -> raise (Invalid_argument "unexpected token") )
  | _ -> raise (Invalid_argument "unknown stack imp")

(* Parse Arithmetic Instruction Modification Parameter*)
let parse_arithmetic_imp tokens =
  match Stream.next tokens with
  | Space -> (
    match Stream.next tokens with
    | Space -> Addtion
    | Tab -> Subtraction
    | LineFeed -> Multiplication
    | _ -> raise (Invalid_argument "unexpected token") )
  | Tab -> (
    match Stream.next tokens with
    | Space -> Division
    | Tab -> Modulo
    | _ -> raise (Invalid_argument "unexpected token") )
  | _ -> raise (Invalid_argument "unknown arithmetic imp")

(* Parse Heap Access Instruction Modification Parameter*)
let parse_heap_imp tokens =
  match Stream.next tokens with
  | Space -> Store
  | Tab -> Retrieve
  | _ -> raise (Invalid_argument "unknown heap access imp")

(* Parse Flow Control Instruction Modification Parameter*)
let parse_flow_control_imp tokens =
  match Stream.next tokens with
  | Space -> (
    match Stream.next tokens with
    | Space -> Mark (fst (parse_number tokens))
    | Tab -> Call (fst (parse_number tokens))
    | LineFeed -> UnconditionalJump (fst (parse_number tokens))
    | _ -> raise (Invalid_argument "unexpected token") )
  | Tab -> (
    match Stream.next tokens with
    | Space -> JumpZero (fst (parse_number tokens))
    | Tab -> JumpNegative (fst (parse_number tokens))
    | LineFeed -> EndSubroutine
    | _ -> raise (Invalid_argument "unexpected token") )
  | LineFeed -> (
    match Stream.next tokens with
    | LineFeed -> EndProgram
    | _ -> raise (Invalid_argument "unexpected token") )
  | _ -> raise (Invalid_argument "unknown flow control imp")

(* Parse I/O Instruction Modification Parameter*)
let parse_io_imp tokens =
  match Stream.next tokens with
  | Space -> (
    match Stream.next tokens with
    | Space -> OutputCharacter
    | Tab -> OutputNumber
    | _ -> raise (Invalid_argument "unexpected token") )
  | Tab -> (
    match Stream.next tokens with
    | Space -> ReadCharacter
    | Tab -> ReadNumber
    | _ -> raise (Invalid_argument "unexpected token") )
  | _ -> raise (Invalid_argument "unknown i/o imp")

(* Parse a stream of tokens into an instruction stream *)
let parse tokens =
  let rec loop acc =
    try
      match Stream.next tokens with
      | Space -> loop (StackManipulation (parse_stack_imp tokens) :: acc)
      | Tab -> (
        match Stream.next tokens with
        | Space -> loop (Arithmetic (parse_arithmetic_imp tokens) :: acc)
        | Tab -> loop (HeapAccess (parse_heap_imp tokens) :: acc)
        | LineFeed -> loop (IO (parse_io_imp tokens) :: acc)
        | _ -> raise (Invalid_argument "unexpected token") )
      | LineFeed -> loop (FlowControl (parse_flow_control_imp tokens) :: acc)
      | EOF | (exception Stream.Failure) -> Stream.of_list (List.rev acc)
    with Stream.Failure -> Stream.of_list (List.rev acc)
  in
  loop []
