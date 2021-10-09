open Core

(* Lexical tokens *)
type token = Space | Tab | LineFeed | EOF

(* Stack manipulation instructions *)
type stack_manipulation = Push of int | Duplicate | Swap | Discard

(* Arithmetic instructions *)
type arithmetic = Addtion | Subtraction | Multiplication | Division | Modulo

(* Heap access instructions *)
type heap_acces = Store | Retrieve

type label = {name: string; target: int}

(* Flow control instructions *)
type flow_control =
  | Mark of label
  | Call of label
  | UnconditionalJump of label
  | JumpZero of label
  | JumpNegative of label
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

let is_whitespace c =
  match char_to_token c with exception Invalid_argument _ -> false | _ -> true

(* Scan a list of tokens from a string *)
let scan input =
  let chars = List.init (String.length input) ~f:(String.get input) in
  let filtered = List.filter chars ~f:is_whitespace in
  let tokens = List.map filtered ~f:(fun c -> char_to_token c) in
  Stream.of_list tokens

(* Parse a number *)
let parse_number tokens =
  let rec parse tokens =
    match Stream.next tokens with
    | LineFeed -> (0, 0)
    | Space ->
        let result = parse tokens in
        let pos = snd result in
        (fst result, pos + 1)
    | Tab ->
        let result = parse tokens in
        let pos = snd result in
        (fst result + (1 lsl pos), pos + 1)
    | _ -> raise (Invalid_argument "unexpected token")
  in
  let sign =
    match Stream.next tokens with
    | Space -> 1
    | Tab -> -1
    | _ -> raise (Invalid_argument "unexpected token")
  in
  let result = sign * fst (parse tokens) in
  result

(* Parse a label. We can't just parse labels are numbers as e.g. [Space;
 * LineFeed] and [Space; Space; LineFeed] are the same number, but two different
 * labels.*)
let parse_label tokens =
  let rec parse tokens =
    match Stream.next tokens with
    | LineFeed -> ""
    | Space -> "S" ^ parse tokens
    | Tab -> "T" ^ parse tokens
    | _ -> raise (Invalid_argument "unexpected token")
  in
  let result = parse tokens in
  {name= result; target= 0}

(* Parse Stack Manipulation Instruction Modification Parameter*)
let parse_stack_imp tokens =
  match Stream.next tokens with
  | Space -> Push (parse_number tokens)
  | LineFeed -> (
    match Stream.next tokens with
    | Space -> Duplicate
    | Tab -> Swap
    | LineFeed -> Discard
    | _ -> raise (Invalid_argument "unexpected token") )
  | Tab -> (
    match Stream.next tokens with
    | Space -> raise (Invalid_argument "unexpected op COPY")
    | LineFeed -> raise (Invalid_argument "unexpected op SLIDE")
    | _ -> raise (Invalid_argument "unexpected token") )
  | _ -> raise (Invalid_argument "unknown stack imp")

(* Parse Arithmetic Instruction Modification Parameter*)
let parse_arithmetic_imp tokens =
  match (Stream.next tokens, Stream.next tokens) with
  | Space, Space -> Addtion
  | Space, Tab -> Subtraction
  | Space, LineFeed -> Multiplication
  | Tab, Space -> Division
  | Tab, Tab -> Modulo
  | _, _ -> raise (Invalid_argument "unexpected token")

(* Parse Heap Access Instruction Modification Parameter*)
let parse_heap_imp tokens =
  match Stream.next tokens with
  | Space -> Store
  | Tab -> Retrieve
  | _ -> raise (Invalid_argument "unknown heap access imp")

(* Parse Flow Control Instruction Modification Parameter*)
let parse_flow_control_imp tokens =
  match (Stream.next tokens, Stream.next tokens) with
  | Space, Space -> Mark (parse_label tokens)
  | Space, Tab -> Call (parse_label tokens)
  | Space, LineFeed -> UnconditionalJump (parse_label tokens)
  | Tab, Space -> JumpZero (parse_label tokens)
  | Tab, Tab -> JumpNegative (parse_label tokens)
  | Tab, LineFeed -> EndSubroutine
  | LineFeed, LineFeed -> EndProgram
  | _, _ -> raise (Invalid_argument "unexpected token")

(* Parse I/O Instruction Modification Parameter*)
let parse_io_imp tokens =
  match (Stream.next tokens, Stream.next tokens) with
  | Space, Space -> OutputCharacter
  | Space, Tab -> OutputNumber
  | Tab, Space -> ReadCharacter
  | Tab, Tab -> ReadNumber
  | _, _ -> raise (Invalid_argument "unexpected token")

(* Parse a stream of tokens into a list of instructions *)
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
      | EOF | (exception Stream.Failure) -> List.rev acc
    with Stream.Failure -> List.rev acc
  in
  loop []
