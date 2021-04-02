open Core

(* Lexical tokens *)
type token = Space | Tab | LineFeed | EOF

(* Stack manipulation instructions *)
type stack_manipulation = Push of int | Duplicate | Swap | Discard

(* Arithmetic instructions *)
type arithmetic =
  | Addtion of int * int
  | Subtraction of int * int
  | Multiplication of int * int
  | Division of int * int
  | Modulo of int * int

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
  List.map chars ~f:(fun c -> char_to_token c)

(* Parse a number *)
let rec parse_number tokens =
  match tokens with
  | LineFeed :: _ -> (0, 0)
  | Space :: t ->
      let result = parse_number t in
      let pos = snd result in
      (fst result, pos + 1)
  | Tab :: t ->
      let result = parse_number t in
      let pos = snd result in
      (fst result + (1 lsl pos), pos + 1)
  | _ -> raise (Invalid_argument "unexpected token")

(* Parse Stack Manipulation Instruction Modification Parameter*)
let parse_stack_imp tokens =
  match tokens with
  | Space :: t -> Push (fst (parse_number t))
  | Tab :: Space :: _ -> Duplicate
  | Tab :: Tab :: _ -> Swap
  | LineFeed :: _ -> Discard
  | _ ->
      raise (Invalid_argument "unknown stack instruction modification parameter")
