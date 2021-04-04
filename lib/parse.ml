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

let print_top_of_stack stack =
  let dup = Stack.copy stack in
  match Stack.pop dup with
  | Some value -> printf "%d\n" value
  | None -> print_endline "nothing there"

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

exception RuntimeError of string

(* Virtual machine state *)
type state = {stack: int Stack.t; heap: Buffer.t; ip: int}

let incriment_ip state = {stack= state.stack; heap= state.heap; ip= state.ip + 1}

let update_ip state ip = {stack= state.stack; heap= state.heap; ip}

(* Execute a StackManipulation command and return the new state *)
let exec_stack_manipulation command state =
  (*print_endline "exec_stack_manipulation" ;*)
  match command with
  | Push value ->
      (*printf "PUSH %d \n" value ;*)
      Stack.push state.stack value ;
      (* printf "STACK SIZE %d \n" (Stack.length state.stack) ; *)
      incriment_ip state
  | Duplicate -> (
    (* printf "TRY DUPLICATE\n" ; *)
    match Stack.pop state.stack with
    | Some value ->
        (* printf "DUPLICATE %d\n" value ; *)
        Stack.push state.stack value ;
        Stack.push state.stack value ;
        (* printf "STACK SIZE %d \n" (Stack.length state.stack) ; *)
        incriment_ip state
    | None -> raise (RuntimeError "stack is empty") )
  | Swap -> (
      let a = Stack.pop state.stack in
      let b = Stack.pop state.stack in
      match (a, b) with
      | Some a, Some b ->
          (* printf "SWAP %d %d\n" a b ; *)
          Stack.push state.stack a ;
          Stack.push state.stack b ;
          incriment_ip state
      | _ -> raise (RuntimeError "stack is empty") )
  | Discard -> (
    match Stack.pop state.stack with
    | Some _ ->
        (* printf "DISCARD %d\n" value ; *)
        incriment_ip state
    | None -> raise (RuntimeError "stack is empty") )

(* Execute an Arithmetic command and return the new state *)
let exec_arithmetic command state =
  let a = Stack.pop state.stack in
  let b = Stack.pop state.stack in
  match (a, b) with
  | Some a, Some b -> (
    match command with
    | Addtion ->
        (* printf "exec_arithmetic: %d + %d\n" a b; *)
        Stack.push state.stack (a + b) ;
        incriment_ip state
    | Subtraction ->
        (* printf "exec_arithmetic: %d - %d\n" a b; *)
        Stack.push state.stack (a - b) ;
        incriment_ip state
    | Multiplication ->
        (* printf "exec_arithmetic: %d * %d\n" a b; *)
        Stack.push state.stack (a * b) ;
        incriment_ip state
    | Division ->
        (* printf "exec_arithmetic: %d / %d\n" a b; *)
        Stack.push state.stack (a / b) ;
        incriment_ip state
    | Modulo ->
        (* printf "exec_arithmetic: %d %% %d\n" a b; *)
        Stack.push state.stack (a % b) ;
        incriment_ip state )
  | _ -> raise (RuntimeError "expected a stack size of at least 2")

(* Execute a HeapAccess command and return the new state *)
let exec_heap_access command state =
  (* print_endline "exec_heap_access" ; *)
  let a = Stack.pop state.stack in
  match a with
  | Some _ -> (
    match command with
    | Store -> incriment_ip state
    | Retrieve -> incriment_ip state )
  | _ -> raise (RuntimeError "expected a stack size of at least 2")

(* Execute a FlowControl command and return the new state *)
let exec_flow_control command state =
  print_endline "exec_flow_control" ;
  match command with
  | Mark _ -> incriment_ip state
  | Call label ->
      print_endline "Call" ;
      Stack.push state.stack state.ip ;
      update_ip state label
  | UnconditionalJump label ->
      printf "UnconditionalJump: %d\n" label ;
      update_ip state label
  | JumpZero label -> (
      printf "MAYBE JumpZero: %d\n" label ;
      match Stack.pop state.stack with
      | Some value ->
          printf "JumpZero if: %d == 0\n" value ;
          if value = 0 then update_ip state label else incriment_ip state
      | None -> raise (RuntimeError "expected a value") )
  | JumpNegative _ ->
      print_endline "JumpNegative" ;
      incriment_ip state
  | EndSubroutine ->
      print_endline "EndSubroutine" ;
      incriment_ip state
  | EndProgram -> raise (RuntimeError "end of program")

(* Execute an IO command and return the new state *)
let exec_io command state =
  (*print_endline "exec_io";*)
  match command with
  | OutputCharacter -> (
    (*print_endline "OutputCharacter";*)
    match Stack.pop state.stack with
    | Some value ->
        printf "%c" (Char.unsafe_of_int value) ;
        incriment_ip state
    | None -> raise (RuntimeError "expected a value") )
  | OutputNumber -> (
    (*print_endline "OutputNumber";*)
    match Stack.pop state.stack with
    | Some value -> printf "%d" value ; incriment_ip state
    | None -> raise (RuntimeError "expected a value") )
  | ReadCharacter -> incriment_ip state
  | ReadNumber -> (*print_endline "ReadNumber" ;*) incriment_ip state

(* 2 pass label resolution *)
let resolve_labels instructions =
  (* Resolve IP values of each label *)
  let rec resolve instructions n labels =
    try
      match List.hd_exn instructions with
      | FlowControl command -> (
        match command with
        | Mark label ->
            let _ = Hashtbl.add labels ~key:label ~data:n in
            resolve (List.tl_exn instructions) (n + 1) labels
        | _ -> resolve (List.tl_exn instructions) (n + 1) labels )
      | _ -> resolve (List.tl_exn instructions) (n + 1) labels
    with Failure _ -> ()
  in
  (* Subsitute label targets with IP values *)
  let apply labels instruction =
    match instruction with
    | StackManipulation _ -> instruction
    | Arithmetic _ -> instruction
    | HeapAccess _ -> instruction
    | FlowControl command -> (
      match command with
      | Mark _ -> FlowControl command
      | Call target ->
          let dest = Hashtbl.find_exn labels target in
          FlowControl (Call dest)
      | UnconditionalJump target ->
          let dest = Hashtbl.find_exn labels target in
          FlowControl (UnconditionalJump dest)
      | JumpZero target ->
          let dest = Hashtbl.find_exn labels target in
          FlowControl (JumpZero dest)
      | JumpNegative target ->
          let dest = Hashtbl.find_exn labels target in
          FlowControl (JumpNegative dest)
      | EndSubroutine -> FlowControl EndSubroutine
      | EndProgram -> FlowControl EndProgram )
    | IO command -> IO command
  in
  let labels = Hashtbl.create (module Int) in
  let _ = resolve instructions 0 labels in
  List.map instructions ~f:(apply labels)

let run instructions =
  let state = {stack= Stack.create (); heap= Buffer.create 128; ip= 0} in
  let instructions = resolve_labels instructions in
  let rec exec state =
    (*printf "Instruction Pointer: %d\n" state.ip ;*)
    match List.nth_exn instructions state.ip with
    | StackManipulation command -> exec (exec_stack_manipulation command state)
    | Arithmetic command -> exec (exec_arithmetic command state)
    | HeapAccess command -> exec (exec_heap_access command state)
    | FlowControl command -> exec (exec_flow_control command state)
    | IO command -> exec (exec_io command state)
  in
  exec state
