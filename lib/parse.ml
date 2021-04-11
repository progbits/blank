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
    | Tab ->
        (* print_endline "NEGATIVE NUMBER!!!"; *)
        -1
    | _ -> raise (Invalid_argument "unexpected token")
  in
  let result = sign * fst (parse tokens) in
  (* printf "PARSED NUMBER: %d\n" result; *)
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
  (* printf "PARSED LABEL: %s\n" result ; *)
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
  | Space, Space ->
      (* print_endline "Mark" ; *)
      Mark (parse_label tokens)
  | Space, Tab ->
      (* print_endline "Call" ; *)
      Call (parse_label tokens)
  | Space, LineFeed ->
      (* print_endline "UnconditionalJump" ; *)
      UnconditionalJump (parse_label tokens)
  | Tab, Space ->
      (* print_endline "JumpZero" ; *)
      JumpZero (parse_label tokens)
  | Tab, Tab ->
      (* print_endline "JumpNegative" ; *)
      JumpNegative (parse_label tokens)
  | Tab, LineFeed ->
      (* print_endline "EndSubroutine" ; *)
      EndSubroutine
  | LineFeed, LineFeed -> (*print_endline "EndProgram" ;*) EndProgram
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

exception RuntimeError of string

(* Virtual machine state *)
type state =
  { call_stack: int Stack.t
  ; stack: int Stack.t
  ; heap: (int, int) Hashtbl.t
  ; current_stdin: string
  ; ip: int }

let incriment_ip state = {state with ip= state.ip + 1}

let update_ip state ip = {state with ip}

(* Read a single character from stdin *)
let read_char state =
  let do_read state =
    printf "do_read: %d\n" (String.length state.current_stdin) ;
    let first_char = String.nget state.current_stdin 0 in
    printf "First char: %c\n" first_char ;
    let remaining_chars =
      match String.length state.current_stdin with
      | 0 -> ""
      | _ ->
          String.slice state.current_stdin 1 (String.length state.current_stdin)
    in
    printf "Remaining char: %s\n" remaining_chars ;
    (first_char, {state with current_stdin= remaining_chars})
  in
  match String.length state.current_stdin with
  | 0 ->
      let line = In_channel.input_line_exn In_channel.stdin in
      do_read {state with current_stdin= line}
  | _ -> do_read state

(* Execute a StackManipulation command and return the new state *)
let exec_stack_manipulation command state =
  (* print_endline "exec_stack_manipulation" ; *)
  (* printf "\tSTACK SIZE %d \n" (Stack.length state.stack) ; *)
  match command with
  | Push value ->
      (* printf "\tPUSH %d \n" value ; *)
      Stack.push state.stack value ;
      (* printf "\tSTACK SIZE %d \n" (Stack.length state.stack) ; *)
      incriment_ip state
  | Duplicate ->
      (* printf "\tDUPLICATE\n" ; *)
      let value = Stack.pop_exn state.stack in
      Stack.push state.stack value ;
      Stack.push state.stack value ;
      (* printf "\tSTACK SIZE %d \n" (Stack.length state.stack) ; *)
      incriment_ip state
  | Swap ->
      let a = Stack.pop_exn state.stack in
      let b = Stack.pop_exn state.stack in
      (* printf "\tSWAP %d %d\n" a b ; *)
      Stack.push state.stack a ;
      Stack.push state.stack b ;
      (* printf "\tSTACK SIZE %d \n" (Stack.length state.stack) ; *)
      incriment_ip state
  | Discard ->
      let _ = Stack.pop_exn state.stack in
      (* printf "\tDISCARD %d \n" value ; *)
      (* printf "\tSTACK SIZE %d \n" (Stack.length state.stack) ; *)
      incriment_ip state

(* Execute an Arithmetic command and return the new state *)
let exec_arithmetic command state =
  (* print_endline "exec_arithmetic" ; *)
  (* printf "\tSTACK SIZE %d \n" (Stack.length state.stack) ; *)
  let right = Stack.pop_exn state.stack in
  let left = Stack.pop_exn state.stack in
  match command with
  | Addtion ->
      (* printf "\tADDITION: %d + %d\n" left right ; *)
      Stack.push state.stack (left + right) ;
      (* printf "\tSTACK SIZE %d \n" (Stack.length state.stack) ; *)
      incriment_ip state
  | Subtraction ->
      (* printf "\tSUBTRACTION: %d - %d\n" left right ; *)
      Stack.push state.stack (left - right) ;
      (* printf "\tSTACK SIZE %d \n" (Stack.length state.stack) ; *)
      incriment_ip state
  | Multiplication ->
      (* printf "\tMULTIPLICATION: %d * %d\n" left right ; *)
      Stack.push state.stack (left * right) ;
      (* printf "\tSTACK SIZE %d \n" (Stack.length state.stack) ; *)
      incriment_ip state
  | Division ->
      (* printf "\tDIVISION: %d / %d\n" left right ; *)
      Stack.push state.stack (left / right) ;
      (* printf "\tSTACK SIZE %d \n" (Stack.length state.stack) ; *)
      incriment_ip state
  | Modulo ->
      (* printf "\tMODULO: %d %% %d = %d\n" left right (left % right) ; *)
      Stack.push state.stack (left % right) ;
      (* printf "\tSTACK SIZE %d \n" (Stack.length state.stack) ; *)
      incriment_ip state

(* Execute a HeapAccess command and return the new state *)
let exec_heap_access command state =
  (* print_endline "exec_heap_access" ; *)
  match command with
  | Store ->
      let value = Stack.pop_exn state.stack in
      let address = Stack.pop_exn state.stack in
      (* printf "STORE %d at %d\n" value address ; *)
      (* printf "STACK SIZE %d \n" (Stack.length state.stack) ; *)
      let _ = Hashtbl.set state.heap ~key:address ~data:value in
      incriment_ip state
  | Retrieve ->
      let address = Stack.pop_exn state.stack in
      let value = Hashtbl.find_exn state.heap address in
      (* printf "RETRIEVED %d from %d\n" value address ; *)
      (* printf "STACK SIZE %d \n" (Stack.length state.stack) ; *)
      let _ = Stack.push state.stack value in
      incriment_ip state

(* Execute a FlowControl command and return the new state *)
let exec_flow_control command state =
  (* print_endline "exec_flow_control" ; *)
  match command with
  | Mark _ ->
      (* printf "Mark: %s\n" label.name ; *)
      (* printf "STACK SIZE %d \n" (Stack.length state.stack) ; *)
      incriment_ip state
  | Call label ->
      (* printf "Call: %d\n" label.target ; *)
      (* printf "CALL STACK SIZE %d \n" (Stack.length state.call_stack) ; *)
      Stack.push state.call_stack (state.ip + 1) ;
      (* printf "CALL STACK SIZE %d \n" (Stack.length state.call_stack) ; *)
      update_ip state label.target
  | UnconditionalJump label ->
      (* printf "UnconditionalJump: %d\n" label.target ; *)
      (* printf "STACK SIZE %d \n" (Stack.length state.stack) ; *)
      update_ip state label.target
  | JumpZero label ->
      (* printf "MAYBE JumpZero: %d\n" label.target ; *)
      (* printf "STACK SIZE %d \n" (Stack.length state.stack) ; *)
      let value = Stack.pop_exn state.stack in
      (* printf "JumpZero if: %d == 0\n" value ; *)
      (* printf "STACK SIZE %d \n" (Stack.length state.stack) ; *)
      if value = 0 then update_ip state label.target else incriment_ip state
  | JumpNegative label ->
      (* printf "MAYBE JumpNegative: %d\n" label.target ; *)
      (* printf "STACK SIZE %d \n" (Stack.length state.stack) ; *)
      let value = Stack.pop_exn state.stack in
      (* printf "JumpNegative if: %d < 0\n" value ; *)
      (* printf "STACK SIZE %d \n" (Stack.length state.stack) ; *)
      if value < 0 then update_ip state label.target else incriment_ip state
  | EndSubroutine ->
      (* print_endline "EndSubroutine" ; *)
      (* printf "CALL STACK SIZE %d \n" (Stack.length state.call_stack) ; *)
      let dest = Stack.pop_exn state.call_stack in
      (* printf "CALL STACK SIZE %d \n" (Stack.length state.call_stack) ; *)
      (* printf "EndSubroutine: %d\n" dest ; *)
      update_ip state dest
  | EndProgram -> raise (RuntimeError "end of program")

(* Execute an IO command and return the new state *)
let exec_io command state =
  (*print_endline "exec_io";*)
  match command with
  | OutputCharacter ->
      (* print_endline "OutputCharacter" ; *)
      let value = Stack.pop_exn state.stack in
      printf "%c%!" (Char.unsafe_of_int value) ;
      incriment_ip state
  | OutputNumber ->
      (* print_endline "OutputNumber" ; *)
      let value = Stack.pop_exn state.stack in
      printf "%d%!" value ; incriment_ip state
  | ReadCharacter ->
      (* print_endline "ReadCharacter" ; *)
      let value, state = read_char state in
      let address = Stack.pop_exn state.stack in
      (* printf "STORE %d at %d\n" value address ; *)
      let _ = Hashtbl.set state.heap ~key:address ~data:(Char.to_int value) in
      incriment_ip state
  | ReadNumber ->
      (* print_endline "ReadNumber" ; *)
      let _ = Out_channel.flush in
      let line = In_channel.input_line_exn In_channel.stdin in
      let value = int_of_string line in
      let address = Stack.pop_exn state.stack in
      (* printf "STORE %d at %d\n" value address ; *)
      let _ = Hashtbl.set state.heap ~key:address ~data:value in
      incriment_ip state

(* 2 pass label resolution *)
let resolve_labels instructions =
  (* printf "List.length instructions: %d\n" (List.length instructions) ; *)
  (* Resolve IP values of each label *)
  let rec resolve instructions n labels =
    (* printf "resolve: %d\n" n ; *)
    try
      match List.hd_exn instructions with
      | FlowControl command -> (
        (* print_endline "FlowControl" ; *)
        match command with
        | Mark label ->
            (* print_endline "Mark" ; *)
            let _ = Hashtbl.add labels ~key:label.name ~data:n in
            (* printf "Found Label %s at %d\n" label.name n ; *)
            resolve (List.tl_exn instructions) (n + 1) labels
        | _ -> resolve (List.tl_exn instructions) (n + 1) labels )
      | _ -> resolve (List.tl_exn instructions) (n + 1) labels
    with Failure _ -> ()
  in
  (* Subsitute label targets with IP values *)
  let apply labels instruction =
    (* print_endline "apply" ; *)
    (* printf "labels size: %d" (Hashtbl.length labels) ; *)
    match instruction with
    | StackManipulation _ -> instruction
    | Arithmetic _ -> instruction
    | HeapAccess _ -> instruction
    | FlowControl command -> (
      match command with
      | Mark _ -> FlowControl command
      | Call target ->
          let dest = Hashtbl.find_exn labels target.name in
          FlowControl (Call {name= target.name; target= dest})
      | UnconditionalJump target ->
          let dest = Hashtbl.find_exn labels target.name in
          FlowControl (UnconditionalJump {name= target.name; target= dest})
      | JumpZero target ->
          let dest = Hashtbl.find_exn labels target.name in
          FlowControl (JumpZero {name= target.name; target= dest})
      | JumpNegative target ->
          let dest = Hashtbl.find_exn labels target.name in
          FlowControl (JumpNegative {name= target.name; target= dest})
      | EndSubroutine -> FlowControl EndSubroutine
      | EndProgram -> FlowControl EndProgram )
    | IO command -> IO command
  in
  let labels = Hashtbl.create (module String) in
  let _ = resolve instructions 0 labels in
  List.map instructions ~f:(apply labels)

let run instructions =
  let state =
    { call_stack= Stack.create ()
    ; stack= Stack.create ()
    ; heap= Hashtbl.create (module Int)
    ; current_stdin= ""
    ; ip= 0 }
  in
  (* List.iter instructions ~f:print_imp ; *)
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
