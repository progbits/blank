open Core
open Parse

exception EndOfProgramException of string

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
    let first_char = String.nget state.current_stdin 0 in
    let remaining_chars =
      match String.length state.current_stdin with
      | 0 -> ""
      | _ ->
          String.slice state.current_stdin 1 (String.length state.current_stdin)
    in
    (first_char, {state with current_stdin= remaining_chars})
  in
  match String.length state.current_stdin with
  | 0 ->
      let line = In_channel.input_line_exn In_channel.stdin ^ "\n" in
      do_read {state with current_stdin= line}
  | _ -> do_read state

(* Execute a StackManipulation command and return the new state *)
let exec_stack_manipulation command state =
  match command with
  | Push value ->
      Stack.push state.stack value ;
      incriment_ip state
  | Duplicate ->
      let value = Stack.pop_exn state.stack in
      Stack.push state.stack value ;
      Stack.push state.stack value ;
      incriment_ip state
  | Swap ->
      let a = Stack.pop_exn state.stack in
      let b = Stack.pop_exn state.stack in
      Stack.push state.stack a ; Stack.push state.stack b ; incriment_ip state
  | Discard ->
      let _ = Stack.pop_exn state.stack in
      incriment_ip state

(* Execute an Arithmetic command and return the new state *)
let exec_arithmetic command state =
  let right = Stack.pop_exn state.stack in
  let left = Stack.pop_exn state.stack in
  match command with
  | Addtion ->
      Stack.push state.stack (left + right) ;
      incriment_ip state
  | Subtraction ->
      Stack.push state.stack (left - right) ;
      incriment_ip state
  | Multiplication ->
      Stack.push state.stack (left * right) ;
      incriment_ip state
  | Division ->
      Stack.push state.stack (left / right) ;
      incriment_ip state
  | Modulo ->
      Stack.push state.stack (left % right) ;
      incriment_ip state

(* Execute a HeapAccess command and return the new state *)
let exec_heap_access command state =
  match command with
  | Store ->
      let value = Stack.pop_exn state.stack in
      let address = Stack.pop_exn state.stack in
      let _ = Hashtbl.set state.heap ~key:address ~data:value in
      incriment_ip state
  | Retrieve ->
      let address = Stack.pop_exn state.stack in
      let value = Hashtbl.find_exn state.heap address in
      let _ = Stack.push state.stack value in
      incriment_ip state

(* Execute a FlowControl command and return the new state *)
let exec_flow_control command state =
  match command with
  | Mark _ -> incriment_ip state
  | Call label ->
      Stack.push state.call_stack (state.ip + 1) ;
      update_ip state label.target
  | UnconditionalJump label -> update_ip state label.target
  | JumpZero label ->
      let value = Stack.pop_exn state.stack in
      if value = 0 then update_ip state label.target else incriment_ip state
  | JumpNegative label ->
      let value = Stack.pop_exn state.stack in
      if value < 0 then update_ip state label.target else incriment_ip state
  | EndSubroutine ->
      let dest = Stack.pop_exn state.call_stack in
      update_ip state dest
  | EndProgram -> raise (EndOfProgramException "end of program")

(* Execute an IO command and return the new state *)
let exec_io command state =
  match command with
  | OutputCharacter ->
      let value = Stack.pop_exn state.stack in
      printf "%c%!" (Char.unsafe_of_int value) ;
      incriment_ip state
  | OutputNumber ->
      let value = Stack.pop_exn state.stack in
      printf "%d%!" value ; incriment_ip state
  | ReadCharacter ->
      let value, state = read_char state in
      let address = Stack.pop_exn state.stack in
      let _ = Hashtbl.set state.heap ~key:address ~data:(Char.to_int value) in
      incriment_ip state
  | ReadNumber ->
      let _ = Out_channel.flush in
      let line = In_channel.input_line_exn In_channel.stdin in
      let value = int_of_string line in
      let address = Stack.pop_exn state.stack in
      let _ = Hashtbl.set state.heap ~key:address ~data:value in
      incriment_ip state

(* 2 pass label resolution *)
let resolve_labels instructions =
  (* Resolve IP values of each label *)
  let rec resolve instructions n labels =
    try
      match List.hd_exn instructions with
      | FlowControl command -> (
        match command with
        | Mark label ->
            let _ = Hashtbl.add labels ~key:label.name ~data:n in
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
  let instructions = resolve_labels instructions in
  let rec exec state =
    match List.nth_exn instructions state.ip with
    | StackManipulation command -> exec (exec_stack_manipulation command state)
    | Arithmetic command -> exec (exec_arithmetic command state)
    | HeapAccess command -> exec (exec_heap_access command state)
    | FlowControl command -> exec (exec_flow_control command state)
    | IO command -> exec (exec_io command state)
  in
  exec state
