open Core
open Lib

(* Get the input filename *)
let get_filename =
  try Some (Array.get (Sys.get_argv ()) 1) with Invalid_argument _ -> None

let () =
  match get_filename with
  | Some filename ->
      let tokens = Parse.scan (In_channel.read_all filename) in
      let instructions = Parse.parse tokens in
      let _ =
        match Vm.run instructions with
        | exception Vm.EndOfProgramException _ -> exit 0
        | _ -> exit 1
      in
      ()
  | None -> print_endline "usage: blank FILENAME"
