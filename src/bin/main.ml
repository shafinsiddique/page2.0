open Scanner
open Parser
(* open Tokens *)
open Printf
open Evaluator

let () = printf "Welcome to Page 2.0: A Lisp Interpreter written in OCaml.\nVersion : 1.0\nBuilt By: Shafin\n\n"

let rec repl_loop ()=
    let _ = printf "> " in
    let input_str = read_line () in
    let tokens = tokenize input_str in
    let _ = match tokens with
        | Ok tokens_list -> let nodes = (parse_expression tokens_list) in
            (match nodes with
                | Ok (ast, _) -> evaluate [ast]; printf "\n"; repl_loop ()
                | Error e -> printf "%s\n" e)
        | _ -> printf "Error Tokenizing." in ()

let () = repl_loop ()
