open Scanner
(* open Parser *)
open Tokens
open Printf
(* open Ast_nodes *)
let tokens = tokenize "(/ 2 3)"

(* let _ = match tokens with
    | Ok tokens_list -> let nodes = (parse_expression tokens_list) in
        begin
        match nodes with
            | Ok (ast, _) -> print_ast ast
            |_ -> printf "Error Parsing"
        end
    | _ -> printf "Error Tokenizing" *)


let _ = match tokens with
    | Ok tokens ->  List.iter print_token tokens
    | _ -> printf "err"
