open Scanner
open Parser
(* open Tokens *)
open Printf
open Evaluator
let tokens = tokenize "(= 4 4)"

let _ = match tokens with
    | Ok tokens_list -> let nodes = (parse_expression tokens_list) in
        begin
        match nodes with
            | Ok (ast, _) -> evaluate [ast]
            |_ -> printf "Error Parsing"
        end
    | _ -> printf "Error Tokenizing"


(* let _ = match tokens with
    | Ok tokens ->  List.iter print_token tokens
    | _ -> printf "err" *)
