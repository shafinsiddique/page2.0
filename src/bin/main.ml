open Scanner
open Tokens
open Printf

let tokens = tokenize "(123)"

let () = match tokens with
    | Ok tokens_list -> List.iter print_token tokens_list
    | Error str -> printf "%s\n" str
