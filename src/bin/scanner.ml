(*
Let's think about the algorithm. We're building a tokenizer.

It will tokenize into Tokens.

So if it sees a Left Paren,

*)
open Tokens

let is_digit chr = match chr with
    | '0' .. '9' -> true
    | _ -> false

let rec scan_integer input index nums =
                    if (index < String.length input && (is_digit input.[index]))
                    then scan_integer input (index+1) (input.[index] :: nums)
                    else nums


let rec pow a b = if (b == 0) then 1 else (a*(pow a (b-1)))

let digit_to_int chr = match chr with
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | _ -> 0

let rec conv_int_list_to_int nums power = match nums with
    | [] -> 0
    | hd::ls -> ((digit_to_int hd)*(pow 10 power)) + (conv_int_list_to_int ls (power+1))

let try_scan_integer input index =
            if (is_digit input.[index])
            then let digits = scan_integer input index [] in
                    let num_of_digits = List.length digits in
                        Ok (IntegerNumber (conv_int_list_to_int digits 0), num_of_digits)
            else Error "Not an integer"


let rec scan_str input index characters =
                        if (index < String.length input && input.[index] <> '\"')
                        then scan_str input (index+1) (input.[index] :: characters)
                        else if (input.[index] == '\"')
                        then Ok (List.rev characters)
                        else Error "Invalid string token, no closing quotations found"



let try_scan_string input index =
            if (input.[index] == '\"')
            then let value = scan_str input (index+1) [] in
                match value with
                    | Ok value -> Ok ((StringVal (String.of_seq (List.to_seq value))), (List.length value)+2)
                    | Error e -> Error e
            else Error "Not a string"


let rec any_of options input index = match options with
            | [] -> Error "Unable to match to a token."
            | hd::ls -> let res = (hd input index) in
                            match res with
                                | Ok value -> Ok value
                                | _ -> any_of ls input index

let try_tokenize_single_char input index = match (input.[index]) with
    | '(' -> Ok (LeftParen, 1)
    | ')' -> Ok (RightParen, 1)
    | '+' -> Ok (Plus, 1)
    | ' ' -> Ok (EmptyToken, 1)
    | _ -> Error "Could not match to a single character."

let rec _tokenize input index tokens =
    if (index < (String.length input))
    then let result = any_of [try_tokenize_single_char; try_scan_integer; try_scan_string] input index in
        match result with
            | Ok (token, chars_consumed) -> _tokenize input (index + chars_consumed) (token :: tokens)
            | _ -> Error "Unable to scan token."
    else Ok tokens

let remove_empty_tokens tokens =
            let rec _remove_empty_tokens tokens new_list = match tokens with
                | [] -> new_list
                | hd::ls -> match hd with
                    | EmptyToken -> _remove_empty_tokens ls new_list
                    | _ -> _remove_empty_tokens ls (hd::new_list)
            in _remove_empty_tokens tokens []

let tokenize input = match (_tokenize input 0 []) with
    | Ok tokens -> Ok (remove_empty_tokens tokens)
    | Error err -> Error err
