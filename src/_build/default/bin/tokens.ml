open Printf

type token =
    LeftParen
    | RightParen
    | IntegerNumber of int
    | StringVal of string
    | EmptyToken
    | Plus
    | Minus
    | Divide
    | Multiply
    | GreaterThan
    | LessThan
    | Equal

let get_format_string token_type = printf "TokenType : %s\n" token_type
let get_format_string_with_int token_type value = printf "TokenType %s, Value %d\n" token_type value
let get_format_string_with_string token_type value = printf "TokenType %s, Value %s\n" token_type value

let print_token token = match token with
    | LeftParen -> get_format_string "LeftParen"
    | RightParen -> get_format_string "RightParen"
    | IntegerNumber value -> get_format_string_with_int "IntegerValue" value
    | StringVal value -> get_format_string_with_string "StringValue" value
    | Plus -> get_format_string "+"
    | Divide -> get_format_string "/"
    | Multiply -> get_format_string "*"
    | Minus -> get_format_string "-"
    | _ -> get_format_string "Needs to be implemented"
