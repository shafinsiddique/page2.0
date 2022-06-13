open Printf

type expression =
    IntExpression of int
    | StringExpression of string

let print_expression expression = match expression with
    | IntExpression value -> printf "%d\n" value
    | StringExpression value -> printf "%s\n" value
