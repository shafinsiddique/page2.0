open Printf

type expression =
    IntExpression of int
    | StringExpression of string
    | BooleanExpression of bool
    | ListExpression of expression list
    | CarExpression of expression
    | CdrExpression of expression

let rec print_expression expression = match expression with
    | IntExpression value -> printf "%d " value
    | StringExpression value -> printf "%s " value
    | BooleanExpression value -> printf "%b " value
    | ListExpression values -> (printf "( list ";
                            List.iter print_expression values;
                            printf ")")
    | CarExpression value -> print_expression value
    | CdrExpression value -> print_expression value
