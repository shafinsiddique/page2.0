(*

We need to write a recursive descent parser for Lisp.

parse :: if token is Integer, return IntegerNOde
if token is String, return StringNode.

( + 2 3)

parse : Return a List of Nodes or an error.


parseExpression :: if token is Integer, return Integer.
If token is String, return Stringnode.

If token is left paren:

advance.
    send to another function.
    The function will


*)

open Ast_nodes
open Tokens

let create_expr_node name sub_expressions = match name with
    | Plus -> AdditionNode sub_expressions
    | Minus -> SubtractionNode sub_expressions
    | Divide -> DivisionNode sub_expressions
    | Multiply -> MultiplicationNode sub_expressions
    | _ -> EmptyNode


let rec parse_expression tokens = match tokens with
    | [] -> Ok (EmptyNode, [])
    | hd::ls -> match hd with
            | IntegerNumber value -> Ok ((IntegerNode value), ls)
            | StringVal value -> Ok ((StringNode value), ls)
            | LeftParen -> parse_compound_expression ls
            | _ -> Error "Unexpected Token."

and parse_compound_expression tokens = match tokens with
    | [] -> Error "No closing parenthesis."
    | hd::ls -> match hd with
        | Plus -> parse_list_expr hd ls []
        | Minus -> parse_list_expr hd ls []
        | Multiply -> parse_list_expr hd ls []
        | Divide -> parse_list_expr hd ls []
        | _ -> Error "Unexpected Token"

and parse_list_expr name tokens sub_expressions = match tokens with
    | [] -> Error "No closing parenthesis."
    | hd::ls -> match hd with
        | RightParen -> Ok ((create_expr_node name sub_expressions), ls)
        | _ -> let sub_expr = parse_expression (hd::ls) in
            match sub_expr with
                | Ok (expr, rest) -> parse_list_expr name rest (expr::sub_expressions)
                | _ -> Error "Unable to parse into expression"
