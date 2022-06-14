open Ast_nodes
open Expression

let get_operation_result expression_name value1 value2 = match expression_name with
    | AdditionNode _ -> Ok (IntExpression (value1 + value2))
    | SubtractionNode _ -> Ok (IntExpression (value1 - value2))
    | MultiplicationNode _ -> Ok (IntExpression (value1 * value2))
    | DivisionNode _ -> Ok (IntExpression (value1 / value2))
    | _ -> Error "some other error"

let operate_on expression_name expr1 expr2 = match expr1 with
    | IntExpression value1 -> (match expr2 with
        | IntExpression value2 -> get_operation_result expression_name value1 value2
        | _ -> Error "Not an Integer Expression. Cannot be mathematically evaluated.")
    | _ -> Error "Not an Integer Expression. Cannot be mathematically evaluated."


let rec _operate_sub_exprs expression_name sub_expressions current = match sub_expressions with
    | [] -> Ok current
    | hd::ls -> let result = (operate_on expression_name current hd) in (match result with
                                                        | Ok expr -> _operate_sub_exprs expression_name ls expr
                                                        | Error _ -> result)

let operate_sub_exprs expression_name sub_expressions = match sub_expressions with
    | [] -> Error "Need 2 or more subexpressions for mathematical operatons."
    | hd::ls -> match ls with
        | [] -> Error "Need 2 or more subexpressions for mathematical operation."
        | _::_ -> _operate_sub_exprs expression_name ls hd


let rec evaluate_sub_expressions sub_expressions results = match sub_expressions with
    | [] -> Ok results
    | hd::ls -> let evaluated = (evaluate_expr hd) in (match evaluated with
                                            | Ok expression -> evaluate_sub_expressions ls (expression::results)
                                            | _ -> Error "Error parsing expression.")

and evaluate_math_expr expr = let sub_expressions = (match expr with
    | AdditionNode nodes -> nodes
    | SubtractionNode nodes -> nodes
    | MultiplicationNode nodes -> nodes
    | DivisionNode nodes -> nodes
    | _ -> [])
    in
    let sub_evaluation = (evaluate_sub_expressions sub_expressions []) in
        match sub_evaluation with
            | Ok expressions -> operate_sub_exprs expr expressions
            | _ -> Error "Error parsing expression"


and evaluate_expr expr = match expr with
    | IntegerNode value -> Ok (IntExpression value)
    | StringNode value -> Ok (StringExpression value)
    | AdditionNode _ -> evaluate_math_expr expr
    | SubtractionNode _ -> evaluate_math_expr expr
    | MultiplicationNode _ -> evaluate_math_expr expr
    | DivisionNode _ -> evaluate_math_expr expr
    | _ -> Error "need to implement evaluator."


let rec evaluate ast = match ast with
    | [] -> ()
    | hd::ls -> let evaluated_expression = (evaluate_expr hd) in (match evaluated_expression with
                                        | Ok expression -> let () = (print_expression expression) in (evaluate ls)
                                        | Error msg -> Printf.printf "Error : %s" msg)
