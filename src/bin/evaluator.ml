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

let compare_sub_expressions comp_type expr1 expr2 = match expr1 with
    | IntExpression value1 -> (match expr2 with
        | IntExpression value2 -> (match comp_type with
            | '>' -> Ok (BooleanExpression (value1 > value2))
            | '<' -> Ok (BooleanExpression (value1 < value2))
            | '=' -> Ok (BooleanExpression (value1 == value2))
            | _ -> Error "Some other character")
        | _ -> Error "Need to be an int expression.")
    | _ -> Error "Need to be an Int Expression."

let compare_expressions comp_type sub_expressions = let first_element = (List.hd sub_expressions) in
    let second_element = (List.hd (List.tl sub_expressions)) in
        compare_sub_expressions comp_type first_element second_element

let length_equals lst expected = (List.length lst) == expected

let rec evaluate_sub_expressions sub_expressions results = match sub_expressions with
    | [] -> Ok results
    | hd::ls -> let evaluated = (evaluate_expr hd) in (match evaluated with
                                            | Ok expression -> evaluate_sub_expressions ls (expression::results)
                                            | _ -> Error "Error parsing expression.")

and evaluate_comparison_expr comp_type sub_nodes = if (List.length sub_nodes <> 2)
                                            then Error "Comparison Expressions require two subexpressions."
                                            else let sub_evaluation = (evaluate_sub_expressions sub_nodes []) in
                                                match sub_evaluation with
                                                    | Ok expressions -> (compare_expressions comp_type expressions)
                                                    | _ -> Error "Error parsing expression."

and evaluate_list_expr sub_expressions = let sub_evaluation = (evaluate_sub_expressions sub_expressions []) in
    match sub_evaluation with
        | Ok expressions -> Ok (ListExpression expressions)
        | _ -> Error "Error parsing list expression."

and evaluate_car_expr sub_expressions = let sub_evaluation = (evaluate_sub_expressions sub_expressions []) in
    match sub_evaluation with
        | Ok expressions -> (if ((List.length expressions) <> 1)
                            then Error "Invalid Cons Expression."
                            else let list_child = (List.hd expressions) in
                                match list_child with
                                    | ListExpression lst -> Ok (CarExpression (List.hd lst))
                                    | _ -> Error "car expects a list.")
        | _ -> Error "Error parsing list expression."



and evaluate_cdr_expr sub_expressions = let sub_evaluation = (evaluate_sub_expressions sub_expressions []) in
    match sub_evaluation with
        | Ok expressions -> (if ((List.length expressions) <> 1)
                            then Error "Invalid Cdr Expression."
                            else let list_child = (List.hd expressions) in
                                match list_child with
                                    | ListExpression lst -> Ok (CdrExpression (ListExpression (List.tl lst)))
                                    | _ -> Error "cdr expects a list.")
        | _ -> Error "Error parsing list expression."

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

and evaluate_length_expr sub_expressions = let sub_evaluation = (evaluate_sub_expressions sub_expressions []) in
    match sub_evaluation with
        | Ok expressions -> (if (length_equals expressions 1)
                            then (let expr = (List.hd expressions) in match expr with
                                | ListExpression lst -> Ok (IntExpression (List.length lst))
                                | StringExpression str -> Ok (IntExpression (String.length str))
                                | _ -> Error "Length cannot be called on non-string or non-list objects.")
                            else (Error "Error parsing length expression"))
        | _ -> Error "Error parsing length subexpressions"

and evaluate_expr expr = match expr with
    | IntegerNode value -> Ok (IntExpression value)
    | StringNode value -> Ok (StringExpression value)
    | AdditionNode _ -> evaluate_math_expr expr
    | SubtractionNode _ -> evaluate_math_expr expr
    | MultiplicationNode _ -> evaluate_math_expr expr
    | DivisionNode _ -> evaluate_math_expr expr
    | ListNode values -> evaluate_list_expr values
    | CarNode values -> evaluate_car_expr values
    | CdrNode values -> evaluate_cdr_expr values
    | ComparisonNode (comp_type, nodes) -> evaluate_comparison_expr comp_type nodes
    | LengthNode values -> evaluate_length_expr values
    | _ -> Error "need to implement evaluator."


let rec evaluate ast = match ast with
    | [] -> ()
    | hd::ls -> let evaluated_expression = (evaluate_expr hd) in (match evaluated_expression with
                                        | Ok expression -> let () =
                                                (print_expression expression;
                                                Printf.printf "\n") in (evaluate ls)
                                        | Error msg -> Printf.printf "Error : %s" msg)
