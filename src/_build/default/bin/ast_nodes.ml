open Printf

type ast_node = AdditionNode of ast_node list
        | IntegerNode of int
        | StringNode of string
        | SubtractionNode of ast_node list
        | MultiplicationNode of ast_node list
        | DivisionNode of ast_node list
        | EmptyNode
        | ComparisonNode of char * ast_node list
        | ListNode of ast_node list

let rec print_compound_node node_name sub_nodes = let () = (printf "%s : " node_name) in (List.iter print_ast sub_nodes)

and print_ast node = match node with
    | AdditionNode nodes -> print_compound_node "Addition" nodes
    | IntegerNode value -> printf "Integer Node %d\n" value
    | StringNode value -> printf "String Node : %s\n" value
    | SubtractionNode nodes -> print_compound_node "Subtraction" nodes
    | DivisionNode nodes -> print_compound_node "Division" nodes
    | MultiplicationNode nodes -> print_compound_node "Multiplication" nodes
    | ComparisonNode (node_type, nodes) -> print_compound_node (sprintf "Comparison Node %c" node_type) nodes
    | _ -> printf "OtherNode"
