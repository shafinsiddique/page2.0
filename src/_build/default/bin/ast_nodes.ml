open Printf

type ast_node = AdditionNode of ast_node list | IntegerNode of int | StringNode of string | EmptyNode

let rec print_ast node = match node with
    | AdditionNode nodes -> let () = printf "Addition Node\n Children : " in (List.iter print_ast nodes)
    | IntegerNode value -> printf "Integer Node %d\n" value
    | StringNode value -> printf "String Node : %s\n" value
    | _ -> printf ""
