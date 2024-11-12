exception not_found

type grid = int list list (* Grid is represented as a matrix. *)
type 'a tree = Empty | Node of ('a tree) list * 'a
type hints = (int list list) * (int list list) (* horizontal and vertical *)

let first tuple = 
    match tuple with
    | (x, y) -> x

let second tuple = 
    match tuple with
    | (x, y) -> y

let list_sum l = 
    List.fold_left (+) 0 l

(* depth is 0-indexed. *)
let rec search (input: hints) (current: grid tree) (depth: int) = 
    match current with
    | Empty -> raise not_found
    | Node(tree_list, node) ->
            let vertical_hint = List.nth (second hints) depth in
            let horizontal_hint = first hints
            let hint_size = List.length (List.nth (second hints) depth) in
            let hint_sum = list_sum 

let main () = 
    let test_inputs: hints list = 
        [([[3];[1];[3];[2];[4]], [[1];[1;1];[3;1];[3];[3]])] in
    Printf.printf "Hello, world!\n"

let () = main ()
