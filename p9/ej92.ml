open Bin_tree;;
let rec insert_tree f e = function
    Empty -> Node (e, Empty, Empty)
    | Node (x, l, r) ->
        if f e x then Node (x, insert_tree f e l, r)
        else Node (x, l, insert_tree f e r)
;;

let sort f l = in_order (List.fold_left (fun a x -> insert_tree f x a) Empty l);;
