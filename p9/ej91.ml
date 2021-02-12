open Bin_tree;;
(* in_order, preorder e postorder *)

let rec fold_tree f a = function
    Empty -> a
    | Node (x, l, r) -> f x (fold_tree f a l) (fold_tree f a r)
;;

let sum t = fold_tree (fun x y z -> x+y+z) 0 t;;

let prod t = fold_tree (fun x y z -> x *. y *. z) 1.0 t;;

let rec map_tree f = function
    Empty -> Empty
    | Node (x,l,r) -> Node (f x, map_tree f l, map_tree f r)
;;

let num_nodes t = fold_tree (fun _ l r -> 1 + l + r) 0 t;;

let in_order t = fold_tree (fun x l r -> l @ (x::r)) [] t;;

let mirror t = fold_tree (fun x l r -> Node (x, r, l)) Empty t;;

exception Zero;;

let prod2 t =
    let rec aux = function
        Empty -> 1.0
        | Node (0., _, _) -> raise Zero
        | Node (x, l, r) -> x *. aux l *. aux r
    in
        try aux t with Zero -> 0.
;;

(*let t3 = Node (12., Node(0. , t, Empty), Empty );; (* Para demostrar que funciona o prod2 *)*)
