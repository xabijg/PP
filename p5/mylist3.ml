let rec remove a l = match l with 
    [] -> raise (Failure "No no hay elementos para borrar")
    | h::t -> if (a=h) then t
    else h::(remove a t);;
    
let remove_all a l =  List.filter (function x-> x<>a) l ;;

let ldif l1 l2= List.fold_left (fun x y -> remove_all y x) l1 l2;;

let lprod l1 l2 = List.concat (List.map (fun a -> List.map (fun b -> a,b) l2) l1);;

let divide l =
    let rec aux acc acc2 = function
    [h] -> (List.rev (h::acc), List.rev acc2)
    | [] -> (List.rev acc, List.rev acc2)
    | h::t -> aux (h::acc) (List.hd t::acc2) (List.tl t)
    in aux [] [] l;;



