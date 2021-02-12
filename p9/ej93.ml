open G_tree;;


let rec breadth_first_t gt =
    let rec aux res level next =
        match level, next with
        | [], [] -> List.rev res
        | [], _ -> aux res (List.rev next) []
        | (Gt (x, l))::t, _ -> aux (x::res) t (List.rev_append l next)
    in
        aux [] [gt] []
;;

let t =
    Gt (0, List.init 300000 (function x -> Gt (x+1, [])))
;;
