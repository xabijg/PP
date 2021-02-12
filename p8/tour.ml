let in_board n m (x,y)=
    x >= 1 && x <= m && y >= 1 && y <= n;;

let not_mem l e =
    not (List.mem e l);;

let legal_moves (x,y) m n visited =
    let moves = [(x-2, y+1);(x-1, y+2);(x+1, y+2);(x+2, y+1);
                 (x+2, y-1);(x+1, y-2);(x-1, y-2);(x-2, y-1)] in
                    List.filter (not_mem visited) (List.filter (in_board m n) moves);;

let tour m n start finish =
    if not ( m >= 1 && n >= 1 && in_board n m (start) && in_board n m (finish) )
    then raise (Invalid_argument "tour")
    else
    let rec aux solution = function
        [] -> raise Not_found
        | h::t -> if (h = finish) then List.rev (h::solution) else
                  try
                    aux (h::solution) (legal_moves h m n (h::solution))
                  with
                    Not_found -> aux solution t

    in
        if start = finish then [start]
        else
            aux [start] (legal_moves start m n [start]);;

let rec chained = function
    [] | [_] -> true
    | (x1,y1)::((x2,y2)::t as l) ->
        let dx = abs (x1-x2) and dy = abs (y1-y2) in
            ( dx = 1 && dy = 2 || dx = 2 && dy = 1 ) &&
            not (List.mem (x1,y1) l) &&
            chained l;;
