let in_board n m (x,y)=
    x >= 1 && x <= m && y >= 1 && y <= n;;

let not_mem l e =
    not (List.mem e l);;

let legal_moves (x,y) m n visited =
    let moves = [(x-2, y+1);(x-1, y+2);(x+1, y+2);(x+2, y+1);
                 (x+2, y-1);(x+1, y-2);(x-1, y-2);(x-2, y-1)] in
                    List.filter (not_mem visited) (List.filter (in_board m n) moves);;

let knight_tour m n start =
    if not ( m >= 1 && n >= 1 && in_board n m (start) )
    then raise (Invalid_argument "knight_tour")
    else
    let rec aux i solution = function
        [] -> raise Not_found
        | h::t -> if i+1 = m*n then List.rev (h::solution) else
                  try
                    aux (i+1) (h::solution) (legal_moves h m n (h::solution))
                  with
                    Not_found -> aux i solution t

    in
        if (m=1 && n=1) then [start]
        else
            aux 1 [start] (legal_moves start m n [start]);;

let rec chained = function
    [] | [_] -> true
    | (x1,y1)::((x2,y2)::t as l) ->
        let dx = abs (x1-x2) and dy = abs (y1-y2) in
            ( dx = 1 && dy = 2 || dx = 2 && dy = 1 ) &&
            not (List.mem (x1,y1) l) &&
            chained l;;
