let is_prm n =
let rec not_divisible_from d =
d * d > n || (n mod d <> 0 && not_divisible_from (d+1)) in
n > 1 && not_divisible_from 2;;

(*Utilizamos is_prm para goldbach*)
let goldbach n =
    let rec aux i =
      if is_prm i && is_prm (n - i) then (i, n-i)
      else aux (i+1) in
    aux 2;;
