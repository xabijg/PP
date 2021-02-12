(*Ejercicio-1*)

let rec gcd x y = 
      if y = 0 then x
      else gcd y (x mod y);;
(* val gcd : int -> int -> int = <fun> *)

let rec gcd x = function
      0 -> x
      | y ->  gcd y (x mod y);;
(* val gcd : int -> int -> int = <fun> *)
	


(*Ejercicio-2*)


let is_prm n =
	let rec not_divisible_from d =
	d * d > n || (n mod d <> 0 && not_divisible_from (d+1)) in
	n > 1 && not_divisible_from 2;;

(*La función comprueba si un número dado n(mayor que 1) es primo o no,retornando un valor de tipo boolean(true o false).Para ello utiliza una función recursiva que comienza en 2 y va comparando si lo divide algún número y si el cuadrado de el valor recursivo intero de not_divisible_from es mayor que n.(d*d>n->comprobamos los divisores menores) *)

let is_prm2 n =
	let rec not_divisible_from d =
	(n mod d <> 0 && not_divisible_from (d+1)) || d * d > n in
	n > 1 && not_divisible_from 2;;

(*La diferencia entre is_prm y is_prm2 se encuentra en la evaluación de las condiciones para saber si es cierto not_divisible_from.En la primera función cuenta con la ventaja de que al ser un or al evaluar la primera condición se ahorra hacer la operación recursiva en gran mayoría de las ocasiones;por el contrario en la segunda para valores altos nos devuelve un Stack overflow pues  tiene que realizar la recursividad en todas las operaciones*)

(*Ejercicio-3*)
let capicua n =
      let rec aux n1 n2 =
          if n1 = 0 then n2 = n
          else aux (n1 / 10) (10*n2 + n1 mod 10)
      in aux n 0;;

