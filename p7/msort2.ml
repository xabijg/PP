(* Usaremos la funcion init posteriormente para crear listas *)

let init = function long -> function f ->
	if long < 0 then
		raise (Invalid_argument "init")
	else let rec aux (i, l) =
			if i = long then l
			else aux (i + 1, f i::l)
	     in List.rev (aux (0, []));;

(* Comprobaremos el rendimiento en tiempo usando la siguiente funcion: 

		let crono = function f -> function g -> function x ->
			let t = Sys.time()
			in Sys.time() -. t;;

Y la siguiente definicion para crear una lista:

		let l = init 100_000 (function x -> Random.int 100_000);;  
*)

open List;;

let rec divide l = match l with
		h1::h2::t -> let t1, t2 = divide t in (h1::t1, h2::t2)
	| _ -> l, [];;

let merge ord l=
	let rec aux l1=function
		[],l|l,[] -> List.rev_append l1 l
		| h1::t1,h2::t2 ->if ord h1 h2 then
		aux (h1::l1)(t1,h2::t2)
						else aux(h2::l1)(h1::t1,t2)
	in aux [] l;;

let rec msort1 = function ord -> function l -> match l with
	[] | _::[] -> l
	| _ -> let l1, l2 = divide l 
in merge ord (msort1 ord l1,msort1 ord l2);; 

(* Puede provocar algun problema la no terminalidad de divide o merge? En caso afirmativo, defina un valor l2 : int list que sea un ejemplo de ello. En caso negativo, defina l2 = [].
	Al igual que con la qsort1, tendriamos que definir un l1 de por lo menos 320000 elementos aproximadamente para que se produzca stack overflow y no
	puedan ordenar. *)

let l2 = init 320_000 (function x -> Random.int 100_000);;
	
(* Defina  de  modo  recursivo  terminal  funciones divide' y merge' que cumplan el mismo cometido que divide y merge, respectivamente. *)

let divide' = function l -> 
	let rec aux = function acu1 -> function acu2 -> function
		[] -> (acu1, acu2)
  		| h::[] -> (h::acu1, acu2)
  		| h1::h2::t -> aux (h1::acu1) (h2::acu2) t
  	in aux [] [] l;;


let merge' g (l1,l2)=
  	let rec aux acc = function
      [],l | l,[] -> List.rev_append acc l
    | h1::t1, h2::t2 -> if g h1 h2 then aux(h1::acc) (t1,h2::t2)
    					else aux (h2::acc) (h1::t1,t2)
  	in aux [] (l1,l2);;
(* Realice una implementacion, msort2, de la ordenacion por fusion utilizando divide' y merge'. Compare el rendimiento en tiempo de ejecucion de msort2 con el de msort1 y con el de qsort3. *)

let rec msort2 = function ord -> function l -> match l with
	[] | _::[] -> l
	| _ -> let (l1, l2 )= divide' l 
	in merge' ord (msort2 ord l1,msort2 ord l2);;

(* Obtenemos, para una lista de 100000 enteros aleatorios de prueba, un tiempo para msort2 de aproximadamente 0.00000599 s, un tiempo para msort1 de aproximadamente 0.000006 s y un
tiempo de aproximadamente 0.00000399 s para el qsort3, por lo que concluimos que msort2 y msort1 son practicamente igual de rapidos y es mas rapido que qsort3 con un porcentaje de penalizacion de aproximadamente un 1.5 % 
Como podemos comprobar, depende tambien mucho del equipo que estemos usando, pues en el que se han realizado estas prácticas realiza las operaciones tan rapido que apenas notaremos las diferencias con listas de estos tamaños.*)

