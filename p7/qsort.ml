open List;;

let rec qsort1 = function ord -> function
	[] -> []
	| h::t -> let after, before = partition (ord h) t 
		  in qsort1 ord before @ h :: qsort1 ord after;;

let rec qsort2 = function ord -> function
	[] -> []
	| h::t -> let after, before = partition (ord h) t 
		  in rev_append (rev (qsort2 ord before)) (h :: qsort2 ord after);;


(* Usaremos la funcion init posteriormente para crear listas *)

let init = function long -> function f ->
	if long < 0 then
		raise (Invalid_argument "init")
	else let rec aux (i, l) =
			if i = long then l
			else aux (i + 1, f i::l)
	     in List.rev (aux (0, []));;


(* 
Casos en los que no es bueno el rendimiento de esta implementacion(qsort1): 
	Al no ser terminal, no será bueno el rendimiento en listas que superen determinado tamaño, pues se van
	acumulando muchas operaciones pendientes en el stack e incluso a partir de un tamaño en concreto se 
	producira overflow.
	Tampoco sera bueno en el caso de que la lista a ordenar este al reves de como queremos que este ordenada,
	puesto que solo va ordenando un elemento en cada iteracion del bucle.

Tiene alguna ventaja qsort2 sobre qsort1? 
	La principal ventaja es que es terminal y no ira dejando operaciones pendientes.

Permite qsort2 ordenar listas que no podrian ordenarse con qsort1? En caso afirmativo, definir un l1 de ejemplo, sino l1 = []
	Si, cuando se trata de listas grandes para que se produzca stack overflow en la qsort1.
	Tendriamos que definir un l1 de al menos 320000 elementos aproximadamente para que se produzca stack overflow y no
	pueda ordenarla.

¿Tiene qsort2 alguna desventaja sobre qsort1?
	La desventaja que tiene es que debe trabajar mas en la ultima linea por hacer "rev" y "rev_append".

	Comprobar si qsort2 es mas lento que qsort1. En caso afirmativo, explicar por que y estimar la penalización en porcentaje de tiempo usado.
	Función para calcular el tiempo: 

		let crono = function f -> function g -> function x ->
			let t = Sys.time()
			in Sys.time() -. t;;
	
	Y la siguiente definicion para crear una lista:

		let l = init 100_000 (function x -> Random.int 100_000);;  

	Obtenemos, para una lista de 100000 enteros aleatorios de prueba, un tiempo para qsort1 de aproximadamente 0.00000599 s y un
	tiempo para qsort2 de aproximadamente 0.000004 s, por lo que qsort1 sería el mas lento con una penalizacion aproximadamente del 1.4975 %.
	Evidentemente,las mediciones dependen tambien mucho de la maquina que estemos usando, pues en el que se han realizado estos analisis realiza las 
	operaciones tan rapido que apenas notaremos las diferencias con listas de estos tamaños.
*)

let l1 = init 320_000 (function x -> Random.int 100_000);;



