let hd = function
	h::_ -> h 
	| [] -> raise(Failure "hd");;

let tl = function
	_::tl -> tl
	| [] -> raise(Failure "tl");;

let length = function l ->
	let rec aux = function long -> function
		[] -> long
		| h::t -> aux (1 + long) t
	in aux 0 l;;
	
let rec compare_lengths = function l1 -> function l2 -> match (l1, l2) with
	([], []) -> 0
	| ([], _) -> -1
	| (_, []) -> 1
	| (_::t1, _::t2) -> compare_lengths t1 t2;;


let nth = function l -> function n ->
	if n < 0 then
		raise (Invalid_argument "nth")
	else let rec aux = function
		([], _) -> raise (Failure "nth")
		| (h::t, n) -> if n = 0 then h else aux (t, n-1)
	in aux (l, n);;

let rec append l1 l2 = match l1 with
	[] -> l2
	| h::t -> h::(append t l2);; 

let init = function long -> function f ->
	if long < 0 then
		raise (Invalid_argument "init")
	else let rec aux (i, l) =
			if i = long then l
			else aux (i + 1, f i::l)
	     in List.rev (aux (0, []));;

let rev = function l ->
	let rec aux = function n -> function
		[] -> n
		| h::t -> aux (h::n) t in aux [] l;;

let rec rev_append = function l1 -> function l2 -> match l1 with
	[] -> l2
	| h::t -> rev_append t (h::l2);;

let rec concat = function
	[] -> []
	| h::t -> append h (concat t);;

let flatten = concat;;

let rec map f = function
	[] -> []
	| h::t -> (f h) :: map f t;;


let rev_map = function f -> function l ->
	let rec aux = function acu -> function
		[] -> acu
		| a::l -> aux (f a ::acu) l
	in aux [] l;;		

let rec map2 f l1 l2 = match (l1, l2) with
	([], []) -> []
	| (h1::t1, h2::t2) -> let r = f h1 h2 in r::map2 f t1 t2
	| (_, _) -> raise (Invalid_argument "map2");; 

let rec fold_left = function f -> function n -> function
	[] -> n
	| h::t -> let r = f n h in fold_left f r t;;

let rec fold_right f l b = match l with
	[] -> b
	| a::t -> f a (fold_right f t b);;


let rec find = function p -> function
	[] -> raise Not_found
	| h::t -> if p h = true then h else find p t;;

let rec for_all = function p -> function
	[] -> true
	| h::t -> p h && for_all p t;;

let exists = function p -> function l -> 
	let rec aux = function r -> function
		[] -> false
		| h::[] -> (p h) || r
		| h::t -> aux (p h || r) t
	in aux false l;;

let rec mem = function a -> function
	[] -> false
	| h::t -> h = a || mem a t;;


let filter = function f -> function l ->
	let rec aux = function acu -> function
		[] -> rev acu
		| h::t -> if f h = true then aux (h::acu) t
			  else aux acu t
	in aux [] l;;

let find_all = filter;;


let partition = function f -> function l ->
	let rec aux = function (acu_true, acu_false) -> function
		[] -> (rev acu_true, rev acu_false)
		| h::t -> if f h = true then aux (h::acu_true, acu_false) t
			  else aux (acu_true, h::acu_false) t
	in aux ([], []) l;;

let rec split = function
	[] -> ([], [])
	| (a, b)::l -> let (sa, sb) = split l in (a::sa, b::sb);;

let combine = function l1 -> function l2 -> 
	let rec aux = function a -> function b -> function acu -> match (a, b) with
		[], [] -> rev acu
    		| h1::t1, h2::t2 -> aux t1 t2 ((h1, h2)::acu)
    		| _ -> raise(Invalid_argument "mycombine")
	in aux l1 l2 [];;











	






	 
	

