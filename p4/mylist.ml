let hd = function
	h ::_ -> h 
	| [] -> raise(Failure "hd");;

let tl = function
	_:: tl -> tl
	| [] -> raise(Failure "tl");;

let rec length = function
	_:: t -> 1 + length t
	| [] -> 0;;
	
let rec compare_lengths l1 l2 = match (l1, l2) with
	([], []) -> 0
	| ([], _) -> -1
	| (_, []) -> 1
	| (_::t1, _::t2) -> compare_lengths t1 t2;;


let rec nth l n =
	if n < 0 then
		raise (Invalid_argument "nth")
	else let rec aux l n = match l with
		[] -> raise (Failure "nth")
		| h::t -> if n = 0 then h else aux t (n - 1)
		in aux l n;;

let rec append l1 l2 = match l1 with
	[] -> l2
	| h::t -> h::(append t l2);; 

let init len f =
	let rec aux (i, l) =
		if i = len then l
		else aux (i + 1, f i::l)
	in List.rev (aux (0, []));;

let rev l = let rec aux n = function
	[] -> n
	| h::t -> aux (h::n) t in aux [] l;;

let rec rev_append l1 l2 = match l1 with
	[] -> l2
	| h::t -> rev_append t (h::l2);;

let rec concat = function
	[] -> []
	| h::t -> append h (concat t);;

let flatten = concat;;

let rec map f = function
	[] -> []
	| h::t -> (f h) :: map f t;;

let rec rev_map f l =
	let rec aux acu = function
		| [] -> acu
		| a::l -> aux (f a ::acu) l
	in aux [] l;;		

let rec map2 f l1 l2 = match (l1, l2) with
	([], []) -> []
	| (h1::t1, h2::t2) -> let r = f h1 h2 in r::map2 f t1 t2
	| (_, _) -> raise (Invalid_argument "map2");; 

let rec fold_left f n = function
	[] -> n
	| h::t -> let r = f n h in fold_left f r t;;

let rec fold_right f l b = match l with
	[] -> b
	| a::t -> f a (fold_right f t b);;

let rec find p = function
	[] -> raise Not_found
	| h::t -> if p h = true then h else find p t;;

let rec for_all p = function
	[] -> true
	| h::t -> p h && for_all p t;;

let rec exists p = function
	[] -> false
	| h::t -> p h || exists p t;;

let rec mem a = function
	[] -> false
	| h::t -> h = a || mem a t;;

let rec filter f l =
	let rec filter_aux acum = function
		[] -> rev acum
		| h::t -> if f h = true then filter_aux (h::acum) t
			else filter_aux acum t
	in filter_aux [] l;;

let find_all = filter;;

let rec partition f l =
	let rec partition_aux (acum_true, acum_false) = function
		[] -> (rev acum_true, rev acum_false)
		| h::t -> if f h = true then partition_aux (h::acum_true,acum_false) t
			  else partition_aux (acum_true, h::acum_false) t
	in partition_aux ([], []) l;;

let rec split = function
	[] -> ([], [])
	| (a, b)::l -> let (sa, sb) = split l in (a::sa, b::sb);;

let rec combine l1 l2 = match (l1, l2) with
	([], []) -> []
	| (h1::t1, h2::t2) -> (h1, h2)::combine t1 t2
	| (_, _) -> raise (Invalid_argument "combine");;





	






	 
	

