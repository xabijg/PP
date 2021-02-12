
let rec suml = function
	[] -> 0
	| h::[] -> h
	| h::h1::t -> suml ((h+h1)::t);; 

let rec maxl = function
	[] -> raise (Failure "maxl")
	| h::[] -> h
	| h::h1::t -> maxl ((max h h1)::t);; 

let to0from = function n ->
	if n < 0 then []
	else let rec aux = function l -> function acu ->
		if acu = n + 1 then l
		else aux (acu::l) (acu + 1)
	     in aux [] 0;;

let fromto = function m -> function n ->
	if m > n then []
	else let rec aux = function l -> function acu ->
		if acu = n + 1 then List.rev l
		else aux (acu::l) (acu + 1)
	     in aux [] m;;

let from1to = function n ->
	if n < 1 then []
	else let rec aux = function l -> function acu ->
		if acu = n + 1 then List.rev l
		else aux (acu::l) (acu + 1)
	     in aux [] 1;;

let append = function l1 -> function l2 ->
    	let rec aux = function acu -> function
        	[] -> acu
        	| h::t -> aux (h::acu) t
    	in aux l2 (List.rev l1);;

let map = function f -> function l ->
	let rec aux = function acu -> function
    		[] -> acu
    		| h::t -> aux (acu @ [f h]) t
  	in aux [] l;;


let power = function x -> function y ->
	if y < 0 then invalid_arg "power"
	else if y = 0 then 1
	     else let rec aux = function a -> function b ->
			if b = 1 then a
			else aux (a*x) (b - 1)
		  in aux x y;;

let incseg = function l ->
	let rec aux = function acu -> function
		[] -> acu
		| h::[] -> acu @ [h]
		| h::h1::t -> aux (acu @ [h]) ((h+h1)::t)
	in aux [] l;;


let remove = function x -> function l ->
	let rec aux = function acu -> function
		[] -> acu
		| h::h1::t -> if h = x then acu @ (h1::t)
			    else aux (acu @ [h]) (h1::t)
		| h::[] -> if h = x then acu else l
	in aux [] l;;

let insert = function x -> function l ->
	let rec aux = function acu -> function
		[] -> [x]
		| h::h1::t -> if x <= h then acu @ (x::h::h1::t)
			      else aux (acu @ [h]) (h1::t)
		| h::[] -> if x <= h then acu @ (x::h::[])
			   else acu @ (h::[x])
	in aux [] l;;

let insert_gen = function f -> function x -> function l ->
	let rec aux = function acu -> function
		[] -> [x]
		| h::h1::t -> if f x h then acu @ (x::h::h1::t)
			      else aux (acu @ [h]) (h1::t)
		| h::[] -> if f x h then acu @ (x::h::[])
			   else acu @ (h::[x])
	in aux [] l;; 
