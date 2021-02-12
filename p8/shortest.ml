
(*Miramos si la posición es válido*)    
    
let esValido fil col (x,y)=
  x >= 1 && x <= fil && y >= 1 && y <= col;; 


(*Saca falso si el elemento está en la lista de movimientos*)

let not_mem l e =
  not(List.mem e l);; 


let rec compare_lengths list1 list2 = match (list1,list2) with
	[],[] -> 0
	| _,[] -> 1
	| [], _ -> -1		
	| _::t1, _::t2 -> compare_lengths t1 t2;;  


(*Chequea los movimientos que se pueden hacer*)

let gen_mov fil col(x,y) registro = 
  let mov = [(x+1, y+2);(x+1, y-2);(x-1, y+2);(x-1, y-2);(x+2, y+1);(x+2, y-1);(x-2, y+1);(x-2, y-1)]     
  in List.filter (not_mem registro)	(List.filter (esValido fil col) mov);; (*Primero mira en el segundo filter las posiciones
                                                                           a las que te puedes mover y en el primer filter mira si ha se han hecho antes*)       (*Comprueba que no visita la misma posicion 2 veces*)
                                                                                 

let tour fil col inicio fin =
  if not (esValido fil col (inicio) && esValido fil col (fin))
  then raise (Invalid_argument "Posicion no valida")             
  else
    let rec aux camino = function 
        [] -> raise Not_found 
      |h::t -> if(h = fin) then List.rev(h::camino) 
          else
            try 
              aux(h::camino) (gen_mov fil col h (h::camino))
            with
              Not_found->aux camino t
    in
    if inicio=fin then [inicio] 
    else aux [inicio] (gen_mov fil col inicio [inicio]);;
        
       
let next (i,j) =
    List.map (fun (x,y)->(i+x,j+y)) [(1,2);(1,-2);(-1,2);(-1,-2);(2,1);(2,-1);(-2,1);(-2,-1)];;

let rec esFinal c =function
    []->(-1,-1)
    |h::t->if h = c then h else esFinal c t;;

let tourAll m n inicio fin =
    let rec aux camino (i,j) = 
        if (i>m)||(i<1) ||(j>n) ||(j<1) then []
        else
            if (i,j) = fin && not_mem camino (i,j) then [List.rev ((i,j)::camino)]
            else
            if not_mem camino (i,j) then
                let jump = next (i,j) in
                    match esFinal fin jump with
                        (-1,-1)-> let rec aux2 saltos= match(List.tl saltos) with
                                    []->aux((i,j)::camino) (List.hd saltos)
                                    |_->aux((i,j)::camino) (List.hd saltos) @ aux2(List.tl saltos)
                                in aux2 jump
                        | sol -> aux ((i,j)::camino) sol

          else []
        in aux [] inicio;;

let escogerMenor lista = match lista with
    []->raise(Invalid_argument"No hay lista")
    |h::[]->h
    |h::t-> let rec aux lista a = match (lista) with
        []->a
        |h::t-> match compare_lengths h a with
                1|0->aux t a
                |_->aux t h
        in aux t h;;                                 


let shortest_tour m n inicio fin = escogerMenor(tourAll m n inicio fin);;
