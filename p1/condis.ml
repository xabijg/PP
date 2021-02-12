
false &&(2/0>0);;
(*:bool=false*)

(*true &&(2/0>0)*)
(*No podemos saber el valor del segundo dato*)

true || (2/0>0);;
(*: bool=true*)

(*false || (2/0>0)*)
(*Exception division by zero,no podemos saber el valor del segundo dato*)

let con=(&&);;
(*val con:bool->bool->bool=<fun>*)

let dis=(||);;
(*val dis:bool->bool->bool=<fun>*)

(&&) (1<0) (2/0>0);;
(*:bool =false*)

(*con (1<0) (2/0>0)*)
(*Exception division by zero*)

(||) (1>0) (2/0>0);;
(*:bool=true*)

(*dis (1>0) (2/0>0)*)
(*Exception division by zero*)




