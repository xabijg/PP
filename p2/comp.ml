let comp f g=function x->f(g x);;
(*val comp:(´a->´b)->(´c->´a)->´c->´b=<fun>*)

let f=let square x=x*x in comp square ((+)1);;
(*val f2:int->int=<fun>*)

f 1,f 2,f 3;;
(*:int*int*int=(4,9,16*)
