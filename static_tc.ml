(* The language *)
type ide = string;;

type texp = CstInt of int | CstFloat of float | CstTrue | CstFalse 
   | CstString of string | Den of ide | Sum of texp * texp | Sub of texp * texp 
   | Neg of texp | Times of texp * texp | Div of texp * texp 
   | Mod of texp * texp | Ifthenelse of texp * texp * texp 
   | BiggerThan of texp * texp | LessThan of texp * texp | Eq of texp * texp 
   | IsZero of texp | And of texp * texp | Or of texp * texp | Not of texp 
   | Concat of texp * texp | Let of ide * texp * texp 
   | Fun of ide * tval * texp | Letrec of ide * ide * tval * tval * texp * texp 
   | Apply of texp * texp
   (* texps specific to sets *)
   | Empty of tval | Singleton of texp | OfOpt of tval * collection
	(* basic ops specific to sets *)
	| Union of texp * texp | Intersection of texp * texp 
	| Difference of texp * texp | Add of texp * texp | Remove of texp * texp 
	| IsInside of texp * texp | IsSubset of texp * texp | IsEmpty of texp 
   | GetMax of texp | GetMin of texp 
	(* functional ops  *)
	| ForAll of texp * texp | Exists of texp * texp | Filter of texp * texp
	| Map of texp * texp
and collection = Empty | Item of texp * collection
and tval = TInt | TFloat | TString | TBool 
                  | TSet of tval | TFun of tval * tval | TUnbound;;

(* Polymorphic tenvironment *)
type 'v tenv = (string * 'v) list;;

let tEmptyEnv = [ ("", TUnbound)] ;;

let bind (s : tval tenv) (i : string) (x : tval) = (i,x)::s;;

let rec lookup (s : tval tenv) (i : string) = match s with
  | [] ->  TUnbound
  | (j,v)::sl when j = i -> v
  | _::sl -> lookup sl i;;

(* Static typechecking *)
let rec typecheck (e:texp) (s:tval tenv) = match e with
   | CstInt(n) -> TInt
   | CstFloat(n) -> TFloat
   | CstTrue -> TBool
   | CstFalse -> TBool
   | CstString(str) -> TString
   | Den(i) -> lookup s i
   | Sum(e1, e2)
   | Sub(e1, e2) -> 
      let t1 = typecheck e1 s in
      let t2 = typecheck e2 s in
          (match (t1,t2) with
             | (TInt,TInt) -> TInt
             | (TFloat,TFloat) -> TFloat
             | (_,_) -> failwith ("invalid type error"))                           
   | Neg(e) -> 
      let t = typecheck e s in
          (match t with
             | TInt -> TInt
             | TFloat -> TFloat
             | (_) -> failwith ("invalid type error")) 
   | Times(e1, e2) 
   | Div(e1, e2) -> 
      let t1 = typecheck e1 s in
      let t2 = typecheck e2 s in
          (match (t1,t2) with
             | (TInt,TInt) -> TInt
             | (TFloat,TFloat) -> TFloat
             | (_,_) -> failwith ("invalid type error"))   
   | Mod(e1, e2) -> 
      let t1 = typecheck e1 s in
      let t2 = typecheck e2 s in
          (match (t1,t2) with
             | (TInt,TInt) -> TInt
             | (_,_) -> failwith ("invalid type error"))
   | Ifthenelse(c,e1,e2) ->  
      let t1 = typecheck c s in
          (match t1 with
             | TBool -> let t2 = typecheck e1 s in 
                           if(t2 = (typecheck e2 s)) 
                           then t2
                           else failwith("type mismatch error")
             | _ -> failwith("invalid type error")) 
   | BiggerThan(e1,e2)  
   | LessThan(e1,e2) 
   | Eq(e1, e2) -> 
      let t1 = typecheck e1 s in
      let t2 = typecheck e2 s in
          (match (t1,t2) with
             | (TInt,TInt) -> TBool
             | (TFloat,TFloat) -> TBool
             | (TString,TString) -> TBool
             | (_,_) -> failwith ("invalid type error"))   
   | IsZero(e) -> 
      let t = typecheck e s in
          (match t with
             | TInt -> TBool
             | (_) -> failwith ("invalid type error")) 
   | And(e1, e2)
   | Or(e1, e2) -> 
      let t1 = typecheck e1 s in
      let t2 = typecheck e2 s in
          (match (t1,t2) with
             | (TBool,TBool) -> TBool
             | (_,_) -> failwith ("invalid type error"))
   | Not(e) -> 
       let t = typecheck e s in
          (match t with
             | TBool -> TBool
             | (_) -> failwith ("invalid type error")) 
   | Concat(e1,e2) -> 
      let t1 = typecheck e1 s in
      let t2 = typecheck e2 s in
          (match (t1,t2) with
             | (TString,TString) -> TString
             | (_,_) -> failwith ("invalid type error"))  
   | Singleton(e) -> TSet(typecheck e s)
   | Empty(t) -> TSet(t)
   | OfOpt(t,collection) ->
      let rec aux item =
          (match item with
             | Item(myType,Empty) -> 
                 if (typecheck myType s) = t
                 then TSet(t)
                 else failwith("type mismatch error")
             | Item(myType,i) ->                  
                 if (typecheck myType s) = t
                 then TSet(t)
                 else failwith("type mismatch error")
             | Empty -> TSet(t))
          in aux collection
   | Union(e1, e2)
   | Intersection(e1,e2)
   | Difference(e1,e2) ->
      let s1 = typecheck e1 s in
      let s2 =typecheck e2 s in
          (match (s1,s2) with
             | (TSet(t1),TSet(t2)) -> 
                 if(t1=t2) 
                 then s1
                 else failwith("type mismatch error")
             | (_,_) -> failwith("invalid type error")) 
   | Add(e1,e2)
   | Remove(e1,e2) -> 
      let set = typecheck e1 s in
      let v = typecheck e2 s in
            (match (set,v) with
          | (TSet(t1),v') -> 
             if(t1=v) 
             then set
             else failwith("type mismatch error")
          | (_,_) -> failwith("invalid type error")) 
   | IsInside(e1,e2) -> 
      let set = typecheck e1 s in
      let v = typecheck e2 s in
            (match set with
          | TSet(t1) -> 
             if(t1=v) 
             then TBool
             else failwith("type mismatch error")
          | (_) -> failwith("invalid type error")) 
   | IsSubset(e1,e2) ->
      let s1 = typecheck e1 s in
      let s2 =typecheck e2 s in
          (match (s1,s2) with
             | (TSet(t1),TSet(t2)) -> 
                 if(t1=t2) 
                 then TBool
                 else failwith("type mismatch error")
             | (_,_) -> failwith("invalid type error")) 
   | GetMax(e)
   | GetMin(e) -> 
       let t = typecheck e s in
          (match t with
             | TSet(TInt) -> TInt
             | TSet(TFloat) -> TFloat
             | (_) -> failwith ("invalid type error")) 
   | IsEmpty(e) -> 
       let t = typecheck e s in
          (match t with
             | TSet(_) -> TBool
             | (_) -> failwith ("invalid type error")) 
   | ForAll(pred,set)
   | Exists(pred,set) ->  
       let p = typecheck pred s in
       let s' = typecheck set s in  
          (match (p,s') with
             | (TFun(t1,t2), TSet(t)) -> 
                 if (t=t1) && t2 = TBool
                 then TBool
                 else failwith("type mismatch error")  
             | (_,_) -> failwith ("invalid type error")) 
   | Filter(pred,set) -> 
       let p = typecheck pred s in
       let s' = typecheck set s in  
          (match (p,s') with
             | (TFun(t1,t2), TSet(t)) -> 
                 if (t=t1) && t2 = TBool
                 then TSet(t1)
                 else failwith("type mismatch error")  
             | (_,_) -> failwith ("invalid type error")) 
   | Map(pred,set) -> 
      let p = typecheck pred s in
      let s' = typecheck set s in 
          (match (p,s') with
               (*By choice, the mapping function can change types of the 
                 resulting set. Hence, there is no need for checking if the
                 types match like we saw in the previous function*)
             | (TFun(t1,t2),TSet(t)) -> TSet(t1)
             | _ -> failwith("invalid type error")) 
   | Let(i, e, ebody) -> typecheck ebody (bind s i (typecheck e s))
   (* Fun(arg, typeof(arg), fbody) -> TFun(typeof(arg),typeof(res))
     That means that TFun is a function that takes an input t1 and returns a 
     a result of a certain type*)
   | Fun(i, t1, e) -> TFun(t1, (typecheck e (bind s i t1)))
   | Letrec(f, arg, t1, t2, fbody, letbody) -> 
      let tenv1 = bind s f (TFun(t1,t2)) in
      let tenv2 = bind tenv1 arg t1 in
      let t = typecheck fbody tenv2 in
          if(t1=typecheck letbody tenv2) 
          then t 
          else failwith ("type mismatch error")
   (*t1 should be a functional type. If t2 is the same as t1, then
      the type of the apply is t2*)
   | Apply (e1, e2) -> 
      let f = typecheck e1 s in 
          (match f with
             | TFun(t1,t2) -> if ((typecheck e2 s)=t1)
                              then t2
                              else failwith("type mismatch error")
             | _ -> failwith("invalid type error"));;      
  
