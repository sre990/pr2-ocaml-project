(* The language *)
type ide = string;;

type typeSet = Int | Float | Bool | String;;

(* ADTs for managing expressions *)
type exp = CstInt of int
   | CstFloat of float | CstTrue | CstFalse | CstString of string | Den of ide
   | Sum of exp * exp | Sub of exp * exp | Neg of exp | Times of exp * exp
   | Div of exp * exp | Mod of exp * exp | Ifthenelse of exp * exp * exp
   | BiggerThan of exp * exp | LessThan of exp * exp | Eq of exp * exp
   | IsZero of exp | And of exp * exp | Or of exp * exp | Not of exp
   | Concat of exp * exp | Let of ide * exp * exp | Fun of ide * exp
   | Letrec of ide * ide * exp * exp | Apply of exp * exp
   (* exps specific to sets *)
   | Empty of typeSet | Singleton of exp | Of of typeSet * exp list
   | OfOpt of typeSet * collection 
	(* basic ops specific to sets *)
	| Union of exp * exp | Intersection of exp * exp | Difference of exp * exp 
	| Add of exp * exp | Remove of exp * exp | IsInside of exp * exp
	| IsSubset of exp * exp | IsEmpty of exp | Head of exp | Length of exp
	| GetMax of exp | GetMin of exp 
	(* functional ops  *)
	| ForAll of exp * exp | Exists of exp * exp | Filter of exp * exp
	| Map of exp * exp
and collection = Empty | Item of exp * collection

(* Polymorphic environment *)
type 'v env = (string * 'v) list;;

(* Expressible Values:the values that can be obtained as a result 
   of the evaluation of an expression *)
type evT = Int of int 
   | Float of float
   | Bool of bool 
   | String of string
   | Set of typeSet * (evT list) 
   | Closure of ide * exp * evT env 
   | RecClosure of ide * ide * exp * evT env 
   | Unbound;;
   
(* This function takes an expressible value and returns its type*)   
let get_type (v : evT) =
   match v with 
      | Int(x) -> (Int : typeSet)
      | Float(x) -> (Float : typeSet)
      | String(x) -> (String : typeSet)
      | Bool(x) -> (Bool : typeSet)
      | _ -> failwith("wrong type error");;
(* This function turns a typeSet into a string*)
let to_str (t : typeSet) =
   match t with
      | Int -> "int"
      | Float -> "float"
      | Bool -> "bool"
      | String -> "string"
(* This function turns an exp into its corresponding typeSet
*)
(*TODO: extend function for float expressions*)
let rec get_type_set (x) : typeSet = 
   match x with
      | CstTrue | CstFalse | Eq(_, _) | And(_, _) | Or(_,_) | Not(_) 
      | BiggerThan(_,_) | LessThan(_,_) | IsZero(_) -> Bool
      | CstString(_) | Concat(_,_) -> String
      | CstInt(_) | Sum(_) | Sub(_) | Times(_) | Mod(_,_) | Neg(_) -> Int
      | Ifthenelse(c, t, e) -> let ret = get_type_set(t) 
                               in if ret = get_type_set(e) 
                                  then ret 
                                  else failwith("invalid type error")
      | Let(_, _, fbody) -> get_type_set(fbody)
      | Fun(_, fbody) -> get_type_set(fbody) 
      | _ -> failwith("invalid type error");;
      
let emptyEnv  = [ ("", Unbound)] ;;
let bind (s: evT env) (i:string) (x:evT) = ( i, x ) :: s;;
let rec lookup (s:evT env) (i:string) = match s with
   | [] ->  Unbound
   | (j,v)::sl when j = i -> v
   | _::sl -> lookup sl i;;

(* Dynamic typecheck *)
let typecheck (x, y) = match x with	
   | "int" -> 
      (match y with 
         | Int(_) -> true
         | _ -> false)
   | "float" -> 
      (match y with 
         | Float(_) -> true
         | _ -> false)
   | "bool" -> 
      (match y with 
         | Bool(_) -> true
         | _ -> false)
   | "string" ->
      (match y with
         | String(_) -> true
         | _ -> false)
   | "set" -> 
      (match y with
         | Set(_, _) -> true
         | _ -> false)
   | _ -> failwith ("not a valid type");;

(* Primitive functions *)
(* integers operations*)
let int_plus(x, y) = 
   match(typecheck("int",x), typecheck("int",y), x, y) with
      | (true, true, Int(v), Int(w)) -> Int(v + w)
      | (_,_,_,_) -> failwith("invalid type error ");;  
let int_minus(x, y) = 
   match(typecheck("int",x), typecheck("int",y), x, y) with
      | (true, true, Int(v), Int(w)) -> Int(v - w)
      | (_,_,_,_) -> failwith("invalid type error ");;
let int_neg(x) =   
   match (typecheck("int",x), x) with
      | (true, Int(u)) -> Int(-u)
      | (_,_) -> failwith("invalid type error ");;  
let int_times(x, y) = 
    match(typecheck("int",x), typecheck("int",y), x, y) with
      | (true, true, Int(v), Int(w)) -> Int(v * w)  
      | (_,_,_,_) -> failwith("invalid type error ");;
let int_div(x, y) = 
    match(typecheck("int",x), typecheck("int",y), x, y) with
      | (true, true, Int(v), Int(w)) -> 
         if w <> 0 then Int(v / w)
         else failwith("division by zero")
      | (_,_,_,_) -> failwith("invalid type error ");;
let int_mod(x, y) = 
    match(typecheck("int",x), typecheck("int",y), x, y) with
      | (true, true, Int(v), Int(w)) -> Int(v mod w)  
      | (_,_,_,_) -> failwith("invalid type error ");; 
let int_eq(x, y) = 
    match(typecheck("int",x), typecheck("int",y), x, y) with
      | (true, true, Int(v), Int(w)) -> Bool(v = w)  
      | (_,_,_,_) -> failwith("invalid type error ");;      
let int_bigger(x, y) = 
    match(typecheck("int",x), typecheck("int",y), x, y) with
      | (true, true, Int(v), Int(w)) -> Bool(v > w)  
      | (_,_,_,_) -> failwith("invalid type error ");;
let int_smaller(x, y) = 
    match(typecheck("int",x), typecheck("int",y), x, y) with
      | (true, true, Int(v), Int(w)) -> Bool(v < w)  
      | (_,_,_,_) -> failwith("invalid type error ");;      
let int_iszero(x) =   
   match (typecheck("int",x), x) with
      | (true, Int(u)) -> Bool(u = 0)
      | (_,_) -> failwith("invalid type error ");;  
(* floats operations*)      
let float_plus(x, y) = 
   match(typecheck("float",x), typecheck("float",y), x, y) with
      | (true, true, Float(v), Float(w)) -> Float(v +. w)
      | (_,_,_,_) -> failwith("invalid type error ");;  
let float_minus(x, y) = 
   match(typecheck("float",x), typecheck("float",y), x, y) with
      | (true, true, Float(v), Float(w)) -> Float(v -. w)
      | (_,_,_,_) -> failwith("invalid type error ");;
let float_neg(x) =   
   match (typecheck("float",x), x) with
      | (true, Float(u)) -> Float(-.(u))
      | (_,_) -> failwith("invalid type error ");;
let float_times(x, y) = 
    match(typecheck("float",x), typecheck("float",y), x, y) with
      | (true, true, Float(v), Float(w)) -> Float(v *. w)  
      | (_,_,_,_) -> failwith("invalid type error ");; 
let float_div(x, y) = 
    match(typecheck("float",x), typecheck("float",y), x, y) with
      | (true, true, Float(v), Float(w)) -> 
         if w <> 0. 
         then Float(v /. w)
         else failwith("division by zero")
      | (_,_,_,_) -> failwith("invalid type error ");;  
let float_eq(x, y) = 
    match(typecheck("float",x), typecheck("float",y), x, y) with
      | (true, true, Float(v), Float(w)) -> Bool(v = w)  
      | (_,_,_,_) -> failwith("invalid type error ");;    
let float_bigger(x, y) = 
    match(typecheck("float",x), typecheck("float",y), x, y) with
      | (true, true, Float(v), Float(w)) -> Bool(v > w)  
      | (_,_,_,_) -> failwith("invalid type error ");;  
let float_smaller(x, y) = 
    match(typecheck("float",x), typecheck("float",y), x, y) with
      | (true, true, Float(v), Float(w)) -> Bool(v < w)  
      | (_,_,_,_) -> failwith("invalid type error ");;
(* boolean operations*)
let bool_and(x, y) = 
   match(typecheck("bool",x), typecheck("bool",y), x, y) with
      | (true, true, Bool(v), Bool(w)) -> Bool(v && w)
      | (_,_,_,_) -> failwith("runtime-error");;
let bool_or(x, y) = 
   match(typecheck("bool",x), typecheck("bool",y), x, y) with
      | (true, true, Bool(v), Bool(w)) -> Bool(v || w)
      | (_,_,_,_) -> failwith("runtime-error");;
let bool_not(x) = 
   match(typecheck("bool",x), x) with
      | (true, Bool(u)) -> Bool(not(u))     
      | (_,_) ->  failwith("runtime-error");;
      
(* string operations*)
let string_eq(x, y) = 
    match(typecheck("string",x), typecheck("string",y), x, y) with
      | (true, true, String(v), String(w)) -> Bool(v = w)  
      | (_,_,_,_) -> failwith("invalid type error ");; 
let string_bigger(x, y) = 
    match(typecheck("string",x), typecheck("string",y), x, y) with
      | (true, true, String(v), String(w)) -> Bool(v > w)  
      | (_,_,_,_) -> failwith("invalid type error ");;
let string_smaller(x, y) = 
    match(typecheck("string",x), typecheck("string",y), x, y) with
      | (true, true, String(v), String(w)) -> Bool(v < w)  
      | (_,_,_,_) -> failwith("invalid type error ");; 
let string_concat(x, y) = 
   match(typecheck("string",x), typecheck("string",y), x, y) with
      | (true, true, String(v), String(w)) -> String(v ^ w)
      | (_,_,_,_) -> failwith("invalid type error ");;
(* set operations *)
(* creates an empty set*)
let set_create_empty (t : typeSet) = Set(t, []);;
(* creates a singleton set*)
let set_create_singleton (v : evT) = Set(get_type v, [v]);;
(* Three functions on a set that return its head, tell us if
    the set is empty and tell us its length, respectively*)
let set_head s =
   match s with
      | Set(_, []) -> failwith("empty set error")
      | Set(_, x::xs) -> x
      | _ -> failwith("not a set error")
let set_isempty (s : evT) =
   match s with
      | Set(myType, []) -> Bool(true)
      | Set(myType, ss) -> Bool(false)
      | _ -> failwith ("not a set error");;
let set_length(s : evT) =
   match s with
      | Set(myType, list) ->  let aux_length list = 
                                 let rec aux acc = function
                                    | [] -> Int(acc)
                                    | _ :: t -> aux (acc + 1) t
                                 in aux 0 list
                              in aux_length list
      | _ -> failwith("not a set error");;
(* helper function that returns true if an element is in a list, 
   false otherwise. *)
let rec aux_is_inside v = function
   | [] -> false
   | x::xs -> x = v || aux_is_inside v xs;;
let set_is_inside ((s : evT), (v : evT))=
   match s with
      | Set(myType, [])-> Bool(false)
      | Set(myType, x::xs)-> if get_type v = myType 
                             then Bool(aux_is_inside v (x::xs))
                             else failwith("type mismatch error") 
      | _ -> failwith("not a set error");;
let set_getmax (s : evT) =
   match s with
      | Set(myType, list) -> 
         let rec aux_max alist amax =
            (match alist with
               | [] -> amax
               | x::xs -> if(x>amax) = true
                          then aux_max xs x
                          else aux_max xs amax)
          in
            (match list with
               | [] -> Unbound
               | x::xs -> aux_max xs x)
      | _ -> failwith("not a set error");;
let set_getmin (s : evT) =
   match s with
      | Set(myType, list) -> 
         let rec aux_min alist amin =
            (match alist with
               | [] -> amin
               | x::xs -> if(x<amin) = true
                          then aux_min xs x
                          else aux_min xs amin)
          in
            (match list with
               | [] -> Unbound
               | x::xs -> aux_min xs x)
      | _ -> failwith("not a set error");;
let set_is_subset ((s1 : evT), (s2 : evT)) =
   match (s1, s2) with
      | (Set(t1, _), Set(t2, [])) -> 
         if(t1=t2) 
         then Bool(false) 
         else failwith("type mismatch error")
      | (Set(t1,l1), Set(t2,l2)) -> 
         if(t1=t2)
         then let rec aux l1 l2 bl = match l1 with
            | [] -> bl
            | x::xs -> if set_is_inside ((Set(t2,l2)),x) = Bool(true)
                       then aux xs l2 (Bool(true))
                       else Bool(false)
          in aux l1 l2 (Bool(true))
          else failwith("type mismatch error")
       | _ -> failwith("not a set error");;
let set_add ((s : evT), (v : evT)) = 
   match s with
      | Set(myType, [])-> if (get_type v) = myType 
                          then set_create_singleton v
                          else failwith("type mismatch error") 
      | Set(myType, lst)-> 
         if set_is_inside(s, v) = Bool(true)
         then failwith("duplicate error") 
         (* not checking again if there is a type mismatch, since
            this is already done by set_is_inside*)
         else Set(myType, v::lst)            
      | _ -> failwith("not a set error");; 
let rec aux_remove x lst = 
   match lst with
      | [] -> []
      | l::ls -> if l = x then ls
                 else l::(aux_remove x ls);;      
let set_remove ((s : evT), (v : evT))  = 
   if set_is_inside(s,v) <> Bool(true)
   then failwith("item not found error")
   else match s with
      | Set(myType, [])-> failwith("empty set error")
      | Set(myType, lst)-> if (myType = get_type v)
                           then Set(myType, (aux_remove v lst))
                           else failwith("type mismatch error")
      | _ -> failwith("not a set error");;
let set_inters ((s1 : evT), (s2 : evT)) = 
   match (s1, s2) with
      | (Set(t1, []), Set(t2, lst)) -> 
         if (t1=t2) then set_create_empty(t1)
                    else failwith("type mismatch error")
      | (Set(t1, lst), Set(t2, [])) -> 
         if (t1=t2) then set_create_empty(t2)
                    else failwith("type mismatch error") 
      | ((Set(t1, l1)),(Set(t2, l2))) -> 
         let rec aux_inters a b = match a with
            | [] -> if b = [] then [] else aux_inters b a
            | x::xs ->
            if aux_is_inside x b then
               let b' = aux_remove x b in x::(aux_inters xs b')
            else aux_inters xs b
         in
            if (t1=t2) then (Set(t1, aux_inters l1 l2))
            else failwith("type mismatch error") 
      | (_,_) -> failwith ("not a set error") ;;
let set_diff ((s1 : evT), (s2 : evT)) = 
   match (s1,s2) with
      | (Set(t1, []), Set(t2, lst)) -> 
         if (t1=t2) then set_create_empty(t2)
         else failwith("type mismatch error") 
      | ((Set(t1, lst)),(Set(t2, []))) -> 
         if (t1=t2) then s1
         else failwith("type mismatch error") 
      | ((Set(t1, l1)),(Set(t2, l2))) -> 
         let rec aux_diff a b = match a with
            | [] -> []
            | x::xs -> if aux_is_inside x b 
                       then aux_diff xs (aux_remove x b)
                       else x::(aux_diff xs b)    
         in
            if (t1=t2) then Set(t1, aux_diff l1 l2)
            else failwith("type mismatch error") 
      | (_,_) -> failwith ("not a set error") ;;
let set_union ((s1 : evT), (s2 : evT)) =
   match(s1,s2) with
      | (Set(t1, []), Set(t2, lst)) -> 
         if (t1=t2) then s2
         else failwith("type mismatch error") 
      | (Set(t1, lst), Set(t2, [])) -> 
         if (t1=t2) then s1
         else failwith("type mismatch error") 
      | ((Set(t1, l1)),(Set(t2, l2))) -> 
         let rec aux_union a b = match (a, b) with
            | (a', []) -> a'
            | ([], b') -> b'
            | (x::xs, x') -> if aux_is_inside x x' 
                             then aux_union xs x'
                             else x::(aux_union xs x')
         in     
            if (t1=t2) then (Set(t1, aux_union l1 l2))
            else failwith("type mismatch error") 
      | (_,_) -> failwith ("not a set error") ;;
	
(* Interpreter *)
let rec eval  (e:exp) (s:evT env) = match e with
   | CstInt(n) -> Int(n)
   | CstFloat(n) -> Float(n)
   | CstTrue -> Bool(true)
   | CstFalse -> Bool(false)
   | CstString(str) -> String(str)
   | Den(i) -> lookup s i
   | Sum(e1, e2) -> (let ev1 = eval e1 s in
                     let ev2 = eval e2 s in
                         match (ev1, ev2) with
                            | (Int(x),Int(y)) 
                               -> int_plus(Int(x),Int(y))
                            | (Float(x),Float(y)) 
                               -> float_plus(Float(x),Float(y))
                           | _ -> failwith ("invalid type error"))
   | Sub(e1, e2) -> (let ev1 = eval e1 s in
                     let ev2 = eval e2 s in
                         match (ev1, ev2) with
                            | (Int(x),Int(y)) 
                               -> int_minus(Int(x),Int(y))
                            | (Float(x),Float(y)) 
                               -> float_minus(Float(x),Float(y))
                           | _ -> failwith ("invalid type error"))
   | Neg(e) -> (let ev = eval e s in 
                  match ev with
                  | Int(x) -> int_neg(Int(x))
                  | Float(x) -> float_neg(Float(x))
                  | _ -> failwith("invalid type error"))
   | Times(e1,e2) -> (let ev1 = eval e1 s in
                      let ev2 = eval e2 s in
                          match (ev1, ev2) with
                             | (Int(x),Int(y)) 
                                -> int_times(Int(x),Int(y))
                             | (Float(x),Float(y)) 
                                -> float_times(Float(x),Float(y))
                             | _ -> failwith ("invalid type error"))
   | Div(e1,e2) -> (let ev1 = eval e1 s in
                    let ev2 = eval e2 s in
                       match (ev1, ev2) with
                          | (Int(x),Int(y)) 
                             -> int_div(Int(x),Int(y))
                          | (Float(x),Float(y)) 
                             -> float_div(Float(x),Float(y))
                          | _ -> failwith ("invalid type error"))
   | Mod(e1,e2) -> int_mod((eval e1 s), (eval e2 s))                             
   | Ifthenelse(e1,e2,e3) -> 
      let g = eval e1 s in
         (match (typecheck("bool", g), g) with
		      | (true, Bool(true)) -> eval e2 s
            | (true, Bool(false)) -> eval e3 s
            | (_, _) -> failwith ("nonboolean guard"))
   | BiggerThan(e1,e2) -> 
      (let ev1 = eval e1 s in
       let ev2 = eval e2 s in
          match (ev1, ev2) with
             | (Int(x),Int(y)) 
                -> int_bigger(Int(x),Int(y))
             | (Float(x),Float(y)) 
                -> float_bigger(Float(x),Float(y))
             | (String(x),String(y)) 
                -> string_bigger(String(x),String(y))
             | _ -> failwith ("invalid type error")) 
   | LessThan(e1,e2) -> 
      (let ev1 = eval e1 s in
       let ev2 = eval e2 s in
          match (ev1, ev2) with
             | (Int(x),Int(y)) 
                -> int_smaller(Int(x),Int(y))
             | (Float(x),Float(y)) 
                -> float_smaller(Float(x),Float(y))
             | (String(x),String(y)) 
                -> string_smaller(String(x),String(y))
             | _ -> failwith ("invalid type error"))
   | Eq(e1, e2) -> (let ev1 = eval e1 s in
                    let ev2 = eval e2 s in
                        match (ev1, ev2) with
                           | (Int(x),Int(y)) 
                              -> int_eq(Int(x),Int(y))
                           | (Float(x),Float(y)) 
                              -> float_eq(Float(x),Float(y))
                           | (String(x),String(y)) 
                              -> string_eq(String(x),String(y))
                           | _ -> failwith ("invalid type error")) 
   | IsZero(e) -> int_iszero(eval e s) 
   | And(e1, e2) -> bool_and((eval e1 s),(eval e2 s))
   | Or(e1, e2) -> bool_or((eval e1 s),(eval e2 s))
   | Not(e) -> bool_not(eval e s)
   | Concat(e1,e2) -> string_concat((eval e1 s),(eval e2 s))       
   | Singleton(e) -> set_create_singleton(eval e s)
   | Empty(t) -> set_create_empty(t)
   | Of(t,collection) -> 
      (match t with
         | Int -> Set(t,(eval_list(t,collection,[],s)))
         | Float -> Set(t,(eval_list(t,collection,[],s)))
         | Bool -> Set(t,(eval_list(t,collection,[],s)))
         | String -> Set(t,(eval_list(t,collection,[],s))))
   (* This is an optimized version of Of. *)
   | OfOpt(myType,collection) -> 
      let rec aux i set =
      (match i with
         (*case 1: item of a certain type with no elements*)
         | Item (t, Empty) -> 
            if (get_type (eval t s)) = myType
            then (set_add(set, (eval t s)))
            else failwith("type mismatch error")
         (*case 2: item of a certain type with an element*)
         | Item (t, Item(t',i')) ->
            if (get_type (eval t s)) = myType
            then aux (Item(t',i')) (set_add(set, (eval t s)))
            else failwith("type mismatch error")
         (* case 0: the set becomes Empty in the end*)  
         | Empty -> set)
      (*This recursive function creates a Set from a collection of items*)
      in aux collection (set_create_empty(myType))            
   | Union(e1, e2) -> set_union((eval e1 s),(eval e2 s))
   | Intersection(e1,e2) -> set_inters((eval e1 s),(eval e2 s))
   | Difference(e1,e2) -> set_diff((eval e1 s),(eval e2 s))
   | Add(e1,e2) -> set_add((eval e1 s), (eval e2 s))
	| Remove(e1,e2) -> set_remove((eval e1 s),(eval e2 s))
	| IsInside(e1,e2) -> set_is_inside((eval e1 s),(eval e2 s))
	| IsSubset(e1,e2) -> set_is_subset((eval e1 s),(eval e2 s))
	| GetMax(e) -> set_getmax(eval e s)
	| GetMin(e) -> set_getmin(eval e s)
	| IsEmpty(e) -> set_isempty(eval e s)
	| Head(e) -> set_head(eval e s)
	| Length(e) -> set_length(eval e s)
	| ForAll(pred,set) -> set_forall((eval pred s), (eval set s))
	| Exists(pred,set) -> set_exists((eval pred s), (eval set s))
	| Filter(pred,set) -> set_filter((eval pred s), (eval set s))
	| Map(pred,set) -> set_map((eval pred s), (eval set s))
   | Let(i, e, ebody) -> eval ebody (bind s i (eval e s))
   | Fun(arg, ebody) -> Closure(arg,ebody,s)
   | Letrec(f, arg, fBody, letBody) -> 
      let benv = bind (s) (f) (RecClosure(f, arg, fBody,s)) in
         eval letBody benv
   | Apply(eF, eArg) ->
      let fclosure = eval eF s in 
         (match fclosure with 
            | Closure(arg, fbody, fDecEnv) ->
               let aVal = eval eArg s in
               let aenv = bind fDecEnv arg aVal in 
                  eval fbody aenv
            | RecClosure(f, arg, fbody, fDecEnv) ->
               let aVal = eval eArg s in
               let rEnv = bind fDecEnv f fclosure in
               let aenv = bind rEnv arg aVal in 
                  eval fbody aenv
           | _ -> failwith("non functional value error"))
(* helper functions used in eval to evaluate a list of expressions*)
and eval_list (t, collection, acc, s) =
   match collection with
      | [] -> acc
      | x::xs -> let aVal = eval x s in 
                 if(typecheck(to_str(t),aVal)) 
                 then let rec add lst v =
                    (match lst with
                       | [] -> [v] 
                       | x::xs -> if (x = v) 
                                  then failwith("duplicate error") 
                                  else x::(add xs v))
                    in (eval_list(t, xs,(add acc aVal), s))
                 else failwith("type mismatch error")
(* helper functions used in eval to evaluate ForAll, Exists, Filter and
   Map respectively*)
and set_forall (pred,set) = 	   
   (match (pred,set) with
         (*pred should be a (boolean) function. We are operating within a
           static scope, therefore we expect pred to be a closure*)
         | (Closure(arg,fbody,fDecEnv), Set(myType,lst)) ->
            let rec aux(aarg, abody, fDecEnv, lst, acc)=
               (*we inspect recursively each item of the set*)
               match lst with
                  |[]-> acc
                  | x::xs -> 
                     let aenv = bind fDecEnv aarg x 
                     (*when we evaluate the body of the function we do it in
                       the environment we found in the closure*)
                     in let acc = eval fbody aenv
                     (*if the item we have applied the predicate to yelds
                       the result true, we continue evaluating the items of
                       the set until we get a false or we reach the end of
                       the set.*) 
                     in if acc = Bool(true) 
                     then aux(aarg,abody,fDecEnv,xs, acc) 
                     else if acc = Bool(false)
                     then acc
                     else failwith("pred not a bool error")
            (*we start with the list of items of the set and a false value*)         
            in aux (arg,fbody,emptyEnv,lst,(Bool(false)))
         | _ -> failwith("invalid type error"))
and set_exists (pred,set) = 	   
   (match (pred,set) with
         (*pred should be a (boolean) function. We are operating within a
           static scope, therefore we expect pred to be a closure*)
         |(Closure(arg,fbody,fDecEnv),Set(myType,lst)) ->
            let rec aux(aarg, abody, fDecEnv, lst, acc)=
               match lst with
                  |[]->acc
                  | x::xs -> 
                     let aenv = bind fDecEnv aarg x
                     (*when we evaluate the body of the function we do it in
                      the environment we found in the closure*) 
                     in let acc = eval fbody aenv 
                     (*if the item we have applied the predicate to yelds
                       the result false, we continue evaluating the items of
                       the set until we get a true or we reach the end of
                       the set.*) 
                     in if acc = Bool(false) 
                     then aux(aarg,abody,fDecEnv,xs, acc) 
                     else if acc = Bool(true)
                     then acc
                     else failwith("pred not a bool error")
            (*we start with the list of items of the set and a false value*)
            in aux (arg,fbody,emptyEnv,lst,(Bool(false)))
      | _ -> failwith("invalid type error"))
and set_filter (pred,set) =
   (match (pred,set) with
      (*pred should be a (boolean) function. We are operating within a
        static scope, therefore we expect pred to be a closure*)
      |(Closure(arg,fbody,fDecEnv),Set(myType, lst)) ->
         let rec aux(aarg, abody, fDecEnv, lst, myType,acc) =
            match lst with
               |[]-> Set(myType,acc)
               | x::xs -> 
                  let aenv= bind fDecEnv aarg x in
                  (*when we evaluate the body of the function we do it in
                  the environment we found in the closure*)  
                  let check = eval fbody aenv in 
                     (*if the item we have applied the predicate to yelds
                       the result true, we put it in the new set. Otherwise,
                       we discard it.*) 
                     if check = Bool(true) 
                     then aux(aarg, abody, fDecEnv,xs, myType,(x::acc)) 
                     else if check = Bool(false) 
                     then aux (aarg,abody,fDecEnv,xs, myType, acc)
                     else failwith("pred not a bool error")
         (*we start with the list of items of the set and an empty list to 
           fill with the filtered values*)
         in aux (arg,fbody,emptyEnv,lst, myType,[])
  |(_,_) -> failwith("invalid type error"))
and set_map (pred,set) =
   (match (pred,set) with
         (*pred should be a function. We are operating within a
           static scope, therefore we expect pred to be a closure*)
      |(Closure(arg,fbody,fDecEnv),Set(myType, lst)) ->
         let rec aux(aarg, abody, fDecEnv, lst, myType,acc) =
            match lst with
               |[]-> acc
               | x::xs -> 
                  let aenv = bind fDecEnv aarg x 
                  (*when we evaluate the body of the function we do it in
                  the environment we found in the closure*) 
                  (*we fill up the new set with the values we have obtained
                    from applying the map function*) 
                  in aux(aarg,abody,fDecEnv,xs, myType,
                                    (set_add(acc,(eval fbody aenv))))
         (*we start with an empty set of a certain type and we fill it with
           the values we have obtained by applying the pred to the list of
           items of the set. The type of each item of the new
           set is the same as that of the mapped function's return type*)
         in aux (arg, fbody, emptyEnv, lst, myType, 
                              set_create_empty(get_type_set(fbody)))
      |(_,_) -> failwith("invalid type error"));;        
  
