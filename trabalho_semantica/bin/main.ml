(* SINTAXE
   
  e ::= n | 
        b | 
        e1 op e2 | 
        if e1 then e2 else e3 |
        x | 
        e1 e2 | 
        fn x:T = e |
        let x:T = e1 in e2 | 
        let rec f:T1 -> T2 = (fn y:T1 => e1) in e2 |
        nil : T |
        e1::e2 |
        isempty e |
        hd e |
        tl e |
        match e1 with | nil -> e2 | x::xs -> e3 |
        nothing : T |
        just e |
        match e1 with | nothing -> e2 | just x -> e3
          
  T ::= int | bool | T1 -> T2 | maybe T | T list
     n conjunto de numerais inteiros
     b {true, false}
     x Ident
     op {+,-,*,div,=,>,>,and,or...}
*) 


type bop = Sum | Sub | Mul | Div 
         | Eq | Gt | Lt | Neq
         | And | Or
  
type tipo = 
    Int (* int *)
  | Bool (* bool *)
  | Arrow of tipo * tipo (* T1 -> T2 *)
  | Maybe of tipo (* maybe T *)
  | List of tipo (* T list *) 
  
type expr = 
    Nv of int (* n *)
  | Bv of bool (* b *)
  | Bop of bop * expr * expr (* e1 op e2  *)
  | If  of expr * expr * expr (* if e1 then e2 else e3 *)
  | Id  of string (* x *)
  | App of expr * expr (* e1 e2 *)
  | Fun of string * tipo * expr (* fn x:T = e *)
  | Let of string * tipo *  expr * expr (* let x:T = e1 in e2 *)
  | LetRec of string * tipo * expr * expr (* let rec f:T1 -> T2 = (fn y:T1 => e1) in e2 *)
  | Nil of tipo (* nil : T *)
  | Cons of expr * expr (* e1::e2 *)
  | IsEmpty of expr (* isempty e *)
  | Hd of expr (* hd e *)
  | Tl of expr (* tl e *)
  | Match of expr * expr * string * string * expr (* match e1 with | nil -> e2 | x::xs -> e3 *)
  | Nothing of tipo (* nothing : T *)
  | Just of expr (* just e *)
  | MatchMaybe of expr * expr * string * expr * string * expr (* match e1 with | nothing -> e2 | just x -> e3 *)
  
(*=========== TYPEINFER ===================*)
type tyEnv =  (string * tipo) list 
    
let rec lookup (g:tyEnv) (x:string) : tipo option =
  match g with
    [] -> None
  | (y,t):: tail -> if x=y then Some t else lookup tail x
          
exception TypeError
let rec typeinfer (g:tyEnv) (e:expr) : tipo =
  match e with
    Nv _ -> Int
  | Bv _ -> Bool
    
  | Bop(o1,e1,e2) ->
      let t1 = typeinfer g e1 in
      let t2 = typeinfer g e2 in
      (match o1 with
         Sum | Sub | Mul | Div ->
           if(t1=Int) && (t2=Int) then Int else raise TypeError
       | Eq | Gt | Lt | Neq ->
           if(t1=Int) && (t2=Int) then Bool else raise TypeError
       | And | Or ->
           if (t1=Bool) && (t2=Bool) then Bool else raise TypeError
      )
      
  | If(e1,e2,e3) ->
      let t1 = typeinfer g e1 in
      if (t1=Bool) then
        let t2 = typeinfer g e2 in
        let t3 = typeinfer g e3 in
        if (t2=t3) then t2 else raise TypeError 
      else raise TypeError
          
  | Id x ->
      (match lookup g x with
         None -> raise TypeError
       | Some t -> t)
      
  | Fun(x,t,e1) ->
      let g' = (x,t)::g in
      let t1 = typeinfer g' e1 in
      Arrow(t,t1)
        
  | Let(x,t,e1,e2) ->
      let t1 = typeinfer g e1 in
      let g' = (x,t)::g in
      let t2 = typeinfer g' e2 in
      if t1=t then t2 else raise TypeError
          
  | App(e1,e2) ->
      let t1 = typeinfer g e1 in
      (match t1 with
         Arrow(t,t') ->
           let t2 = typeinfer g e2 in
           if t=t2 then t' else raise TypeError
       | _ -> raise TypeError
      )
  
        
(* ======= AVALIADOR =========================*)

let value (e:expr) : bool =
  match e with
    Nv _ | Bv _ | Fun _ -> true
  | _ -> false
  
let rec subs (v: expr) (x: string) (e: expr) = 
  match e with
  | Nv _ -> e (* {v/x} n = n *)
  | Bv _ -> e (* {v/x} b = b *)
  | Bop (o, e1, e2) -> Bop (o, subs v x e1, subs v x e2)  (* {v/x} (e1 op e2) = {v/x}e1 op {v/x}e2 *)
  | If (e1, e2, e3) -> If (subs v x e1, subs v x e2, subs v x e3) (* {v/x} (if e1 then e2 else e3) = if {v/x}e1 then {v/x}e2 else {v/x}e3 *)
  | App (e1, e2) -> App (subs v x e1, subs v x e2)  (* {v/x} (e1 e2) = {v/x}e1 {v/x}e2 *)
  | Id y -> 
      if x = y then v (* {v/x} x = v *)
      else e  (* {v/x} y = y (se x̸ = y) *)
  | Fun (y, t, e1) ->
      if x = y then e (* {v/x} (fn x : T ⇒ e) = fn x : T ⇒ e *)
      else Fun (y, t, subs v x e1) (* {v/x} (fn y : T ⇒ e) = fn y : T ⇒ {v/x}e (se x̸ = y) *)
  | Let (y, t, e1, e2) ->
      if x = y then Let (y, t, subs v x e1, e2) (* {v/x}(let x:T = e1 in e2) = let x:T = {v/x}e1 in e2 *)
      else Let (y, t, subs v x e1, subs v x e2) (* {v/x}(let y:T = e1 in e2) = let y:T = {v/x}e1 in {v/x}e2 (se x̸ = y) *)
  | LetRec(f,Arrow(t1,t2),Fun(y,t1,e1),e2) ->
      if x = f then e (* {v/f }(let rec f : T1 → T2 = (fn y:T1 ⇒ e1) = let rec f : T1 → T2 = (fn y:T1 ⇒ e1) in e2) in e2 *)
      else LetRec(f,Arrow(t1,t2),Fun(y,t1,subs v x e1),subs v x e2) (* {v/x}(let rec f : T1 → T2 = (fn y:T1 ⇒ e1) = let rec f : T1 → T2 = {v/x}(fn y:T1 ⇒ e1) in e2) in {v/x} e2 se x̸ = f *)
      
          
exception DivZero
exception FixTypeInfer 
exception NoRuleApplies 
  
let compute (o:bop) (v1:expr) (v2:expr) =
  match (v1,v2) with
    (Nv n1, Nv n2) ->
      (match o with
         Sum -> Nv (n1+n2)
       | Sub -> Nv (n1-n2)
       | Mul -> Nv (n1*n2)
       | Div -> if n2 != 0 then Nv (n1/n2) else raise DivZero
       | Eq -> Bv(n1==n2)
       | Gt -> Bv(n1>n2)
       | Lt -> Bv(n1<n2)
       | Neq -> Bv(n1!=n2)
       | _ -> raise FixTypeInfer )
  | (Bv b1, Bv b2) ->
      (match o with
         And -> Bv(b1 && b2)
       | Or -> Bv(b1 || b2)
       | _ -> raise FixTypeInfer)
  | _ -> raise FixTypeInfer
                  
  
let rec step (e:expr) : expr =
  match e with
    Nv _ -> raise NoRuleApplies
  | Bv _ -> raise NoRuleApplies
              
  | Bop(o,v1,v2) when (value v1) && (value v2) ->
      compute o v1 v2
  | Bop(o,v1,e2) when value v1 ->
      let e2' = step e2 in Bop(o,v1,e2')
  | Bop(o,e1,v2) when value v2 ->
      let e1' = step e1 in Bop(o,e1',v2)

  | If(e1,e2,e3) -> 
      let e1' = step e1 in If(e1',e2,e3) 
        
  | Id _ -> raise NoRuleApplies
  | Fun(_,_,_) -> raise NoRuleApplies
                     
  | Let(x,_,v1,e2) when value v1 -> subs v1 x e2 (* {v1/x} e2 *)
  | Let(x,t,e1,e2) ->
      let e1' = step e1 in Let(x,t,e1',e2)
        
  | App(Fun(x,_,e1), v2) when value v2 ->
      subs v2 x e1
  | App(v1,e2) when value v1 ->
      let e2' = step e2 in App(v1,e2')
  | App(e1,e2) ->
      let e1' = step e1 in App(e1',e2)
  | _ -> raise NoRuleApplies
  
        (*| Iszero Zero -> True
         | Iszero (Succ nv) when isnumericvalue nv -> False
  | Iszero e1 ->
      let e1' = step e1 in Iszero e1'
        
  | Succ e1 -> 
      let e1' = step e1 in Succ e1' *)
        
                                                                               
let rec eval(e:expr) : expr =
  try 
    let e' = step e in eval e'
  with
    NoRuleApplies -> e 
    
    
(*===== INTEPRETADOR ========================*)

let strofexpr (_:expr) = failwith "não implementado"
let stroftipo (_:tipo) = failwith "não implementado" 
  
let interpretador (e:expr) : unit = 
  try
    let t = typeinfer []  e in
    let v = eval e in
    print_endline ((strofexpr e) ^ ":" ^ (stroftipo t) ^ 
                   " = " ^ (strofexpr v))
  with 
    TypeError -> print_endline "Erro de tipo"

(*======== TESTES ===================*) 

(* 
let a:int = 10 in
let b:int = 0 in 
a / b
*)

(* let tst1 = Let("a", Int, Nv 10, 
               Let ("b", Int, Nv 0, 
                    Bop (Div, Id "a", Id "b")))
(* *)

let dobro : int -> int = fn x:int => x * 2 in
dobro 10
*)   
  
(* let tst2 = Let("dobro", Arrow(Int,Int), Fun("x", Int, Bop(Mul, Id "x", Nv 2)), 
               App(Id "dobro", Nv 10))
    
    (* *)

let dobro : int -> int = fn x:int => x * 2 in
        dobro 
   *)   
  
(* let tst3 = Let("dobro", Arrow(Int,Int),Fun("x",Int,Bop (Mul, Id "x", Nv 2)), 
               Id "dobro") *)
 
    (* teste para subs  *)

    (*  fn x:int => x + z  *)
(* let expr1 = Fun("x",Int,  Bop(Sum, Id "x", Id "z")) *)
    
    (* let x = x + 10 in 2 * x *)
(* let expr2 = Let("x", Int, Bop(Sum, Id "x", Nv 10), 
                Bop(Mul, Nv 2, Id "x")) *)
