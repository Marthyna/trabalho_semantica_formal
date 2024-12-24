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
  | True
  | False
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
(* Γ ⊢ n : int (Tint)
   Γ ⊢ b : bool (Tbool)
   (Γ ⊢ e1 : int) && (Γ ⊢ e2 : int) => Γ ⊢ e1 + e2 : int (TOp+)
   (Γ ⊢ e1 : int) && (Γ ⊢ e2 : int) => Γ ⊢ e1 ≥ e2 : bool (TOP≥) 
   (Γ ⊢ e1 : bool) && (Γ ⊢ e2 : T) && (Γ ⊢ e3 : T) => Γ ⊢ if e1 then e2 else e3 : T (Tif)
   Γ(x) = T => Γ ⊢ x : T (Tvar)
   Γ, x : T ⊢ e : T′ => Γ ⊢ fn x : T ⇒ e : T → T′ (Tfn)
   (Γ ⊢ e1 : T → T′) && (Γ ⊢ e2 : T) => Γ ⊢ e1 e2 : T′ (Tapp)
   (Γ ⊢ e1 : T) && (Γ, x : T ⊢ e2 : T′) => Γ ⊢ let x:T = e1 in e2 : T′ (Tlet)
   (Γ, f: T1→ T2, x : T1 ⊢ e1 : T2) && 'Γ, f: T1→ T2 ⊢ e2 : T) => Γ ⊢ let rec f :T1→ T2 = (fn x:T1 ⇒ e1) in e2 : T (Tletrec)
*)
let rec typeinfer (g:tyEnv) (e:expr) : tipo =
  match e with
    Nv _ -> Int (* Γ ⊢ n : int (Tint) *)
  | Bv _ -> Bool (* Γ ⊢ b : bool (Tbool) *)

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
      ) (* (Γ ⊢ e1 : int) && (Γ ⊢ e2 : int) => Γ ⊢ e1 + e2 : int (TOp+) etc *)
    
  | If(e1,e2,e3) ->
      let t1 = typeinfer g e1 in
      if (t1=Bool) then
        let t2 = typeinfer g e2 in
        let t3 = typeinfer g e3 in
        if (t2=t3) then t2 else raise TypeError 
      else raise TypeError (* (Γ ⊢ e1 : bool) && (Γ ⊢ e2 : T) && (Γ ⊢ e3 : T) => Γ ⊢ if e1 then e2 else e3 : T (Tif) *)
      
  | Id x ->
      (match lookup g x with
         None -> raise TypeError
       | Some t -> t) (* Γ(x) = T => Γ ⊢ x : T (Tvar) *)
      
  | Fun(x,t,e1) ->
      let g' = (x,t)::g in
      let t1 = typeinfer g' e1 in
      Arrow(t,t1) (* Γ, x : T ⊢ e : T′ => Γ ⊢ fn x : T ⇒ e : T → T′ (Tfn) *)
        
  | Let(x,t,e1,e2) ->
      let t1 = typeinfer g e1 in
      let g' = (x,t)::g in
      let t2 = typeinfer g' e2 in
      if t1=t then t2 else raise TypeError (*(Γ ⊢ e1 : T) && (Γ, x : T ⊢ e2 : T′) => Γ ⊢ let x:T = e1 in e2 : T′ (Tlet) *)
          
  | App(e1,e2) ->
      let t1 = typeinfer g e1 in
      (match t1 with
         Arrow(t,t') ->
           let t2 = typeinfer g e2 in
           if t=t2 then t' else raise TypeError
       | _ -> raise TypeError
      )  (* (Γ ⊢ e1 : T → T′) && (Γ ⊢ e2 : T) => Γ ⊢ e1 e2 : T′ (Tapp) *)
      | LetRec(f, Arrow(t1, t2), Fun(y, t1', e1), e2) ->
        if t1 = t1' then
          let g' = (f, Arrow(t1, t2)) :: (y, t1) :: g in
          let t1_body = typeinfer g' e1 in
          if t1_body = t2 then typeinfer ((f, Arrow(t1, t2)) :: g) e2
          else raise TypeError
        else raise TypeError
  
    | Nil t -> List t
  
    | Cons(e1, e2) ->
        let t1 = typeinfer g e1 in
        let t2 = typeinfer g e2 in
        (match t2 with
         | List t when t = t1 -> List t
         | _ -> raise TypeError)
  
    | IsEmpty(e) ->
        (match typeinfer g e with
         | List _ -> Bool
         | _ -> raise TypeError)
  
    | Hd(e) ->
        (match typeinfer g e with
         | List t -> t
         | _ -> raise TypeError)
  
    | Tl(e) ->
        (match typeinfer g e with
         | List t -> List t
         | _ -> raise TypeError)
  
    | Match(e1, e2, x, xs, e3) ->
        (match typeinfer g e1 with
         | List t ->
             let t2 = typeinfer g e2 in
             let g' = (x, t) :: (xs, List t) :: g in
             let t3 = typeinfer g' e3 in
             if t2 = t3 then t2 else raise TypeError
         | _ -> raise TypeError)
  
    | Nothing(t) -> Maybe t
  
    | Just(e) ->
        let t = typeinfer g e in
        Maybe t
  
    | MatchMaybe(e1, e2, x, e3, y, e4) ->
        (match typeinfer g e1 with
         | Maybe t ->
             let t2 = typeinfer g e2 in
             let g1 = (x, t) :: g in
             let t3 = typeinfer g1 e3 in
             if t2 = t3 then
               let g2 = (y, Maybe t) :: g in
               let t4 = typeinfer g2 e4 in
               if t2 = t4 then t2 else raise TypeError
             else raise TypeError
         | _ -> raise TypeError)  
          
        
(* ======= AVALIADOR =========================*)

let _value (e:expr) : bool =
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
  | LetRec(f,Arrow(t1,t2),Fun(y,t3,e1),e2) ->
      if x = f then e (* {v/f }(let rec f : T1 → T2 = (fn y:T1 ⇒ e1) = let rec f : T1 → T2 = (fn y:T1 ⇒ e1) in e2) in e2 *)
      else LetRec(f,Arrow(t1,t2),Fun(y,t3,subs v x e1),subs v x e2) (* {v/x}(let rec f : T1 → T2 = (fn y:T1 ⇒ e1) = let rec f : T1 → T2 = {v/x}(fn y:T1 ⇒ e1) in e2) in {v/x} e2 se x̸ = f *)
      
          
exception DivZero
exception FixTypeInfer 
  
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

(* Semantica Operacional big step com substituicao para L1 *)
(* n ⇓ n (bnum)
   b ⇓ b (bbool)
   (e1 ⇓ false) && (e3 ⇓ v) => ρ ⊢ if e1 then e2 else e3 ⇓ v (biff)
   fn x : T ⇒ e ⇓ fn x : T ⇒ e (bfn)
   (e1 ⇓ n1) && (e2 ⇓ n2) && ([[n]] = [[n1]+[[n2]) => e1 + e2 ⇓ n (bop+)
   (e1 ⇓ true) && (e2 ⇓ v) => if e1 then e2 else e3 ⇓ v (bift)
   (e1 ⇓ v′) &&({v′/x} e2 ⇓ v) => let x : T = e1 in e2 ⇓ v (blet)
   (e1 ⇓ fn x : T ⇒ e) && (e2 ⇓ v2) && ({v2/x} e ⇓ v) => e1 e2 ⇓ v (bapp)
   {α/f } e2 ⇓ v => let rec f : T → T ′ = fn x : T ⇒ e1 in e2 ⇓ v (blrec)
     α ≡ fn x : T ⇒ let rec f : T → T ′ = (fn x : T ⇒ e1) in e1
*)
exception EvalError                                                            
let rec eval(e:expr) : expr =
  match e with
  | Nv n -> Nv(n) (* n ⇓ n (bnum) *)
  | Bv b -> Bv(b) (* b ⇓ b (bbool) *)
  | Bop(o,e1,e2) ->
      let n1 = eval e1 in
      let n2 = eval e2 in
      compute o n1 n2 (* (e1 ⇓ n1) && (e2 ⇓ n2) && ([[n]] = [[n1]+[[n2]) => e1 + e2 ⇓ n (bop+) *)
  | If(e1,e2,e3) ->
      let v1 = eval e1 in
      if v1=True then eval e2 else eval e3 (* (e1 ⇓ true) && (e2 ⇓ v) => if e1 then e2 else e3 ⇓ v (bift) *)
  | Fun(_,_,_) -> e (* fn x : T ⇒ e ⇓ fn x : T ⇒ e (bfn) *)
  | Let(x,_,e1,e2) ->
      let v1 = eval e1 in
      eval (subs v1 x e2) (* (e1 ⇓ v′) &&({v′/x} e2 ⇓ v) => let x : T = e1 in e2 ⇓ v (blet) *)
  | App(Fun(x,_,e1),e2) ->
      let f = eval e1 in 
      let v2 = eval e2 in
      eval (subs v2 x f) (* (e1 ⇓ fn x : T ⇒ e) && (e2 ⇓ v2) && ({v2/x} e ⇓ v) => e1 e2 ⇓ v (bapp) *)
  | LetRec(f, Arrow(t,t1), Fun(x, _, e1), e2) ->
      let alpha = Fun(x, t, LetRec(f, Arrow(t,t1), Fun(x, t, e1), e1)) in
      eval (subs alpha f e2) (* {α/f } e2 ⇓ v => let rec f : T → T ′ = fn x : T ⇒ e1 in e2 ⇓ v (blrec) *)
  | Nil _ -> e (* nil ⇓ nil (bnil) *)
  | Cons(e1, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      Cons(v1, v2) (* (e1 ⇓ v1) && (e2 ⇓ v2) => e1::e2 ⇓ v1::v2 (bcons) *)
  | IsEmpty(e1) ->
      let v1 = eval e1 in
      (match v1 with
        | Nil _ -> Bv true
        | Cons(_, _) -> Bv false
        | _ -> raise EvalError) (* isempty nil ⇓ true, isempty v ⇓ false for non-empty lists (bisempty) *)
  | Hd(e1) ->
      let v1 = eval e1 in
      (match v1 with
        | Cons(hd, _) -> hd
        | _ -> raise EvalError) (* hd (v::vs) ⇓ v (bhd) *)
  | Tl(e1) ->
      let v1 = eval e1 in
      (match v1 with
        | Cons(_, tl) -> tl
        | _ -> raise EvalError) (* tl (v::vs) ⇓ vs (btl) *)
  | Match(e1, e2, x, xs, e3) ->
      let v1 = eval e1 in
      (match v1 with
        | Nil _ -> eval e2
        | Cons(hd, tl) -> eval (subs tl xs (subs hd x e3))
        | _ -> raise EvalError) (* match nil -> e2 | x::xs -> e3 (bmatchlist) *)
  | Nothing _ -> e (* nothing ⇓ nothing (bnothing) *)
  | Just(e1) ->
      let v1 = eval e1 in
      Just v1 (* e1 ⇓ v => just e1 ⇓ just v (bjust) *)
  | MatchMaybe(e1, e2, x, e3, _, _) ->
      let v1 = eval e1 in
      (match v1 with
        | Nothing _ -> eval e2
        | Just(vx) -> eval (subs vx x e3)
        | _ -> raise EvalError) (* match nothing -> e2 | just x -> e3 (bmatchmaybe) *)

    
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
