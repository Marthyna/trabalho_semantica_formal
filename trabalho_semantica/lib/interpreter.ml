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

module Interpreter = struct
  type bop = Sum | Sub | Mul | Div 
          | Eq | Gt | Lt | Neq
          | And | Or ;;
    
  type tipo = 
      Int (* int *)
    | Bool (* bool *)
    | Arrow of tipo * tipo (* T1 -> T2 *)
    | Maybe of tipo (* maybe T *)
    | List of tipo (* T list *) ;;
    
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
    | MatchMaybe of expr * expr * expr (* match e1 with | nothing -> e2 | just x -> e3 *) ;;
    
  (*=========== TYPEINFER ===================*)
  type tyEnv =  (string * tipo * expr option) list ;;
      
  let rec lookup (g:tyEnv) (x:string) : (tipo * expr option) option =
    match g with
      [] -> None
    | (y,t,v):: tail -> if x=y then Some(t,v) else lookup tail x ;;
    
    exception TypeError ;;
    let rec stroftipo = function
    | Int -> "int"
    | Bool -> "bool"
    | Arrow (t1, t2) -> stroftipo t1 ^ " -> " ^ stroftipo t2
    | Maybe t -> "maybe " ^ stroftipo t
    | List t -> stroftipo t ^ " list" ;;

  let rec strofexpr = function
    | Nv n  -> string_of_int n
    | Bv b -> string_of_bool b
    | True -> "true"
    | False -> "false"
    | Bop (op, e1, e2) -> 
        let op_str = match op with
          | Sum -> "+"
          | Sub -> "-"
          | Mul -> "*"
          | Div -> "/"
          | And -> "&&"
          | Or -> "||"
          | Eq -> "=="
          | Gt -> ">"
          | Lt -> "<"
          | Neq -> "!="
        in
        "(" ^ strofexpr e1 ^ " " ^ op_str ^ " " ^ strofexpr e2 ^ ")"
    | If (cond, t_branch, f_branch) -> 
        "if " ^ strofexpr cond ^ " then " ^ strofexpr t_branch ^ " else " ^ strofexpr f_branch
    | Id x -> x
    | App (e1, e2) -> strofexpr e1 ^ " " ^ strofexpr e2
    | Fun (x, _, e) -> "fn " ^ x ^ " => " ^ strofexpr e
    | Let (x, _, e1, e2) -> "let " ^ x ^ " = " ^ strofexpr e1 ^ " in " ^ strofexpr e2
    | LetRec (f, _, e1, e2) -> "let rec " ^ f ^ " = " ^ strofexpr e1 ^ " in " ^ strofexpr e2
    | Nil _ -> "[]"
    | Cons (e1, e2) -> strofexpr e1 ^ "::" ^ strofexpr e2
    | IsEmpty e -> "isempty " ^ strofexpr e
    | Hd e -> "hd " ^ strofexpr e
    | Tl e -> "tl " ^ strofexpr e
    | Match (e1, e2, x, xs, e3) -> "match " ^ strofexpr e1 ^ " with | [] -> " ^ strofexpr e2 ^ " | " ^ x ^ "::" ^ xs ^ " -> " ^ strofexpr e3
    | Nothing _ -> "nothing"
    | Just e -> "just " ^ strofexpr e
    | MatchMaybe (e1, e2, e3) -> "match " ^ strofexpr e1 ^ " with | nothing -> " ^ strofexpr e2 ^ " | just x" ^ " -> " ^ strofexpr e3 ;;

  let rec typeinfer (g:tyEnv) (e:expr) : tipo =
    match e with
      Nv _ -> Int (* Γ ⊢ n : int (Tint) *)
    | Bv _ -> Bool (* Γ ⊢ b : bool (Tbool) *)
    | True -> Bool
    | False -> Bool

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
        | Some (t, _) -> t) (* Γ(x) = T => Γ ⊢ x : T (Tvar) *)
        
    | Fun(x,t,e1) ->
        let g' = (x,t,None)::g in
        let t1 = typeinfer g' e1 in
        Arrow(t,t1) (* Γ, x : T ⊢ e : T′ => Γ ⊢ fn x : T ⇒ e : T → T′ (Tfn) *)
          
    | Let(x,t,e1,e2) ->
        let t1 = typeinfer g e1 in
        let g' = (x,t, None)::g in
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

    | LetRec(f, typ, e1, e2) ->
      begin
        match typ with
        | Arrow(t1,t2) -> 
          begin
            match e1 with
            | Fun(y, t1', e1) ->
              if t1 = t1' then
                let g' = (f, Arrow(t1, t2), None) :: (y, t1, None) :: g in
                let t1_body = typeinfer g' e1 in
                if t1_body = t2 then typeinfer ((f, Arrow(t1, t2), None) :: g) e2
                else raise TypeError
              else raise TypeError
            | _ -> raise TypeError
          end
        | _ -> raise TypeError
      end
    
    | Nil t ->
        (* Γ ⊢ nil : t list (Tnil) *)
        List t
    
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
        let t1 = typeinfer g e1 in
        let t2 = typeinfer g e2 in
        let g1 = (x, t1, None) :: (xs, List t1, None) :: g in
        let t3 = typeinfer g1 e3 in
        if t2 = t3 then t2 else raise TypeError

    | Nothing t -> Maybe t

    | Just e ->
        (match typeinfer g e with
          | t -> Maybe t)

    | MatchMaybe(_, e2, e3) ->
        let t2 = typeinfer g e2 in
        let t3 = typeinfer g e3 in
        if t2 = t3 then t2 else raise TypeError ;;       
            
          
  (* ======= AVALIADOR =========================*)

  let _value (e:expr) : bool =
    match e with
      Nv _ | Bv _ | Fun _ -> true
    | _ -> false ;;

  exception FixTypeInfer ;;
    
  let rec subs (v: expr) (x: string) (e: expr) = 
    match e with
      | Nv _ -> e (* {v/x} n = n *)
      | Bv _ -> e (* {v/x} b = b *)
      | True -> e (* {v/x} true = true *)
      | False -> e (* {v/x} false = false *)
      | Nil _ -> e (* {v/x} nil = nil *)
      | Nothing _ -> e (* {v/x} nothing = nothing *)
      | Cons(e1, e2) -> Cons(subs v x e1, subs v x e2) (* {v/x} (e1::e2) = {v/x}e1::{v/x}e2 *)
      | IsEmpty(e) -> IsEmpty(subs v x e) (* {v/x} isempty e = isempty {v/x}e *)
      | Hd(e) -> Hd(subs v x e) (* {v/x} hd e = hd {v/x}e *)
      | Tl(e) -> Tl(subs v x e) (* {v/x} tl e = tl {v/x}e *)
      | Match(e1, e2, y, ys, e3) -> Match(subs v x e1, subs v x e2, y, ys, subs v x e3) (* {v/x} match e1 with | nil -> e2 | y::ys -> e3 = match {v/x}e1 with | nil -> {v/x}e2 | y::ys -> {v/x}e3 *)
      | Just(e) -> Just(subs v x e) (* {v/x} just e = just {v/x}e *)
      | MatchMaybe(e1, e2, e3) -> MatchMaybe(subs v x e1, subs v x e2, subs v x e3) (* {v/x} match e1 with | nothing -> e2 | just z -> e3 = match {v/x}e1 with | nothing -> {v/x}e2 | just z -> {v/x}e3 *)
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
      | LetRec(f, t, e1, e2) ->
        match t with
        | Arrow(_,_) ->
          if x = f then e (* {v/f }(let rec f : T1 → T2 = (fn y:T1 ⇒ e1) = let rec f : T1 → T2 = (fn y:T1 ⇒ e1) in e2) in e2 *)
          else LetRec(f, t, subs v x e1, subs v x e2) (* {v/x}(let rec f : T1 → T2 = (fn y:T1 ⇒ e1) = let rec f : T1 → T2 = {v/x}(fn y:T1 ⇒ e1) in e2) in {v/x} e2 se x̸ = f *)
        | _ -> raise FixTypeInfer ;;
            
  exception DivZero ;;
    
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
    | _ -> raise FixTypeInfer ;;

  exception EvalError ;;

  let rec eval (env: tyEnv) (e:expr) : expr =
    match e with
    | Nv n -> Nv(n) (* n ⇓ n (bnum) *)
    | Bv b -> Bv(b) (* b ⇓ b (bbool) *)
    | True -> Bv true
    | False -> Bv false

    | Bop(o,e1,e2) ->
        let n1 = eval env e1 in
        let n2 = eval env e2 in
        compute o n1 n2 (* (e1 ⇓ n1) && (e2 ⇓ n2) && ([[n]] = [[n1]+[[n2]) => e1 + e2 ⇓ n (bop+) *)
    
    | Id x ->
      (match lookup env x with
        | Some (_,None) -> raise EvalError
        | Some (_,Some v) -> v
        | None -> raise EvalError) (* Γ(x) = v => ρ ⊢ x ⇓ v (bvar) *)
        
    | If(e1,e2,e3) ->
        let v1 = eval env e1 in
        (match v1 with
          | Bv true -> eval env e2
          | Bv false -> eval env e3
          | _ -> raise EvalError) (* (e1 ⇓ false) && (e3 ⇓ v) => ρ ⊢ if e1 then e2 else e3 ⇓ v (biff) *)

    | Fun(_,_,_) -> e (* fn x : T ⇒ e ⇓ fn x : T ⇒ e (bfn) *)

    | Let(x, _, e1, e2) ->
        let v1 = eval env e1 in
        eval env (subs v1 x e2) (* (e1 ⇓ v′) &&({v′/x} e2 ⇓ v) => let x : T = e1 in e2 ⇓ v (blet) *)

    | App(e1, e2) ->
      let f = eval env e1 in
      let v2 = eval env e2 in
      (match f with
      | Fun(x, _, e) ->
          eval env (subs v2 x e)
      | _ -> raise EvalError)
      
    | LetRec(f, t, e1, e2) ->
      (match t with
      | Arrow(_, _) ->
          let rec_env = (f, t, Some e1) :: env in
          eval rec_env e2
      | _ -> raise EvalError)                    
        
    | Nil _ -> e (* nil ⇓ nil (bnil) *)

    | Cons(e1, e2) ->
        let v1 = eval env e1 in
        let v2 = eval env e2 in
        Cons(v1, v2) (* (e1 ⇓ v1) && (e2 ⇓ v2) => e1::e2 ⇓ v1::v2 (bcons) *)
    
    | IsEmpty(e1) ->
        let v1 = eval env e1 in
        (match v1 with
          | Nil _ -> Bv true
          | Cons(_, _) -> Bv false
          | _ -> raise EvalError) (* isempty nil ⇓ true, isempty v ⇓ false for non-empty lists (bisempty) *)

    | Hd(e1) ->
        let v1 = eval env e1 in
        (match v1 with
          | Cons(hd, _) -> hd
          | _ -> raise EvalError) (* hd (v::vs) ⇓ v (bhd) *)

    | Tl(e1) ->
        let v1 = eval env e1 in
        (match v1 with
          | Cons(_, tl) -> tl
          | _ -> raise EvalError) (* tl (v::vs) ⇓ vs (btl) *)

    | Match(e1, e2, x, xs, e3) ->
        let v1 = eval env e1 in
        (match v1 with
          | Nil _ -> eval env e2
          | Cons(hd, tl) -> eval env (subs tl xs (subs hd x e3))
          | _ -> raise EvalError) (* match nil -> e2 | x::xs -> e3 (bmatchlist) *)

    | Nothing _ -> e (* nothing ⇓ nothing (bnothing) *)

    | Just(e1) ->
        let v1 = eval env e1 in
        Just v1 (* e1 ⇓ v => just e1 ⇓ just v (bjust) *)

    | MatchMaybe(e1, e2, e3) ->
        let v1 = eval env e1 in
        (match v1 with
          | Nothing _ -> eval env e2
          | Just v -> eval env (subs v "x" e3)
          | _ -> raise EvalError) ;; (* match nothing -> e2 | just x -> e3 (bmatchmaybe) *)
      
  (*===== INTEPRETADOR ========================*)
     
  let interpretador ~(env: tyEnv) (e: expr) : (string, string) result =
    try
      let t = typeinfer env e in
      let v = eval env e in
      let result = (strofexpr e) ^ ": " ^ (stroftipo t) ^ " = " ^ (strofexpr v) in
      Ok result
    with
    | TypeError -> 
        Error "Erro de tipo"
    | DivZero -> 
        Error "Divisão por zero"
    | Failure msg -> 
        Error msg ;; 
end