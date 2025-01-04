(* interpreter.mli *)

module Interpreter : sig  
    (* ====== TYPES ====== *)
    type bop = Sum | Sub | Mul | Div | Eq | Gt | Lt | Neq | And | Or

    type tipo = 
        Int
    | Bool
    | Arrow of tipo * tipo
    | Maybe of tipo
    | List of tipo

    type expr =
        Nv of int
    | Bv of bool
    | True
    | False
    | Bop of bop * expr * expr
    | If of expr * expr * expr
    | Id of string
    | App of expr * expr
    | Fun of string * tipo * expr
    | Let of string * tipo * expr * expr
    | LetRec of string * tipo * expr * expr
    | Nil of tipo
    | Cons of expr * expr
    | IsEmpty of expr
    | Hd of expr
    | Tl of expr
    | Match of expr * expr * string * string * expr
    | Nothing of tipo
    | Just of expr
    | MatchMaybe of expr * expr * expr

    type tyEnv = (string * tipo * expr option) list

    (* ====== EXCEPTIONS ====== *)

    exception TypeError
    exception DivZero
    exception FixTypeInfer
    exception EvalError

    (* ====== FUNCTIONS ====== *)

    val lookup : tyEnv -> string -> (tipo * expr option) option

    val typeinfer : tyEnv -> expr -> tipo

    val compute : bop -> expr -> expr -> expr

    val eval : tyEnv -> expr -> expr

    val subs : expr -> string -> expr -> expr

    val strofexpr : expr -> string

    val stroftipo : tipo -> string

    val interpretador : env:tyEnv -> expr -> (string, string) result
end