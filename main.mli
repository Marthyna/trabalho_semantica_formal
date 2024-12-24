type bop = Sum | Sub | Mul | Div | Eq | Gt | Lt | Neq | And | Or
type tipo = Int | Bool | Arrow of tipo * tipo
type expr =
    Nv of int
  | Bv of bool
  | Bop of bop * expr * expr
  | If of expr * expr * expr
  | Id of string
  | Fun of string * tipo * expr
  | Let of string * tipo * expr * expr
  | App of expr * expr
type tyEnv = (string * tipo) list
val lookup : tyEnv -> string -> tipo option
exception TypeError
val typeinfer : tyEnv -> expr -> tipo
val value : expr -> bool
exception NoRuleApplies
val subs : expr -> string -> expr -> expr
exception DivZero
exception FixTypeInfer
exception NoRuleApplies
val compute : bop -> expr -> expr -> expr
val step : expr -> expr
val eval : expr -> expr
val strofexpr : expr -> 'a
val stroftipo : tipo -> 'a
val interpretador : expr -> unit
val tst1 : expr
val tst2 : expr
val tst3 : expr
val expr1 : expr
val expr2 : expr
