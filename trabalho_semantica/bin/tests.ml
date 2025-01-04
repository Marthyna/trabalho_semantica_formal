open Interpreter
open QCheck

let initial_env = [("y", Interpreter.Int, Some (Interpreter.Nv 0))]

let tests = [
  (* Test arithmetic operations *)
  Test.make ~name:"interpretador_arithmetic" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.Bop (Sum, Nv 1, Nv 2)) with
    | Ok result -> result = "(1 + 2): int = 3"
    | Error _ -> false
  );

  (* Test type error *)
  Test.make ~name:"interpretador_type_error" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.Bop (Sum, Bv true, Nv 2)) with
    | Error "Erro de tipo" -> true
    | _ -> false
  );

  (* Test division by zero *)
  Test.make ~name:"interpretador_division_by_zero" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.Bop (Div, Nv 6, Nv 0)) with
    | Error "Divisão por zero" -> true
    | _ -> false
  );

  (* Test boolean logic *)
  Test.make ~name:"interpretador_boolean_logic" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.Bop (And, Bv true, Bv false)) with
    | Ok result -> result = "(true && false): bool = false"
    | Error _ -> false
  );

  (* Test if-then-else *)
  Test.make ~name:"interpretador_if_then_else" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.If (Bv true, Nv 1, Nv 2)) with
    | Ok result -> result = "if true then 1 else 2: int = 1"
    | Error _ -> false
  );

  Test.make ~name:"interpretador_if_then_else_false" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.If (Bv false, Nv 1, Nv 2)) with
    | Ok result -> result = "if false then 1 else 2: int = 2"
    | Error _ -> false
  );

  Test.make ~name:"interpretador_nested_arithmetic" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.Bop (Mul, Interpreter.Bop (Sum, Nv 1, Nv 2), Nv 3)) with
    | Ok result -> result = "((1 + 2) * 3): int = 9"
    | Error _ -> false
  );

  Test.make ~name:"interpretador_if_arithmetic" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.If (Interpreter.Bop (Gt, Nv 5, Nv 3), Nv 10, Nv 0)) with
    | Ok result -> result = "if (5 > 3) then 10 else 0: int = 10"
    | Error _ -> false
  );

  Test.make ~name:"interpretador_logical_conditionals" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.If (Interpreter.Bop (And, Bv true, Bv false), Nv 1, Nv 0)) with
    | Ok result -> result = "if (true && false) then 1 else 0: int = 0"
    | Error _ -> false
  );

  Test.make ~name:"interpretador_logical_conditionals_true" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.If (Interpreter.Bop (Or, Bv true, Bv false), Nv 1, Nv 0)) with
    | Ok result -> result = "if (true || false) then 1 else 0: int = 1"
    | Error _ -> false
  );

  Test.make ~name:"interpretador_logical_conditionals_false" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.If (Interpreter.Bop (Or, Bv false, Bv false), Nv 1, Nv 0)) with
    | Ok result -> result = "if (false || false) then 1 else 0: int = 0"
    | Error _ -> false
  );

  Test.make ~name:"interpretador_id" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.Id "y") with
    | Ok result -> result = "y: int = 0"
    | Error _ -> false
  );

  Test.make ~name:"interpretador_let" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.Let ("y", Interpreter.Int, Nv 1, Interpreter.Id "y")) with
    | Ok result -> result = "let y = 1 in y: int = 1"
    | Error _ -> false
  );

  Test.make ~name:"interpretador_letrec" QCheck.unit (fun () ->
    let expr = Interpreter.LetRec(
                "f", 
                 Interpreter.Arrow(Interpreter.Int, Interpreter.Int), 
                 Interpreter.Fun(
                   "x", 
                   Interpreter.Int, 
                   Interpreter.Bop(Sum, Interpreter.Id "x", Interpreter.Nv 1)
                 ), 
                 Interpreter.App(Interpreter.Id "f", Interpreter.Nv 2)
                 ) 
    in
    match Interpreter.interpretador ~env:initial_env expr with
    | Ok result -> result = "let rec f = fn x => (x + 1) in f 2: int = 3"
    | Error _ -> false
  );

  Test.make ~name:"interpretador_nil" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.Nil (Interpreter.Int)) with
    | Ok result -> result = "[]: int list = []"
    | Error _ -> false
  );

  Test.make ~name:"interpretador_cons" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.Cons (Nv 1, Interpreter.Nil (Interpreter.Int))) with
    | Ok result -> result = "1::[]: int list = 1::[]"
    | Error _ -> false
  );

  Test.make ~name:"interpretador_is_empty" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.IsEmpty (Interpreter.Nil (Interpreter.List Interpreter.Int))) with
    | Ok result -> result = "isempty []: bool = true"
    | Error _ -> false
  );

  Test.make ~name:"interpretador_hd" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.Hd (Interpreter.Cons (Nv 1, Interpreter.Nil (Interpreter.Int)))) with
    | Ok result -> result = "hd 1::[]: int = 1"
    | Error _ -> false
  );

  Test.make ~name:"interpretador_tl" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.Tl (Interpreter.Cons (Nv 1, Interpreter.Nil (Interpreter.Int)))) with
    | Ok result -> result = "tl 1::[]: int list = []"
    | Error _ -> false
  );

  Test.make ~name:"interpretador_match" QCheck.unit (fun () ->
    let e1 = Interpreter.Cons(
      Nv 1, 
      Interpreter.Nil(Interpreter.Int)
    ) in
    let expr = Interpreter.Match(
                e1, 
                Nv 0, 
                "x", 
                "xs", 
                Interpreter.Bop(Sum, Hd(e1), Nv 0)
    )
    in
    match Interpreter.interpretador ~env:initial_env expr with
    | Ok result -> result = "match 1::[] with | [] -> 0 | x::xs -> (hd 1::[] + 0): int = 1"
    | Error _ -> false
  );

  Test.make ~name:"interpretador_nothing" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.Nothing (Interpreter.Int)) with
    | Ok result -> result = "nothing: maybe int = nothing"
    | Error _ -> false
  );

  Test.make ~name:"interpretador_just" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.Just (Nv 1)) with
    | Ok result -> result = "just 1: maybe int = just 1"
    | Error _ -> false
  );

  Test.make ~name:"interpretador_match_maybe" QCheck.unit (fun () ->
    let expr = Interpreter.MatchMaybe(
                Interpreter.Just(Nv 1), 
                Nv 0, 
                Interpreter.Bop(Sum, Nv 2, Nv 1))
    in
    match Interpreter.interpretador ~env:initial_env expr with
    | Ok result -> result = "match just 1 with | nothing -> 0 | just x -> (2 + 1): int = 3"
    | Error _ -> false
  );

  (* Test type inference *)
  Test.make ~name:"interpretador_type_inference" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.Fun ("x", Interpreter.Int, Interpreter.Bop (Sum, Interpreter.Id "x", Nv 1))) with
    | Ok result -> result = "fn x => (x + 1): int -> int = fn x => (x + 1)"
    | Error _ -> false
  );

  Test.make ~name:"interpretador_type_inference_error" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.Fun ("x", Interpreter.Int, Interpreter.Bop (Sum, Interpreter.Id "x", Bv true))) with
    | Error "Erro de tipo" -> true
    | _ -> false
  );

  (* Test substitution *)
  Test.make ~name:"interpretador_substitution" QCheck.unit (fun () ->
    match Interpreter.interpretador ~env:initial_env (Interpreter.subs (Interpreter.Bop (Sum, Nv 1, Nv 2)) "x" (Nv 3)) with
    | Ok result -> result = "3: int = 3"
    | Error _ -> false
  );

  (* fibonacci *)
  Test.make ~name:"interpretador_fibonacci" QCheck.unit (fun () ->
    let expr = Interpreter.LetRec(
                "fib", 
                 Interpreter.Arrow(Interpreter.Int, Interpreter.Int), 
                 Interpreter.Fun(
                   "n", 
                   Interpreter.Int, 
                   Interpreter.If(
                     Interpreter.Bop(Lt, Interpreter.Id "n", Nv 2),
                     Interpreter.Id "n",
                     Interpreter.Bop(
                       Sum, 
                       Interpreter.App(Interpreter.Id "fib", Interpreter.Bop(Sub, Interpreter.Id "n", Nv 1)),
                       Interpreter.App(Interpreter.Id "fib", Interpreter.Bop(Sub, Interpreter.Id "n", Nv 2))
                     )
                   )
                 ), 
                 Interpreter.App(Interpreter.Id "fib", Nv 5)
                 ) 
    in
    match Interpreter.interpretador ~env:initial_env expr with
    | Ok result -> result = "let rec fib = fn n => if (n < 2) then n else (fib (n - 1) + fib (n - 2)) in fib 5: int = 5"
    | Error _ -> false
  );

  (*  *)

]

let _ = QCheck_runner.run_tests ~verbose:true tests
