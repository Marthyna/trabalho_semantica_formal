open Interpreter
open QCheck

let initial_env = [("x", Interpreter.Int)]

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
    match Interpreter.interpretador ~env:initial_env (Interpreter.Id "x") with
    | Ok result -> result = "x: int = 0"
    | Error _ -> false
  );

  (* Test nested arithmetic expression *)
]

let _ = QCheck_runner.run_tests ~verbose:true tests
