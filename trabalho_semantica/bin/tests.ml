(*======== TESTES ===================*) 
open Interpreter
open QCheck

let eval = Interpreter.eval

(* Simple assertions *)
let test_numeric_values =
  Test.make ~name:"test_numeric_values" QCheck.unit (fun () ->
    eval (Nv 42) = Nv 42
  )

let test_addition =
  Test.make ~name:"test_addition" QCheck.unit (fun () ->
    eval (Bop (Sum, Nv 1, Nv 2)) = Nv 3
  )

let test_division =
  Test.make ~name:"test_division" QCheck.unit (fun () ->
    eval (Bop (Div, Nv 6, Nv 3)) = Nv 2
  )

let test_division_by_zero =
  Test.make ~name:"test_division_by_zero" QCheck.unit (fun () ->
    try let _ = eval (Bop (Div, Nv 6, Nv 0)) in false
    with Failure _ -> true
  )

(* Boolean logic *)
let test_and_operation =
  Test.make ~name:"test_and_operation" QCheck.unit (fun () ->
    eval (Bop (And, Bv true, Bv false)) = Bv false
  )

let test_or_operation =
  Test.make ~name:"test_or_operation" QCheck.unit (fun () ->
    eval (Bop (Or, Bv false, Bv true)) = Bv true
  )

(* If-then-else *)
let test_if_true =
  Test.make ~name:"test_if_true" QCheck.unit (fun () ->
    eval (If (Bv true, Nv 1, Nv 2)) = Nv 1
  )

let test_if_false =
  Test.make ~name:"test_if_false" QCheck.unit (fun () ->
    eval (If (Bv false, Nv 1, Nv 2)) = Nv 2
  )

(* Test type errors *)
let test_type_error_sum =
  Test.make ~name:"test_type_error_sum" QCheck.unit (fun () ->
    try let _ = eval (Bop (Sum, Bv true, Nv 2)) in false
    with Failure _ -> true
  )

(* Nested arithmetic expressions *)
let test_nested_expression =
  Test.make ~name:"test_nested_expression" QCheck.unit (fun () ->
    let expr = Interpreter.Bop (Mul, Bop (Sum, Nv 1, Nv 2), Nv 3) in
    eval expr = Nv 9
  )

(* Conditional logic with arithmetic *)
let test_conditional_arithmetic =
  Test.make ~name:"test_conditional_arithmetic" QCheck.unit (fun () ->
    let expr = Interpreter.If (Bop (Gt, Nv 5, Nv 3), Nv 10, Nv 0) in
    eval expr = Nv 10
  )

(* Logical expressions with conditionals *)
let test_conditional_logic =
  Test.make ~name:"test_conditional_logic" QCheck.unit (fun () ->
    let expr = Interpreter.If (Bop (And, Bv true, Bv false), Nv 1, Nv 0) in
    eval expr = Nv 0
  )

(* Property-based tests for arithmetic *)
let test_sum_is_int =
  Test.make ~name:"test_sum_is_int" (QCheck.make (Gen.pair Gen.int Gen.int)) (fun (a, b) ->
    eval (Bop (Sum, Nv a, Nv b)) = Nv (a + b)
  )

let test_mul_is_int =
  Test.make ~name:"test_mul_is_int" (QCheck.make (Gen.pair Gen.int Gen.int)) (fun (a, b) ->
    eval (Bop (Mul, Nv a, Nv b)) = Nv (a * b)
  )

let test_gt_is_bool =
  Test.make ~name:"test_gt_is_bool" (QCheck.make (Gen.pair Gen.int Gen.int)) (fun (a, b) ->
    eval (Bop (Gt, Nv a, Nv b)) = Bv (a > b)
  )

(* Run all tests *)
let _ =
  QCheck_runner.run_tests ~verbose:true [
    test_numeric_values;
    test_addition;
    test_division;
    test_division_by_zero;
    test_and_operation;
    test_or_operation;
    test_if_true;
    test_if_false;
    test_type_error_sum;
    test_nested_expression;
    test_conditional_arithmetic;
    test_conditional_logic;
    test_sum_is_int;
    test_mul_is_int;
    test_gt_is_bool;
  ]
