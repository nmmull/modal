open Modal

module L = Ringoid.Linear

let check_linear = Alcotest.check L.test "same modality"

let check_mono =
  let open Mpoly.Mono (L) in
  Alcotest.check test "same monomial"

let check_poly =
  let open Mpoly.Poly (L) in
  Alcotest.check test "same polynomial"

let check_ty =
  let open Lambda_linear in
  Alcotest.check Type.test "same type"

let check_rty =
  let open Lambda_linear in
  Alcotest.(check (result Type.test string) "same type")

let linear_tests =
  let open Alcotest in
  let open L in
  [
    test_case "1 + ω" `Quick (fun () -> check_linear Omega (add One Omega));
    test_case "ω + ω" `Quick (fun () -> check_linear Omega (add Omega Omega));
    test_case "0 · ω" `Quick (fun () -> check_linear Zero (mul Zero Omega));
    test_case "0 ∧ 1" `Quick (fun () -> check_linear Omega (meet Zero One));
    test_case "1 ∧ 1" `Quick (fun () -> check_linear One (meet One One));
  ]

let mono_tests =
  let open Alcotest in
  let open Mpoly.Mono (L) in
  let test_2x2y () =
    let two = const (L.add One One) in
    let x = var "x" in
    let y = var "y" in
    let expected = mul two (mul x (mul two y)) in
    let actual = mul (mul two x) (mul two y) in
    check_mono expected actual
  in
  let test_oy () =
    let y = var "y" in
    let expected = lmul Omega y in
    let actual = mul (const Omega) y in
    check_mono expected actual
  in
  let test_yo () =
    let open Mpoly.Mono (L) in
    let y = var "y" in
    let expected = of_list [Var "y"; Const Omega]  in
    let actual = mul y (const Omega) in
    check_mono expected actual
  in
  let test_xoy () =
    let x = var "x" in
    let y = var "y" in
    let expected = mul x (lmul Omega y) in
    let actual = mul (mul x (const Omega)) y in
    check_mono expected actual
  in
  let test_zero_mid () =
    let x = var "x" in
    let y = var "y" in
    let expected = zero in
    let actual = mul (mul x y) (mul zero (mul x y)) in
    check_mono expected actual
  in
  [
    test_case "coeff 0" `Quick (fun () -> check_linear Zero (coeff zero));
    test_case "1 · 1 · 1" `Quick (fun () -> check_mono one (lmul One (lmul One one)));
    test_case "0 · 1 · 1" `Quick (fun () -> check_mono zero (lmul Zero (lmul One one)));
    test_case "(2 · x) · (2 · y)" `Quick test_2x2y;
    test_case "ω · y" `Quick test_oy;
    test_case "y · ω" `Quick test_yo;
    test_case "(x · ω) · y" `Quick test_xoy;
    test_case "(x · y) · 0 · (x · y)" `Quick test_zero_mid;
  ]

let poly_tests =
  let open Alcotest in
  let open Mpoly.Poly (L) in
  let test_one_plus_one () =
    check_poly (const (L.(add one one))) (add one one)
  in
  let test_x_plus_x () =
    check_poly
      (lmul L.Omega (var "x"))
      (add (var "x") (var "x"))
  in
  let test_x_plus_y () =
    let module M = Mpoly.Mono (L) in
    let module P = Mpoly.Poly (L) in
    check_poly
      (P.of_list [M.var "x"; M.var "y"])
      (add (var "x") (var "y"))
  in
  let test_xy_times_xy () =
    let module M = Mpoly.Mono (L) in
    let module P = Mpoly.Poly (L) in
    check_poly
      (P.of_list
         [
           M.(mul (var "x") (var "y"));
           M.(mul (var "y") (var "x"));
           M.(mul (var "x") (var "x"));
           M.(mul (var "y") (var "y"));
         ])
      (mul (add (var "x") (var "y")) (add (var "x") (var "y")))
  in
  [
    test_case "1 + 1" `Quick test_one_plus_one;
    test_case "x + x" `Quick test_x_plus_x;
    test_case "x + y" `Quick test_x_plus_y;
    test_case  "(x + y) · (x + y)" `Quick test_xy_times_xy;
  ]

let infer_tests =
  let open Alcotest in
  let open Lambda_linear in
  let infer ctxt e = Result.map fst (Lambda_linear.infer ctxt e) in
  let unit_test () =
    check_rty
      (Ok Unit)
      (infer Ctxt.empty Unit)
  in
  let var_test () =
    let ctxt =
      let open Ctxt in
      empty
      |> add_ty_var "A"
      |> add_decl "x" (Var "A")
    in
    check_rty
      (Ok (Var "A"))
      (infer ctxt (Var "x"))
  in
  let var_test_2 () =
    let ctxt =
      let open Ctxt in
      empty
      |> add_ty_var "A"
      |> add_decl "x" (Var "A")
      |> add_decl "y" (Var "A")
    in
    check_rty
      (Ok (Var "A"))
      (infer ctxt (Var "x"))
  in
  let id_test () =
    let ctxt =
      let open Ctxt in
      empty
      |> add_ty_var "A"
    in
    check_rty
      (Ok (Fun (Mexpr.one, Var "A", Var "A")))
      (infer ctxt (Lam (Mexpr.one, "x", Var "A", Var "x")))
  in
  let id_test_2 () =
    let ctxt =
      let open Ctxt in
      empty
      |> add_ty_var "A"
    in
    check_rty
      (Ok (Fun (Const Omega, Var "A", Var "A")))
      (infer ctxt (Lam (Const Omega, "x", Var "A", Var "x")))
  in
  [
    test_case "⊢ ● : unit" `Quick unit_test;
    test_case "A, x : A ⊢ x : A" `Quick var_test;
    test_case "A, x : A, y : A ⊢ x : A" `Quick var_test_2;
    test_case "A ⊢ λ (x :[1] A) x : A[1] → A" `Quick id_test;
    test_case "A ⊢ λ (x :[ω] A) x : A[ω] → A" `Quick id_test_2;
  ]


let () =
  let open Alcotest in
  run "Modal Test Suite" [
    "linear-tests", linear_tests;
    "mono-tests", mono_tests;
    "poly-tests", poly_tests;
    "infer-tests", infer_tests;
  ]
