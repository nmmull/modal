open Utils

module Ringoid = Ringoid
module Mpoly = Mpoly
module Lambda_linear = Lambda_p.Make (Ringoid.Linear)

let ex1 () =
  let open Lambda_linear in
  let e =
    let open Expr in
    LamT ( "A"
         , LamT ( "B"
                , Lam ( Const One
                      , "x"
                      , Var "A"
                      , Lam ( Const Zero
                            , "y"
                            , Var "B"
                            , Var "x"
                            )
                      )
                )
         )
  in infer Ctxt.empty e

let ex2 () =
  let open Lambda_linear in
  let e =
    let open Expr in
    LamT ( "A"
         , LamT ( "B"
                , Lam ( Const One
                      , "x"
                      , Var "A"
                      , Lam ( Const One
                            , "y"
                            , Var "B"
                            , Var "x"
                            )
                      )
                )
         )
  in infer Ctxt.empty e

let ex3 () =
  let open Lambda_linear in
  let e =
    let open Expr in
    LamT ( "A"
         , LamT ( "B"
                , Lam ( Const One
                      , "x"
                      , Var "A"
                      , Lam ( Const Omega
                            , "y"
                            , Var "B"
                            , Var "x"
                            )
                      )
                )
         )
  in infer Ctxt.empty e
