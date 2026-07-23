open Utils

module Make (M : CARRIER) = struct
  module P = Mpoly.Make (M)
  module Mexpr = struct
    type t =
      | Var of string
      | Const of M.t
      | Add of t * t
      | Mul of t * t
      | Meet of t * t

    let zero = Const M.zero
    let one = Const M.one

    let subst p q x =
      let rec go = function
        | Var y -> if y = x then q else Var y
        | Const c -> Const c
        | Add (e1, e2) -> Add (go e1, go e2)
        | Mul (e1, e2) -> Mul (go e1, go e2)
        | Meet (e1, e2) -> Meet (go e1, go e2)
      in go p

    let eval (p : t) : P.t =
      let rec go = function
        | Var x -> P.var x
        | Const c -> P.const c
        | Add (p, q) -> P.add (go p) (go q)
        | Mul (p, q) -> P.mul (go p) (go q)
        | Meet (p, q) -> P.meet (go p) (go q)
      in go p

    let pp = Fmt.using eval P.pp
    let test = Alcotest.of_pp pp
  end
  module Me = Mexpr

  module Mctxt = struct
    module C = Map.Make (String)
    type t = P.t C.t

    let m_of_opt = function
      | None -> P.zero
      | Some a -> a

    let opt_of_m m =
      if m = P.zero
      then None
      else Some m

    let map2 f a b = opt_of_m (f (m_of_opt a) (m_of_opt b))

    let madd = C.merge (fun x a b -> map2 P.add a b)
    let meet = C.merge (fun x a b -> map2 P.meet a b)
    let lmul m = C.filter_map (fun _ a -> map2 P.mul (Some m) (Some a))

    let find x c = m_of_opt (C.find_opt x c)
    let singleton x = C.(empty |> add x P.one)

    let empty = C.empty
    let add = C.add
    let remove = C.remove

    let equal = C.equal P.equal
    let pp = Fmt.(using C.to_list (list (pair string P.pp)))
    let test = Alcotest.testable pp equal
  end
  module Mc = Mctxt

  module Type = struct
    type t =
      | Unit
      | Var of string
      | ForallT of string * t
      | ForallM of string * t
      | Fun of Me.t * t * t

    let alpha_ty b y x=
      let rec go = function
        | Unit -> Unit
        | Var z -> if z = x then Var y else Var z
        | ForallT (z, t) ->
          if z = x
          then ForallT (z, t)
          else ForallT (z, go t)
        | ForallM (m, t) -> ForallM (m, go t)
        | Fun (p, a, b) -> Fun (p, go a, go b)
      in go b

    let alpha_mod b y x =
      let rec go = function
        | Unit -> Unit
        | Var z -> Var z
        | ForallT (z, t) -> ForallT (z, go t)
        | ForallM (m, t) ->
          if m = x
          then ForallM (m, t)
          else ForallM (m, go t)
        | Fun (p, a, b) -> Fun (Me.subst p (Var y) x, go a, go b)
      in go b

    let subst_ty b a alpha =
      let rec go = function
        | Unit -> Unit
        | Var x -> if x = alpha then a else b
        | ForallT (x, t) ->
          if x = alpha
          then ForallT (x, t)
          else
            let new_name = fresh () in
            ForallT (new_name, go (alpha_ty t new_name x))
        | ForallM (m, t) ->
          let new_name = fresh () in
          ForallM (new_name, go (alpha_mod t new_name m))
        | Fun (p, c, d) -> Fun (p, go c, go d)
      in go b

    let subst_mod b q m =
      let rec go = function
        | Unit -> Unit
        | Var x -> Var x
        | ForallT (x, t) -> ForallT (x, go t)
        | ForallM (x, t) ->
          if x = m
          then ForallM (x, t)
          else
            let new_name = fresh () in
            ForallM (new_name, go (alpha_mod t new_name x))
        | Fun (p, c, d) ->
          Fun (Me.subst p q m, go c, go d)
      in go b

    let rec pp ppf =
      let pp_parens ppf ty =
        match ty with
        | Unit | Var _ -> pp ppf ty
        | _ -> Fmt.parens pp ppf ty
      in
      let open Fmt in
      function
      | Unit -> string ppf "unit"
      | Var x -> string ppf x
      | ForallT (x, t) -> pf ppf "∀ %s . %a" x pp t
      | ForallM (x, t) -> pf ppf "∀ %s . %a" x pp t
      | Fun (m, a, b) -> pf ppf "%a[%a] → %a" pp_parens a Me.pp m pp b

    let test = Alcotest.of_pp pp
  end
  module T = Type

  module Expr = struct
    type t =
      | Var of string
      | Lam of Mexpr.t * string * Type.t * t
      | App of t * Mexpr.t * t
      | LamT of string * t
      | AppT of t * Type.t
      | LamM of string * t
      | AppM of t * Mexpr.t
      | Unit
      | LetUnit of Mexpr.t * t * t
  end
  module E = Expr

  module Ctxt = struct
    module C = Map.Make (String)
    type pred =
      | Ty of Type.t
      | IsTyVar
      | IsModVar

    type t = pred C.t

    let empty = C.empty
    let remove = C.remove

    let add_decl x a = C.add x (Ty a)
    let add_ty_var alpha = C.add alpha IsTyVar
    let add_mod_var m = C.add m IsModVar

    let is_ty_var alpha ctxt =
      match C.find_opt alpha ctxt with
      | Some IsTyVar -> true
      | _ -> false

    let is_mod_var m ctxt =
      match C.find_opt m ctxt with
      | Some IsModVar -> true
      | _ -> false

    let find_decl_opt x ctxt =
      match C.find_opt x ctxt with
      | Some (Ty a) -> Some a
      | _ -> None
  end
  module C = Ctxt

  let me_wf ?(bound=[]) ctxt m =
    let rec go : Me.t -> bool = function
      | Var x -> C.is_mod_var x ctxt || List.mem x bound
      | Const _ -> true
      | Add (p, q)
      | Mul (p, q)
      | Meet (p, q) -> go p && go q
    in go m

  let ty_wf ctxt ty =
    let rec go bound : T.t -> bool = function
      | Unit -> true
      | Var x -> C.is_ty_var x ctxt
      | ForallT (x, t) -> false
      | ForallM (m, t) -> go (m :: bound) t
      | Fun (p, a, b) ->
        me_wf ~bound ctxt p
        && go bound a
        && go bound b
    in go [] ty

  let ( let* ) = Option.bind
  let guard b = if b then Some () else None

  let rec infer ctxt (e : Expr.t) =
    match e with
    | Var x ->
      let* a = C.find_decl_opt x ctxt in
      Some (a, Mc.singleton x)
    | Lam (q, x, a, t) ->
      let ctxt = C.add_decl x a ctxt in
      let* (b, gamma) = infer ctxt t in
      let* () = guard (P.lte (Me.eval q) (Mc.find x gamma)) in
      let gamma = C.remove x gamma in
      Some (T.Fun (q, a, b), gamma)
    | App (t, q, u) ->
      begin
        match infer ctxt t, infer ctxt u with
        | Some (Fun (q', a, b), gamma), Some (a', delta) when a = a' && q = q' ->
          Some (b, Mc.(madd gamma (lmul (Me.eval q) delta)))
        | _ -> None
      end
    | LamT (alpha, t) ->
      let ctxt = C.add_ty_var alpha ctxt in
      let* (b, gamma) = infer ctxt t in
      Some (T.ForallT (alpha, b), gamma)
    | AppT (t, a) ->
      begin
        match infer ctxt t with
        | Some (T.ForallT (alpha, b), gamma) ->
          let* () = guard (ty_wf ctxt a) in
          Some (T.subst_ty b a alpha, gamma)
        | _ -> None
      end
    | LamM (m, t) ->
      let ctxt = C.add_mod_var m ctxt in
      let* (b, gamma) = infer ctxt t in
      Some (T.ForallM (m, b), gamma)
    | AppM (t, q) ->
      begin
        match infer ctxt t with
        | Some (T.ForallM (m, b), gamma) ->
          let* () = guard (me_wf ctxt q) in
          Some (T.subst_mod b q m, gamma)
        | _ -> None
      end
    | Unit -> Some (Unit, Mc.empty)
    | LetUnit (p, t, u) ->
      let* (a, gamma) = infer ctxt t in
      let* (c, delta) = infer ctxt u in
      Some (c, Mc.(madd (lmul (Me.eval p) gamma) delta))
end
