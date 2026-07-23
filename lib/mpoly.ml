open Utils

module Mono (M : CARRIER) = struct
  type elt =
    | Var of string
    | Const of M.t

  let compare_elt a b =
    match a, b with
    | Const _, Var _ -> -1
    | Var _, Const _ -> 1
    | Var x, Var y -> String.compare x y
    | Const a, Const b -> M.compare a b

  let equal_elt a b = compare_elt a b = 0

  let pp_elt ppf = function
    | Var x -> Fmt.string ppf x
    | Const m -> M.pp ppf m

  type base = elt list
  type t = M.t * base

  let to_list (c, b) = Const c :: b

  let compare m n = List.compare compare_elt (to_list m) (to_list n)
  let equal x y = compare x y = 0

  let var x = M.one, [Var x]
  let const c = c, []

  let zero = const M.zero
  let one = const M.one

  let coeff (c, _) = c
  let base (_, x) = x

  let update_coeff c (_, x) =
    if M.(equal c zero)
    then zero
    else c, x

  let lmul c mono =
    if equal mono zero
    then zero
    else
      let coeff = M.mul c (coeff mono) in
      if M.(equal coeff zero)
      then zero
      else update_coeff coeff mono

  let lmul_elt elt mono =
    match elt with
    | Const m -> lmul m mono
    | Var y ->
      if equal mono zero
      then zero
      else if M.(equal (coeff mono) one)
      then M.one, Var y :: base mono
      else M.one, Var y :: Const (coeff mono) :: base mono

  let mul mono1 mono2 = lmul (coeff mono1) (List.fold_right lmul_elt (base mono1) mono2)

  let of_list elts = List.fold_right lmul_elt elts one

  let pp ppf mono =
    if M.(equal (coeff mono) zero)
    then Fmt.string ppf "0"
    else
      let elts =
        if M.(equal (coeff mono) one)
        then (base mono)
        else Const (coeff mono) :: (base mono)
      in
      Fmt.list ~sep:(Fmt.any "@;<1 0>·@ ") pp_elt ppf elts

  let test = Alcotest.testable pp equal
end

module Poly (M : CARRIER) = struct
  module Mono = Mono (M)
  module S = Set.Make (Mono)
  type t = S.t

  let compare = S.compare
  let equal = S.equal

  let var x = S.singleton (Mono.var x)
  let const c =
    if M.(equal c zero)
    then S.empty
    else S.singleton (Mono.const c)

  let of_mono m =
    if Mono.(equal m zero)
    then S.empty
    else S.singleton m

  let zero = const M.zero
  let one = const M.one

  let add p q =
    let equal_base (_, x) (_, y) = List.compare Mono.compare_elt x y = 0 in
    let combine mono added =
      match S.find_first_opt (equal_base mono) added with
      | Some m ->
        let new_coeff = M.add (Mono.coeff m) (Mono.coeff mono) in
        let added = added |> S.remove m in
        if M.(equal new_coeff zero)
        then added
        else added |> S.add (Mono.(update_coeff new_coeff mono))
      | None -> S.add mono added
    in S.fold combine p q

  let of_list ms = List.fold_right (fun m ms -> add (of_mono m) ms) ms zero

  let lmul m p =
    if M.(equal m zero)
    then zero
    else
      p
      |> S.map (Mono.lmul m)
      |> S.filter (fun m -> not Mono.(equal m zero))

  let mul p q =
    let lmul_mono m q =
      q
      |> S.map (fun n -> Mono.mul m n)
      |> S.filter (fun m -> not Mono.(equal m zero))
    in
    S.fold (fun m added -> add (lmul_mono m q) added) p zero

  let pp ppf p =
    if S.is_empty p
    then Fmt.string ppf "0"
    else Fmt.(using S.to_list (list ~sep:(any "@<1 0>+@ ") Mono.pp) ppf p)

  let test = Alcotest.testable pp equal
end

module Make (M : CARRIER) = struct
  module Mono = Mono (M)
  module Poly = Poly (M)
  module S = Set.Make (Poly)
  type t = S.t

  let compare = S.compare
  let equal = S.equal

  let var x = S.singleton (Poly.var x)
  let const c = S.singleton (Poly.const c)

  let zero = const M.zero
  let one = const M.one

  let add = S.fold (fun p -> S.map (Poly.add p))

  let lmul m q = S.map (Poly.lmul m) q
  let lmul_poly p q = S.map (Poly.mul p) q
  let mul p q = S.fold (fun r meeted -> S.union (lmul_poly r q) meeted) p S.empty

  let meet = S.union

  let lte p q = equal p (meet p q)

  let pp = Fmt.(using S.to_list ((list ~sep:(any "@;<1 0>∧@ ")) Poly.pp))
  let test = Alcotest.testable pp equal
end
