open Utils

module Make : functor (M : CARRIER) -> sig
  module Mexpr : sig
    type t =
      | Var of string
      | Const of M.t
      | Add of t * t
      | Mul of t * t
      | Meet of t * t

    val pp : t Fmt.t
    val test : t Alcotest.testable

    val zero : t
    val one : t

    val subst : t -> t -> string -> t
    val eval : t -> Mpoly.Make(M).t
  end

  module Mctxt : sig
    type t

    val madd : t -> t -> t
    val meet : t -> t -> t
    val lmul : Mpoly.Make(M).t -> t -> t

    val empty : t
    val singleton : string -> t
    val add : string -> Mpoly.Make(M).t -> t -> t
    val find : string -> t -> Mpoly.Make(M).t
    val remove : string -> t -> t

    val equal : t -> t -> bool
    val pp : t Fmt.t
    val test : t Alcotest.testable
  end

  module Type : sig
    type t =
      | Unit
      | Var of string
      | ForallT of string * t
      | ForallM of string * t
      | Fun of Mexpr.t * t * t

    val subst_ty : t -> t -> string -> t
    val subst_mod : t -> Mexpr.t -> string -> t

    val pp : t Fmt.t
    val test : t Alcotest.testable
  end

  module Expr : sig
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

  module Ctxt : sig
    type t

    val empty : t
    val remove : string -> t -> t

    val add_decl : string -> Type.t -> t -> t
    val add_ty_var : string -> t -> t
    val add_mod_var : string -> t -> t

    val is_ty_var : string -> t -> bool
    val is_mod_var : string -> t -> bool

    val find_decl_opt : string -> t -> Type.t option
  end

  val me_wf : ?bound:string list -> Ctxt.t -> Mexpr.t -> bool
  val ty_wf : Ctxt.t -> Type.t -> bool

  val infer : Ctxt.t -> Expr.t -> (Type.t * Mctxt.t) option
end
