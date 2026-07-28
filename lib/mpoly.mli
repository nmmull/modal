open Utils

module Mono : functor (M : CARRIER) -> sig
  type elt =
    | Var of string
    | Const of M.t

  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : t Fmt.t
  val test : t Alcotest.testable

  val var : string -> t
  val const : M.t -> t

  val of_list : elt list -> t
  val to_list : t -> elt list

  val zero : t
  val one : t

  val coeff : t -> M.t

  val lmul : M.t -> t -> t
  val mul : t -> t -> t
end

module Poly : functor (M : CARRIER) -> sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : t Fmt.t
  val test : t Alcotest.testable

  val var : string -> t
  val const : M.t -> t
  val of_list : Mono(M).t list -> t

  val zero : t
  val one : t

  val add : t -> t -> t
  val lmul : M.t -> t -> t
  val mul : t -> t -> t
end

module Make : functor (M : CARRIER) -> sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : t Fmt.t
  val test : t Alcotest.testable

  val var : string -> t
  val const : M.t -> t

  val zero : t
  val one : t

  val add : t -> t -> t
  val lmul : M.t -> t -> t
  val mul : t -> t -> t
  val meet : t -> t -> t

  val lte : t -> t -> bool
end
