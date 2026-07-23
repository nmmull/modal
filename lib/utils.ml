let fresh =
  let x = ref 0 in
  fun () ->
    let _ = x := ! x + 1 in
    "$" ^ string_of_int (! x)

module type CARRIER = sig
  type t

  val zero : t
  val one : t

  val add : t -> t -> t
  val mul : t -> t -> t
  val meet : t -> t -> t

  val compare : t -> t -> int
  val equal : t -> t -> bool

  val pp : t Fmt.t
  val test : t Alcotest.testable
end
