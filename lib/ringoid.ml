
module Linear = struct
  type t = Zero | One | Omega

  let zero = Zero
  let one = One

  let int_of_t = function
    | Zero -> 0
    | One -> 1
    | Omega -> 2

  let t_of_int = function
    | 0 -> Zero
    | 1 -> One
    | _ -> Omega

  let map2 f x y = t_of_int (f (int_of_t x) (int_of_t y))

  let add = map2 (+)
  let mul = map2 ( * )

  let meet x y =
    match x, y with
    | Zero, Zero -> Zero
    | One, One -> One
    | _ -> Omega

  let compare x y = Int.compare (int_of_t x) (int_of_t y)
  let equal x y = Int.equal (int_of_t x) (int_of_t y)

  let pp ppf = function
    | Zero -> Fmt.string ppf "0"
    | One -> Fmt.string ppf "1"
    | Omega -> Fmt.string ppf "ω"

  let test = Alcotest.testable pp equal
end
