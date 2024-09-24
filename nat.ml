type nat =
  | Z
  | S of nat

let rec add x y =
  match x with
  | Z -> y
  | S x' -> S (add x' y)

let () =
  let one = S Z in
  let two = S one in
  let three = S two in
  let four = S three in
  let five = S four in
  assert(five = add two three)

let rec even x =
  match x with
  | Z -> true
  | S Z -> false
  | S S x' -> even x'

  (* ou alors faire not du prédécesseur *)

let rec pred x =
  match x with
  | Z -> None
  | S x' -> Some x'

let rec half x =
  match x with
  | Z -> Z
  | S Z -> Z
  | S S x' -> S (half x')

let () =
  let one = S Z in
  let two = S one in
  let three = S two in
  let four = S three in
  let five = S four in
  assert(two = half five)

let rec halfwa x =
  match x with
  | None -> None
  | Some Z -> Some Z
  | Some (S Z) -> None
  | Some (S (S x')) -> (
    match halfwa (Some x') with
    | Some half_pred -> Some (S (half_pred))
    | None -> None
  )

let half_panic x =
  halfwa (Some x)