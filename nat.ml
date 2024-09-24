type nat =
  | Z
  | S of nat


let rec add x y =
  match x with
  | Z -> y
  | S x' -> S (add x' y)

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

let half_panic x =
  halfwa Some x

let rec halfwa x =
  match x with
  | None -> None
  | Some Z -> Some Z
  | S Z -> None
  | Some S S x' -> Some S (halfwa (Some x'))