type nat = Z | S of nat

let rec add x y = match x with Z -> y | S x' -> S (add x' y)
let rec even x = match x with Z -> true | S Z -> false | S (S x') -> even x'

(* ou alors faire NOT du prédécesseur *)

let pred x = match x with Z -> None | S x' -> Some x'
let rec half x = match x with Z -> Z | S Z -> Z | S (S x') -> S (half x')

let rec halfway x =
  match x with
  | None -> None
  | Some Z -> Some Z
  | Some (S Z) -> None
  | Some (S (S x')) -> (
      match halfway (Some x') with
      | Some half_pred -> Some (S half_pred)
      | None -> None)

let half_panic x = halfway (Some x)
