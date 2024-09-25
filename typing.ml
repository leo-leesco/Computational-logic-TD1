type prog =
  | Bool of bool
  | Int of int
  | Add of (prog * prog)
  | Lt of (prog * prog)
  | If of (prog * prog * prog)

type typ =
  | TBool
  | TInt

exception Type_error

let rec infer prgm =
  match prgm with
  | Bool b -> TBool
  | Int n -> TInt
  | Add (p1,p2) -> if infer p1 = TInt && infer p2 = TInt then TInt else raise Type_error
  | Lt (p1,p2) -> if infer p1 = TInt && infer p2 = TInt then TBool else raise Type_error
  | If (ass,p1,p2) -> (
      let return_type = infer p1 in
      if infer ass = TBool && return_type = infer p2 then return_type else raise Type_error
    )

let typable prgm =
  try (
    match infer prgm with
    | _ -> true
  )
  with
  | Type_error -> false

let () =
  let expr = If (Lt (Add (Int 1,Int 2),Int 3),Int 4,Int 5) in
  assert(typable expr);
  let mismatched = Add(Int 1,Bool true) in
  assert(not (typable mismatched))

let rec reduce prgm =
  match prgm with
  | Add(Int n,Int m) -> Some(Int n+m)
  | Add(p1,Int m) -> Some Add(reduce p1,Int m)
  | Add(p1,p2) -> Some Add(p1,reduce p2)
  | If(Bool c,p1,p2)-> Some(if c then p1 else p2)
  | If(p,p1,p2) -> Some If(reduce p,p1,p2)
  | Lt(Int n,Int m) -> Some (if n<m then Bool(true) else Bool(false))
  | Lt(p1,Int m) -> Some Lt(reduce p1,Int m)
  | Lt(p1,p2) -> Some Lt(p1,reduce p2)
