type prog =
  | Bool of bool
  | Int of int
  | Add of (prog * prog)
  | Lt of (prog * prog)
  | If of (prog * prog * prog)
  | Pair of (prog * prog)
  | Unit

type typ = TBool | TInt | TPair of (typ * typ) | TUnit

exception Type_error

let rec infer prgm =
  match prgm with
  | Bool b -> TBool
  | Int n -> TInt
  | Add (p1, p2) ->
      if infer p1 = TInt && infer p2 = TInt then TInt else raise Type_error
  | Lt (p1, p2) ->
      if infer p1 = TInt && infer p2 = TInt then TBool else raise Type_error
  | If (ass, p1, p2) ->
      let return_type = infer p1 in
      if infer ass = TBool && return_type = infer p2 then return_type
      else raise Type_error
  | Pair (p1, p2) -> TPair (infer p1, infer p2)
  | Unit -> TUnit

let typable prgm =
  try match infer prgm with _ -> true with Type_error -> false

let () =
  let expr = If (Lt (Add (Int 1, Int 2), Int 3), Int 4, Int 5) in
  assert (typable expr);
  let mismatched = Add (Int 1, Bool true) in
  assert (not (typable mismatched))

let rec reduce prgm =
  match prgm with
  | Int _ | Bool _ | Pair ((Bool _ | Int _), (Bool _ | Int _)) | Unit -> None
  | _ ->
      Some
        (match prgm with
        | Add (Int n, Int m) -> Int (n + m)
        | If (Bool cond, p1, p2) -> if cond then p1 else p2
        | Lt (Int n, Int m) -> Bool (n < m)
        | Add (p1, p2) ->
            Add
              ( (match reduce p1 with Some reduced -> reduced | None -> p1),
                match reduce p2 with Some reduced -> reduced | None -> p2 )
        | If (cond, p1, p2) ->
            If
              ( (match reduce cond with Some reduced -> reduced | None -> cond),
                p1,
                p2 )
        | Lt (p1, p2) ->
            Lt
              ( (match reduce p1 with Some reduced -> reduced | None -> p1),
                match reduce p2 with Some reduced -> reduced | None -> p2 )
        | Pair (p1, p2) ->
            Pair
              ( (match reduce p1 with Some reduced -> reduced | None -> p1),
                match reduce p2 with Some reduced -> reduced | None -> p2 )
        | p -> p)

let rec normalize prgm =
  if typable prgm then
    let reduced = reduce prgm in
    match reduced with Some value -> normalize value | None -> prgm
  else raise Type_error

let () =
  let expr =
    If (Lt (Add (Int 1, Add (Int 2, Int 3)), Int 4), Bool false, Int 5)
  in
  assert (Int 5 = normalize expr)
