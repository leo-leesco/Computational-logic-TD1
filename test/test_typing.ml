open TD1.Typing

let () =
  let expr = If (Lt (Add (Int 1, Int 2), Int 3), Int 4, Int 5) in
  print_endline (string_of_prog expr);
  assert (typable expr);
  let mismatched = Add (Int 1, Bool true) in
  print_endline (string_of_prog mismatched);
  assert (not (typable mismatched))

let () =
  let expr =
    If (Lt (Add (Int 1, Add (Int 2, Int 3)), Int 4), Bool false, Int 5)
  in
  print_endline (string_of_prog expr);
  try
    assert (Int 5 = normalize expr);
    failwith "This assertion should have thrown a Type_error"
  with Type_error -> ()
