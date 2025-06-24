open TD1.Nat

let () =
  let one = S Z in
  let two = S one in
  let three = S two in
  let four = S three in
  let five = S four in
  assert (five = add two three)

let () =
  let one = S Z in
  let two = S one in
  let three = S two in
  let four = S three in
  let five = S four in
  assert (two = half five)
