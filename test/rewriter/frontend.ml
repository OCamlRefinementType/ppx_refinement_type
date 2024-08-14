open Stdlib

let[@rty] foo =
  let x = (true : int) in
  (v > 3 : int)

let foo x = x + 2;;

foo 1
