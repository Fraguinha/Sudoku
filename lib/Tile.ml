type pos = int * int * int

type t =
  { pos : pos
  ; value : int
  ; original : bool
  }

let pos el = el.pos
let value el = el.value

let set el v =
  if v >= 0 && v < 10
  then
    if not el.original
    then { el with value = v }
    else raise (Invalid_argument "Cannot set the value of an original tile.")
  else raise (Invalid_argument "Values must be between [0, 9].")
;;

let make pos value original = { pos; value; original }
