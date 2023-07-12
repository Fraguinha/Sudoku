type el = Tile.t

type t =
  { board : el list
  ; decisions : el list
  ; missing : el list
  }

let print t =
  List.iter
    (fun el ->
      let line, column, _ = Tile.pos el in
      if line > 1 && column = 1
      then (
        print_newline ();
        print_int (Tile.value el);
        print_string "  ")
      else (
        print_int (Tile.value el);
        print_string "  "))
    t.board;
  print_newline ()
;;

let to_list t =
  let { board; decisions = _; missing = _ } = t in
  List.map Tile.value board
;;

let numbers = List.init 9 (fun x -> x + 1)

let valid t position value =
  let line, column, block = Tile.pos position in
  List.for_all
    (fun el ->
      let l, c, b = Tile.pos el in
      let v = Tile.value el in
      not (value = v && (line = l || column = c || block = b)))
    t.board
;;

let backtrack t =
  let missing =
    List.filter (fun el -> Tile.value el = 0) t.board
    |> List.sort (fun a b ->
         let valid_in_a = List.filter (fun i -> valid t a i) numbers in
         let valid_in_b = List.filter (fun i -> valid t b i) numbers in
         List.length valid_in_a - List.length valid_in_b)
  in
  let rec backtrack' t =
    if List.length t.missing = 0
    then t
    else (
      try
        let position = List.hd t.missing in
        let choice = List.find (fun i -> valid t position i) numbers in
        let position = Tile.set position choice in
        let board =
          List.map
            (fun el -> if Tile.pos el = Tile.pos position then position else el)
            t.board
        in
        let decisions = position :: t.decisions in
        let missing = List.tl t.missing in
        backtrack' { board; decisions; missing }
      with
      | Not_found ->
        let position = List.hd t.decisions in
        let last_choice = Tile.value position in
        let choice = List.find (fun i -> i > last_choice && valid t position i) numbers in
        let position = Tile.set position choice in
        let board =
          List.map
            (fun el -> if Tile.pos el = Tile.pos position then position else el)
            t.board
        in
        let decisions = position :: List.tl t.decisions in
        let t = { t with board; decisions } in
        backtrack' t)
  in
  backtrack' { t with missing }
;;

let solve t = backtrack t

let make list =
  if List.length list <> 9 * 9
  then raise (Invalid_argument "Invalid Sudoku Size")
  else (
    let board =
      List.mapi
        (fun i x ->
          let line = (i / 9) + 1 in
          let column = (i mod 9) + 1 in
          let block =
            match line, column with
            | l, c when l >= 1 && l <= 3 && c >= 1 && c <= 3 -> 1
            | l, c when l >= 1 && l <= 3 && c >= 4 && c <= 6 -> 2
            | l, c when l >= 1 && l <= 3 && c >= 7 && c <= 9 -> 3
            | l, c when l >= 4 && l <= 6 && c >= 1 && c <= 3 -> 4
            | l, c when l >= 4 && l <= 6 && c >= 4 && c <= 6 -> 5
            | l, c when l >= 4 && l <= 6 && c >= 7 && c <= 9 -> 6
            | l, c when l >= 7 && l <= 9 && c >= 1 && c <= 3 -> 7
            | l, c when l >= 7 && l <= 9 && c >= 4 && c <= 6 -> 8
            | l, c when l >= 7 && l <= 9 && c >= 7 && c <= 9 -> 9
            | _ -> assert false
          in
          Tile.make (line, column, block) x (x <> 0))
        list
    in
    let decisions = [] in
    let missing = List.filter (fun el -> Tile.value el = 0) board in
    { board; decisions; missing })
;;
