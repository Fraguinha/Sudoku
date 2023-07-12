open Js_of_ocaml

let _ =
  Js.export
    "SudokuSolver"
    (object%js
       method solve input =
         let array = Js.to_array input in
         let list = Array.to_list array in
         let sudoku = Sudoku_solver.Sudoku.make list in
         let solution = Sudoku_solver.Sudoku.solve sudoku in
         let list = Sudoku_solver.Sudoku.to_list solution in
         let array = Array.of_list list in
         Js.array array
    end)
;;
