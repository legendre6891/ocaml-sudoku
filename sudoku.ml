(* sudoku.ml
 * 
      Imperative sudoku solver in ocaml.
  
 *)

open Printf




(* The size of each side of the board. *)
let side_size = 9

(* The size of each side of a small box. *)
let box_size = 3

(* The type of boards. *)
type board = int array array

(* Exception type for board solver. *)
exception Found of board




(* Some convenience functions for dealing with the Option type *)
(* See http://ocaml-lib.sourceforge.net/doc/Option.html *)

let get op =
  match op with 
  | None -> raise @@ Invalid_argument "get"
  | Some x -> x

let is_some op =
  match op with
  | None -> false
  | Some x -> true


let is_none op = not @@ is_some op

(*
 * Board representation:
 * -- a board is a 9x9 two-dimensional array of ints
 * -- each square contains an int between 0 and 9
 * -- 0 means that the board's real value is unknown.
 * 
 *)

let range a b =
  let rec range_helper a b l = 
    if b < a then
      l 
    else
      range_helper a (b-1) (b :: l)
  in
  range_helper a b []





(** Read in a sudoku board from a file.  Return the board. *)
let read_board filename =
  let string_to_array s =
    let arr = (Array.make side_size 0) in
    for i = 0 to (side_size-1) do
      arr.(i) <- (int_of_char s.[i]) - (int_of_char '0')
    done;
    arr
  in
  let infile = open_in filename in
  let arr = Array.make side_size (Array.make side_size 0) in
  for i = 0 to side_size-1 do
    let row = input_line infile in
    arr.(i) <- string_to_array row
  done;
  arr


(** Print a sudoku board.  Return nothing. *)
let print_board chan board =
  let print_row r = 
    for i = 0 to side_size - 1 do
      Printf.printf "%d" r.(i)
    done;
    Printf.printf "\n"
  in
  for i = 0 to side_size - 1 do
    print_row board.(i)
  done


(** Solve a sudoku board. 
    Return an option type giving the solution (a board)
    or None if no solution exists. *)
let solve_board board = 
  let board_coordinates = 
    List.map (fun x -> (x / side_size, x mod side_size)) @@ range 0 @@ side_size * side_size - 1
  in
  let find_empty_board_spots b = 
    List.filter (fun (r,c) -> (b.(r).(c) == 0)) board_coordinates
  in
  let affected_coordinates r c = 
    let same_row r1 c1 = (r == r1) in
    let same_col r1 c1 = (c == c1) in
    let same_box r1 c1 = ((r/box_size) == (r1/box_size) && (c / box_size) == (c1 / box_size)) in
    let a r1 c1 = (same_row r1 c1) || (same_col r1 c1) || (same_box r1 c1) in
    List.filter (fun (r1,c1) -> a r1 c1) board_coordinates
  in
  let delta_candidate r c n arr = 
    List.filter (fun (i,j) -> arr.{i,j,n} == 1) @@ affected_coordinates r c
  in
  let get_candidates b =
    let cand = Bigarray.Array3.create Bigarray.int8_signed Bigarray.c_layout side_size side_size side_size
    in
    begin
      Bigarray.Array3.fill cand 1;
      for r = 0 to side_size - 1 do
        for c = 0 to side_size - 1 do
          let n = b.(r).(c) in
          if n > 0 then
            begin
              let dc = delta_candidate r c (n - 1) cand in
              List.iter (fun (i,j) -> cand.{i,j,n-1} <- 0) dc;
              ();
            end          
        done;
      done;
      cand;
    end
  in
  let extract_candidates cand r c =
    List.filter (fun x -> (cand).{r,c,x} == 1) @@ range 0 8
  in
  let rec solve_board_helper b e c =
    if e == [] then
      Some b
    else
      begin
        let (i, j) = List.hd e in
        let local_candidate = extract_candidates c i j in
        let length_local_candidate = List.length local_candidate in
        try
          for x = 0 to length_local_candidate - 1 do
            let h = List.nth local_candidate x in
            let delta = (delta_candidate i j h c) in 
            begin
              List.iter (fun (i,j) -> (c.{i,j,h} <- 0)) delta;
              b.(i).(j) <- (h+1);
              let result = solve_board_helper b (List.tl e) c in
              if is_some result then
                raise @@ Found (get result)
              else
                begin
                  List.iter (fun (i,j) -> c.{i,j,h} <- 1) delta;
                  b.(i).(j) <- 0;
                end
            end
          done;
          None;
        with Found b -> Some b
      end
  in
  solve_board_helper board (find_empty_board_spots board) (get_candidates board)
                     


(** Solve a sudoku board taken from a file, and print the result. *)
let solve_board_from_file filename =
  let b = read_board filename in
  match solve_board b with
  | None   -> printf "Board has no solution.\n"
  | Some b -> print_board stdout b


(** Entry point. *)
let _ =
  if Array.length Sys.argv <> 2 then
    begin
      fprintf stderr "usage: %s board\n" Sys.argv.(0);
      exit 1
    end
  else
    begin
      solve_board_from_file Sys.argv.(1);
      exit 0
    end


