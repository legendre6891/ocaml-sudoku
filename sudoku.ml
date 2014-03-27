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


(** range a b 
 *
 * For integers a, and b:
 *
 * returns a list [a, ..., b]
 * if a <= b
 *
 * [] if b > a
 **)
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
  (** given a string, return a int array
   * that are the digits of the string 
   *
   * e.g. "123" -> [|1;2;3|]
   **)
  let string_to_array s =
    let arr = (Array.make side_size 0) in
    for i = 0 to (side_size-1) do
      arr.(i) <- (int_of_char s.[i]) - (int_of_char '0')
    done;
    arr
  in
  let infile = open_in filename in
  begin
    let arr = Array.make side_size (Array.make side_size 0) in

    (* read in the 9 lines and use string_to_array
     * to return a board
     *)
    for i = 0 to side_size-1 do
      let row = input_line infile in
      arr.(i) <- string_to_array row
    done;
    close_in infile;
    arr;
  end


(** Print a sudoku board.  Return nothing. *)
let print_board chan board =
  (* Given an int array, print its elements and a new line
   * e.g.
   * [|1;2;3|] --> print "123\n"
   * *)
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

  (* this value is equal to the (int * int) List of 
   * all board coordinates A x A, where
   * A = [0, ..., 9]
   *
   * i.e. board_coordinates = [(0,0); (0,1); ...; (8,8)] 
   * 
   *
   * It is used as an argument in List.iter, List.map, etc.*)
  let board_coordinates = 
    List.map (fun x -> (x / side_size, x mod side_size)) @@ range 0 @@ side_size * side_size - 1
  in

  (* Given a board b (i.e. a 9x9 "matrix" of integers), return
   * the set of all (i,j) such that b_ij = 0 *)
  let find_empty_board_spots b = 
    List.filter (fun (r,c) -> (b.(r).(c) == 0)) board_coordinates
  in

  (* Given a coordinate (r,c), with (0 <= r,c < 9), 
   * return a list A consisting of those coordinates (i,j)
   * such that (r,c) is in the same row, or same column, or same 3x3 box
   * as (i,j)
   *)
  let affected_coordinates r c = 
    let same_row r1 c1 = (r == r1) in
    let same_col r1 c1 = (c == c1) in
    let same_box r1 c1 = ((r/box_size) == (r1/box_size) && (c / box_size) == (c1 / box_size)) in
    let a r1 c1 = (same_row r1 c1) || (same_col r1 c1) || (same_box r1 c1) in
    List.filter (fun (r1,c1) -> a r1 c1) board_coordinates
  in

  (* Given:
    * a coordinate (r,c)
    * a digit n, 1 <= n <= 9
    * a candidate array (see below) arr
    
  * Return a list of coordinates (i,j) such that
  * the digit n is still allowed (see below) 
  * with respect to the candidate array.
  *)
  let delta_candidate r c n arr = 
    List.filter (fun (i,j) -> arr.{i,j,n} == 1) @@ affected_coordinates r c
  in

  (* Given a board b, return a candidate array.
   *
   *
   * A candidate array is a triply indexed matrix A_ijn such that
   * 
   * A_ijn = 0, 1. for 0 <= i,j < 9, 1 <= n <= 9
   *
   * When A_ijn = 1, we say that "the digit n is _allowed in the coordinate (i,j)"
   * Otherwise, we say that "n is disallowed ...". 
   *
   * This function returns a candidate array, such A_ijn = 1 if and only if 
   * none of coordinates (r,c), where (r,c) is in the same row/column/box as
   * (i,j) is filled with the digit n.
   *
   *
   * The candidate array is implemented as a 3D Array from the Bigarray library.
   *)
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
  (* For a given coordinate (r,c) and a candidate array cand,
   * return the set of all digits n such that n is allowed
   * at the coordinate (r,c) *)
  let extract_candidates cand r c =
    List.filter (fun x -> (cand).{r,c,x} == 1) @@ range 0 (side_size - 1)
  in


  (* Given a board b, the list e = {(r,c)} where b(r,c) is not yet filled
   * and a candidate array c, 
   * solve the board b (i.e. return Some b', where b' is a board that is equal
   * to a solution b' of b if a solution exists, or None otherwise).
   *
   *
   * Here is the general algorithm:
   * 
   *
   * If the board has no empty spots, then the board is solved already -- return
   * b.
   *
   *
   * If not, take e1 to be the first (any) empty spot, and set 
   * e1 to be any of the digits allowed at e1, according to the 
   * candidate array. Then call solve_board_helper with
   *
   * b' e' c'
   *
   *
   * where b' is the board b with the addition of new digit at e1
   * e' is e without e1
   * c' is an updated candidate array, the effect of having the new digit at e1
   *
   * Do this for every allowed digit at e1. (Resetting b' = b, c' = c
   * after each attempt.)
   *
   * If no digit is allowed at e1, then we know that the board is invalid.
   *
   * Whenever one allowed digit at e1 returns a valid board, then we solved the
   * board.
   *
   * Note that the invariant is: b is always a valid board.
   *)
  let rec solve_board_helper b e c =
    if e == [] then
      Some b
    else
      begin
        (* e1 = (i,j) *)
        let (i, j) = List.hd e in

        (* local_candidate is the set of all allowed digits at e1 *)
        let local_candidate = extract_candidates c i j in
        let length_local_candidate = List.length local_candidate in
        try
          for x = 0 to length_local_candidate - 1 do
            let h = List.nth local_candidate x in

            (* delta is the change in the candidate array
             * in response to a new digit at e1 = (i,j) ).*)
            let delta = (delta_candidate i j h c) in 
            begin
              (* calculate b' and c' (see above) *)
              List.iter (fun (i,j) -> (c.{i,j,h} <- 0)) delta;
              b.(i).(j) <- (h+1);
              (* try to solve the new board *)
              let result = solve_board_helper b (List.tl e) c in
              if is_some result then
                raise @@ Found (get result)
              else
                begin
                  (* reset b' = b, c' = c (see above)*)
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


