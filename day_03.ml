(* module IntPairs =
   struct
   type t = int * int
   let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with
    | 0 -> Stdlib.compare y0 y1
    | c -> c
   end *)
(* module PairsMap = Hashtbl.Make(IntPairs) *)

let naloga1 points1 =
  let rec aux best = function
    | [] -> best
    | ((a, b as x), _) :: xs ->
      if x <> (0, 0) && Hashtbl.mem points1 x
      then aux (min best ((abs a) + (abs b))) xs
      else aux best xs
  in aux 1000000

let naloga2 points1 =
  let rec aux best = function
    | [] -> best
    | (x, d) :: xs ->
      if x <> (0, 0) && Hashtbl.mem points1 x
      then aux (min best ((Hashtbl.find points1 x) + d)) xs
      else aux best xs
  in aux 1000000

let _ =
  let build_list =
    let aux' acc n m k =
      if not (Hashtbl.mem acc (n, m)) then (Hashtbl.add acc (n, m) k); acc in
    let rec add_line acc k start length dir n = match dir with
      | 'R' -> if length >= 0 then add_line (aux' acc start k n) k (start+1) (length-1) dir (n+1) else acc
      | 'L' -> if length >= 0 then add_line (aux' acc start k n) k (start-1) (length-1) dir (n+1) else acc
      | 'D' -> if length >= 0 then add_line (aux' acc k start n) k (start-1) (length-1) dir (n+1) else acc
      | 'U' -> if length >= 0 then add_line (aux' acc k start n) k (start+1) (length-1) dir (n+1) else acc
      | _ -> Hashtbl.create 0
    in let rec aux acc x y n = function
        | [] -> acc
        | s :: ss ->
          let dir = String.get s 0 in
          let len = int_of_string (String.sub s 1 (String.length s - 1)) in
          (* print_endline "one line"; *)
          match dir with
          | 'R' -> aux (add_line acc y x len dir n) (x + len) y (n+len) ss
          | 'L' -> aux (add_line acc y x len dir n) (x - len) y (n+len) ss
          | 'D' -> aux (add_line acc x y len dir n) x (y - len) (n+len) ss
          | 'U' -> aux (add_line acc x y len dir n) x (y + len) (n+len) ss
          | _ -> Hashtbl.create 0
    in aux (Hashtbl.create 300000) 0 0 0
  and preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina
  and izpisi_datoteko ime_datoteke vsebina =
    let chan = open_out ime_datoteke in
    output_string chan vsebina;
    close_out chan
  in
  let vsebina_datoteke = String.split_on_char '\n' (preberi_datoteko "day_03.in") in
  let path1 = List.nth vsebina_datoteke 0 in
  let path2 = List.nth vsebina_datoteke 1 in
  let tmp = build_list (String.split_on_char ',' path1) in
  let points1 = Hashtbl.copy tmp in Hashtbl.reset tmp;
  let points2 = List.of_seq (Hashtbl.to_seq (build_list (String.split_on_char ',' path2))) in
  let odgovor1 = naloga1 points1 points2
  and odgovor2 = naloga2 points1 points2
  in
  izpisi_datoteko "day_03_1.out" (string_of_int odgovor1);
  izpisi_datoteko "day_03_2.out" (string_of_int odgovor2)
