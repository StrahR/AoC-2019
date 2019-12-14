let create_dep_tree map parent =
  let rec aux = function
    | [] -> ()
    | (child, _) :: xs ->
      (* Hashtbl.remove acc parent; *)
      (* Hashtbl.remove acc child; *)
      Hashtbl.add map parent child;
      aux xs
  in aux

let top_sort map = 
  let visited = Hashtbl.copy (Hashtbl.create 100) in
  let ll = ref [] in
  let rec visit x =
    if Hashtbl.mem visited x then ()
    else (
      (* print_endline x; *)
      let deps = Hashtbl.find_all map x in
      List.iter visit deps;
      Hashtbl.add visited x true;
      ll := x :: !ll
    ) in
  let rec aux = function
    | [] -> !ll
    | x :: xs -> visit x; aux xs
  in aux (List.of_seq (Hashtbl.to_seq_keys map))

let get_order vsebina =
  let map = Hashtbl.copy (Hashtbl.create 100) in
  let rec aux = function
    | [] -> top_sort map
    | (name, (_, tl)) :: xs ->
      create_dep_tree map name tl;
      (* let string_of_list xs =
         List.fold_left (fun acc (p, c) -> acc ^ "\n" ^ (p ^ "~>" ^ c)) ("") (xs)
         in print_endline (string_of_list (List.of_seq (Hashtbl.to_seq map))); *)
      aux xs
  in aux vsebina

let needed mem material amount =
  if amount = 0 then 0
  else (
    (* print_endline material; *)
    let n', _ = List.assoc material mem
    in (n' + amount - 1) / n')

let string_of_list xs =
  List.fold_left (fun acc s -> acc ^ "," ^ (s)) ((List.hd xs)) (List.tl xs)

let split_req s =
  let offset = (if String.contains s '>' then 1 else 0) in
  let s' = String.trim (String.sub s offset (String.length s - offset)) in
  (* print_endline s'; *)
  (* print_endline (List.hd (String.split_on_char ' ' s')); *)
  (* print_endline (List.hd (List.tl (String.split_on_char ' ' s')) ); *)
  let n = int_of_string (List.hd (String.split_on_char ' ' s')) in
  let name = List.hd (List.tl (String.split_on_char ' ' s')) in
  (name, n)

let update_reqs reqs name n =
  if Hashtbl.mem reqs name
  then Hashtbl.replace reqs name (Hashtbl.find reqs name + n)
  else Hashtbl.add reqs name n

let reset_req reqs name = Hashtbl.replace reqs name 0

let amount_of_ore mem material amount =
  if amount = 0 then 0
  else (
    (* print_endline material; *)
    let n', deps = List.assoc material mem in
    if not (List.mem_assoc "ORE" deps)
    then 0
    else 
      (let m = List.assoc "ORE" deps in
       (* print_endline (material ^ string_of_int m); *)
       m * ((amount + n' - 1) / n')))

let naloga1 reqs vsebina =
  let rec aux = function
    | [] -> Hashtbl.find reqs "ORE"
    | (name : string) :: xs ->
      let n, deps = List.assoc name vsebina in
      let k = Hashtbl.find reqs name in
      let m = needed vsebina name k in
      (* if not (List.mem_assoc "ORE" deps) then reset_req reqs name; *)
      (* print_endline (string_of_int (List.length vsebina)); *)
      (* print_endline (name ^ " " ^ (string_of_int n) ^ " " ^ (string_of_int k) ^ " " ^ (string_of_int m)); *)
      List.iter (fun (name, n) -> update_reqs reqs name (n*m)) deps;
      let xs' = xs in
      aux xs'
  in aux (get_order vsebina)

let equation_of_string s =
  match String.split_on_char '=' s with
  | [] | [_] -> ("", (-1, []))
  | s' :: goal :: _ ->
    let name, n = split_req goal in
    let s'' = List.map split_req (String.split_on_char ',' s') in
    (name, (n, s''))

let naloga2 reqs vsebina cap =
  let min = ref 1000 in
  let max = ref 1000000000 in
  while !min < !max - 1 do
    let i = (!min + !max) / 2 in
    Hashtbl.replace reqs "FUEL" i;
    print_endline (string_of_int !min ^ " " ^ string_of_int !max);
    if naloga1 (Hashtbl.copy reqs) vsebina < cap
    then min := i
    else max := i
  done;
  !min

let _ =
  let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina
  and izpisi_datoteko ime_datoteke vsebina =
    let chan = open_out ime_datoteke in
    output_string chan vsebina;
    close_out chan
  in
  let reqs = Hashtbl.copy (Hashtbl.create 100000) in
  Hashtbl.add reqs "FUEL" 1;
  let vsebina_datoteke = List.map equation_of_string (String.split_on_char '\n' (preberi_datoteko "day_14.in")) @ ["ORE", (1, [])] in
  (* print_endline (string_of_list (get_order vsebina_datoteke)); *)
  let odgovor1 = naloga1 (Hashtbl.copy reqs) vsebina_datoteke in
  (* let c = (amount_of_ore vsebina_datoteke "C" 40) in
     print_endline (string_of_int c); *)
  (* let c', _ = (needed vsebina_datoteke "CA" 4) in
     print_endline (string_of_int c'); *)
  let cap = 1000000000000 in
  let odgovor2 = naloga2 (Hashtbl.copy reqs) vsebina_datoteke cap in
  (* print_endline (string_of_int (List.length output1)); *)
  (* print_endline (string_of_int (List.length output2)); *)
  (* print_endline (List.fold_left (fun acc s -> acc ^ " " ^ s) " " outer); *)
  izpisi_datoteko "day_14_1.out" (string_of_int odgovor1);
  izpisi_datoteko "day_14_2.out" (string_of_int odgovor2)
