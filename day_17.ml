let get_from_mem mem p =
  if Hashtbl.mem mem p
  then Hashtbl.find mem p
  else 0

let get_loc rel_pos mode parameter =
  if mode = 1 || mode = 0
  then parameter
  else parameter + rel_pos

let get_data mem rel_pos mode parameter =
  if mode = 1 then parameter
  else get_from_mem mem (get_loc rel_pos mode parameter)

let process kappa op mem rel_pos curr mode1 mode2 mode3 =
  let v1 = get_data mem rel_pos mode1 (get_from_mem mem (curr+1)) in
  let v2 = get_data mem rel_pos mode2 (get_from_mem mem (curr+2)) in
  let l3 = get_loc rel_pos mode3 (get_from_mem mem (curr+3)) in
  let v =  op v1 v2 in
  Hashtbl.replace mem l3 v;
  kappa rel_pos (curr+4)

let arithmethic = process

let comp kappa cond =
  process kappa (fun v1 v2 -> if cond v1 v2 then 1 else 0)

let jump_if kappa cond mem rel_pos curr mode1 mode2 =
  let v1 = get_data mem rel_pos mode1 (get_from_mem mem (curr+1)) in
  let v2 = get_data mem rel_pos mode2 (get_from_mem mem (curr+2)) in
  if cond v1
  then kappa rel_pos (v2)
  else kappa rel_pos (curr+3)

let img_value img p =
  if Hashtbl.mem img p
  then Hashtbl.find img p
  else ' '

let paint img p v = Hashtbl.replace img p v

let new_pos (x, y) (dx, dy) = function
  | 0 -> (x-dy, y+dx), (-dy, dx)
  | _ -> (x+dy, y-dx), (dy, -dx)

let update_pos (x, y) = function
  | '\n' -> (x+1, 0)
  | _ -> (x, y+1)

let intcode mem img =
  let rec aux rel_pos k p dir mode =
    let opt = (get_from_mem mem k) in
    let m1 = (opt /   100) mod 10 in
    let m2 = (opt /  1000) mod 10 in
    let m3 = (opt / 10000) mod 10 in
    match opt mod 100 with
    | 1 ->
      arithmethic aux ( + ) mem rel_pos k m1 m2 m3 p dir mode
    | 2 ->
      arithmethic aux ( * ) mem rel_pos k m1 m2 m3 p dir mode
    | 3 ->
      (* print_endline (string_of_int (List.length)); *)
      let input = img_value img p in
      let loc = get_loc rel_pos m1 (get_from_mem mem (k+1)) in
      Hashtbl.replace mem loc (int_of_char input);
      aux rel_pos (k+2) p dir mode
    | 4 ->
      let v = get_data mem rel_pos m1 (get_from_mem mem (k+1)) in
      paint img p (char_of_int v);
      let p' = update_pos p (char_of_int v) in
      (* print_endline ("4: " ^ string_of_int l1 ^ " " ^ (string_of_int v)); *)
      aux rel_pos (k+2) p' dir (not mode)
    | 5 ->
      jump_if aux (( != ) 0) mem rel_pos k m1 m2 p dir mode
    | 6 ->
      jump_if aux ((  = ) 0) mem rel_pos k m1 m2 p dir mode
    | 7 ->
      comp aux ( < ) mem rel_pos k m1 m2 m3 p dir mode
    | 8 ->
      comp aux ( = ) mem rel_pos k m1 m2 m3 p dir mode
    | 9 ->
      let v1 = get_data mem rel_pos m1 (get_from_mem mem (k+1)) in
      aux (rel_pos + v1) (k+2) p dir mode
    | 99 -> (get_from_mem mem 0)
    | _ -> (-1)
  in aux 0 0 (0, 0) (0, 1) true

let string_of_list xs =
  List.fold_left (fun acc s -> acc ^ "," ^ (string_of_int s)) (string_of_int (List.hd xs)) (List.tl xs)

let img_to_str img width height =
  let rec aux acc w h =
    if w = 0 && h = height then acc
    else if w = width then aux (String.concat "" [acc; "\n"]) (0) (h+1)
    else aux (String.concat "" [acc; Char.escaped (img_value img (w, h))]) (w+1) h
  in aux "" (0) (0)

let is_intersection img w h =
  if img_value img ((w  ), (h  )) = '#'
  && img_value img ((w+1), (h  )) = '#'
  && img_value img ((w  ), (h+1)) = '#'
  && img_value img ((w-1), (h  )) = '#'
  && img_value img ((w  ), (h-1)) = '#'
  then true else false

let naloga1 img width height =
  let rec aux acc w h =
    if w = 0 && h = height then acc
    else if w = width then aux (acc) (0) (h+1)
    else if is_intersection img w h then aux (acc + w*h) (w+1) h
    else aux acc (w+1) h
  in aux 0 (0) (0)

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
  let vsebina_datoteke = Hashtbl.copy (Hashtbl.create 100000) in
  List.iteri (fun n s -> Hashtbl.add vsebina_datoteke n (int_of_string s))
    (String.split_on_char ',' (preberi_datoteko "day_17.in"));
  let img1 = Hashtbl.copy (Hashtbl.create 1000000) in
  let _ = intcode (Hashtbl.copy vsebina_datoteke) img1
  and odgovor1 = naloga1 img1 65 47
  and img2 = Hashtbl.copy (Hashtbl.create 1000000) in
  (* Hashtbl.add img2 (0, 0) 1; *)
  let vsebina_datoteke' = Hashtbl.copy vsebina_datoteke in
  Hashtbl.replace vsebina_datoteke' 0 2;
  let _ = intcode vsebina_datoteke' img2
  in
  (* print_endline (string_of_int (List.length output1)); *)
  (* print_endline (string_of_int (List.length output2)); *)
  (* print_endline (List.fold_left (fun acc s -> acc ^ " " ^ s) " " outer); *)
  izpisi_datoteko "day_17_map.out" (img_to_str img1 65 47);
  izpisi_datoteko "day_17_1.out" (string_of_int odgovor1);
  (* izpisi_datoteko "day_17_2.out" (img_to_str img2 100 100); *)
