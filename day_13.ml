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
  else 0

let paint img p v = Hashtbl.replace img p v

let string_of_list xs =
  List.fold_left (fun acc s -> acc ^ "," ^ (string_of_int s)) (string_of_int (List.hd xs)) (List.tl xs)

let rec string_of_img img width height =
  let rec aux acc w h =
    if w = 0 && h = height then acc
    else if w = width then aux (String.concat "" [acc; "\n"]) (0) (h+1)
    else aux (String.concat "" 
                [acc;
                 (function 0 -> " " | 1 -> "|" | 2 -> "#" | 3 -> "_" | 4 -> "o" | x -> ".") (img_value img (w, h))]) (w+1) h
  in aux "" (0) (0)


let img_of_list img =
  let rec aux acc score = function
    | x :: y :: id :: xs ->
      let acc', score' =
        if x = (-1) && y = 0
        then acc, id
        else (Hashtbl.replace img (x, y) id; acc + (if id = 2 then 1 else 0), score)
      in aux acc' score' xs
    | ll -> acc, score
  in aux 0 0

let new_pos (x, y) (dx, dy) = function
  | 0 -> (x-dy, y+dx), (-dy, dx)
  | _ -> (x+dy, y-dx), (dy, -dx)

let intcode mem img =
  let rec aux rel_pos k input output =
    let opt = (get_from_mem mem k) in
    let m1 = (opt /   100) mod 10 in
    let m2 = (opt /  1000) mod 10 in
    let m3 = (opt / 10000) mod 10 in
    match opt mod 100 with
    | 1 ->
      arithmethic aux ( + ) mem rel_pos k m1 m2 m3 input output
    | 2 ->
      arithmethic aux ( * ) mem rel_pos k m1 m2 m3 input output
    | 3 ->
      if List.length input = 0 then
        (ignore (img_of_list img output);
         print_endline (string_of_img img 42 24);
         let input = 0 (* read_int () *) in
         let loc = get_loc rel_pos m1 (get_from_mem mem (k+1)) in
         Hashtbl.replace mem loc input;
         aux rel_pos (k+2) [] [])
      else (
        match input with [] -> (-2, []) | p :: input' ->
          let loc = get_loc rel_pos m1 (get_from_mem mem (k+1)) in
          Hashtbl.replace mem loc p;
          aux rel_pos (k+2) input' output
      )
    | 4 ->
      let v = get_data mem rel_pos m1 (get_from_mem mem (k+1)) in
      (* print_endline ("4: " ^ string_of_int l1 ^ " " ^ (string_of_int v)); *)
      aux rel_pos (k+2) input (output @ [v])
    | 5 ->
      jump_if aux (( != ) 0) mem rel_pos k m1 m2 input output
    | 6 ->
      jump_if aux ((  = ) 0) mem rel_pos k m1 m2 input output
    | 7 ->
      comp aux ( < ) mem rel_pos k m1 m2 m3 input output
    | 8 ->
      comp aux ( = ) mem rel_pos k m1 m2 m3 input output
    | 9 ->
      let v1 = get_data mem rel_pos m1 (get_from_mem mem (k+1)) in
      aux (rel_pos + v1) (k+2) input output
    | 99 -> (get_from_mem mem 0, output)
    | _ -> (-1, [])
  in aux 0 0

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
    (String.split_on_char ',' (preberi_datoteko "day_13.in"));
  let img1 = Hashtbl.copy (Hashtbl.create 1000000) in
  let _, output1 = intcode (Hashtbl.copy vsebina_datoteke) img1 [] [] in 
  let c1, _ = img_of_list img1 output1 in
  let img2 = Hashtbl.copy (Hashtbl.create 1000000) in
  Hashtbl.replace vsebina_datoteke 0 2;
  let _, output2 = intcode (Hashtbl.copy vsebina_datoteke) img2 []
      (* [0; 0; 0; 1; 1; 1; 0; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
         -1; -1;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         1; 1; 1; 1; 1; 1; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1;
         1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; -1; -1; -1; -1; -1; -1; -1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; -1; -1; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; 1; 1; 1; 1;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
         -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; 1; 0; 0; -1; 1; 1;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1;
         1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 1; 1;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
         -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; 0;
         0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
         1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
         ] *)
      [] in
  let _, s2 = img_of_list img1 output2 in
  (* print_endline (string_of_int (List.length output1)); *)
  (* print_endline (string_of_int (List.length output2)); *)
  (* print_endline (List.fold_left (fun acc s -> acc ^ " " ^ s) " " outer); *)
  izpisi_datoteko "day_13_1.out" (string_of_int c1);
  izpisi_datoteko "day_13_2.out" (string_of_int s2)
