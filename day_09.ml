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

let intcode mem =
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
      begin
        match input with
        | [] -> -17, output
        | p :: input' ->
          let loc = get_loc rel_pos m1 (get_from_mem mem (k+1)) in
          (* print_endline ("3: " ^ string_of_int loc ^ " " ^ (string_of_int p)); *)
          Hashtbl.replace mem loc p;
          aux rel_pos (k+2) input' output
      end
    | 4 ->
      let v = get_data mem rel_pos m1 (get_from_mem mem (k+1)) in
      (* print_endline ("4: " ^ string_of_int l1 ^ " " ^ (string_of_int v)); *)
      aux rel_pos (k+2) input (v :: output)
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
    | 99 -> ((get_from_mem mem 0), List.rev output)
    | _ -> (-1, [])
  in aux 0 0

let string_of_list xs =
  List.fold_left (fun acc s -> acc ^ "," ^ (string_of_int s)) (string_of_int (List.hd xs)) (List.tl xs)

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
  let vsebina_datoteke = Hashtbl.create 100000 in
  List.iteri (fun n s -> Hashtbl.add vsebina_datoteke n (int_of_string s))
    (String.split_on_char ',' (preberi_datoteko "day_09.in"));
  let odgovor1, output1 = intcode (Hashtbl.copy vsebina_datoteke) [1] []
  and odgovor2, output2 = intcode (Hashtbl.copy vsebina_datoteke) [2] []
  in
  (* print_endline (string_of_int (List.length output1)); *)
  (* print_endline (string_of_int (List.length output2)); *)
  (* print_endline (List.fold_left (fun acc s -> acc ^ " " ^ s) " " outer); *)
  izpisi_datoteko "day_09_1.out" (string_of_list output1);
  izpisi_datoteko "day_09_2.out" (string_of_list output2);
