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
  kappa mem rel_pos (curr+4)

let arithmethic = process

let comp kappa cond =
  process kappa (fun v1 v2 -> if cond v1 v2 then 1 else 0)

let jump_if kappa cond mem rel_pos curr mode1 mode2 =
  let v1 = get_data mem rel_pos mode1 (get_from_mem mem (curr+1)) in
  let v2 = get_data mem rel_pos mode2 (get_from_mem mem (curr+2)) in
  if cond v1
  then kappa mem rel_pos (v2)
  else kappa mem rel_pos (curr+3)

let img_value img p =
  if Hashtbl.mem img p
  then Hashtbl.find img p
  else " "

let paint img p v = Hashtbl.replace img p (v |> (function 0 -> "#" | 1 -> "." | 2 -> "O" | _ -> "s"))

let string_of_list xs =
  List.fold_left (fun acc s -> acc ^ "," ^ (string_of_int s)) (string_of_int (List.hd xs)) (List.tl xs)

let rec string_of_img img width height =
  let rec aux acc w h =
    if w = -21 && h = height then acc
    else if w = width then aux (String.concat "" [acc; "\n"]) (-21) (h+1)
    else aux (String.concat "" 
                [acc; (img_value img (w, h))]) (w+1) h
  in aux "" (-21) (-21)


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
  let i = ref (-1) in
  let rec aux mem rel_pos k (px, py) n output =
    let opt = (get_from_mem mem k) in
    let m1 = (opt /   100) mod 10 in
    let m2 = (opt /  1000) mod 10 in
    let m3 = (opt / 10000) mod 10 in
    match opt mod 100 with
    | 1 ->
      arithmethic aux ( + ) mem rel_pos k m1 m2 m3 (px, py) n output
    | 2 ->
      arithmethic aux ( * ) mem rel_pos k m1 m2 m3 (px, py) n output
    | 3 ->
      (* ignore (img_of_list img output); *)
      begin match output with
        | [] ->
          paint img (px, py) 1;
          let loc = get_loc rel_pos m1 (get_from_mem mem (k+1)) in
          if not (Hashtbl.mem img (px+1, py  )) then (Hashtbl.replace mem loc 4; aux (Hashtbl.copy mem) rel_pos (k+2) (px+1, py  ) (n+1) [] |> ignore);
          if not (Hashtbl.mem img (px  , py+1)) then (Hashtbl.replace mem loc 1; aux (Hashtbl.copy mem) rel_pos (k+2) (px  , py+1) (n+1) [] |> ignore);
          if not (Hashtbl.mem img (px-1, py  )) then (Hashtbl.replace mem loc 3; aux (Hashtbl.copy mem) rel_pos (k+2) (px-1, py  ) (n+1) [] |> ignore);
          if not (Hashtbl.mem img (px  , py-1)) then (Hashtbl.replace mem loc 2; aux (Hashtbl.copy mem) rel_pos (k+2) (px  , py-1) (n+1) [] |> ignore);
          (-14, []), !i
        | x :: output' ->
          paint img (px, py) x;
          if x = 0 then (-14, []), !i
          else if x = 2 then (print_endline (string_of_int n); i := n; (-14, []), !i)
          else (
            let loc = get_loc rel_pos m1 (get_from_mem mem (k+1)) in
            if not (Hashtbl.mem img (px+1, py  )) then (Hashtbl.replace mem loc 4; aux (Hashtbl.copy mem) rel_pos (k+2) (px+1, py  ) (n+1) output' |> ignore);
            if not (Hashtbl.mem img (px  , py+1)) then (Hashtbl.replace mem loc 1; aux (Hashtbl.copy mem) rel_pos (k+2) (px  , py+1) (n+1) output' |> ignore);
            if not (Hashtbl.mem img (px-1, py  )) then (Hashtbl.replace mem loc 3; aux (Hashtbl.copy mem) rel_pos (k+2) (px-1, py  ) (n+1) output' |> ignore);
            if not (Hashtbl.mem img (px  , py-1)) then (Hashtbl.replace mem loc 2; aux (Hashtbl.copy mem) rel_pos (k+2) (px  , py-1) (n+1) output' |> ignore);
            (-14, []), !i)
      end
    | 4 ->
      let v = get_data mem rel_pos m1 (get_from_mem mem (k+1)) in
      (* print_endline ("4: " ^ string_of_int l1 ^ " " ^ (string_of_int v)); *)
      (* print_endline "h"; *)
      aux mem rel_pos (k+2) (px, py) n (output @ [v])
    | 5 ->
      jump_if aux (( != ) 0) mem rel_pos k m1 m2 (px, py) n output
    | 6 ->
      jump_if aux ((  = ) 0) mem rel_pos k m1 m2 (px, py) n output
    | 7 ->
      comp aux ( < ) mem rel_pos k m1 m2 m3 (px, py) n output
    | 8 ->
      comp aux ( = ) mem rel_pos k m1 m2 m3 (px, py) n output
    | 9 ->
      let v1 = get_data mem rel_pos m1 (get_from_mem mem (k+1)) in
      aux mem (rel_pos + v1) (k+2) (px, py) n output
    | 99 -> (get_from_mem mem 0, output), !i
    | _ -> (-1, []), !i
  in ((aux mem 0 0 (0, 0) 0 []))


let naloga2 img =
  let rec aux acc (px, py) =
    paint img (px, py) 2;
    let m = ref acc in
    if "." = img_value img (px+1, py  ) then (m := max !m (aux (acc+1) (px+1, py  );));
    if "." = img_value img (px  , py+1) then (m := max !m (aux (acc+1) (px  , py+1);));
    if "." = img_value img (px-1, py  ) then (m := max !m (aux (acc+1) (px-1, py  );));
    if "." = img_value img (px  , py-1) then (m := max !m (aux (acc+1) (px  , py-1);));
    !m
  in aux 0 (-20, 16)

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
    (String.split_on_char ',' (preberi_datoteko "day_15.in"));
  let img1 = Hashtbl.copy (Hashtbl.create 1000000) in
  let _, c1 = intcode (Hashtbl.copy vsebina_datoteke) img1 in
  let c2 = naloga2 img1 in
  (* print_endline (string_of_int (List.length output1)); *)
  (* print_endline (string_of_int (List.length output2)); *)
  (* print_endline (List.fold_left (fun acc s -> acc ^ " " ^ s) " " outer); *)
  izpisi_datoteko "day_15_maze.out" (string_of_img img1 21 21);
  izpisi_datoteko "day_15_1.out" (string_of_int c1);
  izpisi_datoteko "day_15_2.out" (string_of_int c2)
