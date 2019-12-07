module StringSet = Set.Make(String)


let create_orbit_tree =
  let rec aux (acc1, acc2) = function
    | [] -> acc1, acc2
    | (inner, outer) :: xs ->
      (* Hashtbl.remove acc inner; *)
      (* Hashtbl.remove acc outer; *)
      Hashtbl.add acc1 inner outer;
      Hashtbl.add acc2 outer inner;
      aux (acc1, acc2) xs
  in aux (Hashtbl.copy (Hashtbl.create 250000), Hashtbl.copy (Hashtbl.create 250000))


let naloga1 orbits =
  let rec aux acc = function
    | [] -> acc
    | x :: xs ->
      let outer = Hashtbl.find_all orbits x in
      (* print_endline (List.fold_left (fun acc s -> acc ^ " " ^ s) " " outer); *)
      aux (aux ((List.length outer) + acc) outer) xs
  in (* Hashtbl.length orbits + *) aux 0


(* let find_common_ancestor orbits = *)


let naloga2 children parents goal start =
  let rec aux def acc prev = function
    | [] -> def
    | x :: xs ->
      print_endline (List.fold_left (fun acc s -> acc ^ " " ^ s) "" (prev :: x :: xs));
      (* print_endline x; *)
      if x = goal
      then (print_endline (string_of_int acc); acc)
      else
        let outer = List.filter (fun el -> String.compare el prev != 0) (Hashtbl.find_all children x) in
        (* print_endline ("-> " ^ (List.fold_left (fun acc s -> acc ^ " " ^ s) " " outer)); *)
        let inner = List.filter (fun el -> String.compare el prev != 0) (Hashtbl.find_all parents x) in
        (* print_endline ("-> " ^ (List.fold_left (fun acc s -> acc ^ " " ^ s) " " inner)); *)
        aux (aux (aux def (acc) prev xs) (1+acc) x outer) (1+acc) x inner
  in aux 0 (-2) "" [start]


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
  let planets = ref StringSet.empty in
  let vsebina_datoteke =
    List.map (fun s -> 
        let xs = String.split_on_char ')' s in
        planets := StringSet.add (List.hd (List.tl xs)) (StringSet.add (List.hd xs) !planets); 
        List.hd xs, List.hd (List.tl xs)
      )
      (String.split_on_char '\n' (preberi_datoteko "day_06.in")) in
  let children, parents = (create_orbit_tree vsebina_datoteke) in
  let planet_list = List.of_seq (StringSet.to_seq !planets) in
  (* print_string (List.fold_left (fun acc s -> acc ^ " " ^ s) " " planet_list); *)
  let odgovor1 = naloga1 children planet_list
  and odgovor2 = naloga2 children parents "SAN" "YOU"
  in
  izpisi_datoteko "day_06_1.out" (string_of_int odgovor1);
  izpisi_datoteko "day_06_2.out" (string_of_int odgovor2)
