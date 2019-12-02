let naloga1 xs =
  let rec aux mem k = function
    | 1 :: l1 :: l2 :: l3 :: ys ->
      let v = (List.nth mem l1) + (List.nth mem l2) in
      let mem' = List.mapi (fun n x -> if n = l3 then v else x) mem in
      let ys' = if l3 > k + 3
        then List.mapi (fun n x -> if n+4 + k = l3 then v else x) ys
        else ys in
      (* print_endline ("mem: [" ^ (String.concat "; " (List.map string_of_int mem')) ^ "]");
         print_endline ("ys': [" ^ (String.concat "; " (List.map string_of_int ys')) ^ "]"); *)
      aux mem' (k+4) ys'
    | 2 :: l1 :: l2 :: l3 :: ys ->
      let v = (List.nth mem l1) * (List.nth mem l2) in
      let mem' = List.mapi (fun n x -> if n = l3 then v else x) mem in
      let ys' = if l3 > k + 3
        then List.mapi (fun n x -> if n+4 + k = l3 then v else x) ys
        else ys in
      (* print_endline ("mem: [" ^ (String.concat "; " (List.map string_of_int mem')) ^ "]");
         print_endline ("ys': [" ^ (String.concat "; " (List.map string_of_int ys')) ^ "]"); *)
      aux mem' (k+4) ys'
    | 99 :: _ -> List.hd mem
    | _ -> -1
  in aux xs 0 xs


let naloga2 vsebina_datoteke =
  let list = List.init 100 (fun x -> x) in
  let rec aux ys = function
    | [] -> aux (List.tl ys) list
    | x :: xs ->
      if naloga1 ((List.hd vsebina_datoteke) :: (List.hd ys) :: x :: (List.tl (List.tl (List.tl vsebina_datoteke)))) <> 19690720
      then aux ys xs
      else 100*(List.hd ys) + x
  in aux list list

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
  let vsebina_datoteke =
    List.rev (List.rev_map int_of_string (String.split_on_char ',' (preberi_datoteko "day_02.in"))) in
  let odgovor1 = naloga1 (if List.length vsebina_datoteke = 0 then []
                          else (List.hd vsebina_datoteke) :: 12 :: 2 :: (List.tl (List.tl (List.tl vsebina_datoteke))))
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "day_02_1.out" (string_of_int odgovor1);
  izpisi_datoteko "day_02_2.out" (string_of_int odgovor2)
