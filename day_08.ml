let count s =
  let rec aux (c0, c1, c2) = function
    | [] -> (c0, c1, c2)
    | s :: ss -> 
      if s = '0' then aux (c0+1, c1, c2) ss
      else if s = '1' then aux (c0, c1+1, c2) ss
      else if s = '2' then aux (c0, c1, c2+1) ss
      else aux (c0, c1, c2) ss
  in aux (0,0,0) (List.init (String.length s) (String.get s))

let naloga1 xs size =
  let rec aux (z, acc) = function
    | "" -> acc
    | s ->
      (* print_endline s; *)
      let c0, c1, c2 = count (String.sub s 0 size) in
      if z > c0 then aux (c0, c1*c2) (String.sub s size (String.length s - size))
      else aux (z, acc) (String.sub s size (String.length s - size))
  in aux (10000000, 0) xs

let naloga2 s size =
  let img = Hashtbl.create size in
  let rec aux' k = function
    | [] -> ()
    | s :: ss ->
      if s != '2' && not (Hashtbl.mem img k)
      then Hashtbl.add img k (String.init 1 (fun _ -> if s = '0' then ' ' else '0'));
      aux' (k+1) ss in
  let rec aux = function
    | "" -> img
    | s ->
      let layer = String.sub s 0 size in
      aux' 0 (List.init (String.length layer) (String.get layer));
      aux (String.sub s size (String.length s - size))
  in aux s

let rec img_to_str img width height =
  let rec aux acc w h =
    if w = 0 && h = height then acc
    else if w = width then aux (String.concat "" [acc; "\n"]) 0 (h+1)
    else aux (String.concat "" [acc; (Hashtbl.find img (w + width*h))]) (w+1) h
  in aux "" 0 0

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
  let vsebina_datoteke = (preberi_datoteko "day_08.in") in
  (* let vs = List.init (String.length vsebina_datoteke) (String.get vsebina_datoteke) in *)
  let odgovor1 = naloga1 vsebina_datoteke (25*6)
  and odgovor2 = naloga2 vsebina_datoteke (25*6)
  in
  izpisi_datoteko "day_08_1.out" (string_of_int odgovor1);
  izpisi_datoteko "day_08_2.out" (img_to_str odgovor2 25 6)
