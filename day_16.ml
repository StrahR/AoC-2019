let generate_seq lst =
  let rec aux acc repeat start = function
    | 0 -> acc
    | length ->
      let ll = List.length lst in
      (* print_endline (string_of_int ((start mod (ll*repeat)) / ll)); *)
      (* print_endline (string_of_int (repeat)); *)
      aux (List.nth lst ((start mod (ll*repeat)) / repeat) :: acc) repeat (start+1) (length-1)
  in aux []

let fft gen a =
  let ll = Array.length a in
  for _ = 1 to 100 do
    for i = 0 to ll-1 do
      a.(i) <- abs (List.fold_left2 (fun a b c -> a + b*c) 0 (Array.to_list a) (generate_seq gen (i+1) 1 ll)) mod 10
    done
  done

let naloga1 generator a =
  fft generator a;
  let i = ref 0 in
  for k = 0 to 7 do
    i := !i*10 + a.(k)
  done;
  !i

let naloga2 generator a =
  let offset = ref 0 in
  for k = 0 to 6 do
    offset := !offset*10 + a.(k)
  done;
  print_endline (string_of_int !offset ^ "\n" ^ string_of_int 650000);
  let a' = Array.init (Array.length a - !offset) (fun i -> a.(i + !offset)) in
  let ll = Array.length a' - 1 in
  for i = 1 to 100 do
    for k = ll downto 1 do
      a'.(k-1) <- (a'.(k) + a'.(k-1)) mod 10
    done;
  done;
  let i = ref 0 in
  for k = 0 to 7 do
    i := !i*10 + a'.(k)
  done;
  !i

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
  let vs = (preberi_datoteko "day_16.in") in
  let vsebina_datoteke = Array.init (String.length vs) (fun i -> int_of_string (Char.escaped (String.get vs i))) in
  (* let vs = List.init (String.length vsebina_datoteke) (String.get vsebina_datoteke) in *)
  let vsebina_datoteke2 = Array.init (10000 * Array.length vsebina_datoteke) (fun i -> vsebina_datoteke.(i mod (Array.length vsebina_datoteke))) in
  let odgovor1 = naloga1 [0; 1; 0; -1] vsebina_datoteke in
  print_endline (string_of_int (Array.length vsebina_datoteke2));
  let odgovor2 = naloga2 [0; 1; 0; -1] vsebina_datoteke2
  in
  izpisi_datoteko "day_16_1.out" (string_of_int odgovor1);
  izpisi_datoteko "day_16_2.out" (string_of_int odgovor2)
