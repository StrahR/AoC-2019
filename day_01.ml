let naloga1 vsebina_datoteke =
  let rec aux acc = function
    | [] -> string_of_int acc
    | x :: xs -> aux (acc + x / 3 - 2) xs
  in aux 0 vsebina_datoteke

let naloga2 vsebina_datoteke =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (acc + if x > 8 then (aux (x / 3 - 2) [x / 3 - 2]) else 0) xs
  in aux 0 vsebina_datoteke

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
    List.rev_map int_of_string (String.split_on_char '\n' (preberi_datoteko "day_01.in")) in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "day_01_1.out" odgovor1;
  izpisi_datoteko "day_01_2.out" (string_of_int odgovor2)
