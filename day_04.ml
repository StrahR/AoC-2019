let is_good n =
  let rec aux acc acc' = function
    | [] | [_] -> acc && acc'
    | x :: (y :: ys as xs) -> aux (acc || x = y) (acc' && x <= y) xs
  in aux false true (List.of_seq (String.to_seq n))
let naloga1 n m =
  let nums = List.init (m-n+1) (fun x -> string_of_int (x + n)) in
  let good = List.filter is_good nums in
  List.length good
let is_good' n =
  let rec aux acc acc' k = function
    | [] | [_] -> acc && acc'
    | x :: (y :: z :: ys as xs) when x = y && y = z -> aux acc (acc' && x <= y) x xs
    | x :: (y :: ys as xs) ->
      if x = k
      then aux acc (acc' && x <= y) x xs
      else aux (acc || x = y) (acc' && x <= y) ' ' xs
  in aux false true ' ' (List.of_seq (String.to_seq n))
let naloga2 n m =
  let nums = List.init (m-n+1) (fun x -> string_of_int (x + n)) in
  let good = List.filter is_good' nums in
  List.length good

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
    List.rev_map int_of_string (String.split_on_char '-' (preberi_datoteko "day_04.in")) in
  let n = List.nth vsebina_datoteke 1 in
  let m = List.nth vsebina_datoteke 0 in
  let odgovor1 = naloga1 n m
  and odgovor2 = naloga2 n m
  in
  izpisi_datoteko "day_04_1.out" (string_of_int odgovor1);
  izpisi_datoteko "day_04_2.out" (string_of_int odgovor2)
