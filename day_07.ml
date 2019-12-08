let intcode xs =
  let rec aux mem k xs input output =
    let opt = List.hd xs in
    let p1 = 1 = (opt /  100) mod 10 in
    let p2 = 1 = (opt / 1000) mod 10 in
    (* print_endline (string_of_int (opt mod 100)); *)
    match opt mod 100, List.tl xs with
    | 1, l1 :: l2 :: l3 :: ys ->
      let v1 = if p1 then l1 else List.nth mem l1 in
      let v2 = if p2 then l2 else List.nth mem l2 in
      let v =  v1 + v2 in
      let mem' = List.mapi (fun n x -> if n = l3 then v else x) mem in
      let ys' = if l3 > k + 3
        then List.mapi (fun n x -> if n+4 + k = l3 then v else x) ys
        else ys in
      (* print_endline ("mem: [" ^ (String.concat "; " (List.map string_of_int mem')) ^ "]");
         print_endline ("ys': [" ^ (String.concat "; " (List.map string_of_int ys')) ^ "]"); *)
      aux mem' (k+4) ys' input output
    | 2, l1 :: l2 :: l3 :: ys ->
      let v1 = if p1 then l1 else List.nth mem l1 in
      let v2 = if p2 then l2 else List.nth mem l2 in
      let v =  v1 * v2 in
      let mem' = List.mapi (fun n x -> if n = l3 then v else x) mem in
      let ys' = if l3 > k + 3
        then List.mapi (fun n x -> if n+4 + k = l3 then v else x) ys
        else ys in
      (* print_endline ("mem: [" ^ (String.concat "; " (List.map string_of_int mem')) ^ "]");
         print_endline ("ys': [" ^ (String.concat "; " (List.map string_of_int ys')) ^ "]"); *)
      aux mem' (k+4) ys' input output
    | 3, l1 :: ys ->
      begin
        (* print_endline ("input: [" ^ (String.concat "; " (List.map string_of_int input)) ^ "]"); *)
        match input with
        | [] -> -17, output
        | p :: input' -> 
          let mem' = List.mapi (fun k x -> if k = l1 then p else x) mem in
          let ys' = if l1 > k + 1
            then List.mapi (fun n x -> if n+2 + k = l1 then p else x) ys
            else ys in
          aux mem' (k+2) ys' input' output
      end
    | 4, l1 :: ys ->
      let v = if p1 then l1 else List.nth mem l1 in
      (* print_int v; print_newline(); *)
      aux mem (k+2) ys input (v :: output)
    | 5, l1 :: l2 :: ys ->
      let v1 = if p1 then l1 else List.nth mem l1 in
      let v2 = if p2 then l2 else List.nth mem l2 in
      if v1 > 0 then
        let ys' = List.mapi (fun n x -> if n < v2 then None else Some x) mem in
        let ys'' = List.filter_map (fun x -> x) ys' in
        aux mem (k+v2) ys'' input output
      else aux mem (k+3) ys input output
    | 6, l1 :: l2 :: ys ->
      let v1 = if p1 then l1 else List.nth mem l1 in
      let v2 = if p2 then l2 else List.nth mem l2 in
      if v1 = 0 then
        let ys' = List.mapi (fun n x -> if n < v2 then None else Some x) mem in
        let ys'' = List.filter_map (fun x -> x) ys' in
        aux mem (k+v2) ys'' input output
      else aux mem (k+3) ys input output
    | 7, l1 :: l2 :: l3 :: ys ->
      let v1 = if p1 then l1 else List.nth mem l1 in
      let v2 = if p2 then l2 else List.nth mem l2 in
      let v = if v1 < v2 then 1 else 0 in
      let mem' = List.mapi (fun n x -> if n = l3 then v else x) mem in
      let ys' = if l3 > k + 3
        then List.mapi (fun n x -> if n+4 + k = l3 then v else x) ys
        else ys in
      aux mem' (k+4) ys' input output
    | 8, l1 :: l2 :: l3 :: ys ->
      let v1 = if p1 then l1 else List.nth mem l1 in
      let v2 = if p2 then l2 else List.nth mem l2 in
      let v = if v1 = v2 then 1 else 0 in
      let mem' = List.mapi (fun n x -> if n = l3 then v else x) mem in
      let ys' = if l3 > k + 3
        then List.mapi (fun n x -> if n+4 + k = l3 then v else x) ys
        else ys in
      aux mem' (k+4) ys' input output
    | 99, _ -> ((List.hd mem), output)
    | _ -> (-1, [])
  in aux xs 0 xs

let ins_all_positions x l =  
  let rec aux prev acc = function
    | [] -> (prev @ [x]) :: acc |> List.rev
    | hd::tl as l -> aux (prev @ [hd]) ((prev @ [x] @ l) :: acc) tl
  in
  aux [] [] l

let rec permutations = function  
  | [] -> []
  | x::[] -> [[x]] (* we must specify this edge case *)
  | x::xs -> List.fold_left (fun acc p -> acc @ ins_all_positions x p ) [] (permutations xs)

let intcode_output data input output =
  let _, out = intcode data input output
  in List.hd out


let boost vhod = 
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (intcode_output vhod [x; acc] []) xs
  in aux 0
let find_max vhod =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (max acc (boost vhod x)) xs
  in aux 0

let naloga1 vhod =
  let ps = permutations [0; 1; 2; 3; 4] in
  find_max vhod ps

let boost_feedback vhod ps = 
  let rec aux (code, acc) = function
    | [] -> (if code >= 0 then List.hd (acc) else aux (0, List.rev (0 :: List.rev acc)) ps)
    | x :: xs -> aux (intcode vhod (x :: List.rev acc) []) xs
  in aux (0, [0]) ps

let find_max_feedback vhod =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (max acc (boost_feedback vhod x)) xs
  in aux 0

let naloga2 vhod =
  let ps = permutations [5; 6; 7; 8; 9] in
  find_max_feedback vhod ps

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
    List.rev (List.rev_map int_of_string (String.split_on_char ',' (preberi_datoteko "day_07.in"))) in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "day_07_1.out" (string_of_int odgovor1);
  izpisi_datoteko "day_07_2.out" (string_of_int odgovor2)
