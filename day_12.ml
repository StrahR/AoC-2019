type coords = {mutable x : int; mutable y : int; mutable z : int}
type moon = {mutable pos : coords; mutable vel : coords}

let get_coords s =
  let s' = String.sub s 0 (String.length s - 1) in
  let xs = List.map (fun s -> int_of_string (List.nth (String.split_on_char '=' (String.trim s)) 1))
      (String.split_on_char ',' s') in
  {pos = {x = List.nth xs 0; y = List.nth xs 1; z = List.nth xs 2}; vel = {x = 0; y = 0; z = 0}}

let string_of_coords coords =
  "<x=" ^ string_of_int coords.x ^ ", y=" ^ string_of_int coords.y ^ ", z=" ^ string_of_int coords.z^ ">"

let print_moon moon =
  print_string ("pos=" ^ string_of_coords moon.pos);
  print_string ", ";
  print_endline ("vel=" ^ string_of_coords moon.vel)

let apply_gravity' moon moon' =
  if moon.pos.x > moon'.pos.x
  then moon.vel.x <- moon.vel.x - 1 
  else if moon.pos.x < moon'.pos.x
  then moon.vel.x <- moon.vel.x + 1;

  if moon.pos.y > moon'.pos.y
  then moon.vel.y <- moon.vel.y - 1 
  else if moon.pos.y < moon'.pos.y
  then moon.vel.y <- moon.vel.y + 1;

  if moon.pos.z > moon'.pos.z
  then moon.vel.z <- moon.vel.z - 1 
  else if moon.pos.z < moon'.pos.z
  then moon.vel.z <- moon.vel.z + 1

let apply_gravity moon m1 m2 m3 =
  apply_gravity' moon m1;
  apply_gravity' moon m2;
  apply_gravity' moon m3

let apply_velocity moon = 
  moon.pos.x <- moon.pos.x + moon.vel.x;
  moon.pos.y <- moon.pos.y + moon.vel.y;
  moon.pos.z <- moon.pos.z + moon.vel.z

let moon_copy moon =
  {pos = {x = moon.pos.x; y = moon.pos.y; z = moon.pos.z};
   vel = {x = moon.vel.x; y = moon.vel.y; z = moon.vel.z}}

let moon_potential_energy moon =
  abs moon.pos.x + abs moon.pos.y + abs moon.pos.z

let moon_kinetic_energy moon =
  abs moon.vel.x + abs moon.vel.y + abs moon.vel.z 

let moon_total_energy moon =
  moon_potential_energy moon * moon_kinetic_energy moon

let naloga1 io europa ganymede callisto =
  for i = 1 to 1000 do
    let io' = moon_copy io
    and europa' = moon_copy europa
    and ganymede' = moon_copy ganymede
    and callisto' = moon_copy callisto in
    apply_gravity io europa' ganymede' callisto';
    apply_gravity europa io' ganymede' callisto';
    apply_gravity ganymede io' europa' callisto';
    apply_gravity callisto io' europa' ganymede';
    apply_velocity io;
    apply_velocity europa;
    apply_velocity ganymede;
    apply_velocity callisto;
  done;
  print_moon io;
  print_moon europa;
  print_moon ganymede;
  print_moon callisto;
  moon_total_energy io +
  moon_total_energy europa +
  moon_total_energy ganymede +
  moon_total_energy callisto

type moon' = {mutable pos : int; mutable vel : int}

let moon_project (moon : moon) = function
  | 1 -> {pos = moon.pos.x; vel = moon.vel.x}
  | 2 -> {pos = moon.pos.y; vel = moon.vel.y}
  | _ -> {pos = moon.pos.z; vel = moon.vel.z}

let apply_gravity''' moon moon' =
  if moon.pos > moon'.pos
  then moon.vel <- moon.vel - 1 
  else if moon.pos < moon'.pos
  then moon.vel <- moon.vel + 1

let apply_gravity'' moon m1 m2 m3 =
  apply_gravity''' moon m1;
  apply_gravity''' moon m2;
  apply_gravity''' moon m3

let apply_velocity'' moon = 
  moon.pos <- moon.pos + moon.vel

let moon_copy' moon =
  {pos = moon.pos; vel = moon.vel}

let print_moon' moon =
  print_string ("pos=" ^ string_of_int moon.pos);
  print_string ", ";
  print_endline ("vel=" ^ string_of_int moon.vel)

let period io europa ganymede callisto n =
  let c = ref 0 in
  let io_p'' = moon_project io n
  and europa_p'' = moon_project europa n
  and ganymede_p'' = moon_project ganymede n
  and callisto_p'' = moon_project callisto n in
  let io_p = moon_project io n
  and europa_p = moon_project europa n
  and ganymede_p = moon_project ganymede n
  and callisto_p = moon_project callisto n in
  (* print_moon' io_p''; *)
  while !c = 0 || not ((io_p'', europa_p'', ganymede_p'', callisto_p'') = (io_p, europa_p, ganymede_p, callisto_p)) do
    incr c;
    let io_p' = moon_copy' io_p
    and europa_p' = moon_copy' europa_p
    and ganymede_p' = moon_copy' ganymede_p
    and callisto_p' = moon_copy' callisto_p in
    apply_gravity'' io_p europa_p' ganymede_p' callisto_p';
    apply_gravity'' europa_p io_p' ganymede_p' callisto_p';
    apply_gravity'' ganymede_p io_p' europa_p' callisto_p';
    apply_gravity'' callisto_p io_p' europa_p' ganymede_p';
    apply_velocity'' io_p;
    apply_velocity'' europa_p;
    apply_velocity'' ganymede_p;
    apply_velocity'' callisto_p;
    (* print_endline (string_of_int !c); *)
  done;
  !c

let rec gcd a = function
  | 0 -> a
  | b -> gcd b (a mod b)

let lcm a b = a * b / (gcd a b)

let naloga2 io europa ganymede callisto =
  let px = period io europa ganymede callisto 1 in
  let py = period io europa ganymede callisto 2 in
  let pz = period io europa ganymede callisto 3 in
  lcm (lcm px py) pz

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
  let vsebina_datoteke = String.split_on_char '\n' (preberi_datoteko "day_12.in") in
  let io = get_coords (List.nth vsebina_datoteke 0)
  and europa = get_coords (List.nth vsebina_datoteke 1)
  and ganymede = get_coords (List.nth vsebina_datoteke 2)
  and callisto = get_coords (List.nth vsebina_datoteke 3) in
  let odgovor1 = naloga1 (moon_copy io) (moon_copy europa) (moon_copy ganymede) (moon_copy callisto)
  and odgovor2 = naloga2 (moon_copy io) (moon_copy europa) (moon_copy ganymede) (moon_copy callisto)
  in
  izpisi_datoteko "day_12_1.out" (string_of_int odgovor1);
  izpisi_datoteko "day_12_2.out" (string_of_int odgovor2)
