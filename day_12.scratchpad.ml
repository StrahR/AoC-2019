

let naloga2 io europa ganymede callisto =
  let c = ref 0 in
  let io'' = moon_copy io
  and europa'' = moon_copy europa
  and ganymede'' = moon_copy ganymede
  and callisto'' = moon_copy callisto in
  while !c = 0 || not ((io'', europa'', ganymede'', callisto'') = (io, europa, ganymede, callisto)) do
    incr c;
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
    (* print_endline (string_of_int !c); *)
    (* if !c = 2772 then
       (print_moon io;
       print_moon io'';
       print_endline (string_of_bool (io = io''));
       print_moon europa;
       print_moon europa'';
       print_endline (string_of_bool (europa = europa''));
       print_moon ganymede;
       print_moon ganymede'';
       print_endline (string_of_bool (ganymede = ganymede''));
       print_moon callisto;
       print_moon callisto'';
       print_endline (string_of_bool (callisto = callisto''));
       print_endline (string_of_bool ((io'', europa'', ganymede'', callisto'') = (io, europa, ganymede, callisto)));
       ignore(read_int ())) *)
  done;
  print_moon io;
  print_moon europa;
  print_moon ganymede;
  print_moon callisto;
  !c

  =============================================================================
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
  print_moon' io_p'';
  while !c = 0 || (io_p'', europa_p'', ganymede_p'', callisto_p'') != (io_p, europa_p, ganymede_p, callisto_p) do
    print_string (string_of_bool (io_p = io_p''));
    print_string (string_of_bool (europa_p = europa_p''));
    print_string (string_of_bool (ganymede_p = ganymede_p''));
    print_endline (string_of_bool (callisto_p = callisto_p''));
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
