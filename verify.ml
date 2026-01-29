let lit_true l assign =
  let v = abs l in
  let expected = if l > 0 then 1 else -1 in
  assign.(v) = expected

let check_formula filename =
  let ic = open_in filename in
  let nvars = ref 0 in
  let clauses = ref [] in
  try
    while true do
      let line = input_line ic in
      if line <> "" && line.[0] = 'p' then (
        let parts = Str.split (Str.regexp "[ \t]+") line in
        nvars := int_of_string (List.nth parts 2)
      ) else if line <> "" && line.[0] <> 'c' && line.[0] <> '%' then (
        let lits = List.map int_of_string (Str.split (Str.regexp "[ \t]+") line) in
        let lits = List.filter (fun x -> x <> 0) lits in
        if lits <> [] then clauses := lits :: !clauses
      )
    done
  with End_of_file -> close_in ic;
  
  let assign = Array.make (!nvars + 1) 0 in
  (* Use the solution found by debug_sat *)
  assign.(1) <- 1; assign.(2) <- 1; assign.(3) <- 1; assign.(4) <- 1;
  assign.(5) <- 1; assign.(6) <- 1; assign.(7) <- 1; assign.(8) <- 1;
  assign.(9) <- 1; assign.(10) <- 1;
  
  let all_sat = List.for_all (fun c ->
    List.exists (fun l -> lit_true l assign) c
  ) !clauses in
  
  Printf.printf "All clauses satisfied: %b\n" all_sat

let () = check_formula "/tmp/medium.cnf"
