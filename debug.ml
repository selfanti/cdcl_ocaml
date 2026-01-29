let () =
  let clauses = [[4; -18; 19]; [3; 18; -5]; [-5; -8; -15]] in
  let num_vars = 20 in
  Printf.printf "Parsed %d clauses, %d vars\n" (List.length clauses) num_vars
