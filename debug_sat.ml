(* Debug test for SAT solving *)

let rec find_sat clauses assign =
  (* Simple recursive search *)
  if List.for_all (fun c ->
    List.exists (fun l ->
      let v = abs l in
      let value = if l > 0 then 1 else -1 in
      List.nth assign (v - 1) = value
    ) c
  ) clauses then (
    Printf.printf "Found: "; List.iter (fun v -> Printf.printf "%d " v) assign; Printf.printf "\n";
    true
  ) else if List.exists (fun v -> List.nth assign v = 0) [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19] then (
    (* Try next unassigned variable *)
    let rec find_unassigned i =
      if i >= List.length assign then false
      else if List.nth assign i = 0 then (
        (* Try true *)
        let new_assign = List.mapi (fun j v -> if j = i then 1 else v) assign in
        if find_sat clauses new_assign then true
        else (
          (* Try false *)
          let new_assign = List.mapi (fun j v -> if j = i then -1 else v) assign in
          find_sat clauses new_assign
        )
      ) else find_unassigned (i + 1)
    in
    find_unassigned 0
  ) else false

let () =
  let clauses = [[4; -18; 19]; [3; 18; -5]; [-5; -8; -15]; [-20; 7; -16]; [10; -13; -7]; [-12; -9; 17]; [17; 19; 5]; [-16; 9; 15]; [11; -5; -14]; [18; -10; 13]] in
  let assign = [0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0] in
  Printf.printf "Testing...\n";
  if find_sat clauses assign then Printf.printf "SAT\n" else Printf.printf "UNSAT\n"
