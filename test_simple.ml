(* Simple test for SAT solving *)

let () =
  (* A simple satisfiable formula: (x1 OR x2) AND (NOT x1 OR x3) *)
  (* This should be satisfiable with x1=true, x2=true, x3=true *)
  let clauses = [[1; 2]; [-1; 3]] in
  let num_vars = 3 in
  
  Printf.printf "Testing with %d clauses, %d vars\n" (List.length clauses) num_vars;
  
  (* Check if formula is satisfiable by brute force *)
  let all_assignments = [
    [1; 1; 1]; [1; 1; -1]; [1; -1; 1]; [1; -1; -1];
    [-1; 1; 1]; [-1; 1; -1]; [-1; -1; 1]; [-1; -1; -1]
  ] in
  
  let rec check_clause clause assign =
    match clause with
    | [] -> true
    | l :: rest ->
        let v = abs l in
        let expected = if l > 0 then 1 else -1 in
        if List.nth assign (v - 1) = expected then check_clause rest assign
        else true  (* clause satisfied if any literal is true *)
  in
  
  let rec check_formula clauses assign =
    match clauses with
    | [] -> true
    | c :: rest ->
        if not (check_clause c assign) then false
        else check_formula rest assign
  in
  
  List.iter (fun a ->
    if check_formula clauses a then
      Printf.printf "Found satisfying assignment: %d %d %d\n" (List.nth a 0) (List.nth a 1) (List.nth a 2)
  ) all_assignments
