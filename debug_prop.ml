(* Debug propagate *)

type lit = int
type clause = lit list

let var_of (l : lit) : int = abs l
let sign_of (l : lit) : int = if l > 0 then 1 else -1

let lit_true (l : lit) (assign : int array) : bool =
  let v = var_of l in assign.(v) <> 0 && assign.(v) = sign_of l

let lit_unassigned (l : lit) (assign : int array) : bool =
  assign.(var_of l) = 0

let check_clause (c : clause) (assign : int array) : string =
  let satisfied = ref false in
  let falsified = ref true in
  let unassigned_count = ref 0 in
  let unassigned_lits = ref [] in
  
  List.iter (fun l ->
    if lit_true l assign then satisfied := true;
    if lit_unassigned l assign then (
      falsified := false;
      unassigned_count := !unassigned_count + 1;
      unassigned_lits := l :: !unassigned_lits
    ) else if not (lit_true l assign) then (
      (* Literal is false *)
      falsified := !falsified
    )
  ) c;
  
  if !satisfied then "satisfied"
  else if !falsified then "falsified"
  else if !unassigned_count = 1 then Printf.sprintf "unit (unassigned: %d)" (List.hd !unassigned_lits)
  else Printf.sprintf "unassigned (%d lits)" !unassigned_count

let () =
  let assign = Array.make 4 0 in
  assign.(1) <- -1;  (* x1 = false *)
  assign.(2) <- -1;  (* x2 = false *)
  
  let clause = [1; 2; 3] in  (* x1 OR x2 OR x3 *)
  let result = check_clause clause assign in
  Printf.printf "Clause %s with x1=false, x2=false, x3=unassigned: %s\n" 
    (String.concat " " (List.map string_of_int clause)) result
