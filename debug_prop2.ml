(* Debug falsified logic *)

type lit = int

let var_of (l : lit) : int = abs l
let sign_of (l : lit) : int = if l > 0 then 1 else -1
let lit_true (l : lit) (assign : int array) : bool =
  let v = var_of l in assign.(v) <> 0 && assign.(v) = sign_of l

let lit_unassigned (l : lit) (assign : int array) : bool =
  assign.(var_of l) = 0

let () =
  let assign = Array.make 4 0 in
  assign.(1) <- -1;
  assign.(2) <- -1;
  
  let clause = [1; -2; 3] in
  Printf.printf "Clause: ";
  List.iter (fun l -> Printf.printf "%d " l) clause;
  Printf.printf "\n";
  
  let satisfied = ref false in
  let falsified = ref true in
  
  List.iter (fun l ->
    Printf.printf "  Literal %d: " l;
    if lit_true l assign then (
      Printf.printf "TRUE\n";
      satisfied := true
    ) else if lit_unassigned l assign then (
      Printf.printf "UNASSIGNED\n";
      falsified := false
    ) else (
      Printf.printf "FALSE\n";
      falsified := !falsified && true
    )
  ) clause;
  
  Printf.printf "\nResult: satisfied=%b, falsified=%b\n" !satisfied !falsified;
  
  (* Now test with x1=false, x2=true *)
  assign.(1) <- -1;
  assign.(2) <- 1;
  
  Printf.printf "\n--- With x1=false, x2=true ---\n";
  
  let satisfied = ref false in
  let falsified = ref true in
  
  List.iter (fun l ->
    Printf.printf "  Literal %d: " l;
    if lit_true l assign then (
      Printf.printf "TRUE\n";
      satisfied := true
    ) else if lit_unassigned l assign then (
      Printf.printf "UNASSIGNED\n";
      falsified := false
    ) else (
      Printf.printf "FALSE\n";
      falsified := !falsified && true
    )
  ) clause;
  
  Printf.printf "\nResult: satisfied=%b, falsified=%b\n" !satisfied !falsified
