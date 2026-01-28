(* Simple DPLL SAT Solver in OCaml *)
(* Focused on correctness over performance *)

type lit = int
type clause = lit list

let var (l : lit) : int = abs l
let sign (l : lit) : int = if l > 0 then 1 else -1

(* Parse CNF file *)
let parse_cnf (filename : string) : clause list * int =
  let ic = open_in filename in
  let num_vars = ref 0 in
  let clauses = ref [] in
  try
    while true do
      let line = input_line ic in
      if line <> "" then
        match line.[0] with
        | 'c' -> ()
        | 'p' ->
            let parts = Str.split (Str.regexp "[ \t]+") line in
            if List.length parts >= 4 then
              num_vars := int_of_string (List.nth parts 2)
        | '%' | '0' -> raise End_of_file
        | _ ->
            let lits = List.map int_of_string (Str.split (Str.regexp "[ \t]+") line) in
            let lits = List.filter (fun x -> x <> 0) lits in
            if lits <> [] then clauses := lits :: !clauses
    done
  with End_of_file -> close_in ic;
  (!clauses, !num_vars)

(* Check if literal is true/false under assignment *)
let lit_true (l : lit) (assign : int array) : bool =
  let v = var l in assign.(v) <> 0 && assign.(v) = sign l
let lit_false (l : lit) (assign : int array) : bool =
  let v = var l in assign.(v) <> 0 && assign.(v) <> sign l
let lit_unassigned (l : lit) (assign : int array) : bool =
  assign.(var l) = 0

(* Check clause status *)
let clause_status (c : clause) (assign : int array) : [> `Satisfied | `Falsified | `Unit of lit | `Unresolved] =
  let has_true = ref false in
  let unit_lit = ref None in
  let has_unassigned = ref false in
  List.iter (fun l ->
    if lit_true l assign then has_true := true
    else if lit_unassigned l assign then (
      has_unassigned := true;
      if !unit_lit = None then unit_lit := Some l
    )
  ) c;
  if !has_true then `Satisfied
  else if not !has_unassigned then `Falsified
  else if !unit_lit <> None && not (List.exists (fun l -> lit_unassigned l assign && l <> Option.get !unit_lit) c) then
    `Unit (Option.get !unit_lit)
  else `Unresolved

(* Unit propagation - returns false if conflict *)
let unit_propagate (clauses : clause list) (assign : int array) : bool =
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun c ->
      match clause_status c assign with
      | `Unit l when lit_unassigned l assign ->
          let v = var l in
          assign.(v) <- sign l;
          changed := true
      | `Falsified -> ()
      | _ -> ()
    ) clauses
  done;
  not (List.exists (fun c -> clause_status c assign = `Falsified) clauses)

(* Check if all variables assigned and no conflicts *)
let is_solved (clauses : clause list) (num_vars : int) (assign : int array) : bool =
  if List.exists (fun c -> clause_status c assign = `Falsified) clauses then false
  else
    let rec check i =
      if i > num_vars then true
      else if assign.(i) = 0 then false
      else check (i + 1)
    in
    check 1

(* Check if formula is satisfied (even if not all vars assigned) *)
let is_satisfied (clauses : clause list) (assign : int array) : bool =
  not (List.exists (fun c -> clause_status c assign = `Falsified) clauses)

(* Find first unassigned variable *)
let select_var (num_vars : int) (assign : int array) : int option =
  let rec find i =
    if i > num_vars then None
    else if assign.(i) = 0 then Some i
    else find (i + 1)
  in
  find 1

(* DPLL search *)
let rec dpll (clauses : clause list) (num_vars : int) (assign : int array) : bool =
  (* Unit propagation *)
  if not (unit_propagate clauses assign) then false
  else if is_solved clauses num_vars assign then true
  else match select_var num_vars assign with
    | None -> is_satisfied clauses assign
    | Some v ->
        (* Save state by copying assignment *)
        let saved = Array.copy assign in
        (* Try true *)
        assign.(v) <- 1;
        if dpll clauses num_vars assign then true
        else (
          (* Restore and try false *)
          Array.blit saved 0 assign 0 (Array.length assign);
          assign.(v) <- -1;
          dpll clauses num_vars assign
        )

let solve (clauses : clause list) (num_vars : int) : bool * int array =
  let assign = Array.make (num_vars + 1) 0 in
  let result = dpll clauses num_vars assign in
  (result, assign)

(* Print assignment in DIMACS format *)
let print_assignment (assign : int array) (num_vars : int) : unit =
  for v = 1 to num_vars do
    Printf.printf "%d " assign.(v)
  done;
  Printf.printf "0\n"

(* Main function *)
let () =
  let args = Array.to_list Sys.argv in
  match args with
  | [_; filename] ->
      let clauses, num_vars = parse_cnf filename in
      if clauses = [] then (
        Printf.printf "UNSAT\n"
      ) else (
        let result, assign = solve clauses num_vars in
        if result then (
          Printf.printf "SAT\n";
          print_assignment assign num_vars
        ) else (
          Printf.printf "UNSAT\n"
        )
      )
  | _ ->
      Printf.fprintf stderr "Usage: %s <cnf_file>\n" Sys.argv.(0);
      exit 1
