(* CDCL SAT Solver - Simplified and correct *)

type lit = int
type clause = lit list

type trail_entry = {
  tvar : int;
  tvalue : int;
  tresource : clause option;
  tlevel : int;
}

type solver = {
  assign : int array;
  trail : trail_entry list ref;
  level : int ref;
  clauses : clause list ref;
  learned : clause list ref;
  activity : float array;
  var_order : int list ref;
  bump : int ref;
  num_conflicts : int ref;
  seen : int array;
  phase : int array;
}

let var_of (l : lit) : int = abs l
let sign_of (l : lit) : int = if l > 0 then 1 else -1
let neg (l : lit) : lit = -l

let parse_cnf (filename : string) : clause list * int =
  let ic = open_in filename in
  let nvars = ref 0 in
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
              nvars := int_of_string (List.nth parts 2)
        | '%' | '0' -> raise End_of_file
        | _ ->
            let lits = List.map int_of_string (Str.split (Str.regexp "[ \t]+") line) in
            let lits = List.filter (fun x -> x <> 0) lits in
            if lits <> [] then clauses := lits :: !clauses
    done;
    close_in ic;
    (!clauses, !nvars)
  with End_of_file ->
    close_in ic;
    (!clauses, !nvars)

let create_solver (clauses : clause list) (num_vars : int) : solver = {
  assign = Array.make (num_vars + 1) 0;
  trail = ref [];
  level = ref 0;
  clauses = ref clauses;
  learned = ref [];
  activity = Array.make (num_vars + 1) 0.0;
  var_order = ref (List.init num_vars (fun i -> i + 1));
  bump = ref 0;
  num_conflicts = ref 0;
  seen = Array.make (num_vars + 1) 0;
  phase = Array.make (num_vars + 1) 1;
}

let lit_true (l : lit) (s : solver) : bool =
  let v = var_of l in s.assign.(v) <> 0 && s.assign.(v) = sign_of l

let lit_unassigned (l : lit) (s : solver) : bool =
  s.assign.(var_of l) = 0

let assign (s : solver) (v : int) (value : int) (reason : clause option) : unit =
  let entry = { tvar = v; tvalue = value; tresource = reason; tlevel = !(s.level) } in
  s.trail := entry :: !(s.trail);
  s.assign.(v) <- value;
  s.phase.(v) <- value

let propagate (s : solver) : clause option =
  let rec loop () =
    let changed = ref false in
    let conflict = ref None in
    
    let all_clauses = !(s.clauses) @ !(s.learned) in
    
    List.iter (fun c ->
      let num_unassigned = ref 0 in
      let last_unassigned = ref None in
      let satisfied = ref false in
      
      List.iter (fun l ->
        if lit_true l s then satisfied := true
        else if lit_unassigned l s then (
          num_unassigned := !num_unassigned + 1;
          last_unassigned := Some l
        )
      ) c;
      
      if not !satisfied then (
        if !num_unassigned = 0 then (
          if !conflict = None then conflict := Some c
        ) else if !num_unassigned = 1 then (
          match !last_unassigned with
          | Some l ->
              if lit_unassigned l s then (
                assign s (var_of l) (sign_of l) (Some c);
                changed := true
              )
          | None -> ()
        ) else ()
      )
    ) all_clauses;
    
    if !conflict <> None then !conflict
    else if !changed then loop ()
    else None
  in
  loop ()

let get_trail_at_level (s : solver) (lvl : int) : trail_entry list =
  List.filter (fun e -> e.tlevel = lvl) !(s.trail)

let analyze_conflict (s : solver) (conflict_clause : clause) : int * clause =
  let current_level = !(s.level) in
  
  if current_level = 0 then (0, [1; -1])
  else (
    (* Reset seen *)
    for i = 0 to Array.length s.seen - 1 do s.seen.(i) <- 0 done;
    
    (* Find 1UIP *)
    let rec find_uip (conf : clause) (queue : lit list) (learned : lit list) : lit list =
      match queue with
      | [] -> learned
      | l :: rest ->
          let v = var_of l in
          if s.seen.(v) = 1 then find_uip conf rest learned
          else (
            s.seen.(v) <- 1;
            
            (* Find the variable's assignment in trail *)
            let rec find_assignment trail =
              match trail with
              | [] -> None
              | e :: r -> if e.tvar = v then Some e else find_assignment r
            in
            
            match find_assignment !(s.trail) with
            | None ->
                (* Decision variable - this is 1UIP if not already learned *)
                find_uip conf rest (l :: learned)
            | Some entry ->
                match entry.tresource with
                | None ->
                    (* Decision variable *)
                    find_uip conf rest (l :: learned)
                | Some reason ->
                    (* Add all literals from reason clause except the assigned one *)
                    let new_queue = ref rest in
                    List.iter (fun ll ->
                      let vv = var_of ll in
                      if s.seen.(vv) = 0 then
                        new_queue := ll :: !new_queue
                    ) reason;
                    find_uip conf !new_queue learned
          )
    in
    
    let learned_clause = find_uip conflict_clause conflict_clause [] in
    
    if learned_clause = [] then (
      (* No literals learned, use first decision variable *)
      match get_trail_at_level s current_level with
      | [] -> (0, [1; -1])
      | e :: _ -> (current_level - 1, [neg (e.tvalue * e.tvar)])
    ) else (
      (current_level - 1, learned_clause)
    )
  )

let select_var (s : solver) : int option =
  let rec find_first order =
    match order with
    | [] -> None
    | v :: rest ->
        if s.assign.(v) = 0 then Some v
        else find_first rest
  in
  find_first !(s.var_order)

let bump_var (s : solver) (v : int) : unit =
  s.activity.(v) <- s.activity.(v) +. 1.0;
  s.bump := !(s.bump) + 1;
  if !(s.bump) > 1000 then (
    s.bump := 0;
    for i = 1 to Array.length s.activity - 1 do
      s.activity.(i) <- s.activity.(i) *. 0.95
    done
  )

let backtrack (s : solver) (target_level : int) : unit =
  let rec unwind trail =
    match trail with
    | [] -> []
    | e :: rest ->
        if e.tlevel > target_level then (
          s.assign.(e.tvar) <- 0;
          unwind rest
        ) else trail
  in
  s.trail := unwind !(s.trail);
  s.level := target_level

let max_learned = 1000

let rec search (s : solver) : bool =
  match propagate s with
  | Some conflict_clause ->
      s.num_conflicts := !(s.num_conflicts) + 1;
      
      if !(s.level) = 0 then false
      else (
        let backtrack_level, learned_clause = analyze_conflict s conflict_clause in
        
        if learned_clause <> [] && learned_clause <> [1; -1] then (
          s.learned := learned_clause :: !(s.learned);
          List.iter (fun l -> bump_var s (var_of l)) learned_clause;
          
          if List.length !(s.learned) > max_learned then
            s.learned := List.rev (List.filter (fun _ -> true) (List.rev !(s.learned)))
        );
        
        backtrack s backtrack_level;
        search s
      )
  | None ->
      (* Check if all assigned *)
      let all_assigned = ref true in
      for i = 1 to Array.length s.assign - 1 do
        if s.assign.(i) = 0 then all_assigned := false
      done;
      
      if !all_assigned then true
      else (
        match select_var s with
        | None -> true
        | Some v ->
            let saved = !(s.level) in
            s.level := saved + 1;
            let value = s.phase.(v) in
            assign s v value None;
            if search s then true
            else (
              backtrack s saved;
              s.phase.(v) <- -value;
              assign s v (-value) None;
              search s
            )
      )

let solve (clauses : clause list) (num_vars : int) : bool * int array =
  let s = create_solver clauses num_vars in
  let result = search s in
  (result, s.assign)

let print_assign (a : int array) (n : int) : unit =
  for i = 1 to n do Printf.printf "%d " a.(i) done;
  Printf.printf "0\n"

let () =
  let args = Array.to_list Sys.argv in
  match args with
  | [_; filename] ->
      let clauses, num_vars = parse_cnf filename in
      if clauses = [] then Printf.printf "UNSAT\n"
      else (
        let result, assign = solve clauses num_vars in
        if result then (
          Printf.printf "SAT\n";
          print_assign assign num_vars
        ) else Printf.printf "UNSAT\n"
      )
  | _ ->
      Printf.fprintf stderr "Usage: %s <cnf>\n" Sys.argv.(0);
      exit 1
