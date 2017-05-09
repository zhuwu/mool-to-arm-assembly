open MOOL_structs
open Ir3mOOL_structs
open Arm_structs

let get_unique_list l = List.sort_uniq compare l

let remove_element_from_list l element =
  List.filter(fun e -> (compare e element) != 0) l


(* 
 * function generate_optimization_structure splits the code into code blocks.
 * It creates two lists based on the code:
 *   stmt_block and flat_stmt_block
 * 
 * stmt_block is a list of tuple, for each tuple, the contents are:
 * (sub_list, block_counter)
 * for the sub_list, it maps to one code block.
 * The sub_list is a list of tuple, which has the format of (code_statement, statement counter, inner block counter)


 * the flat_stmt_block is a list of tuples, for each tuple, it has the format of
 * (code_statement, code statement counter, block counter, inner_statement_counter).
 * It is the flat format for statement blocks, for easy traversal.

 *)

let generate_optimization_structure (ir3stmts:ir3_stmt list)=
  let next_block = ref [] in
  let stmt_blocks = ref [] in
  let flat_stmt_block = ref [] in

  let stmt_counter = ref 0 in
  let block_counter = ref 0 in
  let inner_stmt_counter = ref 0 in

  let append_stmt_to_blocks stmt =
    next_block := !next_block @ [(stmt, !stmt_counter, !inner_stmt_counter)];
    flat_stmt_block := !flat_stmt_block @ [(stmt, !stmt_counter, !block_counter, !inner_stmt_counter)];
    inner_stmt_counter := !inner_stmt_counter + 1;
    stmt_counter := !stmt_counter + 1;
  in
  let append_block_to_stmt_blocks ()=
    stmt_blocks := !stmt_blocks @ [(!next_block, !block_counter)];
    block_counter := !block_counter + 1;
    inner_stmt_counter := 0;
    next_block := [];
  in

  let rec split_stmt_blocks (ir3stmts:ir3_stmt list) =
    match ir3stmts with
    | [] -> []
    | hd_stmt::tail ->
      let _ = match hd_stmt with
      | IfStmt3 (_,_)
      | GoTo3 _
      | ReturnStmt3 _
      | ReturnVoidStmt3 ->      
        append_stmt_to_blocks hd_stmt;
        append_block_to_stmt_blocks ()

      | Label3 _ ->
        let _ = if (List.length !next_block > 0) then
          append_block_to_stmt_blocks ()
        in
        append_stmt_to_blocks hd_stmt

      | ReadStmt3 _
      | PrintStmt3 _
      | AssignStmt3 (_,_)
      | AssignDeclStmt3 (_,_,_)
      | AssignFieldStmt3 (_,_)
      | MdCallStmt3 _  -> append_stmt_to_blocks hd_stmt in

      split_stmt_blocks tail 
    in
  let _ = split_stmt_blocks ir3stmts in

  let _ =match !next_block with
  | [] -> []
  | hd::tail ->
    append_block_to_stmt_blocks ();
    [] in

  (!flat_stmt_block, !stmt_blocks)

let compare_two_int_lists lx ly =
  if (List.length lx) != (List.length ly)
  then
    false
  else
    let rec compare_list lx ly =
       match lx,ly with
       | ([],[]) -> true
       | (hx::tx, hy::ty) ->
         (hx==hy) && (compare_list tx ty)
    in compare_list lx ly

(* 
 * Function generate execution paths would return 4 data structures.
 * 
 * Among them, block_previous and block_next holds the previous blocks and next blocks 
 * of each block. 
 * Say we have two execution paths of blocks: A-B-C and A-E-B-D-C,
 * the for B, block_previous holds (A, E); block_next holds (C, D).

 * the other two data structures are block_execution_paths and statement_execution_paths.
 * block_execution_paths holds a list of execution path - for each block execution path,
 * it is a list of blocks, which maps to the execution path of the program.

 * statement_execution_paths maps to a list of statement execution paths. For each statement execution
 * path, correspondingly, it is a list of statement tuple - the tuple contains (statement, statement counter,
 * block_counter, inner counter).

 *)

let generate_execution_paths flat_stmt_block stmt_blocks=
  let rec get_label_stmt_counter (label_num:int) flat_block=
    match flat_block with
    | [] -> (-1, -1)
    | hd::tail ->
      let (hd_stmt, counter, block_counter, _) = hd in
      match hd_stmt with
      | Label3 label3 -> 
          if label_num == label3 
            then (counter, block_counter)
          else get_label_stmt_counter label_num tail
      | _ -> get_label_stmt_counter label_num tail
  in

  let block_previous = Hashtbl.create 32 in
  let block_next = Hashtbl.create 32 in
  let largest_block_counter=
    let rev_stmt_blocks = List.rev flat_stmt_block in
    let (last_stmt, _, block_counter, _)::_ = rev_stmt_blocks in
    block_counter 
  in

  let update_prev_blocks (current_block_counter:int) (prev_block_counter:int)=
    try 
      let prev_blocks = Hashtbl.find block_previous current_block_counter in
      let _ = Hashtbl.remove block_previous current_block_counter in
      if (not (List.exists (fun (prev) -> prev == prev_block_counter) prev_blocks))
      then 
        let updated_prev = prev_blocks @ [prev_block_counter] in
        Hashtbl.add block_previous current_block_counter updated_prev
    with Not_found ->
      let updated_prev = [prev_block_counter] in
      Hashtbl.add block_previous current_block_counter updated_prev
  in


  let rec generate_prev_next_block tmp_stmt_blocks=
    match tmp_stmt_blocks with
    | [] -> -1
    | (hd_block, block_counter)::tail ->
      let rev_block = List.rev hd_block in
      let (block_last_stmt, _, _)::_ = rev_block in
      let _ = match block_last_stmt with
      | IfStmt3 (_, label_num) ->
        let (stmt_counter, next_block_counter) = get_label_stmt_counter label_num flat_stmt_block in
        let next_blocks = ref [next_block_counter] in
        let _ = update_prev_blocks next_block_counter block_counter in

        if block_counter < largest_block_counter
        then 
          next_blocks := !next_blocks @ [block_counter + 1];
          let _ = update_prev_blocks (block_counter+1) block_counter
        in
        Hashtbl.add block_next block_counter !next_blocks;
        0
      | GoTo3 label_num ->
        let (stmt_counter, next_block_counter) = get_label_stmt_counter label_num flat_stmt_block in
        let next_blocks = ref [next_block_counter] in
        let _ = update_prev_blocks next_block_counter block_counter in
        Hashtbl.add block_next block_counter !next_blocks;
        0
      
      | ReturnStmt3 _
      | ReturnVoidStmt3 -> -1
     
      | Label3 _ 
      | ReadStmt3 _
      | PrintStmt3 _
      | AssignStmt3 (_,_)
      | AssignDeclStmt3 (_,_,_)
      | AssignFieldStmt3 (_,_)
      | MdCallStmt3 _  -> 
        let _ =
        if (block_counter < largest_block_counter)
        then 
          let next_blocks = [block_counter + 1] in
          let _ = update_prev_blocks (block_counter+1) block_counter in
          Hashtbl.add block_next block_counter next_blocks in
        0

      in
      generate_prev_next_block tail
  in 
  let _ = generate_prev_next_block stmt_blocks in



  let generate_block_execution_paths () =
    let init_execution_paths = [[0]] in
    let final_execution_paths = ref [] in
    let rec get_execution_tuple execution_path tuple=
      match execution_path with
      | [] -> tuple
      | hd::[] -> tuple
      | hd::(next::tail) ->
        let new_tuple = tuple @ [(hd, next)] in
        let tail_tuple = get_execution_tuple (next::tail) new_tuple in
        new_tuple @ tail_tuple
    in


    let rec append_next_block (current_execution_paths:int list list)=
      let new_execution_paths = ref [] in
      let _ = List.map (fun (execution_path:int list) ->
        let last_block::_ = List.rev execution_path in
        try
          let next_blocks = Hashtbl.find block_next last_block in
          let execution_path_tuple = get_execution_tuple execution_path [] in
          List.map (fun (next_counter:int) ->
            if (not (List.exists (fun (cur, next) ->
              (cur == last_block) && (next == next_counter)
            ) execution_path_tuple))
            then
              let new_path = execution_path @ [next_counter] in
              new_execution_paths := !new_execution_paths @ [new_path];
              append_next_block !new_execution_paths
            else
              ()
           ) next_blocks;
          ()
        with
        | _ -> 
          if not (List.exists (fun ex_path ->
              compare_two_int_lists ex_path execution_path
            ) !final_execution_paths)
          then
            final_execution_paths := !final_execution_paths @ [execution_path];
            ()

      ) current_execution_paths in
      ()
    in

    let _ = append_next_block init_execution_paths in
    !final_execution_paths
  in

  let block_execution_paths = generate_block_execution_paths() in
  let stmt_execution_paths = ref [] in



  let rec expand_block_paths (block_path:int list)=
    match block_path with
    | [] -> []
    | hd::tail ->
      let stmts = List.filter (fun (stmt, _, block_counter, _) -> (block_counter == hd) ) flat_stmt_block in
      let tail_stmts = expand_block_paths tail in
      stmts @ tail_stmts
  in

  let rec loop_block_execution_path block_paths = 
    match block_paths with
    | [] -> ()
    | hd_path::tail_paths ->
      let stmt_path = expand_block_paths hd_path in
      stmt_execution_paths := !stmt_execution_paths @ [stmt_path];
      let _ = loop_block_execution_path tail_paths in
      ()
  in
  let _ = loop_block_execution_path block_execution_paths in

  (block_previous, block_next, block_execution_paths, !stmt_execution_paths)

let do_liveness_analysis stmt_blocks flat_stmt_block block_next =
  let in_table = Hashtbl.create 32 in
  let out_table = Hashtbl.create 32 in
  let define_table = Hashtbl.create 32 in
  let use_table = Hashtbl.create 32 in
  let match_idc3_var v = match v with
  | Var3 name -> [name]
  | _ -> [] in
  let generate_use_list exp =
    match exp with
    | BinaryExp3 (_, var1, var2) -> begin
      let var_name1 = match_idc3_var var1 in
      let var_name2 = match_idc3_var var2 in
      get_unique_list (var_name1 @ var_name2)
    end
    | UnaryExp3 (_, var) -> match_idc3_var var
    | FieldAccess3 (o, _) -> [o]
    | Idc3Expr var -> match_idc3_var var
    | MdCall3 (_, vars) ->
      let rec helper vars = match vars with
      | [] -> []
      | head::tail -> (match_idc3_var head)@(helper tail) in
      get_unique_list (helper vars)
    | _ -> []
  in
  (* Build define-use table *)
  let _ = List.iter(fun (block, _) -> 
    List.iter(fun (stmt, stmt_counter, _) -> 
      match stmt with
      | IfStmt3 (exp, _) -> Hashtbl.add use_table stmt_counter (generate_use_list exp)
      | ReadStmt3 var_name ->
        Hashtbl.add use_table stmt_counter [var_name]; Hashtbl.add define_table stmt_counter var_name
      | PrintStmt3 idc3_var -> begin
          match idc3_var with
          | Var3 var_name -> Hashtbl.add use_table stmt_counter [var_name]
          | _ -> ()
        end
      | AssignStmt3 (var_name, exp) ->
        Hashtbl.add define_table stmt_counter var_name; Hashtbl.add use_table stmt_counter (generate_use_list exp)
      | AssignFieldStmt3 (exp1, exp2) ->
        Hashtbl.add use_table stmt_counter (get_unique_list ((generate_use_list exp1) @ (generate_use_list exp2)))
      | MdCallStmt3 exp -> Hashtbl.add use_table stmt_counter (generate_use_list exp)
      | ReturnStmt3 var_name -> Hashtbl.add use_table stmt_counter [var_name]
      | _ -> ()
    ) block;
  ) stmt_blocks in
  (* let _ = print_endline "&&&&& define_table &&&&&&&&" in
  let _ = Hashtbl.iter(fun k v -> 
    let _ = print_string ((string_of_int k) ^ ": ") in
    let _ = print_endline v in ()) define_table in
  print_endline "$$$$$$ use table $$$$$$$$$$";
  let _ = Hashtbl.iter(fun k vs ->
    let _ = print_string ((string_of_int k) ^ ": ") in
    let _ = List.iter(fun v -> print_string (v ^ ", ")) vs in
    let _ = print_endline "" in
    ()
  ) use_table in
  print_endline "%%%%%%"; *)

  
  (* List.iter (fun (stmt, stmt_counter, block_counter, inner_counter) -> 
     let line = (string_of_int block_counter) ^ " " ^ (string_of_int inner_counter)  
                ^ " | " ^ (string_of_int stmt_counter) ^ " " ^ string_of_ir3_stmt stmt in
      print_endline line
  )  flat_stmt_block; *)

  (* Build in-out table *)
  let in_modified = ref true in
  let _ = while !in_modified do
    let _ = in_modified := false in
    List.iter(fun (block, block_counter) ->
      (* let _ = print_endline ("block counter" ^ (string_of_int block_counter)) in *)
      let block_stmt_num = List.length block in
      List.iter(fun (stmt, stmt_counter, inner_stmt_counter) -> 
        let next_stmts = if (inner_stmt_counter < block_stmt_num -1) then
          (* This statment is not the last one in the block. *)
          [stmt_counter + 1]
        else
          (* This statement is the last one in the block *)
          if (not (Hashtbl.mem block_next block_counter)) then
            (* This block is the last block *)
            []
          else begin
            let next_blocks = Hashtbl.find block_next block_counter in
            let stmt_counter_list = ref [] in
            let _ = List.iter(fun b_counter -> 
            let (_, s_counter, _, _) = List.find(fun (_, _, x, y) -> (x == b_counter) && (y == 0)) flat_stmt_block in
              stmt_counter_list := !stmt_counter_list @ [s_counter];
            ) next_blocks in
            !stmt_counter_list
          end in

        (* Get out of current statement *)
        let out_ref = ref [] in
        let _ = List.iter(fun succ ->
          let in_succ = if (Hashtbl.mem in_table succ) then
            Hashtbl.find in_table succ
          else
            [] in
          out_ref := !out_ref @ in_succ
        ) next_stmts in
        let out = get_unique_list (!out_ref) in
        let _ = Hashtbl.replace out_table stmt_counter out in

        (* Get in of current statement *)
        let use = if (Hashtbl.mem use_table stmt_counter) then
          Hashtbl.find use_table stmt_counter
        else
          [] in
        let new_in = get_unique_list (use @ (
          if (Hashtbl.mem define_table stmt_counter) then
            remove_element_from_list out (Hashtbl.find define_table stmt_counter)
          else
            out)) in
        let old_in = if (Hashtbl.mem in_table stmt_counter) then
          Hashtbl.find in_table stmt_counter
        else
          [] in
        let _ = if ((compare new_in old_in) != 0) then
          in_modified := true
        in
        Hashtbl.replace in_table stmt_counter new_in
      ) block
    ) stmt_blocks;
  done in
  (* let _ = print_endline "*** in table" in
  let _ = Hashtbl.iter(fun k vs -> 
    let _ = print_string ((string_of_int k) ^ ": ") in
    let _ = List.iter(fun v -> print_string (v ^ ", ")) vs in
    let _ = print_endline "" in
    ()
  ) in_table in *)
  (* let _ = print_endline "*** out table" in
  let _ = Hashtbl.iter(fun k vs -> 
    let _ = print_string ((string_of_int k) ^ ": ") in
    let _ = List.iter(fun v -> print_string (v ^ ", ")) vs in
    let _ = print_endline "" in
    ()
  ) out_table in *)
  (in_table, out_table, define_table, use_table)

let generate_inference_graph in_table =
  let raw_inference_graph = Hashtbl.create 32 in
  let inference_graph = Hashtbl.create 32 in
  let _ = Hashtbl.iter(fun k var_list ->
    (* let _ = print_string ((string_of_int k) ^ ": ") in
    let _ = List.iter(fun x -> print_string (x ^ ", ")) var_list in
    let _ = print_endline "" in *)
    List.iter(fun x ->
      if (Hashtbl.mem raw_inference_graph x) then begin
        let current_list = Hashtbl.find raw_inference_graph x in
        Hashtbl.replace raw_inference_graph x (current_list @ var_list)
      end else
        Hashtbl.add raw_inference_graph x var_list
    ) var_list;
  ) in_table in

  (* let _ = print_endline "==== raw inference graph ===" in
  let _ = Hashtbl.iter(fun k vs ->
    let _ = print_string (k ^ ": ") in
    let _ = List.iter(fun v -> print_string (v ^ ", ")) vs in
    let _ = print_endline "" in
    ()
  ) raw_inference_graph in *)

  let _ = Hashtbl.iter(fun k var_list ->
    Hashtbl.add inference_graph k (remove_element_from_list (get_unique_list var_list) k)
  ) raw_inference_graph in

  (* let _ = print_endline "==== inference graph ===" in
  let _ = Hashtbl.iter(fun k vs ->
    let _ = print_string (k ^ ": ") in
    let _ = List.iter(fun v -> print_string (v ^ ", ")) vs in
    let _ = print_endline "" in
    ()
  ) inference_graph in *)
  inference_graph

let pick_register inference_graph =
  let max_reg = 5 in
  let copied_graph = Hashtbl.copy inference_graph in
  let stack = Stack.create () in
  let reg_allocation = Hashtbl.create 32 in
  let spill_table = Hashtbl.create 32 in
  let _ = while (Hashtbl.length copied_graph != 0) do
    let v = ref "#" in
    let v_to_spill = ref "#" in
    let edge_max = ref (-1) in
    (* let edge = ref (-1) in *)

    (* Get next vertex needs processing *)
    let _ = Hashtbl.iter(fun k var_list ->
      let edge_length = List.length var_list in
      let _ = if (edge_length < max_reg) then begin
        (* let _ = edge := edge_length in *)
        v := k
      end in
      if (edge_length > !edge_max) then begin
        let _ = edge_max := edge_length in
        v_to_spill := k
      end
    ) copied_graph in

    (* let _ = print_endline ("v: " ^ !v ^ "; v to spill: " ^ !v_to_spill ^ "; edge: " ^ (string_of_int !edge)) in *)
    let selected_v = if ((compare !v "#") != 0) then
      (* No need to spill *)
      let _ = Stack.push !v stack in
      !v
    else begin
      (* Spill *)
      let _ = Hashtbl.add spill_table !v_to_spill true in
      let _ = Stack.push !v_to_spill stack in
      !v_to_spill
    end in

    (* Remove vertex and its edges from inference graph *)
    let _ = Hashtbl.remove copied_graph selected_v in
    let k_list = ref [] in
    let _ = Hashtbl.iter(fun k var_list ->
      if (List.exists(fun x -> (compare x selected_v) == 0) var_list) then
        k_list := k :: !k_list
    ) copied_graph in
    let _ = List.iter(fun x ->
      let var_list = Hashtbl.find copied_graph x in
      Hashtbl.replace copied_graph x (remove_element_from_list var_list selected_v)
    ) !k_list in

   (*  let _ = print_endline "====" in
    let _ = print_endline ("inference graph size: " ^ (string_of_int (Hashtbl.length copied_graph))) in
    let _ = Hashtbl.iter(fun k vs ->
      let _ = print_string (k ^ ": ") in
      let _ = List.iter(fun v -> print_string (v ^ ", ")) vs in
      let _ = print_endline "" in
      ()
    ) copied_graph in *)
    ()
  done in

  (* Assign register. Spilled variables will not have registers, and they will be speicially handled later. *)
  let _ = Stack.iter(fun vertex ->
    if (not (Hashtbl.mem spill_table vertex)) then begin
      let connected_vertices = Hashtbl.find inference_graph vertex in
      let raw_neighbor_regs = ref [] in
      let _ = List.iter(fun x ->
        if (Hashtbl.mem reg_allocation x) then
          raw_neighbor_regs := (Hashtbl.find reg_allocation x)::!raw_neighbor_regs
      ) connected_vertices in
      let neighbor_regs = get_unique_list !raw_neighbor_regs in
      (* let _ = print_string ("1. " ^ vertex ^ ": ") in
      let _ = List.iter(fun x -> print_string (x ^ ", ")) connected_vertices in
      let _ = print_endline "" in
      let _ = print_string ("2. " ^ vertex ^ ": ") in
      let _ = List.iter(fun x -> print_string ((string_of_int x) ^ ", ")) neighbor_regs in
      let _ = print_endline "" in *)
      let reg_index = ref 1 in
      let _ = List.iter(fun x ->
        if (!reg_index == x) then
          reg_index := x + 1
      ) neighbor_regs in
      Hashtbl.add reg_allocation vertex !reg_index
    end
  ) stack in

  (* let _ = print_endline "### Reg Assignment ###" in
  let _ = Hashtbl.iter(fun k v ->
    let _ = print_string (k ^ ": ") in
    print_endline (string_of_int v)
  ) reg_allocation in *)
  (* let _ = print_endline "$$$ spill table $$$" in
  let _ = Hashtbl.iter(fun k v -> print_string k) spill_table in
  let _ = print_endline "" in *)

  (reg_allocation, spill_table)

let get_class_sizes_attr_counters classes =
  let class_sizes = Hashtbl.create 32 in
  let class_attr_counters = Hashtbl.create 32 in
  let class_attr_types = Hashtbl.create 32 in
  let _ = List.iter(fun (c_name, var_list) ->
    let _ = Hashtbl.add class_sizes c_name ((List.length var_list) * 4) in
    let counter = ref 0 in
    let attr_couters = Hashtbl.create 32 in
    let attr_types = Hashtbl.create 32 in
    let _ = List.iter(fun (t, v) ->
      let _ = Hashtbl.add attr_types v t in
      let _ = Hashtbl.add attr_couters v !counter in
      counter := !counter + 1
    ) var_list in
    let _ = Hashtbl.add class_attr_types c_name attr_types in
    Hashtbl.add class_attr_counters c_name attr_couters
  ) classes in
  (class_sizes, class_attr_counters, class_attr_types)

let get_var_type vars v =
  let var_index = ref (-1) in
  let counter = ref (-1) in
  let _ = List.iter(fun (_, var_name) ->
    let _ = counter := !counter + 1 in
    if ((compare var_name v) == 0) then
      var_index := !counter
  ) vars in
  let (var_type, _) = List.nth vars !var_index in
  var_type

let get_class_name vars o =
  let var_type = get_var_type vars o in
  match var_type with
  | ObjectT _ -> string_of_mOOL_type var_type
  | _ -> failwith ("Error: var " ^ o ^ " is not an object!")

let ir3_program_to_ARM (classes, main_method, methods) =
  let (class_sizes, class_attr_counters, class_attr_types) = get_class_sizes_attr_counters classes in

  let data_code = ref [PseudoInstr (".data");
    Label ("L1"); PseudoInstr (".asciz \"%d\\n\"");
    Label ("L2"); PseudoInstr (".asciz \"%s\\n\"");
    Label ("L3"); PseudoInstr (".asciz \"true\"");
    Label ("L4"); PseudoInstr (".asciz \"false\"");
    Label ("L5"); PseudoInstr (".asciz \"\"")] in
  let header = [PseudoInstr (".text"); PseudoInstr (".global main")] in
  let data_code_l_counter = ref 5 in
  let put_string_in_data_code s =
    let _ = data_code_l_counter := !data_code_l_counter + 1 in
    let string_location = "L" ^ (string_of_int !data_code_l_counter) in
    let _ = data_code := !data_code @ [Label (string_location); PseudoInstr (".asciz \"" ^ s ^ "\"")] in
    string_location
  in

  let generate_method_code m =
    let _ = print_endline ("******** method: " ^ m.id3) in
    let in_code = [Label (m.id3); STMFD (["fp"; "lr"; "v1"; "v2"; "v3"; "v4"; "v5"]); ADD ("", false, "fp", "sp", ImmedOp ("#24"))] in
    let method_exit_label = "." ^ m.id3 ^ "_exit" in
    let out_code = [Label (method_exit_label); SUB ("", false, "sp", "fp", ImmedOp ("#" ^ (string_of_int 24))); LDMFD (["fp"; "pc"; "v1"; "v2"; "v3"; "v4"; "v5"])] in

    let ad = Hashtbl.create 32 in
    let temp_reg_usage = Hashtbl.create 2 in

    (* Init temp reg usage table. Use v6 and v7 as temp reg for spilling *)
    let _ = Hashtbl.add temp_reg_usage "v6" false in
    let _ = Hashtbl.add temp_reg_usage "v7" false in
    let get_temp_reg need_check =
      if (need_check) then begin
        if (not (Hashtbl.find temp_reg_usage "v6")) then
          let _ = Hashtbl.replace temp_reg_usage "v6" true in
          "v6"
        else if (not (Hashtbl.find temp_reg_usage "v7")) then
          let _ = Hashtbl.replace temp_reg_usage "v7" true in
          "v7"
        else failwith "No temp register is available!"
      end else "v6" in
    let reset_temp_reg r = Hashtbl.replace temp_reg_usage r false in

    (* Get execution block and conduct liveness analysis *)
    let flat_stmt_block, stmt_blocks = generate_optimization_structure m.ir3stmts in
    let (block_prev, block_next, block_execution_paths, _) = generate_execution_paths flat_stmt_block stmt_blocks in
    let (in_table, out_table, use_table, define_table) = do_liveness_analysis stmt_blocks flat_stmt_block block_next in
    let inference_graph = generate_inference_graph in_table in
    let (reg_allocation, spill_table) = pick_register inference_graph in
    let stack_size = Hashtbl.length spill_table in
    let stack_index = ref 0 in

    (* Init AD for params of method *)
    let param_length = List.length m.params3 in
    let param_assignment_code = ref [] in
    (* assign parameter to target register/stack *)
    let _ = for i = 0 to (param_length - 1) do
      let (_, param_name) = List.nth m.params3 i in
      let spilled = Hashtbl.mem spill_table param_name in
      let p_target_address = if (spilled) then
        let _ = stack_index := !stack_index + 1 in
        [RegPreIndexed ("fp", -24 - !stack_index * 4, false)]
      else begin
        if (Hashtbl.mem reg_allocation param_name) then
          [Reg ("v" ^ (string_of_int (Hashtbl.find reg_allocation param_name)))]
        else
          [] (* This parameter is not live at all. Thus no need to assign address. *)
      end in

      let assignment_code = if (i < 4)
        then begin
          let a_reg_name = "a" ^ (string_of_int (i + 1)) in
          if (List.length p_target_address == 1) then begin
            let param_target_address = List.hd p_target_address in
            match param_target_address with
            | Reg r -> [MOV ("", false, r, RegOp a_reg_name)]
            | RegPreIndexed _ -> [STR ("", "", a_reg_name, param_target_address)]
            | _ -> failwith "Error: unknown target address type 1!"
          end else []
        end
      else
        begin
          let param_original_address = (RegPreIndexed ("fp", (param_length - i) * 4, false)) in
          if (List.length p_target_address == 1) then begin
            let param_target_address = List.hd p_target_address in
            match param_target_address with
            | Reg r -> [LDR ("", "", r, param_original_address)]
            | RegPreIndexed _ ->
              let temp_reg = get_temp_reg false in
              [LDR ("", "", temp_reg, param_original_address); STR ("", "", temp_reg, param_target_address)]
            | _ -> failwith "Error: unknown target address type 2!"
          end else []
        end in
      let _ = param_assignment_code := !param_assignment_code @ assignment_code in
      Hashtbl.add ad param_name p_target_address
    done in

    (* Init AD for local variables *)
    let _ = List.iter(fun (_, var_name) -> Hashtbl.add ad var_name []) m.localvars3 in

    let generate_id3_var_reg id3_var =
      let _ = if ((not (Hashtbl.mem ad id3_var)) || (List.length (Hashtbl.find ad id3_var) == 0)) then
        failwith ("Var " ^ id3_var ^ " is not yet initialized 1!") in
      let v_ad = List.hd (Hashtbl.find ad id3_var) in
      match v_ad with
      | Reg r -> ([], r)
      | RegPreIndexed _ ->
        let temp_reg = get_temp_reg true in
        ([LDR ("", "", temp_reg, v_ad)], temp_reg)
      | _ -> failwith "Error: unknown target address type!" in

    let generate_idc3_var_reg idc3_var =
      match idc3_var with
      | IntLiteral3 i ->
        let temp_reg = get_temp_reg true in
        ([MOV ("", false, temp_reg, ImmedOp ("#" ^ (string_of_int i)))], temp_reg)
      | BoolLiteral3 b ->
        let temp_reg = get_temp_reg true in
        let b_int = if (b) then 1 else 0 in
        ([MOV ("", false, temp_reg, ImmedOp ("#" ^ (string_of_int b_int)))], temp_reg)
      | Var3 v -> generate_id3_var_reg v
      | StringLiteral3 s -> failwith "Error: string is not supported in generate_idc3_var_reg!" in

    let generate_id3_var_op id3_var =
      let _ = if ((not (Hashtbl.mem ad id3_var)) || (List.length (Hashtbl.find ad id3_var) == 0)) then
        failwith ("Var " ^ id3_var ^ " is not yet be initialized 2!") in
      let v_ad = List.hd (Hashtbl.find ad id3_var) in
      match v_ad with
      | Reg r -> ([], RegOp r)
      | RegPreIndexed _ ->
        let temp_reg = get_temp_reg true in
        ([LDR ("", "", temp_reg, v_ad)], RegOp temp_reg)
      | _ -> failwith "Error: unknown target address type!" in

    let generate_idc3_var_op idc3_var = 
      match idc3_var with
      | IntLiteral3 i -> ([], ImmedOp ("#" ^ (string_of_int i)))
      | BoolLiteral3 b ->
        let b_int = if (b) then 1 else 0 in
        ([], ImmedOp ("#" ^ (string_of_int b_int)))
      | Var3 v -> generate_id3_var_op v
      | StringLiteral3 s -> failwith "Error: string is not supported in generate_idc3_var_op!" in

    let generate_md_call_code m_name var_list =
      let var_counter = ref (-1) in
      let param_code = ref [] in
      let _ = List.iter(fun v ->
        let _ = var_counter := !var_counter + 1 in
        let _ = if (!var_counter < 4) then
          let (temp_code, temp_op) = generate_idc3_var_op v in
          param_code := !param_code @ temp_code @ [MOV ("", false, "a" ^ (string_of_int (!var_counter + 1)), temp_op)]
        else
          let (temp_code, temp_reg) = generate_idc3_var_reg v in
          param_code := !param_code @ temp_code @ [STMFD ([temp_reg])]
        in
        let _ = reset_temp_reg "v6" in
        let _ = reset_temp_reg "v7" in ()
      ) var_list in

      !param_code @ [BL ("", m_name ^ "(PLT)")] in

    let generate_assignment_exp_right_code var_reg exp =
      match exp with
      | BinaryExp3 (op, var1, var2) ->
        let (v1_code, v1_reg) = generate_idc3_var_reg var1 in
        let (v2_code, v2_op) = generate_idc3_var_op var2 in
        let op_code = match op with
        | BooleanOp b_op -> begin
          match b_op with
          | "&&" -> [AND ("", false, var_reg, v1_reg, v2_op)]
          | "||" -> [ORR ("", false, var_reg, v1_reg, v2_op)]
          | _ -> failwith "Error: unknown boolean operator!"
        end
        | RelationalOp r_op ->
          let compare_code = [CMP ("", v1_reg, v2_op)] in
          let move_code = match r_op with
          | "==" -> [MOV ("eq", false, var_reg, ImmedOp ("#1")); MOV ("ne", false, var_reg, ImmedOp ("#0"))]
          | "!=" -> [MOV ("ne", false, var_reg, ImmedOp ("#1")); MOV ("eq", false, var_reg, ImmedOp ("#0"))]
          | ">" -> [MOV ("gt", false, var_reg, ImmedOp ("#1")); MOV ("le", false, var_reg, ImmedOp ("#0"))]
          | ">=" -> [MOV ("ge", false, var_reg, ImmedOp ("#1")); MOV ("lt", false, var_reg, ImmedOp ("#0"))]
          | "<" -> [MOV ("lt", false, var_reg, ImmedOp ("#1")); MOV ("ge", false, var_reg, ImmedOp ("#0"))]
          | "<=" -> [MOV ("le", false, var_reg, ImmedOp ("#1")); MOV ("gt", false, var_reg, ImmedOp ("#0"))]
          | _ -> failwith "Error: unknown relational operator!" in
          compare_code @ move_code
        | AritmeticOp a_op -> begin
          match a_op with
          | "+" -> [ADD ("", false, var_reg, v1_reg, v2_op)]
          | "-" -> [SUB ("", false, var_reg, v1_reg, v2_op)]
          | "*" ->
            let (v2_code, v2_reg) = match v2_op with
            | RegOp r -> ([], r)
            | ImmedOp _ ->
              let temp_v2_reg = get_temp_reg true in
              ([MOV ("", false, temp_v2_reg, v2_op)], temp_v2_reg) in
            let mul_code = if ((compare var_reg v1_reg) != 0) then
              (* Pattern: x = y * x; *)
              [MUL ("", false, var_reg, v1_reg, v2_reg)]
            else
              if ((compare var_reg v2_reg) != 0) then
                (* Pattern: x = x * y; *)
                [MUL ("", false, var_reg, v2_reg, v1_reg)]
              else
                (* Pattern: x = x * x; *)
                let temp_reg = get_temp_reg true in
                [MOV ("", false, temp_reg, RegOp v1_reg); MUL ("", false, var_reg, temp_reg, temp_reg)] in
            v2_code @ mul_code
          | "/" -> failwith "Error: division is not supported!"
          | _ -> failwith "Error: unknown arithmetical operator!"
        end
        | _ -> failwith "Error: unknown binary operator 1!"
        in

        v1_code @ v2_code @ op_code 
      | UnaryExp3 (op, var1) -> begin
        match op with
        | UnaryOp u_op -> begin
          match u_op with
          | "!" -> begin
            match var1 with
            | BoolLiteral3 b ->
              if (b) then
                [MOV ("", false, var_reg, ImmedOp ("#0"))]
              else
                [MOV ("", false, var_reg, ImmedOp ("#1"))]
            | Var3 v ->
              let (temp_code, temp_reg) = generate_idc3_var_reg var1 in
              let not_code = [RSB ("", false, var_reg, temp_reg, ImmedOp ("#1"))] in
              temp_code @ not_code
            | _ -> failwith "Error: ! operator cannot be applied!"
          end
          | "-" -> begin
            match var1 with
            | IntLiteral3 i -> [MOV ("", false, var_reg, ImmedOp ("#" ^ (string_of_int (-i))))]
            | Var3 v -> begin
              let (temp_code, temp_reg) = generate_idc3_var_reg var1 in
              let negated_code = [RSB ("", false, var_reg, temp_reg, ImmedOp ("#0"))] in
              temp_code @ negated_code
            end
            | _ -> failwith "Error: - operator cannot be applied!"
          end
          | _ -> failwith "Error: wrong unary operator 1!"
        end
        | _ -> failwith "Error: wrong unary operator 2!"
      end
      | FieldAccess3 (o, field) ->
        let (o_location_code, o_reg) = generate_id3_var_reg o in
        let c_name = get_class_name (m.params3 @ m.localvars3) o in
        let offset = (Hashtbl.find (Hashtbl.find class_attr_counters c_name) field) * 4 in
        o_location_code @ [LDR ("", "", var_reg, RegPreIndexed (o_reg, offset, false))]
      | Idc3Expr (var1) -> begin
        match var1 with
        | IntLiteral3 i -> [MOV ("", false, var_reg, ImmedOp ("#" ^ (string_of_int i)))]
        | BoolLiteral3 b ->
          if (b) then
            [MOV ("", false, var_reg, ImmedOp ("#1"))]
          else
            [MOV ("", false, var_reg, ImmedOp ("#0"))]
        | StringLiteral3 s ->
          let string_location = put_string_in_data_code s in
          [LDR ("", "", var_reg, LabelAddr ("=" ^ string_location))]
        | Var3 v ->
          let _ = if ((not (Hashtbl.mem ad v)) || (List.length (Hashtbl.find ad v) == 0)) then
            failwith ("Var " ^ v ^ " is not yet be initialized 1!") in
          let v_ad = List.hd (Hashtbl.find ad v) in
          match v_ad with
          | Reg r -> [MOV ("", false, var_reg, RegOp r)]
          | RegPreIndexed _ -> [LDR ("", "", var_reg, v_ad)]
          | _ -> failwith "Error: unknown target address type 4!"
      end
      | MdCall3 (m_name, var_list) -> (generate_md_call_code m_name var_list) @ [MOV ("", false, var_reg, RegOp ("a1"))]
      | ObjectCreate3 c ->
        let size = Hashtbl.find class_sizes c in
        let mem_allocation_code = [MOV ("", false, "a1", ImmedOp ("#" ^ (string_of_int size))); BL ("", "_Znwj(PLT)"); MOV ("", false, var_reg, RegOp "a1")] in
        (* Do shallow init *)
        let init_code = ref [] in
        let attr_types = Hashtbl.find class_attr_types c in
        let _ = Hashtbl.iter(fun x y -> (
          let a_type = Hashtbl.find attr_types x in
          let temp_code = match a_type with
          | StringT -> LDR ("", "", "a1", LabelAddr "=L5")
          | _ -> MOV ("", false, "a1", ImmedOp "#0") in
          init_code := !init_code @ [temp_code; STR ("", "", "a1", RegPreIndexed (var_reg, y * 4, false))]
        )) (Hashtbl.find class_attr_counters c) in
        mem_allocation_code @ !init_code
      in

    let generate_arm_stmt stmt =
      let result = match stmt with
      | Label3 l -> [Label ("." ^ (string_of_int l))]
      | ReturnVoidStmt3 -> [B ("", method_exit_label)]
      | GoTo3 l -> [B ("", "." ^ (string_of_int l))]
      | AssignStmt3 (var, exp) ->
        let var_address_in_ad = Hashtbl.find ad var in
        let var_address = if (List.length var_address_in_ad == 1) then
          (* var is already have a register or memory space *)
          List.hd var_address_in_ad
        else begin
          (* assign a register or memory to var. *)
          let spilled = Hashtbl.mem spill_table var in
          if (spilled) then
            let _ = stack_index := !stack_index + 1 in
            RegPreIndexed ("fp", -24 - !stack_index * 4, false)
          else begin
            if (Hashtbl.mem reg_allocation var) then
              Reg ("v" ^ (string_of_int (Hashtbl.find reg_allocation var)))
            else
              Reg (get_temp_reg false)
          end
        end in

        let var_reg = match var_address with
        | Reg r -> r
        | RegPreIndexed _ -> get_temp_reg false
        | _ -> failwith "Error: unknown target address type 3!" in

        (* Start to deal with expressions *)
        let exp_code = generate_assignment_exp_right_code var_reg exp in

        (* Store value back to memory if the variable resides in memory *)
        let mem_code = match var_address with
        | RegPreIndexed _ -> [STR ("", "", var_reg, var_address)]
        | _ -> [] in

        (* Update ad *)
        let _ = Hashtbl.replace ad var [var_address] in
        (* let _ = print_endline ("var: " ^ var ^ ", " ^ (string_of_address_type var_address)) in *)

        exp_code @ mem_code
      | IfStmt3 (exp, l) -> begin
        (* Exp in if has only the following two patterns: (!)[_t11], [_t10,false](==)*)
        match exp with
        | BinaryExp3 (RelationalOp "==", Var3 v, BoolLiteral3 false) ->
          let (temp_code, temp_reg) = generate_id3_var_reg v in
          temp_code @ [CMP ("", temp_reg, ImmedOp ("#0")); B ("eq", "." ^ (string_of_int l))]
        | UnaryExp3 (UnaryOp "!", var1) -> begin
          match var1 with
          | BoolLiteral3 b ->
            if (not b) then
              [B ("", "." ^ (string_of_int l))]
            else
              []
          | Var3 v ->
            let (temp_code, temp_reg) = generate_id3_var_reg v in
            temp_code @ [CMP ("", temp_reg, ImmedOp ("#0")); B ("eq", "." ^ (string_of_int l))]
          | _ -> failwith ("Error: unknown if exp 2! " ^ (string_of_ir3_exp exp))
        end
        | _ -> failwith ("Error: unknown if exp 1! " ^ (string_of_ir3_exp exp))
      end
      | PrintStmt3 var -> begin
        let print_param_code = match var with
        | IntLiteral3 i ->
          [LDR ("", "", "a1", LabelAddr "=L1"); MOV ("", false, "a2", ImmedOp ("#" ^ (string_of_int i)))]
        | BoolLiteral3 b ->
          let label = if (b) then "=L3" else "=L4" in
          [LDR ("", "", "a1", LabelAddr "=L2"); LDR ("", "", "a2", LabelAddr label)]
        | StringLiteral3 s ->
          let string_location = put_string_in_data_code s in
          [LDR ("", "", "a1", LabelAddr "=L2"); LDR ("", "", "a2", LabelAddr ("=" ^ string_location))]
        | Var3 v ->
          let v_type = get_var_type (m.params3 @ m.localvars3) v in
          let (temp_code, temp_reg) = generate_id3_var_reg v in
          match v_type with
          | IntT -> temp_code @ [LDR ("", "", "a1", LabelAddr "=L1"); MOV ("", false, "a2", RegOp temp_reg)]
          | BoolT -> temp_code @ [LDR ("", "", "a1", LabelAddr "=L2"); CMP ("", temp_reg, ImmedOp "#1"); LDR ("eq", "", "a2", LabelAddr "=L3"); LDR ("ne", "", "a2", LabelAddr "=L4")]
          | StringT -> temp_code @ [LDR ("", "", "a1", LabelAddr "=L2"); MOV ("", false, "a2", RegOp temp_reg)]
          | _ -> failwith "Error: illegal print!" in
        print_param_code @ [BL ("", "printf(PLT)")]
      end
      | ReturnStmt3 var ->
        let (temp_code, temp_op) = generate_id3_var_op var in
        temp_code @ [MOV ("", false, "a1", temp_op); B ("", method_exit_label)]
      | MdCallStmt3 exp -> begin
        match exp with
        | MdCall3 (m_name, var_list) -> generate_md_call_code m_name var_list
        | _ -> failwith "Error: illegal method expression!"
      end
      | AssignFieldStmt3 (exp_left, exp_right) ->
        let exp_right_temp_reg = "v6" in
        let exp_right_code = generate_assignment_exp_right_code exp_right_temp_reg exp_right in
        let _ = Hashtbl.replace temp_reg_usage exp_right_temp_reg true in
        let _ = reset_temp_reg "v7" in

        let mem_code = match exp_left with
        | FieldAccess3 (o, field) ->
          let (o_location_code, o_reg) = generate_id3_var_reg o in
          let c_name = get_class_name (m.params3 @ m.localvars3) o in
          let offset = (Hashtbl.find (Hashtbl.find class_attr_counters c_name) field) * 4 in
          o_location_code @ [STR ("", "", exp_right_temp_reg, RegPreIndexed (o_reg, offset, false))]
        | _ -> failwith "Error: wrong left value in field assignment!" in
        exp_right_code @ mem_code
      | ReadStmt3 _ -> [] (* Ignore read statement for now *)
      | AssignDeclStmt3 _ -> [] (* Not used *) in

      (* reset temp reg usage table *)
      let _ = reset_temp_reg "v6" in
      let _ = reset_temp_reg "v7" in
      result in

    let generated_arm_stmts = ref [] in
    let _ = List.iter(fun s -> generated_arm_stmts := !generated_arm_stmts @ generate_arm_stmt s) m.ir3stmts in

    in_code @ [SUB ("", false, "sp", "fp", ImmedOp ("#" ^ (string_of_int (24 + (stack_size) * 4))))] 
      @ !param_assignment_code @ !generated_arm_stmts @ out_code in
  let rec generate_methods_code methods =
    match methods with
    | [] -> []
    | head::tail -> (generate_method_code head) @ (generate_methods_code tail) in
  let main_method_code = generate_method_code main_method in
  let methods_code = generate_methods_code methods in
  !data_code @ header @ main_method_code @ methods_code

(* 
 * After statement_blocks and execution paths are generated,
 * the first step of optimization is to eliminate those blocks which are not 
 * in any of the execution path, since they are dead blocks.
 *)


let eliminate_dead_block flat_stmt_block stmt_blocks block_execution_paths=
  let rec block_in_execution_paths (block:int) (execution_paths:int list list)=
    match execution_paths with
    | [] -> false
    | hd_path::[] ->
      List.exists (fun hd_block -> hd_block == block) hd_path
    | hd_path::tail ->
      (List.exists (fun hd_block -> hd_block == block) hd_path) ||
      (block_in_execution_paths block tail)
  in

  let opt_flat_block = List.filter (fun (_,_, block_counter, _) -> 
    block_in_execution_paths block_counter block_execution_paths
  ) flat_stmt_block  in

  let opt_stmt_blocks = List.filter (fun (_, block_counter) ->
    block_in_execution_paths block_counter block_execution_paths
  ) stmt_blocks in

  (opt_flat_block, opt_stmt_blocks)

let rec optimize_ir3_method_stmts flat_stmt_block stmt_blocks stmt_execution_paths=
  (* 
    check_statement_paths_not_modify_vars checks from stmt_counter to other_stmt_counter, in
    all the execution paths, whether stmt_vars are modified.
    If any of the stmt_vars are modified, then this is a valid common subexpression.
   *)

  let check_statement_paths_not_modify_vars stmt_execution_paths stmt_counter other_stmt_counter stmt_vars=
    let check_cse_stmt_path stmt_path=
      let is_id3_in_list id3  id_list=
        List.exists (fun (id:string)-> ((String.compare id3 id) == 0)) id_list
      in

      (* Only check the path between stmt_counter and other_stmt_counter; which is check_path_activated *)
      let check_path_activated = ref false in
      let rec check_path_stmt_not_modified statement_path stmt_vars=
        match statement_path with
        | [] -> true
        | (hd_stmt, hd_counter, _, _)::tail ->
          if (hd_counter == stmt_counter) then
            begin
              check_path_activated := true;
              (check_path_stmt_not_modified tail stmt_vars)
            end
          else
            begin
              if hd_counter == other_stmt_counter then
                begin
                  check_path_activated := false;
                  true
                end
              else
                begin
                  if !check_path_activated then
                    let val_modified = match hd_stmt with
                                      | ReadStmt3 id3 ->
                                        is_id3_in_list id3 stmt_vars
                                      | AssignStmt3 (id3, expr3) ->
                                        is_id3_in_list id3 stmt_vars
                                      | AssignFieldStmt3 (expr3_1, expr3_2) ->
                                        let field_access = match expr3_1 with
                                            | FieldAccess3 (id3_1, id3_2) ->
                                              id3_1 ^ "." ^ id3_2
                                            | _ -> ""
                                        in
                                        ((String.compare "" field_access) != 0) &&
                                          (is_id3_in_list field_access stmt_vars)

                                      | MdCallStmt3 md_expr ->
                                        let check_mdcall = match md_expr with
                                          | MdCall3 (id3, _) ->
                                            is_id3_in_list id3 stmt_vars
                                          | _ -> false 
                                        in
                                        check_mdcall
                                      | _ -> false
                      in
                    
                     (not val_modified) && (check_path_stmt_not_modified tail stmt_vars)
                  else 
                    (check_path_stmt_not_modified tail stmt_vars)
                end
            end
      in
      (check_path_stmt_not_modified stmt_path stmt_vars)

    in

    let rec check_paths_not_modify_stmt stmt_paths = 
      (* let _ = print_endline (string_of_int (List.length stmt_paths)) in *)
      match stmt_paths with
      | [] -> true
      | stmt_path::tail_stmt_paths ->
      let head_not_modified = check_cse_stmt_path stmt_path in
      let tail_not_modifed = check_paths_not_modify_stmt tail_stmt_paths in
      (head_not_modified && tail_not_modifed) 
    in

    (check_paths_not_modify_stmt stmt_execution_paths)
  in

  let common_subexpr = Hashtbl.create 16 in
  let is_common_subexpr (stmt, stmt_counter, block_counter, inner_counter) 
        (other_stmt, other_stmt_counter, other_block_counter, other_inner_counter) =

    let stmt_vars = ref [] in
    let check_expr_vars expr3=
      match expr3 with
      | BinaryExp3  (ir3_op, idc3_1, idc3_2) ->
        let _ = match idc3_1 with
        | Var3 var3_1 ->
          stmt_vars := !stmt_vars @ [var3_1];
        | _ -> ()
        in

        let _ = match idc3_2 with
        | Var3 var3_2 ->
          stmt_vars := !stmt_vars @ [var3_2];
        | _ -> ()
        in
        ()

      | UnaryExp3 (ir3_op, idc3) ->
        let _ = match idc3 with 
        | Var3 var3 -> stmt_vars := !stmt_vars @ [var3]
        | _ -> ()
        in
        () 
      | FieldAccess3 (id3_1, id3_2) ->
        stmt_vars := !stmt_vars @ [id3_1 ^ "." ^ id3_2]
      | Idc3Expr idc3 ->
        let _ = match idc3 with
        | Var3 var3 -> stmt_vars := !stmt_vars @ [var3]
        | _ -> ()
        in ()
      | MdCall3 (id3_1, idc3_list) ->
        stmt_vars := !stmt_vars @ [id3_1];
        let rec append_idc3_list id3_list=
          match id3_list with
          | [] -> ()
          | hd::tail ->
            let _ = match hd with 
            | Var3 var3 -> stmt_vars := !stmt_vars @ [var3]
            | _ -> () 
            in
            append_idc3_list tail
        in
        append_idc3_list idc3_list
      | _ -> ()
    in

    let _ = match stmt with
      | AssignStmt3 (id_3, expr_3) ->
          stmt_vars := !stmt_vars @ [id_3];
          check_expr_vars expr_3
      | AssignDeclStmt3 (_, id_3, expr_3) ->
          stmt_vars := !stmt_vars @ [id_3];
          check_expr_vars expr_3
      | AssignFieldStmt3 (expr3_1, expr3_2) ->
        check_expr_vars expr3_1;
        check_expr_vars expr3_2
      | _ -> () 
    in

    (check_statement_paths_not_modify_vars stmt_execution_paths stmt_counter other_stmt_counter !stmt_vars)
  in

  let find_common_subexpr () = 
    List.iter (fun (stmt, stmt_counter, block_counter, inner_counter) -> 
      let _ = List.map (fun (other_stmt, other_stmt_counter,
        other_block_counter, other_inner_counter) -> 
          if stmt_counter < other_stmt_counter
          then
            let exp = match stmt with
            | AssignDeclStmt3 (_, _, expression)
            | AssignStmt3 (_, expression)
            | AssignFieldStmt3 (_, expression) ->
              string_of_ir3_exp expression
            | _ -> "" in

            let other_exp = match other_stmt with
            | AssignDeclStmt3 (_, _, o_exp)
            | AssignStmt3 (_, o_exp)
            | AssignFieldStmt3 (_,o_exp) ->
              string_of_ir3_exp o_exp
            | _ -> "" in
            
            if ((String.compare exp "") != 0) && 
                (String.compare exp other_exp) == 0 
                && (is_common_subexpr (stmt, stmt_counter, block_counter, inner_counter)
                  (other_stmt, other_stmt_counter, other_block_counter, other_inner_counter) )
               
            then
                try
                  let (_, s_counter, _, _) = Hashtbl.find common_subexpr other_stmt_counter in
                  (* let _ = print_endline ("!!!!! s_counter:" ^ string_of_int s_counter) in *)

                  if (stmt_counter < s_counter) 
                  then
                    (* let _ = print_endline ("**** stmt_counter: " ^ (string_of_int stmt_counter) ^ "**** s_counter: " ^ (string_of_int s_counter) )in *)
                    begin
                      Hashtbl.remove common_subexpr other_stmt_counter;
                      Hashtbl.add common_subexpr other_stmt_counter 
                        (stmt, stmt_counter, block_counter, inner_counter)
                    end
                with
                | Not_found -> 
                  (* let _ = print_endline ("!!!!! ERROR stmt_counter:" ^ (string_of_int stmt_counter) ^ " !!!!!") in *)
                  Hashtbl.add common_subexpr other_stmt_counter 
                      (stmt, stmt_counter, block_counter, inner_counter);

        ) flat_stmt_block in
        ()
    ) flat_stmt_block
  in
  let _ = find_common_subexpr() in
(* 
  Hashtbl.iter (fun other_stmt_counter  (stmt, stmt_counter, block_counter, inner_counter) -> 
    let _ = print_endline ("Key Counter: " ^ string_of_int other_stmt_counter) in
    let _ = print_endline ("CSE counter: " ^ string_of_int stmt_counter) in

    print_endline ("Statement: " ^ string_of_ir3_stmt stmt)

  ) common_subexpr; *)


  let flat_stmt_without_cse = ref [] in
  let copy_propagate_stmts = ref [] in
  let flat_stmt_after_copy_propagation = ref [] in

  let process_common_subexpr_replacement () = 

    (* other_stmt_counter should be after stmt_counter in execution path. *)
    let check_statement_var_initialized stmt_execution_paths stmt_counter other_stmt_counter=
      let var_not_init = List.exists (fun execution_paths -> 
        let stmt_defined = ref false in
        let rec check_execution_path_not_initialized execution_paths=
          let not_initialized = match execution_paths with
          | [] -> true
          | (_,hd_counter, _, _)::tail ->
            if (hd_counter == stmt_counter) then
              begin
                stmt_defined := true;
                (check_execution_path_not_initialized tail)
              end
            else
              begin
                if (hd_counter == other_stmt_counter) then
                  begin
                    if (!stmt_defined) then
                      false
                    else 
                      true
                  end
                else
                  (check_execution_path_not_initialized tail)
              end
          in
          not_initialized
        in

        (check_execution_path_not_initialized execution_paths)

      ) stmt_execution_paths in
      (not var_not_init)
    in


    let rec replace_common_subexpr flat_stmts =
      match flat_stmts with
      | [] -> ()
      | (stmt, stmt_counter, block_counter, inner_counter)::tail ->
        let _ = 
        try
          let (statement, statement_counter, _, _) = Hashtbl.find common_subexpr stmt_counter in
          let expression = match statement with
          | AssignStmt3 (id_3, expr_3) ->
            Idc3Expr (Var3 id_3)
          | AssignDeclStmt3 (_, id_3, expr_3) ->
            Idc3Expr (Var3 id_3)
          | AssignFieldStmt3 (expr3_1, expr3_2) ->
            expr3_1
          | _ -> raise Not_found in

          let new_stmt = match stmt with
          | AssignStmt3 (id_3, expr_3) ->
              AssignStmt3 (id_3, expression)
          | AssignDeclStmt3 (ir3_type, id_3, expr_3) ->
              AssignDeclStmt3 (ir3_type, id_3, expression)
          | AssignFieldStmt3 (expr3_1, expr3_2) ->
              AssignFieldStmt3 (expr3_1, expression)
          in

          let new_head = (new_stmt, stmt_counter, block_counter, inner_counter) in
          copy_propagate_stmts := !copy_propagate_stmts @ [new_head];
          flat_stmt_without_cse := !flat_stmt_without_cse @ [new_head]
        with
        | _ -> 
          flat_stmt_without_cse := !flat_stmt_without_cse @ 
            [(stmt, stmt_counter, block_counter, inner_counter)]
        in
        replace_common_subexpr tail
    in
    let _ = replace_common_subexpr flat_stmt_block in

    let do_copy_propagation copy_propagate_stmts flat_stmt_without_cse=
      (* let returned_flat_stmt = ref flat_stmt_without_cse in *)
      let new_flat_stmt = ref flat_stmt_without_cse in
      List.iter (fun (stmt, stmt_counter, _, _) ->
        let search_variable = match stmt with
        | AssignStmt3 (id_3, expr_3)
        | AssignDeclStmt3 (_, id_3, expr_3) ->
          id_3
        | AssignFieldStmt3 (expr3_1, expr3_2) ->
          let field_access = match expr3_1 with
          | FieldAccess3 (id3_1, id3_2) -> id3_1 ^ "." ^ id3_2
          | _ -> raise Not_found
          in
          field_access
        | _ -> raise Not_found
        in    

        let replace_variable = match stmt with
        | AssignStmt3 (_, expr_3)
        | AssignDeclStmt3 (_, _, expr_3)
        | AssignFieldStmt3 (_, expr_3) ->
          begin 
            match expr_3 with 
            | Idc3Expr idc_3 ->
              let id3 = match idc_3 with
              | Var3 id -> id
              | _ -> raise Not_found
              in
              id3
            | FieldAccess3 (id3_1, id3_2) -> id3_1 ^ "." ^ id3_2
            | _ -> raise Not_found
          end
        | _ -> raise Not_found 
        in

        let replace_ir3_expression (expr3:ir3_exp)=
          let replace_idc3 (idc_3:idc3)=
            match idc_3 with
            | Var3 id3 ->
              if (String.compare id3 search_variable) == 0 then
                Var3 replace_variable
              else
                idc_3
            | _ -> idc_3
          in

          match expr3 with
          | BinaryExp3 (ir3_op, idc3_1, idc3_2) ->
            let new_idc_1 = replace_idc3 idc3_1 in
            let new_idc_2 = replace_idc3 idc3_2 in
            BinaryExp3 (ir3_op, new_idc_1, new_idc_2)
          | UnaryExp3  (ir3_op, idc3) ->
            let new_idc = replace_idc3 idc3 in
            UnaryExp3 (ir3_op, new_idc)
          | FieldAccess3 (id3_1, id3_2) ->
            if (String.compare (id3_1 ^ "." ^ id3_2) search_variable) == 0 then
              let dot_index = String.index replace_variable '.' in
              let first_id3 = String.sub replace_variable 0 dot_index in
              let second_id3 = String.sub replace_variable dot_index (String.length replace_variable - dot_index) in
              FieldAccess3 (first_id3, second_id3)
            else expr3
          | Idc3Expr idc3 ->
            Idc3Expr (replace_idc3 idc3)
          | MdCall3 (id3, (idc3_list:idc3 list)) ->
            let new_id3 = if (String.compare id3 search_variable) == 0 then replace_variable else id3 in
            let new_idc3_list = ref [] in
            let rec check_mdcall_vars idc3_list=
              match idc3_list with
              | [] -> ()
              | hd::tail ->
                let new_idc3 = replace_idc3 hd in
                new_idc3_list := !new_idc3_list @ [new_idc3];
                check_mdcall_vars tail  
            in
            let _ = check_mdcall_vars idc3_list in
            MdCall3 (new_id3, !new_idc3_list)
          | _ -> expr3
        in

        let original_flat_stmt_without_cse = !new_flat_stmt in
        new_flat_stmt := [];

        List.map (fun (other_stmt, other_stmt_counter, other_block_counter, other_inner_counter) -> 
          try
            let var_initialized = check_statement_var_initialized stmt_execution_paths stmt_counter other_stmt_counter in
            let var_not_modified = check_statement_paths_not_modify_vars
                      stmt_execution_paths stmt_counter other_stmt_counter [search_variable] in

            if var_initialized && var_not_modified 
            then
              let new_stmt = match other_stmt with
              | ReturnStmt3 id_3 ->
                if ((String.compare id_3 search_variable) == 0)
                then
                  ReturnStmt3 replace_variable
                else
                  other_stmt
              | PrintStmt3 idc_3 ->
                begin
                  match idc_3 with
                  | Var3 id3 -> 
                    if (String.compare id3 search_variable) == 0 then
                      let new_idc = (Var3 replace_variable) in
                      PrintStmt3 new_idc
                    else other_stmt 
                  | _ -> other_stmt
                end
              | IfStmt3 (ir3_exp, label3) ->
                let new_expr = replace_ir3_expression ir3_exp in
                IfStmt3 (new_expr, label3)
              | AssignStmt3 (id3, ir3_exp) ->
                let new_expr = replace_ir3_expression ir3_exp in
                AssignStmt3 (id3, new_expr)
              | AssignDeclStmt3 (ir3_type, id3, ir3_exp) ->
                let new_expr = replace_ir3_expression ir3_exp in
                AssignDeclStmt3 (ir3_type, id3, new_expr)
              | MdCallStmt3 ir3_exp ->
                let new_expr = replace_ir3_expression ir3_exp in
                MdCallStmt3 new_expr
              | AssignFieldStmt3 (left_ir3_expr, ir3_exp) ->
                let new_expr = replace_ir3_expression ir3_exp in
                AssignFieldStmt3 (left_ir3_expr, new_expr)
              | _ -> other_stmt 
              in

              new_flat_stmt := !new_flat_stmt @
                [(new_stmt, other_stmt_counter, other_block_counter, other_inner_counter)];
                ()
            else 
              new_flat_stmt := !new_flat_stmt @ 
                [(other_stmt, other_stmt_counter, other_block_counter, other_inner_counter)];
              ()
          with
          | _ ->
              new_flat_stmt := !new_flat_stmt @ 
                [(other_stmt, other_stmt_counter, other_block_counter, other_inner_counter)];
              ()

        ) original_flat_stmt_without_cse;
        (* returned_flat_stmt := ref !new_flat_stmt; *)
        ()
      ) copy_propagate_stmts;
      !new_flat_stmt
    in
(* 
    let _ = List.iter (fun (stmt, stmt_counter, block_counter, inner_counter) -> 
      print_endline ((string_of_int stmt_counter) ^ " " ^ (string_of_ir3_stmt stmt))
    ) !flat_stmt_without_cse in *)

    let flat_stmt_after_copy_propagation = do_copy_propagation !copy_propagate_stmts !flat_stmt_without_cse in
    flat_stmt_after_copy_propagation
  in

  let flat_stmt_after_copy_propagation = process_common_subexpr_replacement() in
(*   let _ = List.iter (fun (stmt, stmt_counter, block_counter, inner_counter) -> 
    print_endline ((string_of_int stmt_counter) ^ " " ^ (string_of_ir3_stmt stmt))
  ) flat_stmt_after_copy_propagation in
 *)
  let updated_stmt_blocks = ref [] in
  let rec find_stmt_by_counter flat_stmt stmt_counter=
    match flat_stmt with
    | [] -> raise Not_found
    | (hd_stmt, hd_counter, _, _)::tail ->
      begin
        if hd_counter == stmt_counter then
          hd_stmt
        else
          find_stmt_by_counter tail stmt_counter
      end
  in

  let _ = List.iter (fun (block, block_counter) -> 
    let new_block = ref [] in
    List.iter (fun (stmt, stmt_counter, inner_counter) -> 
      let new_stmt = find_stmt_by_counter flat_stmt_after_copy_propagation stmt_counter in
      new_block := !new_block @ [(new_stmt, stmt_counter, inner_counter)];
    ) block;
    updated_stmt_blocks := !updated_stmt_blocks @ [(!new_block, block_counter)]
  ) stmt_blocks in

  let (new_block_prev, new_block_next, new_block_execution_paths, new_stmt_execution_paths) = 
    generate_execution_paths flat_stmt_after_copy_propagation !updated_stmt_blocks in

  let (in_table, out_table, define_table, use_table) = do_liveness_analysis 
    !updated_stmt_blocks flat_stmt_after_copy_propagation new_block_next in

  (* eliminate_dead_code would remove code from flat_stmt_block based on liveness defined in out_table. *)
  let eliminate_dead_code flat_stmt_block out_table=
    let (eliminated_flat_stmt, eliminated_stmt_blocks, eliminated_next_block) = (ref [], ref [], ref []) in
    let (e_stmt_counter, e_block_counter, e_inner_counter) = (ref 0, ref 0, ref 0) in

    let append_eliminated_statement stmt stmt_counter block_counter=
      if (!e_block_counter < block_counter)
      then
        begin
          eliminated_stmt_blocks := !eliminated_stmt_blocks @ [(!eliminated_next_block, !e_block_counter)];
          e_block_counter := block_counter;
          eliminated_next_block := [];
          e_inner_counter := 0;
        end
      else ();
      
      eliminated_flat_stmt := !eliminated_flat_stmt @ [(stmt, !e_stmt_counter, !e_block_counter, !e_inner_counter)];
      eliminated_next_block := !eliminated_next_block @ [(stmt, !e_stmt_counter, !e_inner_counter)];
      e_stmt_counter := !e_stmt_counter + 1;
      e_inner_counter := !e_inner_counter + 1
    in

    let rec eliminate_for_flat_stmt_block flat_stmt_block=
      match flat_stmt_block with
      | [] -> 
        if (List.length !eliminated_next_block) > 0
        then
          eliminated_stmt_blocks := !eliminated_stmt_blocks @ [(!eliminated_next_block, !e_block_counter)];

      | (stmt, stmt_counter, block_counter, _)::tail ->
        let var = match stmt with 
        | AssignStmt3 (id3, _) -> id3
        | AssignDeclStmt3 (_, id3, _) -> id3
        | AssignFieldStmt3 (ir3_exp, _) ->
          begin
            match ir3_exp with
            | FieldAccess3 (id3, _) -> id3
            | _ -> ""
          end
        | _ -> "" 
        in
        if ((String.compare var "") == 0) || 
          (List.exists (fun id3 -> (String.compare id3 var) == 0) (Hashtbl.find out_table stmt_counter)) 
        then
          append_eliminated_statement stmt stmt_counter block_counter;

        eliminate_for_flat_stmt_block tail
    in

    let _ = eliminate_for_flat_stmt_block flat_stmt_block in
    (!eliminated_flat_stmt, !eliminated_stmt_blocks)
  in

  let (eliminated_flat_stmt, eliminated_stmt_blocks) = 
    eliminate_dead_code flat_stmt_after_copy_propagation out_table in

(* 
  List.iter (fun (stmt, stmt_counter, block_counter, inner_counter) -> 
     let line = (string_of_int block_counter) ^ " " ^ (string_of_int inner_counter)  
                ^ " | " ^ (string_of_int stmt_counter) ^ " " ^ string_of_ir3_stmt stmt in
      print_endline line
  )  eliminated_flat_stmt; *)

  if (List.length eliminated_flat_stmt) == (List.length flat_stmt_after_copy_propagation) then

    (eliminated_flat_stmt, eliminated_stmt_blocks)
  else
    (* If after the iteration, the length of the code changes, then continue to run the optimization. *)
    let _,_,_,new_execution_paths = generate_execution_paths eliminated_flat_stmt eliminated_stmt_blocks in
    let (rec_flat_stmt, rec_stmt_blocks) = optimize_ir3_method_stmts eliminated_flat_stmt eliminated_stmt_blocks new_execution_paths in
    (rec_flat_stmt, rec_stmt_blocks)


let optimize_ir3_method_body (ir3stmts:ir3_stmt list)=
  let flat_stmt_block, stmt_blocks = generate_optimization_structure ir3stmts
  in

  (* let _ = print_endline "####### Optimizing Method Body #######" in *)
  let (block_prev, block_next, block_execution_paths, stmt_execution_paths) = 
    generate_execution_paths flat_stmt_block stmt_blocks in

  (* Test code here *)
(*   let _ = print_endline "####### Stmt Blocks #######" in
  let _ = List.map (fun (block,counter) ->
    let _ = print_endline "### Block ###" in
    let _ = print_endline (string_of_int counter) in
    List.map (fun (stmt, stmt_counter, inner_counter) -> 
      print_endline (string_of_ir3_stmt stmt)
    ) block
  ) stmt_blocks in

  let _ = print_endline ("Length: " ^ (string_of_int (List.length block_execution_paths))) in

  let _ = List.map (fun (execution_path:int list) -> 
    print_endline (string_of_list execution_path  string_of_int " ")
  ) block_execution_paths in *)

  let opt_flat_stmts, opt_stmt_blocks = 
    eliminate_dead_block flat_stmt_block stmt_blocks block_execution_paths in

  let (flat_stmt_after, _) = optimize_ir3_method_stmts
    opt_flat_stmts opt_stmt_blocks stmt_execution_paths in

  let optimized_ir3_stmts = ref [] in
  let _ = print_endline "###### Debug Optimization Output ######" in
  let _ = List.iter (fun (stmt, stmt_counter, block_counter, inner_counter)  -> 
    (* let _ = print_endline ("Statement Counter:" ^ string_of_int stmt_counter) in *)
    let _ = print_endline (string_of_ir3_stmt stmt) in
    optimized_ir3_stmts := !optimized_ir3_stmts @ [stmt];
    ()
  ) flat_stmt_after in

  !optimized_ir3_stmts


let optimize_ir3 ir3_prog =
  let optimize_ir3_method md3=
    let {
      id3;
      rettype3;
      params3;
      localvars3;
      ir3stmts
    } = md3 in
    let optimized_stmts = optimize_ir3_method_body md3.ir3stmts in
    {
      id3=id3;
      rettype3=rettype3;
      params3=params3;
      localvars3=localvars3;
      ir3stmts=optimized_stmts
    }
  in

  let cdata3, main_method3, method3_list = ir3_prog in
  let opt_main_method3 = optimize_ir3_method main_method3 in
  let opt_method3_list = List.map optimize_ir3_method method3_list in
  (cdata3, opt_main_method3, opt_method3_list)

let ir3_program_to_ARM_optimized ir3_prog =
  let optimized_ir3_prog = optimize_ir3 ir3_prog in
  let _ = print_endline "####### Optimized IR3 #######" in
  let _ = print_endline (string_of_ir3_program optimized_ir3_prog) in
  ir3_program_to_ARM optimized_ir3_prog
