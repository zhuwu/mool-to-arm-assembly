
(* ===================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(* ===================================================== *)

open Printf

let source_files = ref []

let usage_msg = Sys.argv.(1) ^ " <source files>"

let set_source_file arg = source_files := arg :: !source_files

let parse_file file_name = 
  let org_in_chnl = open_in file_name in
  let lexbuf = Lexing.from_channel org_in_chnl in
    try
      print_string "Parsing...\n" ;
    print_string file_name ;
    print_string "\n" ;
    let prog =  MOOL_parser.input (MOOL_lexer.token file_name) lexbuf in
      close_in org_in_chnl;
        prog 
    with
    End_of_file -> exit 0  
    | Parsing.Parse_error ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let tok = Lexing.lexeme lexbuf in
        let () = print_endline ("parsing: tok "^tok) in exit 0 

let generate_file str =
  let oc = open_out "arm.s" in
  let _ = fprintf oc "%s\n" str in
  close_out oc

let _ = 
 begin
  Arg.parse [] set_source_file usage_msg ;
    match !source_files with
    | [] -> print_string "no file provided \n"
    | x::_-> 
      let prog = parse_file x in
      let () = print_endline (MOOL_structs.string_of_mOOL_program prog) in
      let typed_prog = MOOL_annotatedtyping.type_check_mOOL_program prog in
      let () = print_endline (MOOL_structs.string_of_mOOL_program typed_prog) in
      let ir3_prog = (MOOLtoir3.mOOL_program_to_IR3 typed_prog) in
      let () = print_endline (Ir3mOOL_structs.string_of_ir3_program ir3_prog) in
      let arm_prog = 
        if ((Array.length Sys.argv) > 2 && (compare Sys.argv.(1) "o") == 0)
          then Ir3toARM.ir3_program_to_ARM_optimized ir3_prog
        else
          Ir3toARM.ir3_program_to_ARM ir3_prog in
      let arm_prog_str = Arm_structs.string_of_arm_prog arm_prog in
      let () = print_endline arm_prog_str in
      let _ = generate_file arm_prog_str in
      ()
 end
