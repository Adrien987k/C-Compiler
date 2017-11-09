open Cparse
open Genlab

let first_lines =
  ("\t.file   \"test.c\"\n" ^
   "\t.section    .rodata\n"(* ^
   ".LC0:\n" ^
   "\t.string \"La valeur du registre est: %d\\n\"\n"*))

let after_global_dec =
  ("\t.text\n" ^
   "\t.globl  main\n" ^
   "\t.type   main, @function\n" ^
   "main:\n" ^
   "\tpushq   %rbp\n" ^
   "\tmovq   %rsp, %rbp\n")

let last_lines =
  ("\tleave\n" ^
   "\tret\n" ^
   "\t.size   main, .-main\n" ^
   "\t.ident  \"GCC: (Ubuntu 5.4.0-6ubuntu1~16.04.5) 5.4.0 20160609\"\n" ^
   "\t.section         .note.GNU-stack,\"\",@progbits\n")

type env = {
  mutable locals : (string * int) list;
  mutable globals : string list;
  mutable functions : (string * int) list;
}

let contain_global env str = 
  List.exists (fun name -> (compare name str) == 0) env.globals

let find_local env str = 
  try
    let entry = List.find (fun var -> let name, _ = var in
                          (compare name str) == 0)
                          env.locals in
    let _, offset = entry in
    Some offset
  with
    Not_found -> None

let rec compile out decl_list =
  let write = Printf.fprintf out "%s" in

  let rec compile_var_dec var_dec offset is_global env =
    match var_dec with
    | CDECL (_, str) ->
        let _ = write "\nTEST DEC\n" in
        if is_global then
          let _ = env.globals <- (str :: env.globals) in
          let _ = write ("\t.comm   " ^ str ^ ",8,8\n") in
          env
        else
          let _ = env.locals <- ((str, offset) :: env.locals) in
          let _ = write ("\tmovq   $0, " ^ (string_of_int offset) ^ "(%rbp)\n") in
          env
    | CFUN (loc, str, fun_var_dec_list, (_, code)) ->
        let _ = write "\nTEST FUN\n" in
        let env = compile_decl_list fun_var_dec_list env in
        let _ = compile_code env code in env

  and compile_code env code = 
    match code with
    | CBLOCK (var_dec_list, code_list) ->
        let code_list = List.map (fun loc_code -> let _, code = loc_code in code) code_list in
        let local_env = compile_decl_list var_dec_list env in
        List.iter (compile_code local_env) code_list
    | CEXPR (_, expr) -> 
        compile_expr env expr
    | CIF ((_, expr), (_, code1), (_, code2)) -> ()
    | CWHILE ((_, expr), (_, code)) -> ()
    | CRETURN expr_opt ->
      match expr_opt with
      | None -> 
          write "\tmovq   0, %rax\n"
      | Some (_, expr) -> 
          compile_expr env expr

  and compile_expr env expr =
    match expr with
    | VAR str ->
        begin
          match find_local env str with
          | None -> 
              if contain_global env str 
              then write ("\tmovq   " ^ str ^ ", %rax\n")
          | Some offset -> 
              write ("\tmovq   " ^ (string_of_int offset) ^ "(%rbp), %rax\n")
        end
    | CST i -> 
        write ("\tmovq   " ^ (string_of_int i) ^ ", %rax\n")
    | STRING str -> ()
    | SET_VAR (str, (_, expr1)) ->
        let _ = compile_expr env expr1 in
        begin
          match find_local env str with
          | None ->
              if contain_global env str
              then write ("\tmovq   %rax, " ^ str ^ "\n")
          | Some offset -> 
              write ("\tmovq   %rax, " ^ (string_of_int offset) ^ "(%rbp)\n")
        end
    | SET_ARRAY (str, expr1, expr2) -> ()
    | CALL (str, expr_list) -> ()
    | OP1 (mon_op, (_, expr)) ->
        compile_mon_op env mon_op expr;
    | OP2 (bin_op, (_, expr1), (_, expr2)) -> ()
    | CMP (cmp_op, (_, expr1), (_, expr2)) -> ()
    | EIF ((_, expr1), (_, expr2), (_, expr3)) -> ()
    | ESEQ expr_list -> ()
  
  and compile_mon_op env op expr =
    let _ = compile_expr env expr in
    match op with
    | M_MINUS -> write "\tneg   %rax"
    | M_NOT -> write "\tnot   %rax"
    | M_POST_INC -> write "\tinc   %rax"
    | M_POST_DEC -> write "\tdec   %rax"
    | M_PRE_INC -> ()
    | M_PRE_DEC -> ()

  and compile_bin_op op expr1 expr2 env =
    match op with
    | S_MUL -> env
    | S_DIV -> env
    | S_MOD -> env
    | S_ADD -> env
    | S_SUB -> env
    | S_INDEX -> env

  and compile_cmp_op op expr1 expr2 env =
    match op with
    | C_LT -> env
    | C_LE -> env
    | C_EQ -> env
  
  and compile_decl_list decl_list env =
    match decl_list with
    | [] -> env
    | var_dec :: decl_list_next ->
        let env = compile_var_dec var_dec (-4) true env in
        compile_decl_list decl_list_next env
  in

  let env = { locals = []; globals = []; functions = [] } in
  write first_lines;
  let _ = compile_decl_list decl_list env in
  write after_global_dec;
  write last_lines;
