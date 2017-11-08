open Cparse
open Genlab

let first_lines =
  ("\t.file   \"prog.c\"\n" ^
   "\t.section    .rodata\n" ^
   ".LC0:\n" ^
   "\t.string \"La valeur du registre est: %d\\n\"\n" ^
   "\t.text\n" ^
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


let rec compile out decl_list =
  let write = Printf.fprintf out "%s" in
 
  write first_lines;
  
  let rec compile_var_dec var_dec env =
    match var_dec with
    | CDECL (loc, str) -> env
    | CFUN (loc, str, fun_var_dec_list, loc_code) -> env

  and compile_code code env =
    match code with
    | CBLOCK (var_dec_list, code_list) -> env
    | CEXPR expr -> env
    | CIF (expr, code1, code2) -> env
    | CWHILE (expr, code) -> env
    | CRETURN expr_opt ->
      match expr_opt with
      | None -> write "\tmovq   0, %rax "
      | Some expr ->
        let env = compile_expr expr env in
        write "\tmov   %rax "

  and compile_expr loc_expr env =
    let _, expr = loc_expr in
    match expr with
    | VAR str -> env
    | CST i -> env
    | STRING str -> env
    | SET_VAR (str, expr) -> env
    | SET_ARRAY (str, expr1, expr2) -> env
    | CALL (str, expr_list) -> env
    | OP1 (mon_op, expr) -> env
    | OP2 (bin_op, expr1, expr2) -> env
    | CMP (cmp_op, expr1, expr2) -> env
    | EIF (expr1, expr2, expr3) -> env
    | ESEQ expr_list -> env
  
   and compile_mon_op op expr env =
     match op with
     | M_MINUS -> env
     | M_NOT -> env
     | M_POST_INC -> env
     | M_POST_DEC -> env
     | M_PRE_INC -> env
     | M_PRE_DEC -> env

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
  
  and compile_aux out decl_list env =
    match decl_list with
    | [] -> ()
    | var_dec :: decl_list_next ->
        let env = compile_var_dec var_dec env in
        compile_aux out decl_list_next env
  in
  let env = [] in
  let _ = compile_aux out decl_list env in
  write last_lines
