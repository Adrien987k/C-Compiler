open Cparse
open Genlab

let first_lines =
  ("\t.file   \"test.c\"\n" ^
   "\t.section    .rodata\n" (*^
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
  ("\tpopq   %rbp\n" ^
   "\tret\n" ^
   "\t.size   main, .-main\n" ^
   "\t.ident  \"GCC: (Ubuntu 5.4.0-6ubuntu1~16.04.5) 5.4.0 20160609\"\n" ^
   "\t.section         .note.GNU-stack,\"\",@progbits\n")

type env = {
  mutable locals : (string * int) list;
  mutable globals : string list;
  mutable functions : (string * int) list;
  mutable strings : (string * string) list;
  mutable current_offset : int;
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

let find_var env str = 
  match find_local env str with
  | None ->
      if contain_global env str
      then (str ^ "(%rip)")
      else failwith ("Error: unbound value " ^ str)
  | Some offset ->
      ((string_of_int offset) ^ "(%rbp)")

let find_string env str = 
  try
    let entry = List.find (fun var -> let name, label = var in
                          (compare name str) == 0)
                          env.strings in
    let _, label = entry in
    label
  with
    Not_found -> failwith ("Error: unknown string : " ^ str)  


let empty_env = { locals = []; 
                  globals = [];
                  functions = [];
                  strings = [];
                  current_offset = (-8) }

let new_env locs globs fcts strs offset =
   { locals = locs; 
     globals = globs;
     functions = fcts;
     strings = strs;
     current_offset = offset }

let rec compile out decl_list =
  let write = Printf.fprintf out "%s" in

  let print_env env = 
    write ("\n\t--- ENV ---\n\tLOCALS:\n");
    List.iter 
    (fun loc_var -> let name, offset = loc_var in
                    write ("\t" ^ name ^ " : " ^ (string_of_int offset) ^ "\n") 
    ) 
    env.locals;
    write ("\tGLOBALS:\n");
    List.iter (fun name -> write ("\t" ^ name ^ "\n")) env.globals;
    write "\tFUNCTIONS:\n";
    List.iter 
    (fun fct -> let name, addr = fct in
                write ("\t" ^ name ^ " : " ^ (string_of_int addr) ^ "\n")
    )
    env.functions;
    write "\t--- END OF ENV ---\n\n";
  in

  let rec compile_string_decl env decl_list =
    let rec compile_string_decl_expr env expr = 
      match expr with
      | VAR _
      | CST _ -> env
      | STRING str ->
          let str_label = genlab "LC" in
          env.strings <- ((str, str_label) :: env.strings);
          write (str_label ^ ":\n");
          write ("\t.string \"" ^ str ^ "\"\n");
          env
      | SET_VAR (_, (_, expr1))
      | OP1 (_, (_, expr1)) ->
          compile_string_decl_expr env expr1
      | SET_ARRAY (_, (_, expr1), (_, expr2))
      | OP2 (_, (_, expr1), (_, expr2))
      | CMP (_, (_, expr1), (_, expr2)) ->
          let env = compile_string_decl_expr env expr1 in
          compile_string_decl_expr env expr2
      | EIF ((_, expr1), (_, expr2), (_, expr3)) ->
          let env = compile_string_decl_expr env expr1 in
          let env = compile_string_decl_expr env expr2 in
          compile_string_decl_expr env expr3
      | CALL (_, expr_list)
      | ESEQ expr_list -> 
          let expr_list = List.map
                          (fun loc_expr -> let _, expr = loc_expr in expr)
                          expr_list in
          List.fold_left (fun env expr -> compile_string_decl_expr env expr) env expr_list
    in
    let rec compile_string_decl_code env code =
      match code with
      | CBLOCK (_, code_list) -> 
          let code_list = List.map (fun loc_code -> let _, code = loc_code in code) code_list in
          List.fold_left (fun env code -> compile_string_decl_code env code) env code_list 
      | CEXPR (_, expr) ->
          compile_string_decl_expr env expr
      | CIF ((_, expr), (_, code1), (_, code2)) ->
          let env = compile_string_decl_expr env expr in
          let env = compile_string_decl_code env code1 in
          compile_string_decl_code env code2  
      | CWHILE ((_, expr), (_, code)) ->
          let env = compile_string_decl_expr env expr in
          compile_string_decl_code env code
      | CRETURN expr_opt ->
          match expr_opt with
          | None -> env
          | Some (_, expr) -> compile_string_decl_expr env expr

    in
    let compile_string_decl_var_dec env var_dec = 
      match var_dec with
      | CDECL (_, _) -> env
      | CFUN (_, _, fun_var_dec_list, (_, code)) ->
          let env = compile_string_decl env fun_var_dec_list in
          compile_string_decl_code env code
    in
    List.fold_left (fun env var_dec -> compile_string_decl_var_dec env var_dec) env decl_list
  in


  let rec compile_var_dec env var_dec is_global is_main =
    match var_dec with
    | CDECL (_, str) ->
        if is_global then
          let _ = env.globals <- (str :: env.globals) in
          let _ = write ("\t.comm   " ^ str ^ ",8,8\n") in
          env
        else if not is_main then
          let _ = env.locals <- ((str, env.current_offset) :: env.locals) in
          let _ = write ("\tmovq   $0, " ^ (string_of_int env.current_offset) ^ "(%rbp)\n") in
          let _ = env.current_offset <- (env.current_offset - 8) in 
          env
        else env
    | CFUN (_, str, fun_var_dec_list, (_, code)) ->
        if is_global then env else
        let env = compile_decl_list env fun_var_dec_list in
        let _ = compile_code env code in env

  and compile_code env code = 
    match code with
    | CBLOCK (var_dec_list, code_list) ->
        let code_list = List.map (fun loc_code -> let _, code = loc_code in code) code_list in
        let local_env = new_env env.locals env.globals env.functions env.strings env.current_offset in
        let local_env = compile_decl_list local_env var_dec_list in
        List.iter (compile_code local_env) code_list
    | CEXPR (_, expr) -> 
        compile_expr env expr
    | CIF ((_, expr), (_, code1), (_, code2)) ->
        compile_expr env expr;
        let false_label = genlab "false" in
        let next_label = genlab "next" in
        write "\tcmpq   $0, %rax\n";
        write ("\tje " ^ false_label ^ "\n");
        compile_code env code1;
        write ("\tjmp " ^ next_label ^ "\n");
        write (false_label ^ ":\n");
        compile_code env code2;
        write (next_label ^ ":\n");
    | CWHILE ((_, expr), (_, code')) ->
        compile_expr env expr;
        let while_label = genlab "while" in
        write (while_label ^ ":\n");
        let next_label = genlab "next" in
        write "\tcmpq   $0, %rax\n";
        write ("\tje " ^ next_label ^ "\n");
        compile_code env code';
        compile_expr env expr;
        write ("\tjmp   " ^ while_label ^ "\n");
        write (next_label ^ ":\n");
    | CRETURN expr_opt ->
      match expr_opt with
      | None ->
          write "\tmovq   $0, %rax\n"
      | Some (_, expr) ->
          compile_expr env expr

  and compile_expr env expr =
    match expr with
    | VAR str ->
        let var = find_var env str in
        write ("\tmovq   " ^ var ^ ",%rax\n")
    | CST i ->
        write ("\tmovq   $" ^ (string_of_int i) ^ ", %rax\n")
    | STRING str ->
        let str_label = find_string env str in
        write ("\tmovq   $" ^ str_label ^ ", %rax\n")
    | SET_VAR (str, (_, expr1)) ->
        let _ = compile_expr env expr1 in
        let var = find_var env str in
        write ("\tmovq   %rax, " ^ var ^ "\n")
    | SET_ARRAY (str, (_, expr1), (_, expr2)) ->
        compile_expr env expr2;
        begin
          match expr1 with
          | VAR str ->
              let var = find_var env str in
              write ("\tmovq   " ^ var ^  ", %rcx\n");
              write ("\tmovq   %rax, (%rcx, %rax, 8)\n");
          | _ -> failwith "Error: in the affectation a[i]=e, a must be a variable" 
        end
    | CALL (str, expr_list) -> ()
    | OP1 (mon_op, (_, expr)) ->
        compile_mon_op env mon_op expr
    | OP2 (bin_op, (_, expr1), (_, expr2)) ->
        compile_bin_op env bin_op expr1 expr2
    | CMP (cmp_op, (_, expr1), (_, expr2)) ->
        compile_cmp_op env cmp_op expr1 expr2
    | EIF ((_, expr1), (_, expr2), (_, expr3)) ->
        compile_eif env expr1 expr2 expr3
    | ESEQ expr_list ->
        let expr_list = List.map
                        (fun loc_expr -> let _, expr = loc_expr in expr)
                        expr_list in
        List.iter (fun expr1 -> compile_expr env expr1) expr_list

  and compile_mon_op env op expr =
    compile_expr env expr;
    match op with
    | M_MINUS -> write "\tneg   %rax\n"
    | M_NOT -> write "\tnot   %rax\n"
    | M_POST_INC -> write "\tinc   %rax\n"
    | M_POST_DEC ->
      write "\tdec   %rax\n";
      let post_op = 
        (match op with
         | M_POST_INC -> "inc"
         | M_POST_DEC -> "dec"
         | _ -> "") in
      begin 
      match expr with
      | VAR str ->
     
          ()
      | _ -> failwith "Error: cannot do ++"
      end
    | M_PRE_INC -> ()
    | M_PRE_DEC -> ()

  and compile_bin_op env op expr1 expr2 =
    match op with 
    | S_INDEX ->
        compile_expr env expr2;
        begin
          match expr1 with
          | VAR str ->
              let var = find_var env str in
              write ("\tmovq   " ^ var ^  ", %rcx\n");
              write ("\tmovq   (%rcx, %rax, 8), %rax\n")
          | _ -> failwith "Error: in the expression a[i], a must be a variable"
        end
    | _ ->
        compile_expr env expr1;
        write "\tpushq   %rax\n";
        compile_expr env expr2;
        write "\tpopq   %rcx\n";
        begin
          match op with
          | S_MUL -> write "\timulq   %rcx, %rax\n"
          | S_DIV ->
              write "\tmovq   $0, %rdx\n";
              write "\tidivq   %rcx\n"
          | S_MOD ->
              write "\tmovq   $0, %rdx\n";
              write "\tidivq   %rcx\n";
              write "\tmovq   %rdx, %rax\n"
          | S_ADD -> write "\taddq   %rcx, %rax\n"
          | S_SUB -> write "\tsubq   %rcx, %rax\n"
          | S_INDEX -> ()
        end

  and compile_cmp_op env op expr1 expr2 =
    compile_expr env expr1;
    write "\tpushq   %rax\n";
    compile_expr env expr2;
    write "\tpopq   %rcx\n";
    let asm_op = (match op with
    | C_LT -> "jl"
    | C_LE -> "jle"
    | C_EQ -> "je") in
    let label_true = genlab "true" in
    let label_next = genlab "next" in
    write "\tcmpq   %rcx, %rax\n";
    write ("\t" ^ asm_op ^ " " ^ label_true ^ "\n");
    write ("\tmovq   $1, %rax\n");
    write ("\tjmp " ^ label_next ^ "\n");
    write (label_true ^ ":\n\tmovq   $0, %rax\n");
    write (label_next ^ ": \n");

  and compile_eif env expr1 expr2 expr3 =
    compile_expr env expr1;
    let label_true = genlab "true" in
    let label_next = genlab "next" in
    write ("\tcmpq   $0, %rax\n");
    write ("\tje " ^ label_true ^ "\n");
    compile_expr env expr3;
    write ("\tjmp " ^ label_next ^ "\n");
    write (label_true ^ ":\n");
    compile_expr env expr2;
    write (label_next ^ ":\n");


  and compile_decl_list env = List.fold_left
                        (fun env var_dec -> compile_var_dec env var_dec false false)
                        env
  in

  let env = empty_env in

  write first_lines;

  let env = compile_string_decl env decl_list in

  let env = List.fold_left 
            (fun env var_dec -> compile_var_dec env var_dec true false)
            env decl_list in
  
  write after_global_dec;

  let _ = List.fold_left
          (fun env var_dec -> compile_var_dec env var_dec false true)
          env decl_list in
  (* print_env env; *)
  
  write last_lines;
