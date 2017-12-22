open Cparse
open Genlab

type env = {
  locals : (string * int) list;        (* Locals variables with their offset from rbp *)
  globals : string list;
  functions : string list;
  strings : (string * string) list;    (* The string with its label *)
  end_of_function : string;
  catch_label : string;
  depth_try : int;
  finally_label : string;
  return_label : string;
  in_finally : bool;
  current_offset : int;                (* Current offset from rbp,
                                                 used to allocate memory for new local variables *)
 }

let contain_global env str =
  List.exists (fun name -> (compare name str) = 0) env.globals

(* Find a local or a global variable, return the string to instert in the code *)
let find_var env str =
  let find_local env str =
  try
    let entry = List.find (fun var -> let name, _ = var in
                          (compare name str) = 0)
                          env.locals in
    let _, offset = entry in
    Some offset
  with
    Not_found -> None
  in
  match find_local env str with
  | None ->
      if contain_global env str
      then (str ^ "(%rip)")
      else str
  | Some offset ->
      ((string_of_int offset) ^ "(%rbp)")

(* Return the label of a string *)
let find_string env str =
  try
    let entry = List.find (fun var -> let name, label = var in
                          (compare name str) = 0)
                          env.strings in
    let _, label = entry in
    label
  with
    Not_found -> failwith ("Error: unknown string : " ^ str)

(* Return if the string [str] is in the environment [env] *)
let exist_string env str =
  List.exists (fun str_lab ->
      let str', _ = str_lab in
      (String.compare str str' = 0)) env.strings

let find_function env fct =
  List.exists (fun str -> (String.compare str fct = 0)) env.functions

let empty_env = { locals = [];
                  globals = [];
                  functions = [];
                  strings = [];
                  end_of_function = "";
                  catch_label = "";
                  depth_try = 0;
                  finally_label = "";
                  return_label = "";
                  in_finally = false;
                  current_offset = (-8) }

(* Create and return copy of [env] *)
let new_env locs globs fcts strs excp ctch_lab d_ctch fin_lab rtn_lab in_fnl offset =
   { locals = locs;
     globals = globs;
     functions = fcts;
     strings = strs;
     end_of_function = excp;
     catch_label = ctch_lab;
     depth_try = d_ctch;
     finally_label = fin_lab;
     return_label = rtn_lab;
     in_finally = in_fnl;
     current_offset = offset }

let modify_return_label env new_ret_label =
  new_env env.locals env.globals env.functions env.strings env.end_of_function env.catch_label
          env.depth_try env.finally_label new_ret_label env.in_finally env.current_offset

let rec compile out decl_list =
  let write = Printf.fprintf out "%s" in

  (* Search and declare all the strings declarations in the syntax tree *)
  let rec compile_string_decl env decl_list =
    let rec compile_string_decl_expr env expr =
      match expr with
      | VAR _
      | CST _ -> env
      | STRING str ->
          if not (exist_string env str) then
            begin
              let str_label = genlab "LC" in
              let strings = ((str, str_label) :: env.strings) in
              let env = new_env env.locals env.globals env.functions
                        strings env.end_of_function env.catch_label
                        env.depth_try env.finally_label env.return_label env.in_finally env.current_offset in
              write (str_label ^ ":\n");
              let str = String.escaped str in
              write ("\t.string \"" ^ str ^ "\"\n");
              env
            end
          else env
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
          begin
            match expr_opt with
            | None -> env
            | Some (_, expr) -> compile_string_decl_expr env expr
          end
      | CTHROW (str, (_, expr)) ->
          let env = compile_string_decl_expr env (STRING str) in
          compile_string_decl_expr env expr
      | CTRY ((_, code), excp_list, code_opt) ->
          let env = compile_string_decl_code env code in
          let env = List.fold_left
                    (fun env excp -> let str, _, (_, code') = excp in
                                     let env = compile_string_decl_expr env (STRING str) in
                                     compile_string_decl_code env code')
                    env excp_list in
          begin
            match code_opt with
            | Some (_, code') -> compile_string_decl_code env code'
            | None -> env
          end
    in
    let compile_string_decl_var_dec env var_dec =
      match var_dec with
      | CDECL (_, _) -> env
      | CFUN (_, _, fun_var_dec_list, (_, code)) ->
          let env = compile_string_decl env fun_var_dec_list in
          compile_string_decl_code env code
    in
    let env = compile_string_decl_expr env (STRING "SPECIAL DEBUG %d\n") in
    List.fold_left (fun env var_dec -> compile_string_decl_var_dec env var_dec) env decl_list
  in

  (* Search and declare all globals variables *)
  let compile_globals_var_decs env decl_list =
    let compile_global_var_dec env var_dec =
      match var_dec with
      | CDECL (_, str) ->
          let new_globals = (str :: env.globals) in
          let env = new_env env.locals new_globals env.functions env.strings
                    env.end_of_function env.catch_label env.depth_try
                    env.finally_label env.return_label env.in_finally env.current_offset in
          let _ = write ("\t.comm   " ^ str ^ ",8,8\n") in
          env
      | _ -> env
    in
    write ("\t.comm   .exception_raised,8,8\n");
    write ("\t.comm   .return_label_set,8,8\n");
    List.fold_left (fun env var_dec -> compile_global_var_dec env var_dec) env decl_list
  in

  (* Compile all functions *)
  let rec compile_functions_decs env decl_list =
    let compile_function_dec env var_dec =
      match var_dec with
      | CDECL (_, str) -> env
      | CFUN (_, str, fun_var_dec_list, (_, code)) ->
          (* If the function has not been already declared *)
          if not (find_function env str) then
            begin
              let new_functions = (str :: env.functions) in
              let end_of_function_label = genlab "end_of_function" in
              let env = new_env env.locals env.globals new_functions
                        env.strings end_of_function_label env.catch_label
                        env.depth_try env.finally_label env.return_label env.in_finally env.current_offset in
              write ("\t.text\n");
              write ("\t.globl " ^ str ^ "\n");
              write ("\t.type " ^ str ^ ", @function\n");
              write (str ^ ":\n");
              write ("\tpushq   %rbp\n");
              write ("\tmovq   %rsp, %rbp\n");
              let env = compile_var_decl_list env fun_var_dec_list in
              let nb_args = List.length fun_var_dec_list in
              let registers = ["%rdi"; "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9"] in
              (* Copy the arguments of the function in locals variables *)
              let _ = List.iteri (fun i var_dec ->
                  if i < 6 && i < nb_args then
                    (* Put arguments at -8(%rbp), -16(%rbp)... *)
                    write ("\tmovq   " ^ (List.nth registers i) ^ ", " ^ (string_of_int ((i * (-8)) - 8)) ^ "(%rbp)\n");
                  if i >= 6 && i < nb_args then
                    (* If there is more than 6 arguments, the rest are at 16(%rbp), 24(%rbp)... *)
                    begin
                      write ("\tmovq   " ^ (string_of_int (((i - 5) * 8) + 8)) ^  "(%rbp), %r10\n");
                      write ("\tmovq   %r10, " ^  (string_of_int ((i * (-8)) - 8)) ^ "(%rbp)\n")
                    end
                ) fun_var_dec_list in
              let _ = compile_code env code in
              write (env.end_of_function ^ ":\n");
              write ("\tleave\n");
              write ("\tret\n");
              write ("\t.size   " ^ str ^ ", .-" ^ str ^ "\n");
              env
            end
          else failwith ("Error: function " ^ str ^ " is already defined")
    in
    List.fold_left (fun env var_dec ->
                      let env = new_env [] env.globals env.functions
                                env.strings env.end_of_function env.catch_label env.depth_try
                                env.finally_label env.return_label env.in_finally (-8) in
                      compile_function_dec env var_dec)
    env decl_list

  (* Compile a list of variable declarations *)
  and compile_var_decl_list env decl_list =
    let offset = env.current_offset in
    let env = List.fold_left
    (fun env var_dec -> compile_var_dec env var_dec)
    env decl_list
    in
    let new_offset = (offset - env.current_offset) in
    if new_offset <> 0 then
      let _ = write ("\tsubq   $" ^ (string_of_int new_offset) ^ ", %rsp\n") in
      env
    else env

  (* Compile a variable declaration *)
  and compile_var_dec env var_dec =
    match var_dec with
    | CDECL (_, str) ->
        let new_locals = ((str, env.current_offset) :: env.locals) in
        let new_offset = (env.current_offset - 8) in
        let env = new_env new_locals env.globals env.functions
                  env.strings env.end_of_function env.catch_label env.depth_try
                  env.finally_label env.return_label env.in_finally new_offset in
        env
    | CFUN (_, str, fun_var_dec_list, (_, code)) ->
        env

  and compile_code env code =
    match code with
    | CBLOCK (var_dec_list, code_list) ->
        let code_list = List.map (fun loc_code -> let _, code = loc_code in code) code_list in
        (* Create a copy of the current environment *)
        let local_env = compile_var_decl_list env var_dec_list in
        List.fold_left (fun env code' ->
            let new_env = compile_code env code' in
            modify_return_label local_env new_env.return_label
          ) local_env code_list;
    | CEXPR (_, expr) ->
        compile_expr env expr;
        env
    | CIF ((_, expr), (_, code1), (_, code2)) ->
        compile_expr env expr;
        let false_label = genlab "false" in
        let next_label = genlab "next" in
        write "\tcmpq   $0, %rax\n";
        write ("\tje " ^ false_label ^ "\n");
        let _ = compile_code env code1 in
        write ("\tjmp " ^ next_label ^ "\n");
        write (false_label ^ ":\n");
        let _ = compile_code env code2 in
        write (next_label ^ ":\n");
        env
    | CWHILE ((_, expr), (_, code')) ->
        compile_expr env expr;
        let while_label = genlab "while" in
        write (while_label ^ ":\n");
        let next_label = genlab "next" in
        write "\tcmpq   $0, %rax\n";
        write ("\tje " ^ next_label ^ "\n");
        let _ = compile_code env code' in
        compile_expr env expr;
        write ("\tjmp   " ^ while_label ^ "\n");
        write (next_label ^ ":\n");
        env;
    | CRETURN expr_opt ->
        let env =
        (
          begin
            if env.in_finally then
              begin
                write ("\tmovq   $0, .exception_raised(%rip)\n");
                write ("\tmovq   $0, .return_label_set(%rip)\n");
              end
            else ();
           end;
            if env.depth_try > 0  && ((String.compare env.finally_label "") != 0) then
              begin
                let return_label = genlab "return" in
                write ("\tmovq   $1, .return_label_set(%rip)\n");
                write ("\tmovq   $" ^ return_label ^ ", %r13\n");
                write ("\tjmp   " ^ env.finally_label ^ "\n");
                write (return_label ^ ":\n");
                new_env env.locals env.globals env.functions
                  env.strings env.end_of_function env.catch_label env.depth_try
                  "" return_label env.in_finally env.current_offset;
              end
            else env
        )
        in
        begin
          match expr_opt with
          | None ->
            write "\tmovq   $0, %rax\n";
            write ("\tleave\n\tret\n");
          | Some (_, expr) ->
            compile_expr env expr;
            write ("\tleave\n\tret\n");
        end;
        env
    | CTHROW (str, (_, expr)) ->
        compile_expr env expr;
        write ("\tmovq   $1, .exception_raised(%rip)\n");
        write ("\tmovq   %rax, %r12\n");
        let excp = find_string env str in
        write ("\tmovq   $" ^ excp ^ ", %r14\n");
        begin
          if env.depth_try > 0 then
            write ("\tjmp   " ^ env.catch_label ^ "\n")
          else ()
        end;
        write ("\tjmp   " ^ env.end_of_function ^ "\n");
        env
    | CTRY ((_, code), excp_list, loc_code_opt) ->
      let label_end = genlab "end" in
      let finally_label =
        begin
          match loc_code_opt with
          | Some _ -> genlab "finally"
          | None -> ""
        end
      in
      let new_depth_try = (env.depth_try) + 1 in
      let labels = [] in
      let labels = List.fold_left
        (fun labs _ ->
           let label = genlab "exception" in
           label :: labs
        ) labels excp_list
      in
      let old_catch_label = env.catch_label in
      let new_catch_label = genlab "start_catch" in
      let env = new_env env.locals env.globals env.functions
                env.strings env.end_of_function new_catch_label new_depth_try
                finally_label env.return_label env.in_finally env.current_offset in
      let env = compile_code env code in
      write ("\tjmp   " ^ label_end ^ "\n");
      let env = new_env env.locals env.globals env.functions
                env.strings env.end_of_function old_catch_label
                (env.depth_try - 1) env.finally_label env.return_label env.in_finally env.current_offset in
      write (new_catch_label ^ ":\n");
      let _ = List.iteri
        (fun i excp -> let name, var, (_, code) = excp in
          let str_excp = find_string env name in
          write ("\tcmpq   $" ^ str_excp ^ ", %r14\n");
          write ("\tje " ^ (List.nth labels i) ^ "\n");
        ) excp_list
      in
      if (String.compare env.finally_label "") != 0 then
        write ("\tjmp   " ^ env.finally_label ^ "\n")
      else
        write ("\tmovq   $42, %rax\n");
        write ("\tleave\n\tret\n");
      let _ = List.iteri
        (fun i excp ->
           let name, var, (_, code) = excp in
           (* let label_excp = find_exception env name in *)
           write ((List.nth labels i) ^ ":\n");
           let new_locals = ((var, env.current_offset) :: env.locals) in
           let new_offset = (env.current_offset - 8) in
           let env = new_env new_locals env.globals env.functions
                     env.strings env.end_of_function env.catch_label
                     env.depth_try env.finally_label env.return_label env.in_finally new_offset
           in
           write ("\tmovq   $0, .exception_raised(%rip)\n");
           write ("\tsubq   $8, %rsp\n");
           write ("\tmovq   %r12, " ^ (string_of_int (env.current_offset + 8)) ^ "(%rbp)\n");
           let _ = compile_code env code in
           write ("\tjmp   " ^ label_end ^ "\n")
        ) excp_list
      in
      let env = new_env env.locals env.globals env.functions
                env.strings env.end_of_function env.catch_label env.depth_try
                "" env.return_label env.in_finally env.current_offset in
      write (label_end ^ ":\n");
      let env =
        begin
          match loc_code_opt with
          | None -> env
          | Some (_, code') ->
            write (finally_label ^ ":\n");
            let env = new_env env.locals env.globals env.functions
                env.strings env.end_of_function env.catch_label env.depth_try
                env.finally_label env.return_label true env.current_offset in
            let env = compile_code env code' in
            let env = new_env env.locals env.globals env.functions
                env.strings env.end_of_function env.catch_label env.depth_try
                env.finally_label env.return_label false env.current_offset in
            let next_label = genlab "next" in

            (* write ("\tmovq   $.LC_1, %rdi\n");
            write ("\tmovq   .return_label_set(%rip), %rsi\n");
            write ("\tmovq   $0, %rax\n");
            write ("\tcall   printf\n");
            write ("\tcltq\n"); *)

            write ("\tcmpq   $0, .return_label_set(%rip)\n");
            write ("\tje " ^ next_label ^ "\n");
            write ("\tmovq   $0, .return_label_set(%rip)\n");
            write ("\tjmp *%r13\n");
            write (next_label ^ ":\n");
            let next_label = genlab "next" in
            write ("\tcmpq   $0, .exception_raised(%rip)\n");
            write ("\tje " ^ next_label ^ "\n");
            write ("\tleave\n\tret\n");
            write (next_label ^ ":\n");
            write ("\tmovq   $0, .return_label_set(%rip)\n");
            env
        end
       in
       env

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
        write "\tpushq   %rax\n";
        compile_expr env expr1;
        write "\tpopq   %rcx\n";
        let var = find_var env str in
        write ("\tmovq   " ^ var ^  ", %rdx\n");
        write ("\tmovq   %rcx, (%rdx, %rax, 8)\n");
    | CALL (str, expr_list) ->
        let expr_list = List.map
                        (fun loc_expr -> let _, expr = loc_expr in expr)
                        expr_list
        in
        (*write ("\tpushq   %r10\n");
        write ("\tpushq   %r11\n");*)
        let registers = ["%rdi"; "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9"] in
        (* Arguments must be evaluate from left to right *)
        let expr_list = List.rev expr_list in
        (* Push all arguments on the stack *)
        List.iter
        (fun expr ->
          compile_expr env expr;
          write ("\tpushq   %rax\n");
        )
        expr_list;
        (* Remove the 6 fisrt arguments of the stack and put them in the appropriate register  *)
        List.iteri
        (fun i expr ->
          if i < 6 then
            write ("\tpopq   " ^ (List.nth registers i) ^ "\n")
        )
        expr_list;
        write ("\tmovq   $0, %rax\n");
        write ("\tcall   " ^ str ^ "\n");
        let functs_64_bits = ["fopen"; "malloc"; "calloc"; "realloc"; "exit"] in
        (* Check if the function's result is 32 bits,
           In that case, add the instruction 'cltq' in order to convert the result from 32 to 64 bits *)
        let must_align =
          (not (List.exists (fun fct -> (String.compare fct str) = 0) functs_64_bits))
          && not (find_function env str) in
        let _ = (if must_align then write "\tcltq\n") in
        (*write ("\tpopq   %r11\n");
        write ("\tpopq   %r10\n");*)
        let excp_not_raised = genlab "excp_not_raised" in
        write ("\tcmpq   $0, .exception_raised(%rip)\n");
        write ("\tje   " ^ excp_not_raised ^ "\n");
        begin
          if env.depth_try > 0 then
            begin
 (*           write ("DEPTH TRY " ^ (string_of_int env.depth_try) ^  "\n"); *)
            write ("\tjmp   " ^ env.catch_label ^ "\n");
            end
          else ();
        end;
        begin
          if not env.in_finally then
             write ("\tleave\n\tret\n")
          else ()
        end;
        write (excp_not_raised ^ ":\n");
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
    match op with
    | M_MINUS ->
        compile_expr env expr;
        write "\tneg   %rax\n"
    | M_NOT ->
        compile_expr env expr;
        write "\tnot   %rax\n"
    | _ ->
        let op, post =
        (match op with
         | M_POST_INC -> ("inc", true)
         | M_POST_DEC -> ("dec", true)
         | M_PRE_INC -> ("inc", false)
         | M_PRE_DEC -> ("dec", false)
         | _ -> ("", false)) in
        begin
          match expr with
          | VAR str ->
              let var = find_var env str in
              compile_expr env expr;
              begin
                match post with
                | true ->
                    write ("\tmovq   %rax, %rcx\n");
                    write ("\t" ^ op ^ "    %rcx\n");
                    write ("\tmovq   %rcx, " ^ var ^ "\n");
                | false ->
                    write ("\t" ^ op ^ "    %rax\n");
                    write ("\tmovq   %rax, " ^ var ^ "\n");
              end
          | OP2 (bin_op, (_, expr1), (_, expr2)) ->
              begin
                match bin_op with
                | S_INDEX ->
                    begin
                      match expr1 with
                      | VAR str' ->
                          let var = find_var env str' in
                          compile_expr env expr2;
                          write ("\tmovq   " ^ var ^ ", %rcx\n");
                          write ("\tmovq   (%rcx, %rax, 8), %rdx\n");
                          write ("\tmovq   %rdx, %rbx\n");
                          write ("\t" ^ op ^ "    %rdx\n");
                          write ("\tmovq   %rdx, (%rcx, %rax, 8)\n");
                          begin
                            match post with
                            | false -> write ("\tmovq   %rdx, %rax\n"); (* ++a[i] or --a[i] *)
                            | true -> write ("\tmovq   %rbx, %rax\n");  (* a[i]++ or a[i]-- *)
                          end
                      | _ -> failwith ("Error: in the expression a[i], a must be a variable")
                    end
                | _ -> failwith ("Error: cannot do " ^ op)
              end
          | _ -> failwith ("Error: cannot do " ^ op)
        end

  and compile_bin_op env op expr1 expr2 =
    match op with
    | S_INDEX ->
        begin
          match expr1 with
          | VAR str ->
              let var = find_var env str in
              compile_expr env expr2;
              write ("\tmovq   " ^ var ^  ", %rcx\n");
              write ("\tmovq   (%rcx, %rax, 8), %rax\n")
          | OP2 (bin_op, (_, expr1'), (_, expr2')) as expr' ->
              begin
                match bin_op with
                | S_INDEX ->
                    compile_expr env expr2;
                    write ("\tpushq   %rax\n");
                    compile_expr env expr';
                    write ("\tpopq   %rcx\n");
                    write ("\tmovq   %rax, %rdx\n");
                    write ("\tmovq   (%rdx, %rcx, 8), %rax\n")
                | _ -> failwith "Error: in the expression a[i1]...[in], a must be a variable"
              end
          | CALL (str, loc_expr_list) ->
              compile_expr env (CALL (str, loc_expr_list));
              write ("\tmovq   %rax, %rcx\n");
              compile_expr env expr2;
              write ("\tmovq   (%rcx, %rax, 8), %rax\n")
          | _ -> failwith "Error: in the expression a[i], a must be a variable !"
        end
    | _ ->
        (* Put expr1 in rax and expr2 in rcx *)
        compile_expr env expr2;
        write "\tpushq   %rax\n";
        compile_expr env expr1;
        write "\tpopq   %rcx\n";
        begin
          match op with
          | S_MUL -> write "\timulq   %rcx, %rax\n"
          | S_DIV ->
              write "\tcqto\n";
              write "\tidivq   %rcx\n"
          | S_MOD ->
              write "\tcqto\n";
              write "\tidivq   %rcx\n";
              write "\tmovq   %rdx, %rax\n"
          | S_ADD -> write "\taddq   %rcx, %rax\n"
          | S_SUB -> write "\tsubq   %rcx, %rax\n"
          | S_INDEX -> ()
        end

  and compile_cmp_op env op expr1 expr2 =
    (* Put expr1 in rax and expr2 in rcx *)
    compile_expr env expr2;
    write "\tpushq   %rax\n";
    compile_expr env expr1;
    write "\tpopq   %rcx\n";
    let asm_op = (match op with
    | C_LT -> "jl"
    | C_LE -> "jle"
    | C_EQ -> "je") in
    let label_true = genlab "true" in
    let label_next = genlab "next" in
    write "\tcmpq   %rcx, %rax\n";
    write ("\t" ^ asm_op ^ " " ^ label_true ^ "\n");
    write ("\tmovq   $0, %rax\n");
    write ("\tjmp " ^ label_next ^ "\n");
    write (label_true ^ ":\n\tmovq   $1, %rax\n");
    write (label_next ^ ": \n");

  and compile_eif env expr1 expr2 expr3 =
    compile_expr env expr1;
    let label_false = genlab "false" in
    let label_next = genlab "next" in
    write ("\tcmpq   $0, %rax\n");
    write ("\tje " ^ label_false ^ "\n");
    compile_expr env expr2;
    write ("\tjmp " ^ label_next ^ "\n");
    write (label_false ^ ":\n");
    compile_expr env expr3;
    write (label_next ^ ":\n");

  in
  let env = empty_env in
  write ("\t.file   \"test.c\"\n");
  let env = compile_globals_var_decs env decl_list in
  write ("\t.section    .rodata\n");
  write ("\tmovq   $0, .exception_raised(%rip)\n");
  write ("\tmovq   $0, .return_label_set(%rip)\n");
  let env = compile_string_decl env decl_list in
  let _ = compile_functions_decs env decl_list in ()
