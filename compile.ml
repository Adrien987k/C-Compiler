open Cparse
open Genlab

type env = {
  locals : (string * int) list;        (* Locals variables with their offset from rbp *)
  globals : string list;
  functions : string list;
  strings : (string * string) list;    (* The string with its label *)
  catch_label : string;                (* When compiling a CTRY, the current label to jump to in order to
                                          be at the start of the corresponding catchs *)
  depth_try : int;                     (* The current depth of nested trys. Ex: try { try { depth = 2 } } *)
  finally_labels : string list;        (* The labels to jump to in order to exectute the current finallys *)
  in_finally : bool;                   (* Indicate if the code is currently inside a finally *)
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
                  catch_label = "";
                  depth_try = 0;
                  finally_labels = [];
                  in_finally = false;
                  current_offset = (-8) }

(* Create and return copy of [env] *)
let new_env locs globs fcts strs ctch_lab d_ctch fin_lab in_fnl offset =
   { locals = locs;
     globals = globs;
     functions = fcts;
     strings = strs;
     catch_label = ctch_lab;
     depth_try = d_ctch;
     finally_labels = fin_lab;
     in_finally = in_fnl;
     current_offset = offset }

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
                        strings env.catch_label
                        env.depth_try env.finally_labels env.in_finally env.current_offset in
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
          (* An exception is declared as a string *)
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
    List.fold_left (fun env var_dec -> compile_string_decl_var_dec env var_dec) env decl_list
  in

  (* Search and declare all globals variables *)
  let compile_globals_var_decs env decl_list =
    let compile_global_var_dec env var_dec =
      match var_dec with
      | CDECL (_, str) ->
          let new_globals = (str :: env.globals) in
          let env = new_env env.locals new_globals env.functions env.strings
                    env.catch_label env.depth_try
                    env.finally_labels env.in_finally env.current_offset in
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
              let env = new_env env.locals env.globals new_functions
                        env.strings env.catch_label
                        env.depth_try env.finally_labels env.in_finally env.current_offset in
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
              write ("\tleave\n");
              write ("\tret\n");
              write ("\t.size   " ^ str ^ ", .-" ^ str ^ "\n");
              env
            end
          else failwith ("Error: function " ^ str ^ " is already defined")
    in
    List.fold_left (fun env var_dec ->
                      let env = new_env [] env.globals env.functions
                                env.strings env.catch_label env.depth_try
                                env.finally_labels env.in_finally (-8) in
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
                  env.strings env.catch_label env.depth_try
                  env.finally_labels env.in_finally new_offset in
        env
    | CFUN (_, str, fun_var_dec_list, (_, code)) ->
        env

  and compile_code env code =
    match code with
    | CBLOCK (var_dec_list, code_list) ->
        let code_list = List.map (fun loc_code -> let _, code = loc_code in code) code_list in
        (* Create a copy of the current environment *)
        let local_env = compile_var_decl_list env var_dec_list in
        let _ = List.fold_left (fun env code' -> compile_code env code') local_env code_list in
        env
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
        let env, return_set =
        (
          begin
            (* If a return is reached in a finally, there is no more exception to handled *)
            if env.in_finally then
              begin
                write ("\tmovq   $0, .exception_raised(%rip)\n");
                write ("\tmovq   $0, .return_label_set(%rip)\n");
              end
            else ();
          end;
          (* If we are in a try and there is a finally, the finally should be executed befere the return *)
          if env.depth_try > 0  && (List.length env.finally_labels) > 0 then
            begin
              begin
                match expr_opt with
                | None ->
                  write ("\tmovq   $0, %r15\n");
                | Some (_, expr) ->
                  compile_expr env expr;
                  write ("\tmovq   %rax, %r15\n");
              end;
              (* for each finally labels *)
              List.iter (fun finally_label ->
                  (* execute the finally and if no return is found in the finally, return here *)
                  let return_label = genlab "return" in
                  (* Indicate that there is a return in the current try *)
                  write ("\tmovq   $1, .return_label_set(%rip)\n");
                  (* Store the label to get to it in r13 *)
                  write ("\tmovq   $" ^ return_label ^ ", %r13\n");
                  (* Jump to the finally *)
                  write ("\tjmp   " ^ finally_label ^ "\n");
                  write (return_label ^ ":\n");
               ) env.finally_labels;
              (new_env env.locals env.globals env.functions env.strings env.catch_label env.depth_try
                      [] env.in_finally env.current_offset, true);
            end
          else (env, false)
        )
        in
        begin
          if env.depth_try > 0 && return_set then
            begin
              write ("\tmovq   %r15, %rax\n");
            end
          else
            match expr_opt with
            | None ->
              write "\tmovq   $0, %rax\n";
            | Some (_, expr) ->
              compile_expr env expr;
        end;
        write ("\tleave\n\tret\n");
        env
    | CTHROW (str, (_, expr)) ->
        compile_expr env expr;
        write ("\tmovq   $1, .exception_raised(%rip)\n");
        write ("\tmovq   %rax, %r12\n");
        let excp = find_string env str in
        write ("\tmovq   $" ^ excp ^ ", %r14\n");
        begin
          (* If we are in a try, jump to the start of the catchs *)
          if env.depth_try > 0 then
            write ("\tjmp   " ^ env.catch_label ^ "\n")
          else ()
        end;
        write ("\tleave\n\tret\n");
        env
    | CTRY ((_, code), excp_list, loc_code_opt) ->
      let label_end = genlab "end" in
      let new_finally_labels, current_finally_label =
        begin
          match loc_code_opt with
          | Some _ ->
              let cur_f_lb = (genlab "finally") in
              cur_f_lb :: env.finally_labels, cur_f_lb
          | None -> env.finally_labels, ""
        end
      in
      let new_depth_try = (env.depth_try) + 1 in
      (* Store the labels corresponding to the start of each catch of the try *)
      let labels = [] in
      let labels = List.fold_left
        (fun labs _ ->
           let label = genlab "exception" in
           label :: labs
        ) labels excp_list
      in
      (* Store the old catch label in order to retore it after executing the code *)
      let old_catch_label = env.catch_label in
      let new_catch_label = genlab "start_catch" in
      let env = new_env env.locals env.globals env.functions
                env.strings new_catch_label new_depth_try
                new_finally_labels env.in_finally env.current_offset in
      let env = compile_code env code in
      write ("\tjmp   " ^ label_end ^ "\n");
      let env = new_env env.locals env.globals env.functions
                env.strings old_catch_label
                (env.depth_try - 1) env.finally_labels env.in_finally env.current_offset in
      write (new_catch_label ^ ":\n");
      (* When an exception is raised, For each catch search for the correct catch to jump to *)
      let _ = List.iteri
        (fun i excp -> let name, var, (_, code) = excp in
          let str_excp = find_string env name in
          write ("\tcmpq   $" ^ str_excp ^ ", %r14\n");
          write ("\tje " ^ (List.nth labels i) ^ "\n");
        ) excp_list
      in
      (* If no exception corresponds to the raised exception, execute the finally if there is one.
         Otherwise put a number diffrent from 0 in rax and leave the function *)
      if (List.length env.finally_labels) > 0 then
        write ("\tjmp   " ^ (List.hd env.finally_labels) ^ "\n")
      else
        write ("\tmovq   $42, %rax\n");
        write ("\tleave\n\tret\n");
      (* For each (catch E e), allocate a variable named e on the stack, store the value of r12 in it
         execute the corresponding code, then jump to the end of the catchs *)
      let _ = List.iteri
        (fun i excp ->
           let name, var, (_, code) = excp in
           write ((List.nth labels i) ^ ":\n");
           let new_locals = ((var, env.current_offset) :: env.locals) in
           let new_offset = (env.current_offset - 8) in
           let env = new_env new_locals env.globals env.functions
                     env.strings env.catch_label
                     env.depth_try env.finally_labels env.in_finally new_offset
           in
           write ("\tmovq   $0, .exception_raised(%rip)\n");
           write ("\tsubq   $8, %rsp\n");
           write ("\tmovq   %r12, " ^ (string_of_int (env.current_offset + 8)) ^ "(%rbp)\n");
           let _ = compile_code env code in
           write ("\tjmp   " ^ label_end ^ "\n")
        ) excp_list
      in
      (* Delete the first label of the list, retore the the old finally labels list *)
      let old_finally_labels =
        begin
          match env.finally_labels with
          | [] -> []
          | _ :: old_f_lbs -> old_f_lbs
        end
      in
      let env = new_env env.locals env.globals env.functions
                env.strings env.catch_label env.depth_try
                old_finally_labels env.in_finally env.current_offset in
      write (label_end ^ ":\n");
      (* Compile the finally if there is one *)
      begin
        match loc_code_opt with
        | None -> env
        | Some (_, code') ->
          write (current_finally_label ^ ":\n");
          let env = new_env env.locals env.globals env.functions
              env.strings env.catch_label env.depth_try
              env.finally_labels true env.current_offset in
          let env = compile_code env code' in
          let env = new_env env.locals env.globals env.functions
              env.strings  env.catch_label env.depth_try
              env.finally_labels false env.current_offset in
          (* Check if there were a return in the try and if yes, jump to it *)
          let next_label = genlab "next" in
          write ("\tcmpq   $0, .return_label_set(%rip)\n");
          write ("\tje " ^ next_label ^ "\n");
          write ("\tmovq   $0, .return_label_set(%rip)\n");
          write ("\tjmp *%r13\n");
          write (next_label ^ ":\n");
          (* Check if the finally raised an exception *)
          let next_label = genlab "next" in
          write ("\tcmpq   $0, .exception_raised(%rip)\n");
          write ("\tje " ^ next_label ^ "\n");
          write ("\tleave\n\tret\n");
          write (next_label ^ ":\n");
          write ("\tmovq   $0, .return_label_set(%rip)\n");
          env
        end

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
        (* Check if the function raised an exception *)
        let excp_not_raised = genlab "excp_not_raised" in
        write ("\tcmpq   $0, .exception_raised(%rip)\n");
        write ("\tje   " ^ excp_not_raised ^ "\n");
        begin
          if env.depth_try > 0 then
            begin
            write ("\tjmp   " ^ env.catch_label ^ "\n");
            end
          else ();
        end;
        begin
          (* If we are in finally we have to continue executing the finally *)
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
