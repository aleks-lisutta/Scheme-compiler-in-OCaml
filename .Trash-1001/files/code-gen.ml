#use "semantic-analyser.ml";;
open Printf

(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the offset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
     For example: [(Sexpr(Nil), (1, "T_NIL"))]
   *)
  val make_consts_tbl : expr' list -> (sexpr * (int * string)) list
  (*val make_consts_tbl : expr' list -> (sexpr*int) list*)

  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
     For example: [("boolean?", 0)]
   *)  
  val make_fvars_tbl : expr' list -> (string * int) list


  (* If you change the types of the constants and fvars tables, you will have to update
     this signature to match: The first argument is the constants table type, the second 
     argument is the fvars table type, and the third is an expr' that has been annotated 
     by the semantic analyser.
   *)
  val generate : (sexpr * (int * string)) list -> (string * int) list -> expr' -> string
end;;

module Code_Gen : CODE_GEN = struct

    let rec remove_dups old_list new_list=
    match old_list with
    | []-> new_list
    | e::s->
        if (List.mem e new_list)
        then remove_dups s new_list
        else remove_dups s (new_list@[e])
    
    let value_of_sxpr sxpr=
    match sxpr with
    | ScmVoid-> 1
    | ScmNil->  1
    | ScmBoolean(_)-> 2
    | ScmChar(_)->2
    | ScmString(x)->1+8+(String.length x)
    | ScmSymbol(_)-> 1+8
    | ScmNumber(_)-> 1+8+8
    | ScmVector(lst)-> 1+8+((List.length lst)*8)
    | ScmPair(_,_)-> 1+8+8

    let find_sxpr_3 sxpr table=
      let rec f table=
      match table with
      | (e1,(e2,e3))::s ->
          if (sexpr_eq e1 sxpr)
          then e2
          else f s
      | _ -> raise X_this_should_not_happen
      in f table

      let find_sxpr_2 sxpr table=
      let rec f table=
      match table with
      | (e1,e2)::s ->
          if (sexpr_eq e1 sxpr)
          then e2
          else f s
      | _ -> raise X_this_should_not_happen
      in f table




  let make_consts_tbl asts =

      let find_const sxpr table=
        let rec find_value table_to_check=
        match table_to_check with
        | e::s->
            if (sexpr_eq sxpr e)
            then 0
            else 
              let sum= find_value s in
              sum + (value_of_sxpr e)
        | _ -> raise X_this_should_not_happen
        in find_value table
      in

    let rec into_const const table=
    (*let ()=Printf.printf "%s\n" (string_of_sexpr const) in*)
    match const with
    | ScmChar(x)-> table@[const]
    | ScmString(x)->table@[const]
    | ScmSymbol(x)->table@[(ScmString(x));const]
    | ScmNumber(x) -> table@[const]
    | ScmVector(lst)->
        let vec=List.fold_left (fun acc sexpr-> into_const sexpr acc) table lst in
        vec@[const]
    | ScmPair(first,rest)->
        (*let ()=Printf.printf "%s %s %s" (string_of_sexpr const) (string_of_sexpr first) (string_of_sexpr rest) in *)
        let first=into_const first table in
        let rest= into_const rest first in
        rest@[const]
    | _ -> table
    in
        let rec find_all_constants expr table=
    match expr with
    | ScmConst'(const)-> into_const const table
    | ScmVar'(_)-> table
    | ScmBox'(_)-> table
    | ScmBoxGet'(_)->table
    | ScmBoxSet'(_,expr)->find_all_constants expr table
    | ScmIf'(test,dit,dif)->
        let table_test= find_all_constants test table in
        let table_dit= find_all_constants dit table_test in
        find_all_constants dif table_dit
    | ScmSeq'(lst)->
        List.fold_left (fun acc x-> find_all_constants x acc) table lst
    | ScmSet'(_,expr)-> find_all_constants expr table
    | ScmDef'(_,expr)-> find_all_constants expr table
    | ScmOr'(lst)->
        List.fold_left (fun acc x-> find_all_constants x acc) table lst
    | ScmLambdaSimple'(args,body)-> find_all_constants body table
    | ScmLambdaOpt'(args,opt,body)-> find_all_constants body table
    | ScmApplic'(func,args)->
        List.fold_left (fun acc x-> find_all_constants x acc) table ([func]@args)
    | ScmApplicTP'(func,args)->
        List.fold_left (fun acc x-> find_all_constants x acc) table ([func]@args)
    in
    let const_table_with_dups=
        List.fold_left (fun acc x-> find_all_constants x acc) [] asts in
    let must_to_be=[ScmVoid;ScmNil;ScmBoolean(false);ScmBoolean(true)] in
    let const_table_without_dups= remove_dups (must_to_be@const_table_with_dups) [] in
    let table=List.map (fun x-> (x,(find_const x const_table_without_dups))) const_table_without_dups in


    let string_for_sxpr sxpr=
    match sxpr with
    | ScmVoid -> "db T_VOID"
    | ScmNil -> "db T_NIL"
    | ScmBoolean(false)-> "MAKE_BOOL(0)"
    | ScmBoolean(true)-> "MAKE_BOOL(1)"
    | ScmChar(x)->Printf.sprintf "MAKE_LITERAL_CHAR(%c)" x
    | ScmString(x)-> Printf.sprintf "MAKE_LITERAL_STRING \"%s\"" x 
    | ScmSymbol(x)->
                  let offset= find_sxpr_2 (ScmString(x)) table in
                  Printf.sprintf "MAKE_LITERAL_SYMBOL(%d)" offset
    | ScmNumber(ScmReal(x))->Printf.sprintf "MAKE_LITERAL_FLOAT(%f)" x
    | ScmNumber(ScmRational(x,y))->
              Printf.sprintf "MAKE_LITERAL_RATIONAL(%d,%d)" x y
    | ScmVector(lst)-> 
              let indexes=List.map (fun x->Printf.sprintf "const_tbl+%d" (find_sxpr_2 x table)) lst in
              let string_list= List.fold_left (fun acc x-> Printf.sprintf "%s %s" acc x) "" indexes in
              Printf.sprintf "MAKE_LITERAL_VECTOR %d %s" (List.length lst) string_list
    | ScmPair(first,rest)->
        let first=find_sxpr_2 first table in
        let rest=find_sxpr_2 rest table in
        Printf.sprintf"MAKE_LITERAL_PAIR(%d,%d)" first rest 
    in
    List.map (fun (x,y)->(x,(y,(string_for_sxpr x)))) table






  let make_fvars_tbl asts =
    
    let rec extract_vars expr var_list=
      match expr with
      | ScmConst'(_)-> var_list
      | ScmVar'(VarFree(x))-> var_list@[x] (*aleks need to check *)
      | ScmVar'(_)->var_list
      | ScmBox'(_)->var_list(*aleks need to check *)
      | ScmBoxGet'(_)->var_list
      | ScmBoxSet'(_,expr)-> extract_vars expr var_list
      | ScmIf'(test,dit,dif)->
          let table_test=extract_vars test var_list in
          let table_dit=extract_vars dit table_test in
          extract_vars dif table_dit
      | ScmSeq'(lst)->
          fold_left_for_var_table lst var_list
      | ScmSet'(VarFree(x),expr)->
          let table_vr=var_list@[x] in
          extract_vars expr table_vr
      | ScmSet'(_,expr)-> extract_vars expr var_list
      | ScmDef'(VarFree(x),expr)->
          let table_vr=var_list@[x] in
          let result=extract_vars expr table_vr in
          result
      | ScmDef'(_,expr)-> extract_vars expr var_list
      | ScmOr'(lst)-> 
          fold_left_for_var_table lst var_list
      | ScmLambdaSimple'(args,body)-> extract_vars body var_list
      | ScmLambdaOpt'(args,opt,body)-> extract_vars body var_list
      | ScmApplic'(func,args)-> fold_left_for_var_table ([func]@args) var_list
      | ScmApplicTP'(func,args)-> fold_left_for_var_table ([func]@args) var_list


      and fold_left_for_var_table lst var_table=
       List.fold_left (fun acc expr-> extract_vars expr acc) var_table lst in 

    let rec create_f_vars_tbl exprs_list var_table=
      match exprs_list with
      | []-> var_table
      | e::s ->
          let update_list=extract_vars e var_table in
          create_f_vars_tbl s update_list  
    in

    let rec number_for_vars table n=
    match table with
    | []-> []
    | e::s->
      let rest= number_for_vars s (n+8) in
      [(e,n)]@rest in

    let table_var_with_dups= create_f_vars_tbl asts [] in
    let table_var_with_prims = table_var_with_dups@[
    "boolean?"; "flonum?"; "rational?";
    "pair?"; "null?"; "char?"; "string?";
    "procedure?"; "symbol?";
    "string-length"; "string-ref"; "string-set!";
    "make-string"; "symbol->string";
    "char->integer"; "integer->char"; "exact->inexact";
    "eq?";
    "+"; "*"; "/"; "="; "<";
    "numerator"; "denominator"; "gcd";"cons";"car"; "cdr";"set-car!" ; "set-cdr!"; "apply" ] in
    let table_var_without_dups=remove_dups table_var_with_prims [] in
  
    number_for_vars table_var_without_dups 0


  let lab = ref 0;;
  let inc() = lab := !lab+1;;

  let rec find_sexp_fvar_table name table=
  match table with
  | (e1,e2)::s->
    if (e1=name) then e2 else find_sexp_fvar_table name s
  | _ -> raise X_this_should_not_happen

  
  let generate consts fvars e =
          let rec func_yaki expr ev dep=
        match expr with
        | ScmConst'(x)-> Printf.sprintf "MAKE_CONST_CODE %d" (fst(List.assoc x consts))
        | ScmVar'(VarParam(_,minor))-> Printf.sprintf "mov rax, PVAR(%d)" minor
        | ScmSet'(VarParam(_,minor),vl)->
            let vl= func_yaki vl ev dep in
            let set_string=Printf.sprintf "MAKE_PARAM_SET_CODE %d" minor in
            Printf.sprintf "%s\n%s" vl set_string
        | ScmVar'(VarBound(_,major,minor))->
            Printf.sprintf "MAKE_VAR_BOUND_CODE %d, %d" major minor
        | ScmSet'(VarBound(_,major,minor),vl)->
            let vl=func_yaki vl ev dep in
            let set_string= Printf.sprintf "MAKE_BOUND_SET_CODE %d, %d" major minor in
            Printf.sprintf "%s\n%s" vl set_string
        | ScmVar'(VarFree(v))->
            let offset=List.assoc v fvars in
              Printf.sprintf "MAKE_GET_FREE_VAR %d" offset
        | ScmSet'(VarFree(v),vl)->
            let vl= func_yaki vl ev dep in
            let string_code=Printf.sprintf "MAKE_SET_FREE_VAR %d" (List.assoc v fvars) in
            Printf.sprintf "%s\n%s" vl string_code
        | ScmDef'(VarFree(vr),vl)->
            let vl=func_yaki vl ev dep in
            let offset= List.assoc vr fvars in
            let string_code=Printf.sprintf "MAKE_DEFINE %d" offset in
            Printf.sprintf "%s\n%s" vl string_code
        | ScmSeq' (lst)->
            List.fold_left (fun acc x-> Printf.sprintf "%s\n%s" acc (func_yaki x ev dep)) "" lst
        | ScmOr' (lst)->
            let ()=inc() in
            (List.fold_left (fun acc x-> Printf.sprintf "%s\n%s\ncmp rax, CONST_TABLE+2\njne Lexit%d\n" acc (func_yaki x ev dep ) !lab) "" lst)^(Printf.sprintf "\nLexit%d:" !lab)
        | ScmIf' (test,dit,dif) ->
            let ()=inc() in
            let iflab = !lab in
            Printf.sprintf "%s\ncmp rax, CONST_TABLE+2\nje Lelse%d\n%s\njmp Lexit%d\nLelse%d:\n%s\nLexit%d:\n" (func_yaki test ev dep) iflab (func_yaki dit ev dep) iflab iflab (func_yaki dif ev dep) iflab
        
        | ScmBoxGet'(v) -> Printf.sprintf "%s\nmov rax, qword [rax]" (func_yaki (ScmVar'(v)) ev dep) 
        | ScmBox'(VarParam(_,minor))-> Printf.sprintf "\nMAKE_BOX %d\n" minor
        | ScmBoxSet'(v,exp) -> 
        Printf.sprintf "%s\npush rax\n%s\npop qword [rax]\nmov rax, SOB_VOID_ADDRESS\n" (func_yaki exp ev dep)  (func_yaki (ScmVar'(v)) ev dep) 
        | ScmLambdaSimple'(args,body) -> 
            let ()=inc() in
            let lambdaLab= !lab in
            let body=func_yaki body 0 (dep+1) in
            let make_frame=Printf.sprintf "MAKE_FRAME_LAMBDA_SIMPLE %d" (dep) in
            let code=Printf.sprintf 
            "
            mov rbx,rax
            MAKE_CLOSURE(rax,rbx,code%d)\n
            jmp end_code%d\n

            code%d:\n
            push rbp\n
            mov rbp,rsp\n
            %s\n
            leave\n
            ret\n
            end_code%d:\n" lambdaLab lambdaLab lambdaLab body lambdaLab in
            Printf.sprintf "%s\n%s" make_frame code
        | ScmApplic'(func,args)->
            let push_magic="push SOB_NIL_ADDRESS\n  ;;magic" in
            let push_args= List.fold_right (fun  x acc-> Printf.sprintf "%s\n%s\npush rax\n" acc (func_yaki x 0 dep) )  args ""  in
            let push_n=Printf.sprintf
             "push qword %d
             %s
             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             call rax" ((List.length args)+1) (func_yaki func ev dep) in
            let all_args = Printf.sprintf "%s\n%s\n%s\n" push_magic push_args push_n in
            let pops = 
            "add rsp , 8*1 \n
            pop rbx\n
            lea rsp , [rsp + 8* rbx]" in
            all_args^pops
        | ScmApplicTP'(func,args)->
            let push_magic="push SOB_NIL_ADDRESS\n  ;;magic" in
            let push_args= List.fold_right (fun  x acc-> Printf.sprintf "%s\n%s\npush rax\n" acc (func_yaki x 0 dep) )  args ""  in
            let push_n=Printf.sprintf
             "push qword %d
             %s
             CLOSURE_ENV rbx, rax
             push rbx
             push qword [rbp+WORD_SIZE*1]
             push qword [rbp]
             SHIFT_FRAME (%d+4)
             pop rbp
             CLOSURE_CODE rax, rax
             jmp rax" ((List.length args)+1) (func_yaki func ev dep) ((List.length args)+1) in
             Printf.sprintf "%s\n%s\n%s\n" push_magic push_args push_n
        | ScmLambdaOpt'(args,opt,body) ->
            let ()=inc() in
            let lambdaLab= !lab in
            let body=func_yaki body 0 (dep+1) in
            let make_frame=Printf.sprintf "MAKE_FRAME_LAMBDA_SIMPLE %d" (dep) in
            let len=List.length args in
            let code=Printf.sprintf 
            "
            MAKE_CLOSURE(rax,rdx,code%d)          ;; lambda
            jmp end_code%d               ;; lambda

            code%d:                        ;; lambda 
            push rbp
            mov rbp,rsp


            mov rcx,[rbp+WORD_SIZE*3] ;;rcx=n 
            sub rcx, %d ;;rcx=n-(length of args)      ;;len
            dec rcx
            cmp rcx,0\n
            je noOpt%d\n                                ;;;; lambda
            

            lea rdx, [(4+%d)*WORD_SIZE+rbp]  ;;len
            MAKE_LIST rax, rcx, rdx


loop_in%d:                              ;;lambda

            lea rbx, [rbp+3*WORD_SIZE]

            mov rcx,qword [rbp+3*8] ;; rcx=n
            dec rcx
            lea rcx, [rbx+WORD_SIZE*rcx]
            mov qword [rcx], rax


            mov rdx,qword [rbp+3*8]
            sub rdx, %d             ;;len
            sub rdx, 2


            mov rcx,%d              ;;len

            cmp rdx, 0
            jle noOpt%d             ;;lambda

            cmp rcx,0
            je only_n_env_ret_rbp%d
            lea rbx, [rbp+4*WORD_SIZE]

    loop_args%d:             ;;lambda
            dec rcx
            lea r10, [rbx+WORD_SIZE*rcx]

            lea r11, [r10+WORD_SIZE*rdx]
            mov r10, qword [r10]
            mov qword [r11],r10   
            cmp rcx,0
            jne loop_args%d ;;lambda

only_n_env_ret_rbp%d:
            lea rax, [rbp+WORD_SIZE*(3+rdx)]
            lea r12, [%d+2]
            mov qword [rax],r12              ;;len
            
            sub rax,8
            mov rbx, qword [rbp+2*8]
            mov qword [rax] ,rbx
            
            sub rax,8
            mov rbx,qword [rbp+1*8]
            mov qword [rax] ,rbx

            sub rax,8
            mov rbx,qword [rbp]
            mov qword [rax] ,rbx

            mov rbp,    rax
            mov rsp,    rbp

            noOpt%d:\n              ;;lambda
            %s\n                ;;body
            leave\n
            ret\n
            end_code%d:\n           ;; lambda
            "
            lambdaLab lambdaLab lambdaLab len lambdaLab len lambdaLab len len lambdaLab lambdaLab lambdaLab lambdaLab lambdaLab   len  lambdaLab body  lambdaLab in
            Printf.sprintf "%s\n%s" make_frame code
        | _ ->  raise X_not_yet_implemented
        in
        func_yaki e 0 0
        


         


        (*



        let var_definitions=List.fold_left (fun acc (e1,e2)-> Printf.sprintf "%s\nMAKE_VAR %s %d" acc e1 e2) "" fvars in 
        Printf.sprintf "%s\n%s" var_definitions code*)
    (*create_code consts fvars e 0 0*)


end;;

