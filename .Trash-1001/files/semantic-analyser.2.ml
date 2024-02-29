(* semantic-analyser.ml
 * The semantic analysis phase of the compiler
 *
 * Programmer: Mayer Goldberg, 2021
 *)

#use "tag-parser.ml";;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;

type var' = 
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int;;

type expr' =
  | ScmConst' of sexpr
  | ScmVar' of var'
  | ScmBox' of var'
  | ScmBoxGet' of var'
  | ScmBoxSet' of var' * expr'
  | ScmIf' of expr' * expr' * expr'
  | ScmSeq' of expr' list
  | ScmSet' of var' * expr'
  | ScmDef' of var' * expr'
  | ScmOr' of expr' list
  | ScmLambdaSimple' of string list * expr'
  | ScmLambdaOpt' of string list * string * expr'
  | ScmApplic' of expr' * (expr' list)
  | ScmApplicTP' of expr' * (expr' list);;



let var_eq v1 v2 =
match v1, v2 with
  | VarFree (name1), VarFree (name2) -> String.equal name1 name2
  | VarBound (name1, major1, minor1), VarBound (name2, major2, minor2) ->
    major1 = major2 && minor1 = minor2 && (String.equal name1 name2)
  | VarParam (name1, index1), VarParam (name2, index2) ->
       index1 = index2 && (String.equal name1 name2)
  | _ -> false

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | ScmConst' (sexpr1), ScmConst' (sexpr2) -> sexpr_eq sexpr1 sexpr2
  | ScmVar' (var1), ScmVar' (var2) -> var_eq var1 var2
  | ScmIf' (test1, dit1, dif1), ScmIf' (test2, dit2, dif2) -> (expr'_eq test1 test2) &&
                                            (expr'_eq dit1 dit2) &&
                                              (expr'_eq dif1 dif2)
  | (ScmSeq' (exprs1), ScmSeq' (exprs2) | ScmOr' (exprs1), ScmOr' (exprs2)) ->
        List.for_all2 expr'_eq exprs1 exprs2
  | (ScmSet' (var1, val1), ScmSet' (var2, val2) | ScmDef' (var1, val1), ScmDef' (var2, val2)) ->
        (var_eq var1 var2) && (expr'_eq val1 val2)
  | ScmLambdaSimple' (vars1, body1), ScmLambdaSimple' (vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) && (expr'_eq body1 body2)
  | ScmLambdaOpt' (vars1, var1, body1), ScmLambdaOpt' (vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) && (expr'_eq body1 body2)
  | ScmApplic' (e1, args1), ScmApplic' (e2, args2) ->
     (expr'_eq e1 e2) && (List.for_all2 expr'_eq args1 args2)
  | ScmApplicTP' (e1, args1), ScmApplicTP' (e2, args2) ->
      (expr'_eq e1 e2) && (List.for_all2 expr'_eq args1 args2)
  | ScmBox' (v1), ScmBox' (v2) -> var_eq v1 v2
  | ScmBoxGet' (v1), ScmBoxGet' (v2) -> var_eq v1 v2
  | ScmBoxSet' (v1, e1), ScmBoxSet' (v2, e2) -> (var_eq v1 v2) && (expr'_eq e1 e2)
  | _ -> false;;


module type SEMANTIC_ANALYSIS = sig
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
  val run_semantics : expr -> expr'
end;; (* end of module type SEMANTIC_ANALYSIS *)

module Semantic_Analysis : SEMANTIC_ANALYSIS = struct

  let rec lookup_in_rib name = function
    | [] -> None
    | name' :: rib ->
       if name = name'
       then Some(0)
       else (match (lookup_in_rib name rib) with
             | None -> None
             | Some minor -> Some (minor + 1));;

  let rec lookup_in_env name = function
    | [] -> None
    | rib :: env ->
       (match (lookup_in_rib name rib) with
        | None ->
           (match (lookup_in_env name env) with
            | None -> None
            | Some(major, minor) -> Some(major + 1, minor))
        | Some minor -> Some(0, minor));;

  let tag_lexical_address_for_var name params env = 
    match (lookup_in_rib name params) with
    | None ->
       (match (lookup_in_env name env) with
        | None -> VarFree name
        | Some(major, minor) -> VarBound(name, major, minor))
    | Some minor -> VarParam(name, minor);;



  (* run this first! *)
  let annotate_lexical_addresses pe = 
   let rec run pe params env =
      match pe with
      | ScmConst(x)-> ScmConst'(x)
      | ScmVar(x)->ScmVar'(tag_lexical_address_for_var x params env)
      | ScmIf(test,dit,dif)->
          ScmIf'((run test params env) ,(run dit params env), (run dif params env))
      | ScmSeq(list) ->
          let list= List.map (fun x-> run x params env) list in
          ScmSeq'(list)
      | ScmSet(ScmVar(x),y)->ScmSet'((tag_lexical_address_for_var x params env) , (run y params env))
      | ScmDef(ScmVar(x),y)->ScmDef'((tag_lexical_address_for_var x params env), (run y params env))
      | ScmOr(list)->
          let list= List.map (fun x-> run x params env) list in
          ScmOr'(list)
      | ScmLambdaSimple(lambda_params,body)->
          ScmLambdaSimple'(lambda_params, (run body lambda_params ([params]@env)))
      | ScmLambdaOpt(list_params,opt,body)-> ScmLambdaOpt'(list_params, opt, (run body (list_params@[opt]) ([params]@env)))
      | ScmApplic(fanc, list)-> ScmApplic'( (run fanc params env), (List.map (fun x-> run x params env) list))
      | _ -> raise X_this_should_not_happen
          
   in 
   run pe [] [];;

  let rec rdc_rac s =
    match s with
    | [e] -> ([], e)
    | e :: s ->
       let (rdc, rac) = rdc_rac s
       in (e :: rdc, rac)
    | _ -> raise X_this_should_not_happen;;
  
  (* run this second! *)
  let annotate_tail_calls pe =
   let rec run pe in_tail =
      match pe with
      | ScmApplic'(fanc, args)->
          let fanc=run fanc false in
          let args= List.map (fun x->run x false) args in
          if in_tail
            then ScmApplicTP'(fanc,args)
            else ScmApplic'(fanc,args)
      | ScmConst'(x)->pe
      | ScmVar'(x) -> pe
      | ScmIf'(test,dit,dif)->ScmIf'((run test false), (run dit in_tail), (run dif in_tail))
      | ScmSeq'(list)->
          let (rdc, rac) = rdc_rac list in
          let rdc= List.map (fun x-> run x false) rdc in
          let rac= run rac in_tail in
          ScmSeq'(rdc@[rac])
      | ScmSet'(x,y)-> ScmSet'(x,(run y false))
      | ScmDef'(x,y)-> ScmDef'(x,(run y false))
      | ScmOr'(list)->
          let (rdc, rac) = rdc_rac list in
          let rdc= List.map (fun x-> run x false) rdc in
          let rac= run rac in_tail in
          ScmOr'(rdc@[rac])
      | ScmLambdaSimple'(args,body)->ScmLambdaSimple'(args,(run body true))
      | ScmLambdaOpt'(args,opt,body)->ScmLambdaOpt'(args,opt,(run body true))
      | _ -> raise X_this_should_not_happen
   in 
   run pe false;;

  (* boxing *)

  let rec write_name_func name expr=
    match expr with
    | ScmConst'(x)->(false,false)
    | ScmVar'(x)->(false,false)
    | ScmIf'(test,dit,dif)->
        let (t1,t2)=write_name_func name test in
        let (p1,p2)=write_name_func name dit in
        let (m1,m2)=write_name_func name dif in
        ((t1||p1||m1),(t2||p2||m2))
    | ScmSeq'(list)->List.fold_left (fun (acc,p) x-> if acc then (acc,p) else write_name_func name x) (false,false) list
    | ScmSet'(VarParam(nv,_),_)->(false,nv=name)
    | ScmSet'(VarBound(nv,_,_),_)->(nv=name,false)
    | ScmSet'(_,x)->write_name_func name x
    | ScmDef'(_,x)-> write_name_func name x
    | ScmOr'(list)->List.fold_left (fun (acc,p) x-> if acc then (acc,p) else write_name_func name x) (false,false) list
    | ScmLambdaSimple'(args,body)->
      if (List.mem name args) then (false,false) else write_name_func name body
    | ScmLambdaOpt'(args,opt,body)-> if (List.mem name (args@[opt])) then (false,false) else write_name_func name body
    | ScmApplic'(f,args)->List.fold_left (fun (acc,p) x-> if acc then (acc,p) else write_name_func name x) (false,false) ([f]@args)
    | ScmApplicTP'(f,args)->List.fold_left (fun (acc,p) x-> if acc then (acc,p) else write_name_func name x) (false,false) ([f]@args)
    | ScmBoxSet'(_,value)-> write_name_func name value 
    | _ -> (false,false)

  let read_name_func name exp=
    let rec func expr=
    match expr with
    | ScmConst'(x)->(false,false)
    | ScmVar'(VarBound(nv,_,_))->(nv=name,false)
    | ScmVar'(VarParam(nv,_))->(false,nv=name)
    | ScmVar'(_)->(false,false)
    |ScmIf'(test,dit,dif)->
        let (t1,t2)=func test in
        let (p1,p2)=func dit in
        let (m1,m2)=func dif in
        ((t1||p1||m1),(t2||p2||m2))
    | ScmSeq'(list)-> List.fold_left 
        (fun (b,p) x-> 
            let (bo,pr)=func x in
            ((b||bo),(p||pr))) (false,false) list
    | ScmSet'(_,y)-> func y
    | ScmDef'(_,y)-> func y
    | ScmOr'(list)-> List.fold_left 
        (fun (b,p) x-> 
            let (bo,pr)=func x in
            ((b||bo),(p||pr))) (false,false) list
    | ScmLambdaSimple'(args,body)->
    if (List.mem name args) then (false,false) else func body
    | ScmLambdaOpt'(args,opt,body)-> if (List.mem name (args@[opt])) then (false,false) else func body
    | ScmApplic'(f,args)-> List.fold_left 
        (fun (b,p) x-> 
            let (bo,pr)=func x in
            ((b||bo),(p||pr))) (false,false) ([f]@args)
    | ScmApplicTP'(f,args)-> List.fold_left 
        (fun (b,p) x-> 
            let (bo,pr)=func x in
            ((b||bo),(p||pr))) (false,false) ([f]@args)
    | ScmBoxSet'(_,value)-> func value
    | _ -> (false,false) 
    in func exp;;

    (* let read_name_func1 name exp lambda=
      let rec find_read exp lambda lst=
        match exp with
        | ScmVar'(VarBound(n,_,_))-> if (n=name) then lst@[lambda] else lst
        | ScmVar'(VarParam(n,_))-> if (n=name) then lst@[lambda] else lst
        | ScmIf'(test,dit,dif)-> List.fold_left (fun acc x-> find_read x lambda acc) lst [test;dit;dif]
        | ScmSeq'(l)-> List.fold_left (fun acc x-> find_read x lambda acc) lst l
        | ScmSet'(_,vl)-> find_read vl lambda lst
        | ScmBoxSet'(_,vl) ->find_read vl lambda lst
        | ScmOr'(l)-> List.fold_left (fun acc x-> find_read x lambda acc) lst l
        | ScmApplic'(func,args)-> List.fold_left (fun acc x-> find_read x lambda acc) lst [func]@args
        | ScmApplicTP'(func,args) -> List.fold_left (fun acc x-> find_read x lambda acc) lst [func]@args
        | ScmLambdaSimple'(args,body)-> if (List.mem name args) then lst else find_read body exp lst
        | ScmLambdaOpt'(args,opt,body)-> if (List.mem name (args@[opt])) then lst else find_read body exp lst
        | _ -> lst
      in find_read exp lambda [];;

    let write_name_func1 name exp lambda=
      let rec find_write exp lambda lst=
        match exp with
        | ScmIf'(test,dit,dif)-> List.fold_left (fun acc x-> find_write x lambda acc) lst [test;dit;dif]
        | ScmSeq'(l)-> List.fold_left (fun acc x-> find_write x lambda acc) lst l
        | ScmSet'(n,vl)-> if (n=name) then find_write vl lambda lst@[lambda] else find_write vl lambda lst
        | ScmBoxSet'(_,vl) ->find_write vl lambda lst
        | ScmOr'(l)-> List.fold_left (fun acc x-> find_write x lambda acc) lst l
        | ScmApplic'(func,args)-> List.fold_left (fun acc x-> find_write x lambda acc) lst [func]@args
        | ScmApplicTP'(func,args) -> List.fold_left (fun acc x-> find_write x lambda acc) lst [func]@args
        | ScmLambdaSimple'(args,body)-> if (List.mem name args) then lst else find_write body exp lst
        | ScmLambdaOpt'(args,opt,body)-> if (List.mem name (args@[opt])) then lst else find_write body exp lst
        | _ -> lst
      in find_write exp lambda [];; *)



  let create_tuples name=
  (fun body->
    let read_name= read_name_func name body in
    let write_name= write_name_func name body in
    (read_name,write_name)
  )
  
  let change_body name body=
    let rec func expr=
      match expr with
      | ScmConst'(x)-> expr
      | ScmVar'(VarParam(n,_) as x)->
          if (n=name)
          then ScmBoxGet'(x)
          else expr
      | ScmVar'(VarBound(n,_,_) as x)->
          if (n=name)
          then ScmBoxGet'(x)
          else expr
      | ScmVar'(x)->expr
      | ScmIf'(test,dit,dif) ->
          let test= func test in
          let dit=func dit in
          let dif=func dif in ScmIf'(test,dit,dif)
      | ScmSeq'(list)->
          let list= List.map func list in
          ScmSeq'(list)
      | ScmSet'((VarParam(n,_) as set),x)->
          let value= func x in
          if (n=name)
            then ScmBoxSet'(set,value)
          else ScmSet'(set, value)
      | ScmSet'((VarBound(n,_,_) as set),x)->
          let value= func x in
          if(n=name)
          then  
            ScmBoxSet'(set,value)
          else ScmSet'(set,value)
      | ScmSet'(x,y)->ScmSet'(x,(func y))
      | ScmDef'(x,y)-> ScmDef'(x,(func y))
      | ScmOr'(list)->
          let list= List.map func list in
          ScmOr'(list)
      | (ScmLambdaSimple'(args,lsbody) as lambda)->
          if (List.mem name args) then lambda else ScmLambdaSimple'(args, (func lsbody))
      | (ScmLambdaOpt'(args,opt,lsbody) as lambda)->
          if (List.mem name (args@[opt])) then lambda else ScmLambdaOpt'(args,opt, (func lsbody))
      | ScmApplic'(f,args)->
          let f=func f in
          let args= List.map func args in
          ScmApplic'(f,args)
      | ScmApplicTP'(f,args)->
          let f=func f in
          let args= List.map func args in
          ScmApplicTP'(f,args)
      | ScmBoxSet'(x,y)-> ScmBoxSet'(x,(func y))
      | _ ->expr
    in func body;;



  let rec check_read_write num_order list=
  match list with
  | []-> false
  | (num,read,true)::s ->
        if (num_order=num)
          then check_read_write num_order s
          else true
  | e::s-> check_read_write num_order s
  ;;

        
  let rec rw_in_tup rb rp wb wp list=
  match list with
  | []-> ((rb && (wb || wp)) || (rp && wb))
  |e::s ->
    match e with
    | ((true,x),(true,y))-> if (rb || wb || rp || wp || x || y) then true else rw_in_tup true x true y s
    | ((true,x),(false,true)) -> if (wp || wb || rb || x) then true else rw_in_tup true (rp||x) false true s
    | ((true,x), (false,false))-> if (wb || wp) then true else rw_in_tup true (x||rp) false false s
    | ((false,true), (true,y)) -> if (wb || rb ||rp||y) then true else rw_in_tup false true true (y||wp) s
    | ((false,true),(false,true))-> if(wb || rb) then true else rw_in_tup false true false true s
    | ((false,true), (false,false))-> if (wb) then true else rw_in_tup rb true false wp s
    | ((false,false),(true,y))-> if (rb || rp) then true else rw_in_tup false false true (y||wp) s
    | ((false,false),(false,true)) -> if (rb) then true else rw_in_tup false rp wp true s
    | ((false,false),(false,false))-> rw_in_tup rb rp wb wp s





  ;; 
(*
    let rec var_tostring vr=
    match vr with
    | VarFree(name)-> name
    |VarBound(name,_,_)->name
    |VarParam(name,_)->name 

    let rec to_string expr=
      match expr with
      | ScmConst'(x)-> string_of_sexpr x
      | ScmVar'(name)-> var_tostring name
      | ScmBox'(name)-> ("(ScmBox"^(var_tostring name)^")")
      | ScmBoxGet'(name)->("(ScmBoxGet"^(var_tostring name)^")")
      | ScmBoxSet'(name,value)->("(ScmBoxSet "^(var_tostring name)^" "^(to_string value)^")")
      | ScmIf'(test,dit,dif)->
          let test=to_string test in
          let dit= to_string dit in
          let dif=to_string dif in
          ("(if ("^ test^")\n then"^ dif^"\nelse"^ dif^")\n")
      | ScmSeq'(list)->
            let list=List.fold_left (fun acc b-> acc^(to_string b)) "" list in
            ("(seq\n"^ list^")\n")
      | ScmSet'(name,value)->
            let name= var_tostring name in
            let value= to_string value in
            ("(Set! "^ name^" "^ value^")")
      | ScmDef'(name,value)->
            let name= var_tostring name in
            let value= to_string value in
            ("(Def "^ name^" "^ value^")")
      | ScmOr'(list)->
            let list=List.fold_left (fun acc b-> acc^(to_string b)) "" list in
            ("(or "^ list^")\n")
      | ScmLambdaSimple'(args,body)->
            let args= List.fold_left (fun acc b-> acc^" "^b) "" args in
            let body= to_string body in
            ("(lambda\t("^ args^") "^body^")")
      | ScmLambdaOpt'(args,x,body)->
            let args= List.fold_left (fun acc b-> acc^" "^b) "" args in
            let body= to_string body in
            ("(lambda\t("^ args^" . "^ x^") "^body^")")
      | ScmApplic'(func,args)->
            let args= List.map to_string args in
            let args= List.fold_left (fun acc b-> acc^" "^b) "" args in
            let func= to_string func in
            ("(Applic "^func^" "^ args^")")
      | ScmApplicTP'(func,args)->
            let args= List.map to_string args in
            let args= List.fold_left (fun acc b-> acc^" "^b) "" args in
            let func= to_string func in
            ("(ApplicTP "^func^" "^args^")")
  *)

  let need_box_set arg body c=
    let ((rb,rp),(wb,wp))=c in
    let create_tuple_func=create_tuples arg in
    let list_read_write =List.map create_tuple_func body in
    let need_to_change= rw_in_tup  rb rp wb wp list_read_write in (*
    let () = List.iter (fun ((x,y),(a,b)) -> (printf "((%b,%b),(%b,%b))\n" x y a b)) ([c]@list_read_write) in
    let () = printf "%s NTC? %b\n" arg need_to_change in*)
    need_to_change

  let check_arg_bodies name bodies=
    let create_tuple_func=create_tuples name in
    let list_read_write =List.map create_tuple_func bodies in
    let need_to_change= rw_in_tup  false false false false list_read_write in
  (*
    let () = printf "%s:\n" name in*)
    (*let () = List.iter (fun ((x,v),(u,y))->(printf "((%b,%b),(%b,%b))" x v u y)) list_read_write in
    
    let () = printf "%b name %s yaki:\n" need_to_change name in *)
    if (need_to_change)
      then(true,(List.map (fun body-> change_body name body) bodies))
      else (false,bodies)
    ;;

let control_box arg body c =
      let rec func expr =
      match expr with
      | ScmConst'(x)-> false
      | ScmVar'(x)-> false
      | ScmBox'(x) -> false
      | ScmBoxGet'(x)-> false
      | ScmBoxSet'(name,value)-> func value  
      | ScmIf'(test,dit,dif)-> ((func test) || (func dit) || (func dif))
      | ScmSeq'(list)-> need_box_set arg list c
      | ScmSet'(name,value)-> func value
      | ScmOr'(list)-> need_box_set arg list c
      | ScmLambdaSimple'(args_lambda,body)-> false 
      | ScmLambdaOpt'(args_lambda,opt,body)->false
      | ScmApplic'(f,args_app)-> need_box_set arg ([f]@args_app) c
      | ScmApplicTP'(f,args_app)-> need_box_set arg ([f]@args_app) c
      | _ ->false
    in func body



  let rec box_set expr =
    match expr with
    | ScmConst'(x)->expr
    | ScmVar'(x)->expr
    | ScmIf'(test,dit,dif)-> ScmIf'(box_set test, box_set dit,box_set dif)
    | ScmSeq'(list)->
        let list= List.map (fun x-> box_set x) list in
        ScmSeq'(list)
    | ScmSet'(x,y)-> ScmSet'(x,box_set y)
    | ScmDef'(x,y)-> ScmDef'(x,box_set y)
    | ScmOr'(list)->
        let list= List.map (fun x-> box_set x) list in
        ScmOr'(list)
    | ScmApplic'(fanc, list)->
        let fanc=box_set fanc in
        let list= List.map (fun x-> box_set x) list in
        ScmApplic'(fanc,list)
    | ScmApplicTP'(fanc, list)->
        let fanc=box_set fanc in
        let list= List.map (fun x-> box_set x) list in
        ScmApplicTP'(fanc,list)

    | ScmLambdaSimple'(args,ScmSeq'(list))->
        let body=check_list_box args list [] 0 in
        ScmLambdaSimple'(args,body)

    | ScmLambdaSimple'(args,ScmApplic'(f,f_args))->
        let body=check_args_box args f f_args false [] 0 in
        ScmLambdaSimple'(args,body)

    | ScmLambdaSimple'(args,ScmApplicTP'(f,f_args))->
        let body=check_args_box args f f_args true [] 0 in
        ScmLambdaSimple'(args,body)
    
    | ScmLambdaSimple'(args, ScmOr'(list))->
        let body= check_or_box args list [] 0 in
        ScmLambdaSimple'(args,body)
    | ScmLambdaSimple'(args,ScmIf'(test,dit,dif))->
        let body= check_if_box args test dit dif [] 0 in
        ScmLambdaSimple'(args,body)
    | ScmLambdaSimple'(args,ScmSet'(name,value))->
        let body= check_set_box args name value false [] 0 in
        ScmLambdaSimple'(args,body)
    | ScmLambdaSimple'(args,ScmBoxSet'(name,value))->
        let body= check_set_box args name value true [] 0 in
        ScmLambdaSimple'(args,body) 

    | ScmLambdaSimple'(args,body)->ScmLambdaSimple'(args,box_set body)

    | ScmLambdaOpt'(args,opt,ScmSeq'(list))->
        let body=check_list_box (args@[opt]) list [] 0 in
        ScmLambdaOpt'(args,opt,body)

    | ScmLambdaOpt'(args,opt,ScmApplic'(f,f_args))->
        let body=check_args_box (args@[opt]) f f_args false [] 0 in
        ScmLambdaOpt'(args,opt,body)

    | ScmLambdaOpt'(args,opt,ScmApplicTP'(f,f_args))->
        let body=check_args_box (args@[opt]) f f_args true [] 0 in
        ScmLambdaOpt'(args,opt,body)
    
    | ScmLambdaOpt'(args,opt,ScmOr'(body))->
        let body= check_or_box (args@[opt]) body [] 0 in
        ScmLambdaOpt'(args,opt,body)
    
    | ScmLambdaOpt'(args,opt,ScmIf'(test,dit,dif))->
        let body= check_if_box (args@[opt]) test dit dif [] 0 in
        ScmLambdaOpt'(args,opt,body)
    | ScmLambdaOpt'(args,opt,ScmSet'(name,value))->
        let body= check_set_box (args@[opt]) name value false [] 0 in
        ScmLambdaOpt'(args,opt,body)
    | ScmLambdaOpt'(args,opt,ScmBoxSet'(name,value))->
        let body= check_set_box (args@[opt]) name value true [] 0 in
        ScmLambdaOpt'(args,opt,body) 

    | ScmLambdaOpt'(args,opt,body)->ScmLambdaOpt'(args,opt,box_set body)
    
    | ScmBoxSet'(x,y)->ScmBoxSet'(x,(box_set y))
    | _ ->expr

    
    

    and check_set_box args name value bool list n =
    let nstr = 
      (match name with 
      | VarParam(x,i) -> x
      | VarBound(x,i,j) -> x
      | _ -> "") in
    match args with
    | []->
    let value= box_set value in
        if(List.length list)>0
        then
          if (bool || (List.mem nstr (List.map
           (fun x -> match x with 
              | ScmSet'(VarParam(e,n),ScmBox'(VarParam(x,y)))-> e
              | _ -> raise X_this_should_not_happen) list)))
            then ScmSeq'(list@[ScmBoxSet'(name,value)])
            else ScmSeq'(list@[ScmSet'(name,value)])
        else
          if bool
            then ScmBoxSet'(name,value)
            else ScmSet'(name,value)
    | e::s->
        let c = e=nstr in
        let need_to_change= control_box e value ((false,false),(false,c)) in
        (*let () = printf "%s NTC? %b" e need_to_change in*)
        if(need_to_change)
          then
           let value=change_body e value in
           check_set_box s name value bool (list@[(ScmSet'(VarParam(e,n),ScmBox'(VarParam(e,n))))]) (n+1)
          else
           check_set_box s name value bool list (n+1) 


    and check_if_box args test dit dif list n=
    match args with
    | []->
      let test= box_set test in
      let dit=box_set dit in
      let dif=box_set dif in
        if(List.length list)>0
        then
            ScmSeq'(list@[ScmIf'(test,dit,dif)])
        else
            ScmIf'(test,dit,dif)
    | e::s->

        let need_to_change= ((control_box e test ((false,false),(false,false))) || (control_box e dit ((false,false),(false,false))) || (control_box e dif ((false,false),(false,false)))) in
        if(need_to_change)
          then
           let test=change_body e test in
           let dit= change_body e dit in
           let dif=change_body e dif in
           check_if_box s test dit dif (list@[(ScmSet'(VarParam(e,n),ScmBox'(VarParam(e,n))))]) (n+1)
          else
           check_if_box s test dit dif list (n+1)

    and check_or_box args bodies list n=
    match args with
    | [] ->
        let bodies= List.map (fun x-> box_set x) bodies in
        if(List.length list)>0
        then ScmSeq'(list@[ScmOr'(bodies)])
        else ScmOr'(bodies)
    | e::s ->
        let (bool,bodies)= check_arg_bodies e bodies in
        if(bool)
          then check_or_box s bodies (list@[(ScmSet'(VarParam(e,n),ScmBox'(VarParam(e,n))))]) (n+1)
          else check_or_box s bodies list (n+1)


  and check_list_box args bodies list n=
    match args with
    | [] ->
        let bodies= List.map (fun x-> box_set x) bodies in
        ScmSeq'(list@bodies)
    | e::s ->
        let (bool,bodies)= check_arg_bodies e bodies in
        if(bool)
          then check_list_box s bodies (list@[(ScmSet'(VarParam(e,n),ScmBox'(VarParam(e,n))))]) (n+1)
          else check_list_box s bodies list (n+1)
  
  and check_args_box args f bodies is_tp list n=
    match args with
    | [] ->
        let f= box_set f in
        let bodies= List.map (fun x-> box_set x) bodies in
        if List.length list=0 
        then
          if is_tp
            then
              ScmApplicTP'(f,bodies)
            else
              ScmApplic'(f,bodies)
        else
          if is_tp
            then
              ScmSeq'(list@[ScmApplicTP'(f,bodies)])
            else
              ScmSeq'(list@[ScmApplic'(f,bodies)])
    | e::s ->
        let x =check_arg_bodies e ([f]@bodies) in
        match x with
        | (true,f::bodies)-> check_args_box s f bodies is_tp (list@[ScmSet'(VarParam(e,n),ScmBox'(VarParam(e,n)))]) (n+1)
        | (false, f::bodies)-> check_args_box s f bodies is_tp list (n+1)
        | _ -> raise X_this_should_not_happen



  let run_semantics expr =
    box_set
      (annotate_tail_calls
         (annotate_lexical_addresses expr))
  (* let run_semantics expr =
    box_set
         (annotate_lexical_addresses expr) *)

end;; (* end of module Semantic_Analysis *)
