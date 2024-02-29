#use "reader.ml";;
open Printf;;
type expr =
  | ScmConst of sexpr
  | ScmVar of string
  | ScmIf of expr * expr * expr
  | ScmSeq of expr list
  | ScmSet of expr * expr
  | ScmDef of expr * expr
  | ScmOr of expr list
  | ScmLambdaSimple of string list * expr
  | ScmLambdaOpt of string list * string * expr
  | ScmApplic of expr * (expr list);;

exception X_syntax_error of sexpr * string;;
exception X_reserved_word of string;;
exception X_proper_list_error;;
exception X_not_implemented;;

let rec list_to_proper_list = function
| [] -> ScmNil
| hd::[] -> ScmPair (hd, ScmNil)
| hd::tl -> ScmPair (hd, list_to_proper_list tl);;

let rec list_to_improper_list = function
| [] -> raise X_proper_list_error
| hd::[] -> hd
| hd::tl -> ScmPair (hd, list_to_improper_list tl);;

let rec scm_append scm_list sexpr =
match scm_list with
| ScmNil -> sexpr
| ScmPair (car, cdr) -> ScmPair (car, scm_append cdr sexpr)
| _ -> raise (X_syntax_error (scm_list, "Append expects a proper list"))

let rec scm_map f sexpr =
match sexpr with
| ScmNil -> ScmNil
| ScmPair (car, cdr) -> ScmPair (f car, scm_map f cdr)
| _ -> raise (X_syntax_error (sexpr, "Map expects a list"));;

let rec scm_zip f sexpr1 sexpr2 =
match sexpr1, sexpr2 with
| ScmNil, ScmNil -> ScmNil
| ScmPair (car1, cdr1), ScmPair (car2, cdr2) -> ScmPair (f car1 car2, scm_zip f cdr1 cdr2)
| _, _ ->
    let sepxrs = list_to_proper_list [ScmSymbol "sexpr1:"; sexpr1; ScmSymbol "sexpr2:"; sexpr2] in
    raise (X_syntax_error (sepxrs, "Zip expects 2 lists of equal length"));;

let rec scm_list_to_list = function
| ScmPair (hd, tl) -> hd::(scm_list_to_list tl)
| ScmNil -> []
| sexpr -> raise (X_syntax_error (sexpr, "Expected proper list"));;

let rec scm_is_list = function
| ScmPair (hd, tl) -> scm_is_list tl
| ScmNil -> true
| _ -> false

let rec scm_list_length = function
| ScmPair (hd, tl) -> 1 + (scm_list_length tl)
| ScmNil -> 0
| sexpr -> raise (X_syntax_error (sexpr, "Expected proper list"));;

let rec untag expr =
let untag_vars vars = List.map (fun s -> ScmSymbol s) vars in
let untag_tagged_list tag exprs = list_to_proper_list (ScmSymbol tag::(List.map untag exprs)) in

let untag_lambda_opt vars var body =
let vars = match vars with
| [] -> ScmSymbol var
| _ -> list_to_improper_list (untag_vars (vars@[var])) in
list_to_proper_list ([ScmSymbol "lambda"; vars]@body) in

match expr with
| (ScmConst (ScmSymbol(_) as sexpr)
    | ScmConst (ScmNil as sexpr)
    | ScmConst (ScmPair (_, _) as sexpr)) -> list_to_proper_list [ScmSymbol "quote"; sexpr]
| ScmConst s -> s
| ScmVar (name) -> ScmSymbol(name)
| ScmIf (test, dit, ScmConst (ScmVoid)) -> untag_tagged_list "if" [test; dit]
| ScmIf (test, dit, dif) -> untag_tagged_list "if" [test; dit; dif]
| ScmSeq(exprs) -> untag_tagged_list "begin" exprs
| ScmSet (var, value) -> untag_tagged_list "set!" [var; value]
| ScmDef (var, value) -> untag_tagged_list "define" [var; value]
| ScmOr (exprs) -> untag_tagged_list "or" exprs
| ScmLambdaSimple (vars, ScmSeq(body)) ->
    let vars = list_to_proper_list (untag_vars vars) in
    let body = List.map untag body in
    list_to_proper_list ([ScmSymbol "lambda"; vars]@body)
| ScmLambdaSimple (vars, body) ->
    let vars = list_to_proper_list (untag_vars vars) in
    list_to_proper_list ([ScmSymbol "lambda"; vars; untag body])
| ScmLambdaOpt (vars, var, ScmSeq(body)) ->
    let body = List.map untag body in
    untag_lambda_opt vars var body
| ScmLambdaOpt (vars, var, body) ->
    let body = [untag body] in
    untag_lambda_opt vars var body
| ScmApplic(procedure, args) -> list_to_proper_list (List.map untag (procedure::args));;


let rec string_of_expr expr =
string_of_sexpr (untag expr)

let scm_number_eq n1 n2 =
match n1, n2 with
| ScmRational(numerator1, denominator1), ScmRational(numerator2, denominator2) ->
        numerator1 = numerator2 && denominator1 = denominator2
| ScmReal(real1), ScmReal(real2) -> abs_float(real1 -. real2) < 0.001
| _, _ -> false

let rec sexpr_eq s1 s2 =
match s1, s2 with
| (ScmVoid, ScmVoid) | (ScmNil, ScmNil)  -> true
| ScmBoolean(bool1), ScmBoolean(bool2) -> bool1 = bool2
| ScmChar(char1), ScmChar(char2) -> char1 = char2
| ScmString(string1), ScmString(string2) -> String.equal string1 string2
| ScmSymbol(symbol1), ScmSymbol(symbol2) -> String.equal symbol1 symbol2
| ScmNumber(number1), ScmNumber(number2) -> scm_number_eq number1 number2
| ScmVector(sexprs1), ScmVector(sexprs2) -> List.for_all2 sexpr_eq sexprs1 sexprs2
| ScmPair(car1, cdr1), ScmPair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
| _, _ -> false

let rec expr_eq e1 e2 =
  match e1, e2 with
  | ScmConst (sexpr1), ScmConst (sexpr2) -> sexpr_eq sexpr1 sexpr2
  | ScmVar (var1), ScmVar (var2) -> String.equal var1 var2
  | ScmIf (test1, dit1, dif1), ScmIf (test2, dit2, dif2) -> (expr_eq test1 test2) &&
                                            (expr_eq dit1 dit2) &&
                                              (expr_eq dif1 dif2)
  | (ScmSeq(exprs1), ScmSeq(exprs2) | ScmOr (exprs1), ScmOr (exprs2)) ->
        List.for_all2 expr_eq exprs1 exprs2
  | (ScmSet (var1, val1), ScmSet (var2, val2) | ScmDef (var1, val1), ScmDef (var2, val2)) ->
        (expr_eq var1 var2) && (expr_eq val1 val2)
  | ScmLambdaSimple (vars1, body1), ScmLambdaSimple (vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) && (expr_eq body1 body2)
  | ScmLambdaOpt (vars1, var1, body1), ScmLambdaOpt (vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) && (expr_eq body1 body2)
  | ScmApplic (e1, args1), ScmApplic (e2, args2) ->
     (expr_eq e1 e2) && (List.for_all2 expr_eq args1 args2)
  | _ -> false;;

module type TAG_PARSER = sig
  val tag_parse_expression : sexpr -> expr
end;; 

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "unquote";
   "unquote-splicing"];;

let rec tag_parse_expression sexpr =
let sexpr = macro_expand sexpr in

match sexpr with 
(* Implement tag parsing here *)
| ScmNil-> ScmConst (ScmNil)
| ScmBoolean(_) -> ScmConst(sexpr)
| ScmChar(_) ->  ScmConst(sexpr)
| ScmNumber(_)-> ScmConst(sexpr)
| ScmString(_)-> ScmConst(sexpr)
| ScmPair(ScmSymbol("quote"),ScmPair(y,z))-> ScmConst(y)
| ScmSymbol(x) -> smybol_parse x
| ScmPair(ScmSymbol("if"), ScmPair(test,ScmPair(ift,ScmPair(iff,ScmNil))))->if_parser test ift iff
| ScmPair(ScmSymbol("if"), ScmPair(test,ScmPair(ift,ScmNil)))->if_parser2 test ift
| ScmPair(ScmSymbol("or"),x)-> or_parser x
| ScmPair(ScmSymbol("lambda"),ScmPair(args,bodies))-> lambda_parser args bodies
| ScmPair(ScmSymbol("define"),ScmPair(vr,vl)) -> define_parser vr vl
| ScmPair(ScmSymbol("set!"),ScmPair(var,ScmPair(vl,ScmNil)))-> set_parser var vl sexpr
| ScmPair(ScmSymbol("begin"),y)-> begin_parser y
| ScmPair(ScmSymbol(x),y)-> app_parser x y sexpr
| ScmPair(ScmPair(ScmSymbol("lambda"),ScmPair(args,bodies)),rest)-> app_lambda args bodies rest
| ScmPair(ScmPair(x,y),z)-> app_parser2 (ScmPair(x,y)) z sexpr
| ScmPair(x,ScmNil)-> begin_parser x
| ScmPair(x,y)->app_parser2 x y sexpr


| _ -> raise (X_syntax_error (sexpr, "Sexpr structure not recognized1"))

and begin_parser list=
  match list with
  | ScmPair(x,ScmNil)-> tag_parse_expression x
  | _-> ScmSeq(map_parse list)

and app_lambda args bodies rest=
  let lam=lambda_parser args bodies in
  ScmApplic(lam, map_parse rest)

and app_parser2 rator rands sexpr=
  ScmApplic(tag_parse_expression rator,map_parse rands)

and app_parser rator rans sexpr=
  if (List.mem rator reserved_word_list)
    then raise (X_syntax_error (sexpr, "Expected variable on LHS of app1"))
    else ScmApplic(ScmVar(rator),map_parse rans)
  
and map_parse list=
  List.map tag_parse_expression (scm_list_to_list list)

and set_parser var vl sexpr=
  match var with
  | ScmSymbol(var)->ScmSet(ScmVar(var),tag_parse_expression vl)
  | _ -> raise (X_syntax_error (sexpr, "Expected variable on LHS of set!2"))


and define_parser vr vl =
  match vr with
  | ScmSymbol(x)->if(List.mem x reserved_word_list)
                  then raise (X_syntax_error (ScmPair(ScmSymbol("define"),ScmPair(vr,ScmPair(vl,ScmNil))), "Expected variable on LHS of define"))
                  else
                    let vl_new=scm_list_to_list vl in
                    if((List.length vl_new)=1) 
                      then ScmDef(ScmVar(x),(tag_parse_expression (List.hd vl_new)))
                      else raise (X_syntax_error (ScmPair(ScmSymbol("define"),ScmPair(vr,ScmPair(vl,ScmNil))), "Expected variable on LHS of define"))
  | ScmPair(ScmSymbol(x),y)->
          (match y with
          | ScmSymbol(_)->ScmDef(ScmVar(x), lambda_parser y vl)
          | ScmPair(_,_)->  ScmDef(ScmVar(x),lambda_parser y vl)
          | _ -> raise (X_syntax_error (vr, "if structure not recognized")))
  | _ -> raise (X_syntax_error (vr, "vr structure not recognized"))

and smybol_parse x =
  if (List.mem x reserved_word_list)
   then raise (X_reserved_word x)
  else ScmVar(x)


and if_parser test ift iff =
  ScmIf((tag_parse_expression test),(tag_parse_expression ift),(tag_parse_expression iff))

and if_parser2 test ift =
  ScmIf((tag_parse_expression test),(tag_parse_expression ift),ScmConst(ScmVoid))



and lambda_parser args bodies =
  match args with
  | ScmPair(a,b) -> 
    if scm_is_list args then
      let args_list=List.map check_if_symbol (scm_list_to_list args) in
      let bodies_list=List.map tag_parse_expression (scm_list_to_list bodies) in
      let len=List.length bodies_list in
      if(len=1)
        then ScmLambdaSimple(args_list,(List.hd bodies_list))
        else (if (len=0)
                then ScmLambdaSimple(args_list,ScmConst(ScmVoid))
                else ScmLambdaSimple(args_list, (ScmSeq(bodies_list))))
    else
      let args_list = firsts args in
      let rest = last args in
      let args_list = List.map check_if_symbol (scm_list_to_list args_list) in
      let bodies_list=List.map tag_parse_expression (scm_list_to_list bodies) in
      let len=List.length bodies_list in
      if(len=1)
      then ScmLambdaOpt(args_list,rest,(List.hd bodies_list))
      else (if (len=0)
            then ScmLambdaOpt(args_list,rest,ScmConst(ScmVoid)) 
            else ScmLambdaOpt(args_list,rest,(ScmSeq(bodies_list))))
  | ScmSymbol(x) -> 
      let bodies_list=List.map tag_parse_expression (scm_list_to_list bodies) in
      let len=List.length bodies_list in
      if(len=1)
      then ScmLambdaOpt([],x,(List.hd bodies_list))
      else (if (len=0)
        then ScmLambdaOpt([],x,ScmConst(ScmVoid))
        else ScmLambdaOpt([],x,(ScmSeq(bodies_list))))
  | ScmNil-> let bodies_list=List.map tag_parse_expression (scm_list_to_list bodies) in
      let len=List.length bodies_list in
      if (len=1)
      then ScmLambdaSimple([],(List.hd bodies_list))
      else (if (len=0)
          then ScmLambdaSimple([],ScmConst(ScmVoid))
          else ScmLambdaSimple([],(ScmSeq(bodies_list))))
  | x -> raise (X_syntax_error (x, "Sexpr structure not recognized2"))

and check_if_symbol = function
  | ScmSymbol(x)->x
  | x -> raise (X_syntax_error (x, "Sexpr structure not recognized3"))
  
and firsts ls =
  match ls with
  | ScmPair(x,y) -> ScmPair(x, (firsts y))
  | _ -> ScmNil

and last ls =
  match ls with
  | ScmPair(x, y) -> last y
  | ScmSymbol(x) -> x
  | x -> raise (X_syntax_error (x, "Sexpr structure not recognized4"))

and macro_expand sexpr =
match sexpr with
(* Handle macro expansion patterns here *)
| ScmPair(ScmSymbol("cond"),ScmPair(first,rest))-> macro_expand (expend_cond first rest)
| ScmPair(ScmSymbol("let"),ScmPair(args,bodies))-> expend_let args bodies
| ScmPair(ScmSymbol("let*"),ScmPair(args,bodies))-> expend_let_star args bodies
| ScmPair(ScmSymbol("letrec"),ScmPair(args,bodies))-> expend_letrec args bodies
| ScmPair(ScmSymbol("quasiquote"),ScmPair(ScmSymbol(x),ScmNil))-> list_to_proper_list [ScmSymbol("quote");ScmSymbol(x)]
| ScmPair(ScmSymbol("quasiquote"),ScmPair(ScmNil,ScmNil))-> ScmPair(ScmSymbol("quote"),ScmPair(ScmNil,ScmNil))
| ScmPair(ScmSymbol("quasiquote"),ScmPair(ScmPair(x,y),ScmNil))-> quote_parse x y
| ScmPair(ScmSymbol("and"),x)->and_parser x
| _ -> sexpr

and and_parser x=
match x with
| ScmNil-> ScmBoolean(true)
| ScmPair(x,ScmNil)-> x
| ScmPair(x,y)-> list_to_proper_list [ScmSymbol("if");x ;and_parser y;ScmBoolean(false)]
| _ -> raise (X_syntax_error (x, "Sexpr structure not recognized4"))


and quote_parse first rest=
  match first with
  (*| ScmPair(ScmSymbol("unquote"),ScmPair(ScmSymbol(x),ScmNil)),ScmNil->ScmSymbol(x)*)
  (* | ScmPair(ScmSymbol("unquote"),ScmPair(a,ScmNil)),ScmPair(x,y)-> list_to_proper_list [ScmSymbol("cons"); a; (quote_parse x y)] *)
  | ScmPair(ScmSymbol("unquote-splicing"),ScmPair(a,ScmNil))-> list_to_improper_list [ScmSymbol("append"); a; yaki_add rest]
  | _ ->
    match rest with
    | ScmPair(ScmSymbol("unquote-splicing"),ScmPair(b,ScmNil))-> list_to_improper_list [ScmSymbol("cons"); yaki_add first; b]
    | _ -> list_to_proper_list [ScmSymbol("cons"); yaki_add first ; yaki_add rest]

  (* | ScmPair(ScmSymbol("unquote"),ScmPair(a,ScmNil)),ScmNil-> list_to_proper_list []
  | ScmPair(ScmSymbol("unquote-splicing"),ScmPair(a,ScmNil)),ScmNil-> ScmPair(ScmSymbol("quote") ,ScmPair(ScmPair(ScmSymbol("unquote-splicing"),ScmPair(a,ScmNil)),ScmNil))
  | ScmNil,ScmNil-> ScmPair(ScmSymbol("quote"),ScmPair(first,ScmNil))
  | ScmPair(ScmSymbol("unquote-splicing"),ScmPair(a,ScmNil)),ScmNil-> list_to_proper_list [ScmSymbol("append"); a]
  | ScmPair(ScmSymbol("unquote-splicing"),ScmPair(a,ScmNil)),ScmPair(x,y)-> list_to_proper_list [ScmSymbol("append"); a; (quote_parse x y)]
  | ScmSymbol(x),ScmNil-> list_to_proper_list [ScmSymbol("cons");ScmPair(ScmSymbol("quote"),ScmPair(first,ScmNil))]
  | ScmSymbol(x),ScmPair(y,z)-> list_to_proper_list [ScmSymbol("cons"); ScmPair(ScmSymbol("quote"),ScmPair(first,ScmNil)); (quote_parse y z)]
  | ScmPair(x,y),ScmNil -> list_to_proper_list [ScmSymbol("cons"); (quote_parse x y)]
  | ScmPair(x,y),ScmPair(a,b) -> list_to_proper_list [ScmSymbol("cons"); (quote_parse x y);(quote_parse a b)]
  | _,_-> raise (X_syntax_error (first, "Sexpr structure not recognized QQ")) *)

and yaki_add sexp=
  match sexp with
  | ScmPair(ScmSymbol("unquote"),ScmPair(a,ScmNil))-> a
  | ScmPair(ScmSymbol("unquote-splicing"),ScmPair(a,ScmNil))-> ScmPair(ScmSymbol("quote") ,ScmPair(ScmPair(ScmSymbol("unquote-splicing"),ScmPair(a,ScmNil)),ScmNil))
  | ScmSymbol(x)-> list_to_proper_list [ScmPair(ScmSymbol("quote"),ScmPair(sexp,ScmNil))]
  | ScmNil-> ScmPair(ScmSymbol("quote"),ScmPair(ScmNil,ScmNil)) 
  | ScmPair(x,y)-> quote_parse x y
  | _ -> sexp

and expend_letrec args bodies =
  match args with
  | ScmNil-> expend_let args bodies
  | _->
  let vrs = get_firsts args in
  let vls = get_seconds args in
    
  let add_quote= list_to_proper_list [ScmSymbol("quote"); ScmSymbol("whatever")] in
  let whatevs = scm_map (fun x -> ScmPair(x,add_quote)) vrs in
  let sets = scm_zip (fun x y -> ScmPair(ScmSymbol("set!"),ScmPair(x,ScmPair(y,ScmNil)))) vrs vls in
  let last_let= bodies in
  expend_let whatevs (scm_append sets last_let)

  


and expend_let args bodies=
  let vrs = get_firsts args in
  let vls = get_seconds args in
  ScmPair(ScmPair(ScmSymbol("lambda"),ScmPair(vrs,bodies)),vls)

and expend_let_star args bodies=
  match args with
  | ScmNil -> macro_expand (ScmPair(ScmSymbol("let"),ScmPair(args,bodies)))
  | ScmPair(x,ScmNil) -> macro_expand (ScmPair(ScmSymbol("let"),ScmPair(args,bodies)))
  | ScmPair(x,y) -> macro_expand (ScmPair(ScmSymbol("let"),ScmPair(ScmPair(x,ScmNil),ScmPair(ScmPair(ScmSymbol("let*"),ScmPair(y,bodies)),ScmNil))))
  | x -> raise (X_syntax_error (x, "Sexpr structure not recognized3"))

and get_firsts list=
  match list with
  |ScmNil->ScmNil
  |ScmPair(ScmPair(x,_),y)->ScmPair(x,get_firsts y)
  |_ ->raise (X_syntax_error (list, "Sexpr structure not recognized6"))

and get_seconds list=
  match list with
  | ScmNil->ScmNil
  | ScmPair(ScmPair(_,ScmPair(z,ScmNil)),y)->ScmPair(z,get_seconds y)
  | ScmPair(ScmPair(_,ScmPair(z,x)),y)->ScmPair(ScmPair(z,x),get_seconds y)
  |_ ->raise (X_syntax_error (list, "Sexpr structure not recognized7"))

and match_parse x y=
  match x with
  | ScmPair(t,b)->ScmPair(ScmSymbol("if"),ScmPair(b,x))
  | _ ->raise (X_syntax_error (x, "Sexpr structure not recognized8 - match_parse"))

and expend_cond first rest=
  let begin_add=ScmPair(ScmSymbol("begin"),ScmNil) in
  match rest, first with
  | _, ScmPair(x,ScmPair(ScmSymbol("=>"),y))-> macro_expand ( spc_cond x y rest)
  | _, ScmPair(ScmSymbol("else"),y)-> scm_append begin_add y  
  | ScmNil,ScmPair(t,ScmPair(b,ScmNil))->ScmPair(ScmSymbol("if"),ScmPair(t,ScmPair(b,ScmNil)))
  | ScmPair(ScmPair(ScmSymbol("else"),app),_), ScmPair(t,ScmPair(b,ScmNil))->ScmPair(ScmSymbol("if"),ScmPair(t, ScmPair(b, app)))
  | ScmPair(f,r),ScmPair(t,ScmPair(b,ScmNil))->ScmPair(ScmSymbol("if"),ScmPair(t, ScmPair(b, ScmPair(expend_cond f r,ScmNil))))
  | ScmNil, ScmPair(y,z)->list_to_proper_list [ScmSymbol("if"); y;scm_append begin_add z]
   |ScmPair(ScmPair(ScmSymbol("else"),x), _ ),ScmPair(y,z)->
        (if((scm_list_length x)=1)
          then list_to_proper_list [ScmSymbol("if");y;scm_append begin_add z;x]
          else list_to_proper_list [ScmSymbol("if"); y ;scm_append begin_add z;scm_append begin_add x])
  | ScmPair(x,y),ScmPair(w,z)->list_to_proper_list [ScmSymbol("if"); w ;scm_append begin_add z; expend_cond x y]
  | x,y ->raise (X_syntax_error (first, "y structure not recognized9"))

and spc_cond test body rest=
  let dif=list_to_proper_list [ScmSymbol("rest")] in
  let dit=list_to_proper_list [(list_to_proper_list [ScmSymbol("f")]);ScmSymbol("value")] in
  let full_if=list_to_proper_list [ScmSymbol("if");ScmSymbol("value");dit;dif] in
  let half_if= list_to_proper_list [ScmSymbol("if");ScmSymbol("value");dit] in

  let value=list_to_proper_list [ScmSymbol("value");test] in
  let f=list_to_proper_list [ScmSymbol("f"); create_empty_lambda body] in
  let rest_var=list_to_proper_list [ScmSymbol("rest");create_empty_lambda (add_cond_rest rest)] in
  let full_args= list_to_proper_list [value;f;rest_var] in
  let half_args= list_to_proper_list [value;f] in
  
  match rest with
  | ScmNil-> expend_let half_args (list_to_proper_list [(half_if)])
  | _->expend_let full_args (list_to_proper_list [(full_if)])

and add_cond_rest rest=
  match rest with
  | ScmPair(ScmPair(ScmSymbol("else"),x),_)-> x
  | ScmPair(x,y) ->ScmPair(expend_cond x y, ScmNil)
  | ScmNil -> ScmNil
  | _ -> raise (X_syntax_error (rest, "bad cond rib"))
(*  

  match sexpr, rest with
  | ScmPair(x,ScmPair(ScmSymbol("=>"),y)) , ScmNil->  ScmPair(ScmPair(ScmSymbol("value"),ScmPair(x,ScmNil)),ScmPair(ScmPair(ScmSymbol("f"),ScmPair(y,ScmNil)),ScmNil)) half_if
*)
and create_empty_lambda body=
  ScmPair(ScmSymbol("lambda"),ScmPair(ScmNil,body))

and or_parser list =
  let list=scm_list_to_list list in
  let len=List.length list in
  if (len=0)
    then ScmConst(ScmBoolean false)
  else(
      if (len=1)
        then
        tag_parse_expression (List.hd list)
        else
        ScmOr(List.map tag_parse_expression list)
  )




and macro_cond_to_if_with_else sexpr x=
  match sexpr with
  | ScmPair(ft,fb)-> ScmPair(ScmSymbol("if"),ScmPair(ft,ScmPair(fb,x)))
  | x -> raise (X_syntax_error (x, "Sexpr structure not recognized"))

and macro_cond_to_if_without_else sexpr=
  match sexpr with
  | ScmPair(ft,fb)-> ScmPair(ScmSymbol("if"),ScmPair(ft,fb))
  | x -> raise (X_syntax_error (x, "Sexpr structure not recognized"))
end;; 

