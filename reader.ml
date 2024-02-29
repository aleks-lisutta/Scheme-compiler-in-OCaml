(* reader.ml
 * A skeleton for the reader for the 2021-2022 course on compiler-construction
 *)

#use "pc.ml";;
open Printf
let rec gcd a b =
  match (a, b) with
  | (0, b) -> b
  | (a, 0) -> a
  | (a, b) -> gcd b (a mod b);;

type scm_number =
  | ScmRational of (int * int)
  | ScmReal of float;;

type sexpr =
  | ScmVoid
  | ScmNil
  | ScmBoolean of bool
  | ScmChar of char
  | ScmString of string
  | ScmSymbol of string
  | ScmNumber of scm_number
  | ScmVector of (sexpr list)
  | ScmPair of (sexpr * sexpr);;

module type READER = sig
    val nt_sexpr : sexpr PC.parser
end;; (* end of READER signature *)

module Reader : READER = struct
open PC;;

let unitify nt = pack nt (fun _ -> ());;

let rec pow e b = 
  match e with 
  | 0.0 -> 1.0
  | 1.0 -> b
  | n -> if n>0.0 then b*.(pow (e-.1.0) b) else (pow (e+.1.0) b) /. b

let rec shrink = (fun v -> if -1.0<v && v<1.0 then v else shrink (v/.10.0)) 

let rec nt_whitespace str =
  const (fun ch -> ch <= ' ') str
and nt_end_of_line_or_file str =
  let nt1 = unitify (char '\n') in
  let nt2 = unitify nt_end_of_input in
  let nt1 = disj nt1 nt2 in
  nt1 str
and nt_line_comment str = 
  let pref = word ";;;" in
  unitify (caten (caten pref (star (diff nt_any nt_end_of_line_or_file))) nt_end_of_line_or_file) str
and nt_paired_comment str = 
  unitify (caten (caten (char '{') (star nt_sexpr)) (char '}')) str
and nt_sexpr_comment str = 
  let pref = word "#;" in
  unitify (caten pref nt_sexpr) str
and nt_comment str =
  disj_list
    [nt_line_comment;
     nt_paired_comment;
     nt_sexpr_comment] str
and nt_skip_star str =
  let nt1 = disj (unitify nt_whitespace) nt_comment in
  let nt1 = unitify (star nt1) in
  nt1 str
and make_skipped_star (nt : 'a parser) =
  let nt1 = caten nt_skip_star (caten nt nt_skip_star) in
  let nt1 = pack nt1 (fun (_, (e, _)) -> e) in
  nt1
and make_int res = 
  let (s,ls) = res in
  let i = list_to_int ls in
  let b = 
    match s with
    | None -> 1
    | Some c -> if c='+' then 1 else -1
    in
  i*b
and list_to_int ls =(List.fold_left (fun a b -> a*10+b-48 ) 0 (List.map int_of_char ls) ) 
and nt_int str = 
  let nt1 = maybe (disj (const (fun c->c='+')) (const (fun c->c='-'))) in
  let nt1 = caten nt1 nt_integer_part in
  let nt1 = pack nt1 (fun res -> make_int res) in
  let nt1 = not_followed_by nt1 (word ".") in
  nt1 str
and make_rat res = 
  let ((i,s),n) = res in
  if (n=0)
   then (raise X_no_match)
   else 
    let d = gcd i n in
   (ScmRational(i/d,n/d))

and nt_frac str = 
  let nt1 = caten (caten nt_int (const (fun c -> c='/'))) nt_int in
  let nt1 = pack nt1 (fun res -> make_rat res) in
  nt1 str
and nt_integer_part str = plus (range '0' '9' ) str
and nt_mantissa str = nt_integer_part str
and nt_exponent str = 
  let ntTok = disj_list [word_ci "e"; word "*10^"; word "*10**"] in
  let ntEx = caten ntTok nt_int in
  ntEx str
and makeFA res = 
  let (((i,d),m),e) = res in
  let ip = float_of_int (list_to_int i) in
  let mp = 
    match m with 
    | None -> 0.0
    | Some v -> shrink (float_of_int (list_to_int v))
    in
  let tep = 
    match e with
    | None -> (['0'],0)
    | Some v -> v
    in
  let (tp,ep) = tep in
  ScmReal ((ip+.mp)*.(pow (float_of_int ep) 10.0))
and nt_floatA str = 
  let nt1 = caten (caten (caten nt_integer_part (word ".")) (maybe nt_mantissa)) (maybe nt_exponent) in
  let nt1 = pack nt1 (fun res -> makeFA res) in
  nt1 str
and makeFB res = 
  let (((d,m),e)) = res in
  let mp = shrink (float_of_int (list_to_int m)) in
  let tep = 
    match e with
    | None -> (['0'],0)
    | Some v -> v
    in
  let (tp,ep) = tep in
  ScmReal (mp*.(pow (float_of_int ep) 10.0)) 
and nt_floatB str = 
  let nt1 = caten (caten (word ".")  nt_mantissa) (maybe nt_exponent) in
  let nt1 = pack nt1 (fun res -> makeFB res) in
  nt1 str
and makeFC res = 
  let (i,e) = res in
  let ip = float_of_int (list_to_int i) in
  let (tp,ep) = e in
  ScmReal (ip*.(pow (float_of_int ep) 10.0))
and nt_floatC str = 
  let nt1 = caten nt_integer_part nt_exponent in
  let nt1 = pack nt1 (fun res -> makeFC res) in
  nt1 str
and nt_float str = pack (caten (maybe (disj (char '+') (char '-'))) (disj_list [nt_floatA;nt_floatB;nt_floatC])) (fun (a,b)->b) str
and nt_number str =
  let nt1 = nt_float in
  let nt2 = nt_frac in
  let nt3 = pack nt_int (fun n -> ScmRational(n, 1)) in
  let nt1 = disj nt1 (disj nt2 nt3) in
  let nt1 = pack nt1 (fun r -> ScmNumber r) in
  let nt1 = not_followed_by nt1 nt_symbol_char in
  nt1 str
and nt_boolean str =  
  let nt1 = pack (word_ci "#f") (fun bool -> ScmBoolean false) in
  let nt2 = pack (word_ci "#t") (fun bool -> ScmBoolean true) in
  let nt1 = disj nt1 nt2 in
  let nt1 = not_followed_by nt1 nt_symbol in
  nt1 str
and nt_char_simple str = 
  let nt1 = (const (fun ch2 -> ' '<ch2)) in
  let nt1 = not_followed_by nt1 nt_symbol in
  nt1 str
and make_named_char char_name ch =
  pack (word_ci char_name) (fun _->ch)
and nt_char_named str =
  let nt1 =
    disj_list [(make_named_char "newline" '\n');
               (make_named_char "page" '\012');
               (make_named_char "return" '\r');
               (make_named_char "space" ' ');
               (make_named_char "tab" '\t');
               (make_named_char "nul" (char_of_int 0))] in
  nt1 str
and nt_char_hex str =
    let letter=range_ci 'a' 'f' in
    (disj nt_digit letter) str
and nt_digit = range '0' '9'
and nt_char str = 
(pack (caten nt_charPrefix 
          (disj_list[nt_hexadecimal_char;nt_char_simple; nt_char_named]))
               (fun (x,y)-> ScmChar(y))) str
and nt_charPrefix = word "#\\"
and nt_symbol_char str = 
  (disj_list[char '!'; char '$'; char '-';range '*' '+'; range_ci 'a' 'z'; range '^' '_';range '<' '?'; range '/' ':'; range '0' '9']) str
and nt_symbol str =
  let nt1 = plus nt_symbol_char in
  let nt1 = pack nt1 list_to_string in
  let nt1 = pack nt1 (fun name -> ScmSymbol name) in
  let nt1 = diff nt1 nt_number in
  nt1 str
and nt_hexadecimal_char str =
  let nt1=pack (caten (word_ci "x") (plus nt_char_hex)) (fun (a,b)-> b) in
  let listHex= pack nt1 (fun b-> (List.map charHex_to_intHex b)) in
  let hexValue_of_List=pack listHex listChar_to_Hex in
  let char_return=pack hexValue_of_List (fun b->char_of_int b)  in
  char_return str


and listChar_to_Hex list=
  List.fold_left (fun a b->(a*16+b)) 0 list

and charHex_to_intHex c=
  let c=int_of_char c in
  let zero=int_of_char '0' in
  let ascii_a=int_of_char 'a' in
  if (zero<=c && c<=(zero+9)) 
    then (c-zero)
    else (
      if (ascii_a<=c && c<=(ascii_a+5)) 
        then (c-ascii_a+10)
        else
          (raise X_no_match)
  )

and nt_string_lit_char str = 
  let nt1 = (const (fun ch -> true)) in
  let nt2 = (const (fun ch -> ch='\\' || ch='"' || ch='~')) in
  let nt1 = (diff nt1 nt2) in
  pack nt1 (fun res -> ScmChar (res)) str

and make_meta res = (* need to add tilde *)
let c = List.hd ((List.tl) res)in
let c = 
  match c with
  | '\\' -> '\\'
  | 't' -> '\t'
  | '\"' -> '\"'
  | 'r' -> '\r'
  | 'n' -> '\n'
  | 'f' -> char_of_int 12
  | '~' -> '~'
  | _ -> raise (X_no_match)
  in
c
and nt_string_meta_char str = 
  let nt1 = disj_list [word "\\\\"; word "\\\""; word_ci "\\t"; word_ci"\\f"; word_ci "\\n"; word_ci "\\r"; word "~~"] in
  let nt1 = pack nt1 (fun res -> ScmChar(make_meta res)) in
  nt1 str

and nt_string_hex_char str =
  let nt1 = (caten (word "\\") (caten nt_hexadecimal_char (word ";"))) in
  (pack nt1 (fun (a,(b,c))-> ScmChar(b))) str

and nt_string_char str = (disj_list [nt_string_lit_char;nt_string_meta_char;nt_string_hex_char;nt_string_interpolated]) str
          
and nt_check ls=
  if (List.for_all (fun e -> match e with | ScmChar v -> true | _ -> false) ls)
  then
    ScmString (List.fold_left (fun str ch -> match ch with | ScmChar v -> (str ^ (String.make 1 v)) | _ -> "") "" ls)
  else
    (
      if (List.length ls)=1
      then List.hd ls
      else
      (
        ScmPair(ScmSymbol("string-append"),(List.fold_right apeendOrPair ls ScmNil))
      )
    )
  and apeendOrPair first rest=
    match rest with
    | ScmNil ->(
      match first with
      | ScmChar(c)->ScmPair(ScmString(""^(String.make 1 c)),rest)
      | _ -> ScmPair(first,rest)
              )
    | ScmPair(a,b) ->(
                      match a with
                      | ScmPair(a,b) -> (
                                     match first with
                                     | ScmChar(c)-> ScmPair(ScmString(""^(String.make 1 c)),rest)
                                     | _ -> ScmPair(first,rest) 
                                   )
                      | ScmString(a)-> (
                                    match first with
                                    | ScmChar(c)-> ScmPair(ScmString((String.make 1 c)^a),b)
                                    | _ -> ScmPair(first,rest) 
                                        )  
                      |_ -> raise (X_no_match)
                   )
    |_ -> raise (X_no_match)


  
    

and nt_string str = 
  let nt1 = (caten (caten (word "\"") (star nt_string_char)) (word "\"")) in
  let nt1 = pack nt1 (fun ((q1,s),q2) -> nt_check s) in
  nt1 str

and make_format res =
  ScmPair(ScmSymbol("format"),ScmPair(ScmString "~a",ScmPair(res,ScmNil)))
and nt_string_intr str = 
  let nt1 = (caten (caten (word "\"") (star nt_string_interpolated)) (word "\"")) in
  let nt1 = pack nt1 (fun ((a,b),c) ->  b) in
  nt1 str
and nt_string_interpolated str=
  let fill = maybe (star nt_whitespace ) in
  let nt1 = caten (word "~{") fill in
  let nt2 = caten fill (char '}') in
  let nt1 = pack (caten (caten nt1 nt_sexpr) nt2) (fun ((a,b),c) -> make_format b) in
  nt1 str

and nt_vector str =
  let nt1 = word "#(" in
  let nt2 = caten nt_skip_star (char ')') in
  let nt2 = pack nt2 (fun _ -> ScmVector []) in
  let nt3 = plus nt_sexpr in
  let nt4 = char ')' in
  let nt3 = caten nt3 nt4 in
  let nt3 = pack nt3 (fun (sexprs, _) -> ScmVector sexprs) in
  let nt2 = disj nt2 nt3 in
  let nt1 = caten nt1 nt2 in
  let nt1 = pack nt1 (fun (_, sexpr) -> sexpr) in
  nt1 str
and nt_list str = disj nt_proper_list nt_improper_list str 

and nt_proper_list str= 
  let nt1= star (char ' ') in
  let nt1 = pack (caten nt1 (caten nt_sexpr (maybe (char ' ')))) (fun (a,(b,c))->b) in 
  let nt1= star nt1 in
  let nt1 = pack (caten (char '(') nt1) (fun (a,b)-> b) in
  let nt1 = pack (caten  nt1 (star (char ' '))) (fun (a,b)-> a) in
  let nt1= pack (caten nt1 (char ')')) (fun (a,b)->a) in
  (pack nt1 (fun a -> (list_to_pairs a ScmNil))) str


and nt_improper_list str = 
  let dot = caten (char '.') (char ' ') in
  let spaced = (pack (caten nt_sexpr (maybe (char ' '))) (fun (a,b)->a)) in (* still supports (a b. c) *)
  let fin = (pack (caten nt_sexpr (star (char ' '))) (fun (a,b)->a)) in
  let nt1 = caten (star (char ' ')) (caten (caten (plus spaced) dot) fin ) in
  let nt1 = pack nt1 (fun (a,b)-> b) in
  let nt1 = pack (caten (char '(') nt1) (fun (a,b)-> b) in
  let nt1 = pack (caten nt1 (char ')')) (fun (a,b)->a) in
  pack nt1 (fun ((a,b),c) -> (list_to_pairs a c)) str

and list_to_pairs list first=
  List.fold_right (fun a b-> (ScmPair(a,b))) list first

and nt_quote str =
  let q = pack (char '\'') (fun c -> ScmSymbol ("quote")) in
  let nt1 = caten q nt_sexpr in 
  pack nt1 (fun (a,b) -> ScmPair(a,ScmPair(b,ScmNil))) str
and nt_quasi str =
  let q = pack (char '`') (fun c -> ScmSymbol ("quasiquote")) in
  let nt1 = caten q nt_sexpr in 
  pack nt1 (fun (a,b) -> ScmPair(a,ScmPair(b,ScmNil))) str
and nt_unquote str = 
  let q = pack (char ',') (fun c -> ScmSymbol ("unquote")) in
  let nt1 = caten q nt_sexpr in 
  pack nt1 (fun (a,b) -> ScmPair(a,ScmPair(b,ScmNil))) str
and nt_splice str =
  let q = pack (word ",@") (fun c -> ScmSymbol ("unquote-splicing")) in
  let nt1 = caten q nt_sexpr in 
  pack nt1 (fun (a,b) -> ScmPair(a,ScmPair(b,ScmNil))) str
and nt_quoted_forms str = disj_list [nt_quote; nt_quasi; nt_unquote; nt_splice] str

and nt_sexpr str =
  let nt1 =
    disj_list [nt_number; nt_boolean; nt_char; nt_symbol;
               nt_string; nt_vector; nt_list; nt_quoted_forms] in
  let nt1 = make_skipped_star nt1 in
  nt1 str;;

end;; (* end of struct Reader *)

let rec string_of_sexpr = function
  | ScmVoid -> "#<void>"
  | ScmNil -> "()"
  | ScmBoolean(false) -> "#f"
  | ScmBoolean(true) -> "#t"
  | ScmChar('\n') -> "#\\newline"
  | ScmChar('\r') -> "#\\return"
  | ScmChar('\012') -> "#\\page"
  | ScmChar('\t') -> "#\\tab"
  | ScmChar(' ') -> "#\\space"
  | ScmChar(ch) ->
     if (ch < ' ')
     then let n = int_of_char ch in
          Printf.sprintf "#\\x%x" n
     else Printf.sprintf "#\\%c" ch
  | ScmString(str) ->
     Printf.sprintf "\"%s\""
       (String.concat ""
          (List.map
             (function
              | '\n' -> "\\n"
              | '\012' -> "\\f"
              | '\r' -> "\\r"
              | '\t' -> "\\t"
              | ch ->
                 if (ch < ' ')
                 then Printf.sprintf "\\x%x;" (int_of_char ch)
                 else Printf.sprintf "%c" ch)
             (string_to_list str)))
  | ScmSymbol(sym) -> sym
  | ScmNumber(ScmRational(0, _)) -> "0"
  | ScmNumber(ScmRational(num, 1)) -> Printf.sprintf "%d" num
  | ScmNumber(ScmRational(num, -1)) -> Printf.sprintf "%d" (- num)
  | ScmNumber(ScmRational(num, den)) -> Printf.sprintf "%d/%d" num den
  | ScmNumber(ScmReal(x)) -> Printf.sprintf "%f" x
  | ScmVector(sexprs) ->
     let strings = List.map string_of_sexpr sexprs in
     let inner_string = String.concat " " strings in
     Printf.sprintf "#(%s)" inner_string
  | ScmPair(ScmSymbol "quote",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf "'%s" (string_of_sexpr sexpr)
  | ScmPair(ScmSymbol "quasiquote",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf "`%s" (string_of_sexpr sexpr)
  | ScmPair(ScmSymbol "unquote",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf ",%s" (string_of_sexpr sexpr)
  | ScmPair(ScmSymbol "unquote-splicing",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf ",@%s" (string_of_sexpr sexpr)
  | ScmPair(car, cdr) ->
     string_of_sexpr' (string_of_sexpr car) cdr
and string_of_sexpr' car_string = function
  | ScmNil -> Printf.sprintf "(%s)" car_string
  | ScmPair(cadr, cddr) ->
     let new_car_string =
       Printf.sprintf "%s %s" car_string (string_of_sexpr cadr) in
     string_of_sexpr' new_car_string cddr
  | cdr ->
     let cdr_string = (string_of_sexpr cdr) in
     Printf.sprintf "(%s . %s)" car_string cdr_string;;
