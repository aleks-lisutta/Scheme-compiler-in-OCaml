[1mFile "./reader.ml", line 177, characters 38-44[0m:
177 |                (make_named_char "nul" ScmNil)] in
                                            [1;31m^^^^^^[0m
[1;31mError[0m: This expression has type sexpr but an expression was expected of type
         char
[1mFile "./tag-parser.ml", line 104, characters 0-15[0m:
104 | string_of_sexpr (untag expr)
      [1;31m^^^^^^^^^^^^^^^[0m
[1;31mError[0m: Unbound value string_of_sexpr
Hint: Did you mean string_of_expr?
[1mFile "./semantic-analyser.ml", line 46, characters 46-54[0m:
46 |   | ScmConst' (sexpr1), ScmConst' (sexpr2) -> sexpr_eq sexpr1 sexpr2
                                                   [1;31m^^^^^^^^[0m
[1;31mError[0m: Unbound value sexpr_eq
Hint: Did you mean expr'_eq?
[1mFile "./code-gen.ml", line 63, characters 14-22[0m:
63 |           if (sexpr_eq e1 sxpr)
                   [1;31m^^^^^^^^[0m
[1;31mError[0m: Unbound value sexpr_eq
