[1mFile "./reader.ml", line 177, characters 39-42[0m:
177 |                (make_named_char "nul", '\0')] in
                                             [1;31m^^^[0m
[1;31mError[0m: Illegal backslash escape in string or character (\0)
[1mFile "./tag-parser.ml", line 4, characters 16-21[0m:
4 |   | ScmConst of sexpr
                    [1;31m^^^^^[0m
[1;31mError[0m: Unbound type constructor sexpr
Hint: Did you mean expr?
[1mFile "./semantic-analyser.ml", line 18, characters 17-22[0m:
18 |   | ScmConst' of sexpr
                      [1;31m^^^^^[0m
[1;31mError[0m: Unbound type constructor sexpr
Hint: Did you mean expr'?
[1mFile "./code-gen.ml", line 17, characters 24-29[0m:
17 |   val make_consts_tbl : expr' list -> (sexpr * (int * string)) list
                             [1;31m^^^^^[0m
[1;31mError[0m: Unbound type constructor expr'
