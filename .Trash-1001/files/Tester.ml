
# use "pc.ml" ;; 
# use "reader.ml" ;;
open Reader;;
open PC;;


let {index_from=src;index_to=dst;found=ff}=(test_string nt_boolean "#t"  0) in
Printf.printf "%s\n" (if ((src=0) && (dst=2) && (ff=ScmBoolean(true))) then ("pass") else ("fail - test_string nt_boolean \"#t\"  0 "));;

let {index_from=src;index_to=dst;found=ff}=(test_string nt_boolean "#T"  0) in
Printf.printf "%s\n" (if ((src=0) && (dst=2) && (ff=ScmBoolean(true))) then ("pass") else ("fail - test_string nt_boolean \"#T\"  0 "));;

let {index_from=src;index_to=dst;found=ff}=(test_string nt_boolean "#f"  0) in
Printf.printf "%s\n" (if ((src=0) && (dst=2) && (ff=ScmBoolean(false))) then ("pass") else ("fail - test_string nt_boolean \"#f\"  0 "));;

let {index_from=src;index_to=dst;found=ff}=(test_string nt_boolean "#F"  0) in
Printf.printf "%s\n" (if ((src=0) && (dst=2) && (ff=ScmBoolean(false))) then ("pass") else ("fail - test_string nt_boolean \"#F\"  0 "));;

let {index_from=src;index_to=dst;found=ff}=(test_string nt_char "#\\a"  0) in
Printf.printf "%s\n" (if ((src=0) && (dst=3) && (ff=ScmChar('a'))) then ("pass") else ("fail - test_string nt_char \"#\\a\"  0 "));;

let {index_from=src;index_to=dst;found=ff}=(test_string nt_char "#\\A"  0) in
Printf.printf "%s\n" (if ((src=0) && (dst=3) && (ff=ScmChar('A'))) then ("pass") else ("fail - test_string nt_char \"#\\A\"  0 "));;

let {index_from=src;index_to=dst;found=ff}=(test_string nt_symbol "lambda"  0) in
Printf.printf "%s\n" (if ((src=0) && (dst=6) && (ff=ScmSymbol("lambda"))) then ("pass") else ("fail - test_string nt_symbol \"lambda\"  0 "));;

let {index_from=src;index_to=dst;found=ff}=(test_string nt_symbol "if"  0) in
Printf.printf "%s\n" (if ((src=0) && (dst=2) && (ff=ScmSymbol("if"))) then ("pass") else ("fail - test_string nt_symbol \"if\"  0 "));;

let {index_from=src;index_to=dst;found=ff}=(test_string nt_char "#\\space"  0) in
Printf.printf "%s\n" (if ((src=0) && (dst=7) && (ff=ScmChar(' '))) then ("pass") else ("fail - test_string nt_char \"#\\space\"  0 "));;

let {index_from=src;index_to=dst;found=ff}=(test_string nt_char "#\\return"  0) in
Printf.printf "%s\n" (if ((src=0) && (dst=8) && (ff=ScmChar('\r'))) then ("pass") else ("fail - test_string nt_char \"#\\return\"  0 "));;

let {index_from=src;index_to=dst;found=ff}=(test_string nt_char "#\\newline"  0) in
Printf.printf "%s\n" (if ((src=0) && (dst=9) && (ff=ScmChar('\n'))) then ("pass") else ("fail - test_string nt_char \"#\\newline\"  0 "));;

let {index_from=src;index_to=dst;found=ff}=(test_string nt_char "#\\tab"  0) in
Printf.printf "%s\n" (if ((src=0) && (dst=5) && (ff=ScmChar('\t'))) then ("pass") else ("fail - test_string nt_char \"#\\tab\"  0 "));;

try ( 
let {index_from=src;index_to=dst;found=ff}=(test_string nt_char "#\\ponpon"  0) in 
Printf.printf "fail\n")
with PC.X_no_match-> Printf.printf "pass\n";;

try (
let {index_from=src;index_to=dst;found=ff}=(test_string nt_char "#\\gafrur"  0) in 
Printf.printf "fail\n")
with PC.X_no_match-> Printf.printf "pass\n";;
