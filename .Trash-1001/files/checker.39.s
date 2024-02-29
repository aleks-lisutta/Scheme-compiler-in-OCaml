;;; All the macros and the scheme-object printing procedure
;;; are defined in compiler.s
%include "compiler.s"

section .bss
;;; This pointer is used to manage allocations on our heap.
malloc_pointer:
    resq 1

;;; here we REServe enough Quad-words (64-bit "cells") for the free variables
;;; each free variable has 8 bytes reserved for a 64-bit pointer to its value
fvar_tbl:
    resq 37

section .data
const_tbl:
db T_VOID
db T_NIL
MAKE_BOOL(0)
MAKE_BOOL(1)
MAKE_LITERAL_RATIONAL(0,1)
MAKE_LITERAL_STRING "whatever"
MAKE_LITERAL_SYMBOL(23)
MAKE_LITERAL_RATIONAL(-1,1)
MAKE_LITERAL_CHAR(0)
MAKE_LITERAL_RATIONAL(7,1)
MAKE_LITERAL_CHAR(102)

;;; These macro definitions are required for the primitive
;;; definitions in the epilogue to work properly
%define SOB_VOID_ADDRESS const_tbl+0
%define SOB_NIL_ADDRESS const_tbl+1
%define SOB_FALSE_ADDRESS const_tbl+2
%define SOB_TRUE_ADDRESS const_tbl+4

global main
section .text
main:
    ;; set up the heap
    mov rdi, GB(2)
    call malloc
    mov [malloc_pointer], rax

    ;; Set up the dummy activation frame
    ;; The dummy return address is T_UNDEFINED
    ;; (which a is a macro for 0) so that returning
    ;; from the top level (which SHOULD NOT HAPPEN
    ;; AND IS A BUG) will cause a segfault.
    push 0                ; argument count
    push SOB_NIL_ADDRESS  ; lexical environment address
    push T_UNDEFINED      ; return address
    push rbp                    
    mov rbp, rsp                ; anchor the dummy frame

    ;; Set up the primitive stdlib fvars:
    ;; Since the primtive procedures are defined in assembly,
    ;; they are not generated by scheme (define ...) expressions.
    ;; This is where we simulate the missing (define ...) expressions
    ;; for all the primitive procedures.
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, boolean?)
mov [fvar_tbl+0], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, flonum?)
mov [fvar_tbl+8], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, rational?)
mov [fvar_tbl+16], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, pair?)
mov [fvar_tbl+24], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, null?)
mov [fvar_tbl+32], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, char?)
mov [fvar_tbl+40], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, string?)
mov [fvar_tbl+48], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, procedure?)
mov [fvar_tbl+56], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, symbol?)
mov [fvar_tbl+64], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, string_length)
mov [fvar_tbl+72], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, string_ref)
mov [fvar_tbl+80], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, string_set)
mov [fvar_tbl+88], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, make_string)
mov [fvar_tbl+96], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, symbol_to_string)
mov [fvar_tbl+104], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, char_to_integer)
mov [fvar_tbl+112], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, integer_to_char)
mov [fvar_tbl+120], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, exact_to_inexact)
mov [fvar_tbl+128], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, eq?)
mov [fvar_tbl+136], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, add)
mov [fvar_tbl+144], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, mul)
mov [fvar_tbl+152], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, div)
mov [fvar_tbl+160], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, eq)
mov [fvar_tbl+168], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, lt)
mov [fvar_tbl+176], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, numerator)
mov [fvar_tbl+184], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, denominator)
mov [fvar_tbl+192], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, gcd)
mov [fvar_tbl+200], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, car)
mov [fvar_tbl+216], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, cdr)
mov [fvar_tbl+224], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, cons)
mov [fvar_tbl+208], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, set_car)
mov [fvar_tbl+232], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, set_cdr)
mov [fvar_tbl+240], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, apply)
mov [fvar_tbl+248], rax

user_code_fragment:

;;; The code you compiled will be added here.
;;; It will be executed immediately after the closures for 
;;; the primitive procedures are set up.

            mov rax, 0
            cmp rax, 0
            jne malc1
            MALLOC rax, 8
            mov qword [rax], SOB_NIL_ADDRESS
            jmp fix1
            malc1:
            mov rcx, 8
            MALLOC rax, rcx
            mov rcx, qword [rbp+2*8]
            COPY_ENV rcx, rax, 0
            mov rdx, rax
            PUSH_ARGS 

            fix1:
            

            mov rbx,rax
            MAKE_CLOSURE(rax,rbx,code1)

            jmp end_code1


            code1:

            push rbp

            mov rbp,rsp

            mov rax, PVAR(0)
cmp rax, SOB_FALSE_ADDRESS
je Lelse2
mov rax, qword const_tbl+2
jmp Lexit2
Lelse2:
mov rax, qword const_tbl+4
Lexit2:


            leave

            ret

            end_code1:

MAKE_DEFINE 256
	call write_sob_if_not_void

push SOB_NIL_ADDRESS
  ;;magic

MAKE_GET_FREE_VAR 168
push rax

push 2
             
            mov rax, 0
            cmp rax, 0
            jne malc3
            MALLOC rax, 8
            mov qword [rax], SOB_NIL_ADDRESS
            jmp fix3
            malc3:
            mov rcx, 8
            MALLOC rax, rcx
            mov rcx, qword [rbp+2*8]
            COPY_ENV rcx, rax, 0
            mov rdx, rax
            PUSH_ARGS 

            fix3:
            

            mov rbx,rax
            MAKE_CLOSURE(rax,rbx,code3)

            jmp end_code3


            code3:

            push rbp

            mov rbp,rsp

            
            mov rax, 1
            cmp rax, 0
            jne malc4
            MALLOC rax, 8
            mov qword [rax], SOB_NIL_ADDRESS
            jmp fix4
            malc4:
            mov rcx, 16
            MALLOC rax, rcx
            mov rcx, qword [rbp+2*8]
            COPY_ENV rcx, rax, 1
            mov rdx, rax
            PUSH_ARGS 

            fix4:
            

            mov rbx,rax
            MAKE_CLOSURE(rax,rbx,code4)

            jmp end_code4


            code4:

            push rbp

            mov rbp,rsp

            push SOB_NIL_ADDRESS
  ;;magic

mov rax, qword const_tbl+6
push rax

mov rax, PVAR(0)
push rax

push 3
             MAKE_VAR_BOUND_CODE 0, 0
             mov r9, PARAM_COUNT
             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             push qword [rbp+WORD_SIZE*1]
             push qword [rbp]
             YAKI_SHIFT_FRAME 7
             pop rbp
             lea rsp ,[rsp + 8*(r9+4)]
             jmp rax


            leave

            ret

            end_code4:


            leave

            ret

            end_code3:

             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             call rax
add rsp , 8*1 

            pop rbx

            lea rsp , [rsp + 8* rbx]
MAKE_DEFINE 264
	call write_sob_if_not_void


            mov rax, 0
	        cmp rax,0
	        jne dep_0_5
	        MALLOC rax,8
	        mov qword [rax], SOB_NIL_ADDRESS
	        jmp test_5
	        dep_0_5:
	        lea rax, [(0+1)*WORD_SIZE]
	        MALLOC rax, rax
	        mov rcx, qword [rbp+2*8]
	        COPY_ENV rcx, rax, 0
	        mov rdx, rax
	        PUSH_ARGS

            test_5:


            MAKE_CLOSURE(rax,rax,code5)        
            jmp end_code5               ;; lambda

            code5:                        ;; lambda 
            push rbp
            mov rbp,rsp


            mov rcx,[rbp+WORD_SIZE*3] ;;rcx=n 
            sub rcx, 0 ;;rcx=n-(length of args)      
            dec rcx
            cmp rcx,0

            je noOpt5
                                ;;;; lambda
            

            lea rdx, [(4+0)*WORD_SIZE+rbp]  ;;len
            MAKE_LIST rax, rcx, rdx


loop_in5:                              ;;lambda

            lea rbx, [rbp+3*WORD_SIZE]

            mov rcx,qword [rbp+3*8] ;; rcx=n
            dec rcx
            lea rcx, [rbx+WORD_SIZE*rcx]
            mov qword [rcx], rax


            mov rdx,qword [rbp+3*8]
            sub rdx, 0             ;;len
            sub rdx, 2


            mov rcx, 0              ;;len

            cmp rdx, 0
            jle noOpt5             ;;lambda

            cmp rcx,0
            je only_n_env_ret_rbp5
            lea rbx, [rbp+4*WORD_SIZE]

    loop_args5:             ;;lambda
            dec rcx
            lea r10, [rbx+WORD_SIZE*rcx]

            lea r11, [r10+WORD_SIZE*rdx]
            mov r10, qword [r10]
            mov qword [r11],r10   
            cmp rcx,0
            jne loop_args5 ;;lambda

only_n_env_ret_rbp5:
            lea rax, [rbp+WORD_SIZE*(3+rdx)]
            mov r12, 0+2
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

            noOpt5:
              ;;lambda
            mov rax, PVAR(0)
                ;;body
            leave

            ret

            end_code5:
           ;; lambda
            

MAKE_DEFINE 272
	call write_sob_if_not_void

push SOB_NIL_ADDRESS
  ;;magic

MAKE_GET_FREE_VAR 224
push rax

MAKE_GET_FREE_VAR 24
push rax

MAKE_GET_FREE_VAR 32
push rax

push 4
             
            mov rax, 0
            cmp rax, 0
            jne malc6
            MALLOC rax, 8
            mov qword [rax], SOB_NIL_ADDRESS
            jmp fix6
            malc6:
            mov rcx, 8
            MALLOC rax, rcx
            mov rcx, qword [rbp+2*8]
            COPY_ENV rcx, rax, 0
            mov rdx, rax
            PUSH_ARGS 

            fix6:
            

            mov rbx,rax
            MAKE_CLOSURE(rax,rbx,code6)

            jmp end_code6


            code6:

            push rbp

            mov rbp,rsp

            push SOB_NIL_ADDRESS
  ;;magic

mov rax, qword const_tbl+40
push rax

push 2
             
            mov rax, 1
            cmp rax, 0
            jne malc7
            MALLOC rax, 8
            mov qword [rax], SOB_NIL_ADDRESS
            jmp fix7
            malc7:
            mov rcx, 16
            MALLOC rax, rcx
            mov rcx, qword [rbp+2*8]
            COPY_ENV rcx, rax, 1
            mov rdx, rax
            PUSH_ARGS 

            fix7:
            

            mov rbx,rax
            MAKE_CLOSURE(rax,rbx,code7)

            jmp end_code7


            code7:

            push rbp

            mov rbp,rsp

            

MAKE_BOX 0

MAKE_PARAM_SET_CODE 0

            mov rax, 2
            cmp rax, 0
            jne malc8
            MALLOC rax, 8
            mov qword [rax], SOB_NIL_ADDRESS
            jmp fix8
            malc8:
            mov rcx, 24
            MALLOC rax, rcx
            mov rcx, qword [rbp+2*8]
            COPY_ENV rcx, rax, 2
            mov rdx, rax
            PUSH_ARGS 

            fix8:
            

            mov rbx,rax
            MAKE_CLOSURE(rax,rbx,code8)

            jmp end_code8


            code8:

            push rbp

            mov rbp,rsp

            
push SOB_NIL_ADDRESS
  ;;magic

mov rax, PVAR(0)
push rax

push 2
             MAKE_VAR_BOUND_CODE 1, 0
             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             call rax
add rsp , 8*1 

            pop rbx

            lea rsp , [rsp + 8* rbx]
cmp rax, SOB_FALSE_ADDRESS
jne Lexit9

push SOB_NIL_ADDRESS
  ;;magic

mov rax, PVAR(0)
push rax

push 2
             MAKE_VAR_BOUND_CODE 1, 1
             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             call rax
add rsp , 8*1 

            pop rbx

            lea rsp , [rsp + 8* rbx]
cmp rax, SOB_FALSE_ADDRESS
je Lelse10
push SOB_NIL_ADDRESS
  ;;magic

push SOB_NIL_ADDRESS
  ;;magic

mov rax, PVAR(0)
push rax

push 2
             MAKE_VAR_BOUND_CODE 1, 2
             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             call rax
add rsp , 8*1 

            pop rbx

            lea rsp , [rsp + 8* rbx]
push rax

push 2
             MAKE_VAR_BOUND_CODE 0, 0
mov rax, qword [rax]
             mov r9, PARAM_COUNT
             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             push qword [rbp+WORD_SIZE*1]
             push qword [rbp]
             YAKI_SHIFT_FRAME 6
             pop rbp
             lea rsp ,[rsp + 8*(r9+4)]
             jmp rax

jmp Lexit10
Lelse10:
mov rax, qword const_tbl+2
Lexit10:

cmp rax, SOB_FALSE_ADDRESS
jne Lexit9

Lexit9:

            leave

            ret

            end_code8:

push rax
mov rax, PVAR(0)
pop qword [rax]
mov rax, SOB_VOID_ADDRESS

mov rax, PVAR(0)
mov rax, qword [rax]

            leave

            ret

            end_code7:

             mov r9, PARAM_COUNT
             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             push qword [rbp+WORD_SIZE*1]
             push qword [rbp]
             YAKI_SHIFT_FRAME 6
             pop rbp
             lea rsp ,[rsp + 8*(r9+4)]
             jmp rax


            leave

            ret

            end_code6:

             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             call rax
add rsp , 8*1 

            pop rbx

            lea rsp , [rsp + 8* rbx]
MAKE_DEFINE 280
	call write_sob_if_not_void

push SOB_NIL_ADDRESS
  ;;magic

MAKE_GET_FREE_VAR 32
push rax

MAKE_GET_FREE_VAR 144
push rax

MAKE_GET_FREE_VAR 248
push rax

push 4
             
            mov rax, 0
            cmp rax, 0
            jne malc11
            MALLOC rax, 8
            mov qword [rax], SOB_NIL_ADDRESS
            jmp fix11
            malc11:
            mov rcx, 8
            MALLOC rax, rcx
            mov rcx, qword [rbp+2*8]
            COPY_ENV rcx, rax, 0
            mov rdx, rax
            PUSH_ARGS 

            fix11:
            

            mov rbx,rax
            MAKE_CLOSURE(rax,rbx,code11)

            jmp end_code11


            code11:

            push rbp

            mov rbp,rsp

            
            mov rax, 1
	        cmp rax,0
	        jne dep_0_12
	        MALLOC rax,8
	        mov qword [rax], SOB_NIL_ADDRESS
	        jmp test_12
	        dep_0_12:
	        lea rax, [(1+1)*WORD_SIZE]
	        MALLOC rax, rax
	        mov rcx, qword [rbp+2*8]
	        COPY_ENV rcx, rax, 1
	        mov rdx, rax
	        PUSH_ARGS

            test_12:


            MAKE_CLOSURE(rax,rax,code12)        
            jmp end_code12               ;; lambda

            code12:                        ;; lambda 
            push rbp
            mov rbp,rsp


            mov rcx,[rbp+WORD_SIZE*3] ;;rcx=n 
            sub rcx, 1 ;;rcx=n-(length of args)      
            dec rcx
            cmp rcx,0

            je noOpt12
                                ;;;; lambda
            

            lea rdx, [(4+1)*WORD_SIZE+rbp]  ;;len
            MAKE_LIST rax, rcx, rdx


loop_in12:                              ;;lambda

            lea rbx, [rbp+3*WORD_SIZE]

            mov rcx,qword [rbp+3*8] ;; rcx=n
            dec rcx
            lea rcx, [rbx+WORD_SIZE*rcx]
            mov qword [rcx], rax


            mov rdx,qword [rbp+3*8]
            sub rdx, 1             ;;len
            sub rdx, 2


            mov rcx, 1              ;;len

            cmp rdx, 0
            jle noOpt12             ;;lambda

            cmp rcx,0
            je only_n_env_ret_rbp12
            lea rbx, [rbp+4*WORD_SIZE]

    loop_args12:             ;;lambda
            dec rcx
            lea r10, [rbx+WORD_SIZE*rcx]

            lea r11, [r10+WORD_SIZE*rdx]
            mov r10, qword [r10]
            mov qword [r11],r10   
            cmp rcx,0
            jne loop_args12 ;;lambda

only_n_env_ret_rbp12:
            lea rax, [rbp+WORD_SIZE*(3+rdx)]
            mov r12, 1+2
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

            noOpt12:
              ;;lambda
            push SOB_NIL_ADDRESS
  ;;magic

mov rax, PVAR(1)
push rax

push 2
             MAKE_VAR_BOUND_CODE 0, 2
             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             call rax
add rsp , 8*1 

            pop rbx

            lea rsp , [rsp + 8* rbx]
cmp rax, SOB_FALSE_ADDRESS
je Lelse13
push SOB_NIL_ADDRESS
  ;;magic

push SOB_NIL_ADDRESS
  ;;magic

mov rax, PVAR(0)
push rax

mov rax, qword const_tbl+49
push rax

push 3
             MAKE_GET_FREE_VAR 152
             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             call rax
add rsp , 8*1 

            pop rbx

            lea rsp , [rsp + 8* rbx]
push rax

mov rax, qword const_tbl+6
push rax

push 3
             MAKE_VAR_BOUND_CODE 0, 1
             mov r9, PARAM_COUNT
             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             push qword [rbp+WORD_SIZE*1]
             push qword [rbp]
             YAKI_SHIFT_FRAME 7
             pop rbp
             lea rsp ,[rsp + 8*(r9+4)]
             jmp rax

jmp Lexit13
Lelse13:
push SOB_NIL_ADDRESS
  ;;magic

push SOB_NIL_ADDRESS
  ;;magic

push SOB_NIL_ADDRESS
  ;;magic

mov rax, PVAR(1)
push rax

MAKE_VAR_BOUND_CODE 0, 1
push rax

push 3
             MAKE_VAR_BOUND_CODE 0, 0
             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             call rax
add rsp , 8*1 

            pop rbx

            lea rsp , [rsp + 8* rbx]
push rax

mov rax, qword const_tbl+49
push rax

push 3
             MAKE_GET_FREE_VAR 152
             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             call rax
add rsp , 8*1 

            pop rbx

            lea rsp , [rsp + 8* rbx]
push rax

mov rax, PVAR(0)
push rax

push 3
             MAKE_VAR_BOUND_CODE 0, 1
             mov r9, PARAM_COUNT
             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             push qword [rbp+WORD_SIZE*1]
             push qword [rbp]
             YAKI_SHIFT_FRAME 7
             pop rbp
             lea rsp ,[rsp + 8*(r9+4)]
             jmp rax

Lexit13:

                ;;body
            leave

            ret

            end_code12:
           ;; lambda
            


            leave

            ret

            end_code11:

             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             call rax
add rsp , 8*1 

            pop rbx

            lea rsp , [rsp + 8* rbx]
MAKE_DEFINE 288
	call write_sob_if_not_void

push SOB_NIL_ADDRESS
  ;;magic

MAKE_GET_FREE_VAR 96
push rax

MAKE_GET_FREE_VAR 216
push rax

MAKE_GET_FREE_VAR 32
push rax

push 4
             
            mov rax, 0
            cmp rax, 0
            jne malc14
            MALLOC rax, 8
            mov qword [rax], SOB_NIL_ADDRESS
            jmp fix14
            malc14:
            mov rcx, 8
            MALLOC rax, rcx
            mov rcx, qword [rbp+2*8]
            COPY_ENV rcx, rax, 0
            mov rdx, rax
            PUSH_ARGS 

            fix14:
            

            mov rbx,rax
            MAKE_CLOSURE(rax,rbx,code14)

            jmp end_code14


            code14:

            push rbp

            mov rbp,rsp

            
            mov rax, 1
	        cmp rax,0
	        jne dep_0_15
	        MALLOC rax,8
	        mov qword [rax], SOB_NIL_ADDRESS
	        jmp test_15
	        dep_0_15:
	        lea rax, [(1+1)*WORD_SIZE]
	        MALLOC rax, rax
	        mov rcx, qword [rbp+2*8]
	        COPY_ENV rcx, rax, 1
	        mov rdx, rax
	        PUSH_ARGS

            test_15:


            MAKE_CLOSURE(rax,rax,code15)        
            jmp end_code15               ;; lambda

            code15:                        ;; lambda 
            push rbp
            mov rbp,rsp


            mov rcx,[rbp+WORD_SIZE*3] ;;rcx=n 
            sub rcx, 1 ;;rcx=n-(length of args)      
            dec rcx
            cmp rcx,0

            je noOpt15
                                ;;;; lambda
            

            lea rdx, [(4+1)*WORD_SIZE+rbp]  ;;len
            MAKE_LIST rax, rcx, rdx


loop_in15:                              ;;lambda

            lea rbx, [rbp+3*WORD_SIZE]

            mov rcx,qword [rbp+3*8] ;; rcx=n
            dec rcx
            lea rcx, [rbx+WORD_SIZE*rcx]
            mov qword [rcx], rax


            mov rdx,qword [rbp+3*8]
            sub rdx, 1             ;;len
            sub rdx, 2


            mov rcx, 1              ;;len

            cmp rdx, 0
            jle noOpt15             ;;lambda

            cmp rcx,0
            je only_n_env_ret_rbp15
            lea rbx, [rbp+4*WORD_SIZE]

    loop_args15:             ;;lambda
            dec rcx
            lea r10, [rbx+WORD_SIZE*rcx]

            lea r11, [r10+WORD_SIZE*rdx]
            mov r10, qword [r10]
            mov qword [r11],r10   
            cmp rcx,0
            jne loop_args15 ;;lambda

only_n_env_ret_rbp15:
            lea rax, [rbp+WORD_SIZE*(3+rdx)]
            mov r12, 1+2
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

            noOpt15:
              ;;lambda
            push SOB_NIL_ADDRESS
  ;;magic

mov rax, PVAR(1)
push rax

push 2
             MAKE_VAR_BOUND_CODE 0, 0
             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             call rax
add rsp , 8*1 

            pop rbx

            lea rsp , [rsp + 8* rbx]
cmp rax, SOB_FALSE_ADDRESS
je Lelse16
push SOB_NIL_ADDRESS
  ;;magic

mov rax, qword const_tbl+66
push rax

mov rax, PVAR(0)
push rax

push 3
             MAKE_VAR_BOUND_CODE 0, 2
             mov r9, PARAM_COUNT
             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             push qword [rbp+WORD_SIZE*1]
             push qword [rbp]
             YAKI_SHIFT_FRAME 7
             pop rbp
             lea rsp ,[rsp + 8*(r9+4)]
             jmp rax

jmp Lexit16
Lelse16:
push SOB_NIL_ADDRESS
  ;;magic

push SOB_NIL_ADDRESS
  ;;magic

mov rax, PVAR(1)
push rax

push 2
             MAKE_VAR_BOUND_CODE 0, 1
             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             call rax
add rsp , 8*1 

            pop rbx

            lea rsp , [rsp + 8* rbx]
push rax

mov rax, PVAR(0)
push rax

push 3
             MAKE_VAR_BOUND_CODE 0, 2
             mov r9, PARAM_COUNT
             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             push qword [rbp+WORD_SIZE*1]
             push qword [rbp]
             YAKI_SHIFT_FRAME 7
             pop rbp
             lea rsp ,[rsp + 8*(r9+4)]
             jmp rax

Lexit16:

                ;;body
            leave

            ret

            end_code15:
           ;; lambda
            


            leave

            ret

            end_code14:

             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             call rax
add rsp , 8*1 

            pop rbx

            lea rsp , [rsp + 8* rbx]
MAKE_DEFINE 96
	call write_sob_if_not_void

push SOB_NIL_ADDRESS
  ;;magic

mov rax, qword const_tbl+85
push rax

mov rax, qword const_tbl+68
push rax

push 3
             MAKE_GET_FREE_VAR 96
             CLOSURE_ENV rbx, rax
             push rbx
             CLOSURE_CODE rax, rax
             call rax
add rsp , 8*1 

            pop rbx

            lea rsp , [rsp + 8* rbx]
	call write_sob_if_not_void
  check:
  ;;; Clean up the dummy frame, set the exit status to 0 ("success"), 
   ;;; and return from main
   pop rbp
   add rsp, 3*8
   mov rax, 0

   ret
boolean?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov sil, byte [rsi]
	cmp sil, T_BOOL
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

flonum?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov sil, byte [rsi]
	cmp sil, T_FLOAT
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

rational?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov sil, byte [rsi]
	cmp sil, T_RATIONAL
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

pair?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov sil, byte [rsi]
	cmp sil, T_PAIR
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

null?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov sil, byte [rsi]
	cmp sil, T_NIL
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

char?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov sil, byte [rsi]
	cmp sil, T_CHAR
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

string?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov sil, byte [rsi]
	cmp sil, T_STRING
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

symbol?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov sil, byte [rsi]
	cmp sil, T_SYMBOL
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

procedure?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov sil, byte [rsi]
	cmp sil, T_CLOSURE
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

div:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	mov dl, byte [rsi]
             cmp dl, T_FLOAT
	     jne .div_rat
             FLOAT_VAL rsi, rsi 
          movq xmm0, rsi
          FLOAT_VAL rdi, rdi 
          movq xmm1, rdi
	  divsd xmm0, xmm1
          movq rsi, xmm0
          MAKE_FLOAT(rax, rsi)
             jmp .op_return
          .div_rat:
             DENOMINATOR rcx, rsi
	  DENOMINATOR rdx, rdi
	  NUMERATOR rsi, rsi
	  NUMERATOR rdi, rdi
          MAKE_RATIONAL(rax, rdx, rdi)
         mov PVAR(1), rax
         pop rbp
         jmp mul
	  mov rax, rcx
	  mov rdi, rsi
          .gcd_loop:
     and rdi, rdi
     jz .end_gcd_loop
     cqo
     idiv rdi
     mov rax, rdi
     mov rdi, rdx
     jmp .gcd_loop	
     .end_gcd_loop:
	  mov rdi, rax
	  mov rax, rsi
	  cqo
	  idiv rdi
	  mov rsi, rax
	  mov rax, rcx
	  cqo
	  idiv rdi
	  mov rcx, rax
          cmp rcx, 0
          jge .make_rat
          imul rsi, -1
          imul rcx, -1
          .make_rat:
          MAKE_RATIONAL(rax, rsi, rcx)
          .op_return:
         pop rbp
         ret

mul:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	mov dl, byte [rsi]
             cmp dl, T_FLOAT
	     jne .mul_rat
             FLOAT_VAL rsi, rsi 
          movq xmm0, rsi
          FLOAT_VAL rdi, rdi 
          movq xmm1, rdi
	  mulsd xmm0, xmm1
          movq rsi, xmm0
          MAKE_FLOAT(rax, rsi)
             jmp .op_return
          .mul_rat:
             DENOMINATOR rcx, rsi
	  DENOMINATOR rdx, rdi
	  NUMERATOR rsi, rsi
	  NUMERATOR rdi, rdi
          imul rsi, rdi
	 imul rcx, rdx
	  mov rax, rcx
	  mov rdi, rsi
          .gcd_loop:
     and rdi, rdi
     jz .end_gcd_loop
     cqo
     idiv rdi
     mov rax, rdi
     mov rdi, rdx
     jmp .gcd_loop	
     .end_gcd_loop:
	  mov rdi, rax
	  mov rax, rsi
	  cqo
	  idiv rdi
	  mov rsi, rax
	  mov rax, rcx
	  cqo
	  idiv rdi
	  mov rcx, rax
          cmp rcx, 0
          jge .make_rat
          imul rsi, -1
          imul rcx, -1
          .make_rat:
          MAKE_RATIONAL(rax, rsi, rcx)
          .op_return:
         pop rbp
         ret

add:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	mov dl, byte [rsi]
             cmp dl, T_FLOAT
	     jne .add_rat
             FLOAT_VAL rsi, rsi 
          movq xmm0, rsi
          FLOAT_VAL rdi, rdi 
          movq xmm1, rdi
	  addsd xmm0, xmm1
          movq rsi, xmm0
          MAKE_FLOAT(rax, rsi)
             jmp .op_return
          .add_rat:
             DENOMINATOR rcx, rsi
	  DENOMINATOR rdx, rdi
	  NUMERATOR rsi, rsi
	  NUMERATOR rdi, rdi
          imul rsi, rdx
	 imul rdi, rcx
	 add rsi, rdi
	 imul rcx, rdx
	  mov rax, rcx
	  mov rdi, rsi
          .gcd_loop:
     and rdi, rdi
     jz .end_gcd_loop
     cqo
     idiv rdi
     mov rax, rdi
     mov rdi, rdx
     jmp .gcd_loop	
     .end_gcd_loop:
	  mov rdi, rax
	  mov rax, rsi
	  cqo
	  idiv rdi
	  mov rsi, rax
	  mov rax, rcx
	  cqo
	  idiv rdi
	  mov rcx, rax
          cmp rcx, 0
          jge .make_rat
          imul rsi, -1
          imul rcx, -1
          .make_rat:
          MAKE_RATIONAL(rax, rsi, rcx)
          .op_return:
         pop rbp
         ret

eq:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	mov dl, byte [rsi]
             cmp dl, T_FLOAT
	     jne .eq_rat
             FLOAT_VAL rsi, rsi
	 FLOAT_VAL rdi, rdi
	 cmp rsi, rdi
             jmp .op_return
          .eq_rat:
             NUMERATOR rcx, rsi
	 NUMERATOR rdx, rdi
	 cmp rcx, rdx
	 jne .false
	 DENOMINATOR rcx, rsi
	 DENOMINATOR rdx, rdi
	 cmp rcx, rdx
         .false:
          .op_return:
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

lt:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	mov dl, byte [rsi]
             cmp dl, T_FLOAT
	     jne .lt_rat
             FLOAT_VAL rsi, rsi
	 movq xmm0, rsi
	 FLOAT_VAL rdi, rdi
	 movq xmm1, rdi
	 cmpltpd xmm0, xmm1
         movq rsi, xmm0
         cmp rsi, 0
             jmp .op_return
          .lt_rat:
             DENOMINATOR rcx, rsi
	 DENOMINATOR rdx, rdi
	 NUMERATOR rsi, rsi
	 NUMERATOR rdi, rdi
	 imul rsi, rdx
	 imul rdi, rcx
	 cmp rsi, rdi
          .op_return:
      jl .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

string_length:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	STRING_LENGTH rsi, rsi
         MAKE_RATIONAL(rax, rsi, 1)
         pop rbp
         ret

string_ref:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	STRING_ELEMENTS rsi, rsi
         NUMERATOR rdi, rdi
         add rsi, rdi
         mov sil, byte [rsi]
         MAKE_CHAR(rax, sil)
         pop rbp
         ret

string_set:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	mov rdx, PVAR(2)
	STRING_ELEMENTS rsi, rsi
         NUMERATOR rdi, rdi
         add rsi, rdi
         CHAR_VAL rax, rdx
         mov byte [rsi], al
         mov rax, SOB_VOID_ADDRESS
         pop rbp
         ret

make_string:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	NUMERATOR rsi, rsi
         CHAR_VAL rdi, rdi
         and rdi, 255
         MAKE_STRING rax, rsi, dil
         pop rbp
         ret

symbol_to_string:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	SYMBOL_VAL rsi, rsi
	 STRING_LENGTH rcx, rsi
	 STRING_ELEMENTS rdi, rsi
	 push rcx
	 push rdi
	 mov dil, byte [rdi]
	 MAKE_CHAR(rax, dil)
	 push rax
	 MAKE_RATIONAL(rax, rcx, 1)
	 push rax
	 push 2
	 push SOB_NIL_ADDRESS
	 call make_string
	 add rsp, 4*8
	 STRING_ELEMENTS rsi, rax   
	 pop rdi
	 pop rcx
	 cmp rcx, 0
	 je .end
         .loop:
	 lea r8, [rdi+rcx]
	 lea r9, [rsi+rcx]
	 mov bl, byte [r8]
	 mov byte [r9], bl
	 loop .loop
         .end:
         pop rbp
         ret

eq?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	cmp rsi, rdi
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

char_to_integer:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	CHAR_VAL rsi, rsi
	 and rsi, 255
	 MAKE_RATIONAL(rax, rsi, 1)
         pop rbp
         ret

integer_to_char:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	NUMERATOR rsi, rsi
	 and rsi, 255
	 MAKE_CHAR(rax, sil)
         pop rbp
         ret

exact_to_inexact:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	DENOMINATOR rdi, rsi
	 NUMERATOR rsi, rsi 
	 cvtsi2sd xmm0, rsi
	 cvtsi2sd xmm1, rdi
	 divsd xmm0, xmm1
	 movq rsi, xmm0
	 MAKE_FLOAT(rax, rsi)
         pop rbp
         ret

numerator:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	NUMERATOR rsi, rsi
	 mov rdi, 1
	 MAKE_RATIONAL(rax, rsi, rdi)
         pop rbp
         ret

denominator:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	DENOMINATOR rsi, rsi
	 mov rdi, 1
	 MAKE_RATIONAL(rax, rsi, rdi)
         pop rbp
         ret

gcd:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	xor rdx, rdx
	 NUMERATOR rax, rsi
         NUMERATOR rdi, rdi
         .gcd_loop:
     and rdi, rdi
     jz .end_gcd_loop
     cqo
     idiv rdi
     mov rax, rdi
     mov rdi, rdx
     jmp .gcd_loop	
     .end_gcd_loop:
	 mov rdx, rax
         cmp rdx, 0
         jge .make_result
         neg rdx
         .make_result:
         MAKE_RATIONAL(rax, rdx, 1)
         pop rbp
         ret

car:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	
      CAR rax, rsi
      
         pop rbp
         ret

cdr:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	
      CDR rax, rsi
      
         pop rbp
         ret

cons:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	
    MAKE_PAIR(rax,rsi,rdi)
    
         pop rbp
         ret

set_car:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	
        CAR rsi, rsi
        mov [rsi], rdi
        mov rax, SOB_VOID_ADDRESS
        
         pop rbp
         ret

set_cdr:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	
        CDR rsi, rsi
        mov [rsi], rdi
        mov rax, SOB_VOID_ADDRESS
        
         pop rbp
         ret

apply:
       push rbp
       mov rbp, rsp 
        
      MAKE_APPLY 1
         pop rbp
         ret