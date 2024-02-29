%define T_UNDEFINED 0
%define T_VOID 1
%define T_NIL 2
%define T_RATIONAL 3
%define T_FLOAT 4
%define T_BOOL 5
%define T_CHAR 6
%define T_STRING 7
%define T_SYMBOL 8
%define T_CLOSURE 9
%define T_PAIR 10
%define T_VECTOR 11

%define TYPE_SIZE 1
%define WORD_SIZE 8
	
%define KB(n) n*1024
%define MB(n) 1024*KB(n)
%define GB(n) 1024*MB(n)

%macro MAKE_LITERAL 2
		db %1
		%2
%endmacro

%define PARAM_COUNT qword [rbp+3*WORD_SIZE]
%define MAKE_LITERAL_FLOAT(val) MAKE_LITERAL T_FLOAT, dq val
%define MAKE_LITERAL_CHAR(val) MAKE_LITERAL T_CHAR, db val
%define MAKE_NIL db T_NIL
%define MAKE_VOID db T_VOID
%define MAKE_BOOL(val) db T_BOOL, val
%define MAKE_LITERAL_SYMBOL(val) MAKE_LITERAL T_SYMBOL,dq (val+CONST_TABLE)
%macro MAKE_LITERAL_STRING 1
db T_STRING
dq (%%end_str - %%str)
%%str:
db %1
%%end_str:
%endmacro

%macro MAKE_LITERAL_VECTOR 0-*
	db T_VECTOR
	dq %0
	%rep %0
	dq %1
	%rotate 1
	%endrep
%endmacro



%macro SKIP_TYPE_TAG 2
	mov %1, qword [%2+TYPE_SIZE]	
%endmacro	

%define NUMERATOR SKIP_TYPE_TAG

%macro DENOMINATOR 2
	mov %1, qword [%2+TYPE_SIZE+WORD_SIZE]
%endmacro

%macro CHAR_VAL 2
	movzx %1, byte [%2+TYPE_SIZE]
%endmacro

%define FLOAT_VAL SKIP_TYPE_TAG

%define STRING_LENGTH SKIP_TYPE_TAG
%define VECTOR_LENGTH SKIP_TYPE_TAG

%define SYMBOL_VAL SKIP_TYPE_TAG

%macro STRING_ELEMENTS 2
	lea %1, [%2+TYPE_SIZE+WORD_SIZE]
%endmacro
%define VECTOR_ELEMENTS STRING_ELEMENTS

%define CAR SKIP_TYPE_TAG

%macro CDR 2
	mov %1, qword [%2+TYPE_SIZE+WORD_SIZE]
%endmacro

%define CLOSURE_ENV CAR

%define CLOSURE_CODE CDR

%define PVAR(n) qword [rbp+(4+n)*WORD_SIZE]

; returns %2 allocated bytes in register %1
; Supports using with %1 = %2
%macro MALLOC 2
	add qword [malloc_pointer], %2
	push %2
	mov %1, qword [malloc_pointer]
	sub %1, [rsp]
	add rsp, 8
%endmacro
	
; Creates a short SOB with the
; value %2
; Returns the result in register %1
%macro MAKE_CHAR_VALUE 2
	MALLOC %1, 1+TYPE_SIZE
	mov byte [%1], T_CHAR
	mov byte [%1+TYPE_SIZE], %2
%endmacro

; %1 = size of frame (constant)
%macro SHIFT_FRAME 1
mov r13, PARAM_COUNT
add r13, 4
mov r11,1
mov rsi ,%1
inc rsi
%%start_loop_frame:
cmp r11, rsi
je %%shift_frame_end
dec r13
mov r12, rbp
shl r11,3
sub r12, r11
shr r11,3
mov r12,qword [r12]
lea r10, [rbp+WORD_SIZE*r13]
mov qword [r10], r12
add r11,1
jmp %%start_loop_frame
%%shift_frame_end:
mov rbp, 	r10
mov rsp,	rbp
%endmacro

;; %1 = size of frame (constant)
%macro YAKI_SHIFT_FRAME 1
	push rbx
	push rax
	mov rax, PARAM_COUNT
	add rax, 4 
%assign i 1
%rep %1
	dec rax
	mov rbx, qword [rbp-WORD_SIZE*i]
	mov qword [rbp+WORD_SIZE*rax], rbx
%assign i i+1
%endrep
	pop rax
	pop rbx
%endmacro


%macro LIST_LEN 1
	mov r12,0
	mov r9, %1 
	%%listLenLoop:
	cmp r9, SOB_NIL_ADDRESS
	je %%endListLen
	inc r12
	CDR r9, r9
	jmp %%listLenLoop
	%%endListLen:
	mov rax, r12
%endmacro

%macro LIST_GET 2
	mov r13, %1
	mov r14, %2
	%%listGetLoop:
	cmp r14,0
	je %%endListGet
	CDR r13,r13
	dec r14
	jmp %%listGetLoop
	%%endListGet:
	CAR rax,r13
%endmacro

%macro PUSH_REVERSE 1
	mov r10, %1 
	LIST_LEN %1
	mov r11, rax
	%%loopPushRev:
	cmp r11,0
	je %%endPushRev
	dec r11
	LIST_GET %1,r11
	push rax

	jmp %%loopPushRev
	%%endPushRev:
%endmacro




; Creates a long SOB with the
; value %2 and type %3.
; Returns the result in register %1
%macro MAKE_LONG_VALUE 3
	MALLOC %1, TYPE_SIZE+WORD_SIZE
	mov byte [%1], %3
	mov qword [%1+TYPE_SIZE], %2
%endmacro

%define MAKE_INT(r, val) MAKE_LONG_VALUE r, val, T_INTEGER
%define MAKE_FLOAT(r,val) MAKE_LONG_VALUE r, val, T_FLOAT
%define MAKE_CHAR(r,val) MAKE_CHAR_VALUE r, val

%define TYPE(r) byte [r]
%define DATA(r) [r + TYPE_SIZE]
%define INT_DATA(r) qword DATA(r)
%define FLOAT_DATA(r) qword DATA(r)
%define CHAR_DATA(r) byte DATA(r)
%define BOOL_DATA(r) byte DATA(r)
%define STR_LEN(r) qword DATA(r)
%define STR_DATA_PTR(r) r + WORD_BYTES + TYPE_SIZE
%define STRING_REF(r, i) byte [r+WORD_BYTES + TYPE_SIZE + i]


%define CONST_TABLE const_tbl
%define FVARS_TABLE fvar_tbl
 

%macro MAKE_CONST_CODE 1
	mov rax, qword CONST_TABLE+%1
%endmacro

%macro MAKE_VAR_PARAM_CODE 1
	mov rax, qword [rbp +8*(4+%1)]
%endmacro




%macro MAKE_BOX 1
	mov rbx, PVAR(%1)
	MALLOC rax, WORD_SIZE
	mov qword [rax], rbx
%endmacro



%macro MAKE_PARAM_SET_CODE 1
	mov qword [rbp+WORD_SIZE*(4+%1)],rax
	mov rax,SOB_VOID_ADDRESS
%endmacro

%macro MAKE_VAR_BOUND_CODE 2
	mov rax, qword [rbp+WORD_SIZE*2]
	mov rax, qword [rax+WORD_SIZE*%1]
	cmp rax, SOB_NIL_ADDRESS
	je %%fii	
	mov rax, qword [rax+WORD_SIZE*%2]
	%%fii:
%endmacro

%macro MAKE_BOUND_SET_CODE 2
	mov rbx , qword [rbp+WORD_SIZE*2]
	mov rbx , qword [rbx+WORD_SIZE*%1]
	mov qword [rbx+WORD_SIZE*%2] , rax
	mov rax, SOB_VOID_ADDRESS
%endmacro

%macro MAKE_VAR 2
	mov [FVARS_TABLE+%1],qword (CONST_TABLE+%2)
	mov rax, FVARS_TABLE+%1
%endmacro

%macro MAKE_GET_FREE_VAR 1
	mov rax, qword [FVARS_TABLE+%1]
%endmacro


%macro MAKE_SET_FREE_VAR 1
	mov qword [FVARS_TABLE+%1], rax
	mov rax , SOB_VOID_ADDRESS
%endmacro

%macro MAKE_DEFINE 1
	mov qword [FVARS_TABLE+%1], rax
	mov rax , SOB_VOID_ADDRESS
%endmacro

;; %1 old env
;; %2 new env
;; %3 i
%macro COPY_ENV 3 
	mov rbx, %3
	%%copyEnv:
	cmp rbx,0
	je %%endCopyEnv
	dec rbx
	push rbx
	shl rbx,3
	add rbx,%1
	mov rdx,qword [rbx]
	pop rbx
	mov rdi, rbx
	add rdi, 1
	push rdi
	shl rdi,3
	add rdi,%2
	mov qword [rdi], rdx
	pop rdi
	jmp %%copyEnv
%%endCopyEnv:
%endmacro

;;push all the args from this frame
%macro PUSH_ARGS 0
	mov rcx, qword [rbp+3*8];;the number of args
	cmp rcx,0
	je %%fix
	shl rcx, 3
	MALLOC  rax, rcx
	shr rcx, 3
	mov r9,rdx
%%start_push:
	cmp rcx , 0
	je %%end
	dec rcx
	mov rdx, PVAR(rcx)
	lea r10, [rax+WORD_SIZE*rcx]
	mov qword [r10], rdx
	jmp %%start_push
%%end:
	mov qword [r9], rax
	mov rax,r9
	jmp %%end1
%%fix:
	mov qword [rdx], SOB_NIL_ADDRESS
%%end1:
%endmacro



; Create a string of length %2
; from char %3.
; Stores result in register %1
%macro MAKE_STRING 3
	lea %1, [%2+WORD_SIZE+TYPE_SIZE]
	MALLOC %1, %1
	mov byte [%1], T_STRING
	mov qword [%1+TYPE_SIZE], %2
	push rcx
	add %1,WORD_SIZE+TYPE_SIZE
	mov rcx, %2
	cmp rcx, 0
%%str_loop:
	jz %%str_loop_end
	dec rcx
	mov byte [%1+rcx], %3
	jmp %%str_loop
%%str_loop_end:
	pop rcx
	sub %1, WORD_SIZE+TYPE_SIZE
%endmacro

; Create a vector of length %2
; from array of elements in register %3
; Store result in register %1
%macro MAKE_VECTOR 3
	lea %1, [%2*WORD_SIZE+WORD_SIZE+TYPE_SIZE]
	MALLOC %1, %1
	mov byte [%1], T_VECTOR
	mov qword [%1+TYPE_SIZE], %2

    push rbx
    push rcx
    push %1
    add %1,WORD_SIZE+TYPE_SIZE
    mov rcx, %2
%%vector_loop:
    cmp rcx, 0
    je %%vector_loop_end
    mov rbx,qword [%3]
    mov qword [%1], rbx
    add %1, WORD_SIZE
    add %3, WORD_SIZE
    dec rcx
    jmp %%vector_loop
%%vector_loop_end:
    pop %1
    pop rcx
    pop rbx
%endmacro
; Create a vector of length %2
; from array of elements in register %3
; Store result in register %1
%macro MAKE_LIST  3

	mov rbx,qword [%3]
	MAKE_PAIR(%1,rbx,SOB_NIL_ADDRESS)
    push %1
	dec rcx
%%list_loop:
    cmp rcx, 0
    je %%end_list_loop

	add %3,	WORD_SIZE
	mov rbx, qword [%3]

	MAKE_PAIR(r10,rbx,SOB_NIL_ADDRESS)
	add %1, TYPE_SIZE
	add %1, WORD_SIZE
	mov qword [%1], r10
	mov %1,r10
    dec rcx
    jmp %%list_loop
%%end_list_loop:
    pop %1
%endmacro

;;; Creates a SOB with tag %2 
;;; from two pointers %3 and %4
;;; Stores result in register %1
%macro MAKE_TWO_WORDS 4 
        MALLOC %1, TYPE_SIZE+WORD_SIZE*2
        mov byte [%1], %2
        mov qword [%1+TYPE_SIZE], %3
        mov qword [%1+TYPE_SIZE+WORD_SIZE], %4
%endmacro

%macro MAKE_WORDS_LIT 3
	db %1
        dq %2
        dq %3
%endmacro


%macro MAKE_APPLY 1
	mov r10, qword [rbp+3*WORD_SIZE]
	mov r9, r10
	sub r9,2
	mov rdx,r9
	mov r11, PVAR(r9)
	mov rbx, r11
	push SOB_NIL_ADDRESS
	PUSH_REVERSE rbx

	mov r9,rdx
	mov r15,1
%%start_loopa:
	cmp r15,  rdx
	jg %%end_loopa
	dec r9
	push qword [rbp+(4+r9)*WORD_SIZE]
	inc r15
 	jmp	%%start_loopa
%%end_loopa:
	pop rcx
	LIST_LEN rbx
	add rax, rdx
	mov r10, rax
	add r10, 4 ;; how much to move frame
	push rax
	CLOSURE_ENV rax,rcx
	push rax
	push qword [rbp+1*WORD_SIZE]
	push qword [rbp]

	SHIFT_FRAME r10
	pop rbp
	CLOSURE_CODE rax,rcx
	jmp rax

%endmacro

;; %1 = length %2= list
%macro MAKE_APP_LIST 2
	lea r14, [%1*WORD_SIZE]
	MALLOC r15, r14

%%start_make_app:
	cmp r14, 0
	je %%end_make_app
	sub r14, WORD_SIZE
	lea r8, [%2+r14]
	mov r8,qword [r8]
	lea r13, [r15+r14]
	mov qword [r13], r8
	jmp %%start_make_app
%%end_make_app:
	mov rax,r15
	
%endmacro



%define MAKE_RATIONAL(r, num, den) \
	MAKE_TWO_WORDS r, T_RATIONAL, num, den

%define MAKE_LITERAL_RATIONAL(num, den) \
	MAKE_WORDS_LIT T_RATIONAL, num, den
	
%define MAKE_PAIR(r, car, cdr) \
        MAKE_TWO_WORDS r, T_PAIR, car, cdr

%define MAKE_LITERAL_PAIR(car, cdr) \
        MAKE_WORDS_LIT T_PAIR, (car+CONST_TABLE), (cdr+CONST_TABLE)

%define MAKE_CLOSURE(r, env, body) \
        MAKE_TWO_WORDS r, T_CLOSURE, env, body


	
;;; Macros and routines for printing Scheme OBjects to STDOUT
%define CHAR_NUL 0
%define CHAR_TAB 9
%define CHAR_NEWLINE 10
%define CHAR_PAGE 12
%define CHAR_RETURN 13
%define CHAR_SPACE 32
%define CHAR_DOUBLEQUOTE 34
%define CHAR_BACKSLASH 92
	
extern printf, malloc
global write_sob, write_sob_if_not_void
	
write_sob_undefined:
	push rbp
	mov rbp, rsp

	mov rax, qword 0
	mov rdi, .undefined
	call printf

	pop rbp
	ret

section .data
.undefined:
	db "#<undefined>", 0

section .text
write_sob_rational:
	push rbp
	mov rbp, rsp

	mov rdx, rsi
	NUMERATOR rsi, rdx
	DENOMINATOR rdx, rdx
	
	cmp rdx, 1
	jne .print_fraction

	mov rdi, .int_format_string
	jmp .print

.print_fraction:
	mov rdi, .frac_format_string

.print:	
	mov rax, 0
	call printf

	pop rbp
	ret

section .data
.int_format_string:
	db "%ld", 0
.frac_format_string:
	db "%ld/%ld", 0

section .text
write_sob_float:
	push rbp
	mov rbp, rsp

	FLOAT_VAL rsi, rsi
	movq xmm0, rsi
	mov rdi, .float_format_string
	mov rax, 1

	;; printf-ing floats (among other things) requires the stack be 16-byte aligned
	;; so align the stack *downwards* (take up some extra space) if needed before
	;; calling printf for floats
	and rsp, -16 
	call printf

	;; move the stack back to the way it was, cause we messed it up in order to
	;; call printf.
	;; Note that the `leave` instruction does exactly this (reset the stack and pop
	;; rbp). The instructions are explicitly layed out here for clarity.
	mov rsp, rbp
	pop rbp
	ret
	
section .data
.float_format_string:
	db "%f", 0		

section .text
write_sob_char:
	push rbp
	mov rbp, rsp

	CHAR_VAL rsi, rsi

	cmp rsi, CHAR_NUL
	je .Lnul

	cmp rsi, CHAR_TAB
	je .Ltab

	cmp rsi, CHAR_NEWLINE
	je .Lnewline

	cmp rsi, CHAR_PAGE
	je .Lpage

	cmp rsi, CHAR_RETURN
	je .Lreturn

	cmp rsi, CHAR_SPACE
	je .Lspace
	jg .Lregular

	mov rdi, .special
	jmp .done	

.Lnul:
	mov rdi, .nul
	jmp .done

.Ltab:
	mov rdi, .tab
	jmp .done

.Lnewline:
	mov rdi, .newline
	jmp .done

.Lpage:
	mov rdi, .page
	jmp .done

.Lreturn:
	mov rdi, .return
	jmp .done

.Lspace:
	mov rdi, .space
	jmp .done

.Lregular:
	mov rdi, .regular
	jmp .done

.done:
	mov rax, 0
	call printf

	pop rbp
	ret

section .data
.space:
	db "#\space", 0
.newline:
	db "#\newline", 0
.return:
	db "#\return", 0
.tab:
	db "#\tab", 0
.page:
	db "#\page", 0
.nul:
	db "#\nul", 0
.special:
	db "#\x%02x", 0
.regular:
	db "#\%c", 0

section .text
write_sob_void:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .void
	call printf

	pop rbp
	ret

section .data
.void:
	db "#<void>", 0
	
section .text
write_sob_bool:
	push rbp
	mov rbp, rsp

	cmp word [rsi], word T_BOOL
	je .sobFalse
	
	mov rdi, .true
	jmp .continue

.sobFalse:
	mov rdi, .false

.continue:
	mov rax, 0
	call printf	

	pop rbp
	ret

section .data			
.false:
	db "#f", 0
.true:
	db "#t", 0

section .text
write_sob_nil:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .nil
	call printf

	pop rbp
	ret

section .data
.nil:
	db "()", 0

section .text
write_sob_string:
	push rbp
	mov rbp, rsp

	push rsi

	mov rax, 0
	mov rdi, .double_quote
	call printf
	
	pop rsi

	STRING_LENGTH rcx, rsi
	STRING_ELEMENTS rax, rsi

.loop:
	cmp rcx, 0
	je .done
	mov bl, byte [rax]
	and rbx, 0xff

	cmp rbx, CHAR_TAB
	je .ch_tab
	cmp rbx, CHAR_NEWLINE
	je .ch_newline
	cmp rbx, CHAR_PAGE
	je .ch_page
	cmp rbx, CHAR_RETURN
	je .ch_return
	cmp rbx, CHAR_DOUBLEQUOTE
	je .ch_doublequote
	cmp rbx, CHAR_BACKSLASH
	je .ch_backslash
	cmp rbx, CHAR_SPACE
	jl .ch_hex
	
	mov rdi, .fs_simple_char
	mov rsi, rbx
	jmp .printf
	
.ch_hex:
	mov rdi, .fs_hex_char
	mov rsi, rbx
	jmp .printf
	
.ch_tab:
	mov rdi, .fs_tab
	mov rsi, rbx
	jmp .printf
	
.ch_page:
	mov rdi, .fs_page
	mov rsi, rbx
	jmp .printf
	
.ch_return:
	mov rdi, .fs_return
	mov rsi, rbx
	jmp .printf

.ch_newline:
	mov rdi, .fs_newline
	mov rsi, rbx
	jmp .printf

.ch_doublequote:
	mov rdi, .fs_doublequote
	mov rsi, rbx
	jmp .printf

.ch_backslash:
	mov rdi, .fs_backslash
	mov rsi, rbx

.printf:
	push rax
	push rcx
	mov rax, 0
	call printf
	pop rcx
	pop rax

	dec rcx
	inc rax
	jmp .loop

.done:
	mov rax, 0
	mov rdi, .double_quote
	call printf

	pop rbp
	ret
section .data
.double_quote:
	db CHAR_DOUBLEQUOTE, 0
.fs_simple_char:
	db "%c", 0
.fs_hex_char:
	db "\x%02x;", 0	
.fs_tab:
	db "\t", 0
.fs_page:
	db "\f", 0
.fs_return:
	db "\r", 0
.fs_newline:
	db "\n", 0
.fs_doublequote:
	db CHAR_BACKSLASH, CHAR_DOUBLEQUOTE, 0
.fs_backslash:
	db CHAR_BACKSLASH, CHAR_BACKSLASH, 0

section .text
write_sob_pair:
	push rbp
	mov rbp, rsp

	push rsi
	
	mov rax, 0
	mov rdi, .open_paren
	call printf

	mov rsi, [rsp]

	CAR rsi, rsi
	call write_sob

	mov rsi, [rsp]
	CDR rsi, rsi
	call write_sob_pair_on_cdr
	
	add rsp, 1*8
	
	mov rdi, .close_paren
	mov rax, 0
	call printf

	pop rbp
	ret

section .data
.open_paren:
	db "(", 0
.close_paren:
	db ")", 0

section .text
write_sob_pair_on_cdr:
	push rbp
	mov rbp, rsp

	mov bl, byte [rsi]
	cmp bl, T_NIL
	je .done
	
	cmp bl, T_PAIR
	je .cdrIsPair
	
	push rsi
	
	mov rax, 0
	mov rdi, .dot
	call printf
	
	pop rsi

	call write_sob
	jmp .done

.cdrIsPair:
	CDR rbx, rsi
	push rbx
	CAR rsi, rsi
	push rsi
	
	mov rax, 0
	mov rdi, .space
	call printf
	
	pop rsi
	call write_sob

	pop rsi
	call write_sob_pair_on_cdr

.done:
	pop rbp
	ret

section .data
.space:
	db " ", 0
.dot:
	db " . ", 0

section .text
write_sob_symbol:
	push rbp
	mov rbp, rsp

	SYMBOL_VAL rsi, rsi
	
	STRING_LENGTH rcx, rsi
	STRING_ELEMENTS rax, rsi

	mov rdx, rcx

.loop:
	cmp rcx, 0
	je .done
	mov bl, byte [rax]
	and rbx, 0xff

	cmp rcx, rdx
	jne .ch_simple
	cmp rbx, '+'
	je .ch_hex
	cmp rbx, '-'
	je .ch_hex
	cmp rbx, 'A'
	jl .ch_hex

.ch_simple:
	mov rdi, .fs_simple_char
	mov rsi, rbx
	jmp .printf
	
.ch_hex:
	mov rdi, .fs_hex_char
	mov rsi, rbx

.printf:
	push rax
	push rcx
	mov rax, 0
	call printf
	pop rcx
	pop rax

	dec rcx
	inc rax
	jmp .loop

.done:
	pop rbp
	ret
	
section .data
.fs_simple_char:
	db "%c", 0
.fs_hex_char:
	db "\x%02x;", 0	

section .text
write_sob_closure:
	push rbp
	mov rbp, rsp

	CLOSURE_CODE rdx, rsi
	CLOSURE_ENV rsi, rsi

	mov rdi, .closure
	mov rax, 0
	call printf

	pop rbp
	ret
section .data
.closure:
	db "#<closure [env:%p, code:%p]>", 0

section .text
write_sob_vector:
    push rbp
    mov rbp, rsp

    push rsi

    mov rax, 0
    mov rdi, .vector_open_paren
    call printf

    mov rsi, [rsp]

    SKIP_TYPE_TAG rcx, rsi
    VECTOR_ELEMENTS rax, rsi

.loop:
    cmp rcx, 0
    je .done

    mov rsi, [rax]
    push rax
    push rcx
    call write_sob
    pop rcx
    pop rax

    dec rcx
    jz .done

    push rax
    push rcx
    mov rax, 0
    mov rdi, .vector_space
    call printf
    pop rcx
    pop rax

    add rax, WORD_SIZE
    jmp .loop

.done:
    mov rax, 0
    mov rdi, .vector_close_paren
    call printf

    pop rsi

    pop rbp
    ret

section .data
.vector_open_paren:
    db "#(", 0

.vector_space:
    db " ", 0

.vector_close_paren:
    db ")", 0

section .text
write_sob:
	mov rbx, 0
	mov bl, byte [rsi]	
	jmp qword [.jmp_table + rbx * 8]

section .data
.jmp_table:
	dq write_sob_undefined, write_sob_void, write_sob_nil
	dq write_sob_rational, write_sob_float, write_sob_bool
	dq write_sob_char, write_sob_string, write_sob_symbol
	dq write_sob_closure, write_sob_pair, write_sob_vector

section .text
write_sob_if_not_void:
	mov rsi, rax
	mov bl, byte [rsi]
	cmp bl, T_VOID
	je .continue

	call write_sob
	
	mov rax, 0
	mov rdi, .newline
	call printf
	
.continue:
	ret
section .data
.newline:
	db CHAR_NEWLINE, 0
