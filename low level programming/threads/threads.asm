BITS 64

GLOBAL th_create, th_yield, th_exit, run, th_getid

%macro SAVESTATE 0
	push rbx
	push rbp
	push r12
	push r13
	push r14
	push r15
	push rdi
	push rsi
%endmacro

%macro LOADSTATE 0
	pop rsi
	pop rdi
	pop r15
	pop r14
	pop r13
	pop r12
	pop rbp
	pop rbx
%endmacro

SECTION .bss
	arr resb 67108864
	queue resb 257
	stack resq 257 ;; + sentinel
	rstack resq 1
	currexe resb 1

SECTION .data
	first: dq 0
	last: dq 0

SECTION .data
	curr: db -1

SECTION .text

th_create:
	mov r10, rdi
	mov r9, rsi
	mov r8, rdx
	mov rdx, stack
	xor eax, eax
	mov al, [curr]
	inc rax
	mov r11, rax
th_create_loop1:
	cmp rax, 256
	je th_create_loop1_out
	mov rcx, [rdx + 8*rax]
	test rcx, rcx
	jz th_create_finish
	inc rax
	jmp th_create_loop1
th_create_loop1_out:
	mov rax, 0
th_create_loop2:
	cmp rax, r11
	je th_create_faild
	mov rcx, [rdx + 8*rax]
	test rcx, rcx
	jz th_create_finish
	inc rax
	jmp th_create_loop2
th_create_finish:
	mov rsi, r8
	mov rdi, r9
	mov r9, queue
	mov rcx, [last]
	mov BYTE [r9 + rcx], al
	lea r9, [rdx + 8*rax]
	mov r11, 256
	dec rcx
	cmovs rcx, r11
	mov [last], rcx
	mov [curr], rax
	mov r8, rax
	inc rax
	mov rdx, 65536
	mul rdx
	add rax, arr
	xchg rsp, rax
	push r10
	SAVESTATE
	xchg rsp, rax
	mov [r9], rax
	mov rax, r8
	ret
th_create_faild:
	mov rax, -1
	ret
th_yield:
	SAVESTATE
	xor ecx, ecx
	mov cl, BYTE [currexe]
	mov rdx, stack
	mov [rdx + 8*rcx], rsp
	mov r8, 256
	mov rdx, queue
	mov rax, [last]
	mov BYTE [rdx + rax], cl
	dec rax
	cmovs rax, r8
	mov [last], rax
th_load:
	mov rcx, [first]
	cmp rcx, rax
	je th_load_end_finite
	mov rdx, queue
	xor eax, eax
	mov al, BYTE [rdx + rcx]
	mov rdx, stack
	mov BYTE [currexe], al
	mov rsp, [rdx + 8*rax]
	dec rcx
	cmovs rcx, r8
	mov [first], rcx
	LOADSTATE
	ret
th_load_end_finite:
	mov rsp, [rstack]
	LOADSTATE
	ret
run:
	SAVESTATE
	mov [rstack], rsp
	mov r8, 256
	mov rax, [last]
	jmp th_load
th_exit:
	mov rax, [currexe]
	mov rdx, stack
	xor ecx, ecx
	mov [rdx + 8*rax], rcx
	mov r8, 256
	mov rax, [last]
	jmp th_load
th_getid:
	mov rax, [currexe]
	ret

