BITS 64
SECTION .text
GLOBAL sort
sort:
	push rbx
	push r12
	cmp rsi, 2
	jbe NON_RECURSIVE
	mov rbx, rdi
	mov r12, rsi
	sar rsi, 1
	call sort
	mov rsi, r12
	sar rsi, 1
	mov rdi, rsi
	sal rdi, 3
	add rdi, rbx
	neg rsi
	add rsi, r12
	call sort
NON_RECURSIVE:
	mov r11, r12
	sal r11, 3
	sub rsp, r11
	mov rax, rbx
	mov rcx, r12
	sar rcx, 1
	sal rcx, 3
	add rcx, rbx
	mov rdx, rsp
	mov r8, rcx
	lea r9, [rbx+8*r12]
MAIN_LOOP:
	cmp rax, r8
	jae END_MAIN_LOOP
	cmp rcx, r9
	jae END_MAIN_LOOP
	cmp rax, rcx
	ja MAIN_LOOP_COND
	mov r10, [rax]
	add rax, 8
	jmp MAIN_LOOP_COND_END
MAIN_LOOP_COND:
	mov r10, [rcx]
	add rcx, 8
MAIN_LOOP_COND_END:
	mov [rdx], r10
	add rdx, 8
	jmp MAIN_LOOP
END_MAIN_LOOP:
LOOP_1:
	cmp rax, r8
	jae END_LOOP_1
	mov r10, [rax]
	mov [rdx], r10
	add rax, 8
	jmp LOOP_1
END_LOOP_1:
LOOP_2:
	cmp rcx, r9
	jae END_LOOP_2
	mov r10, [rcx]
	mov [rdx], r10
	add rcx, 8
	jmp LOOP_2
END_LOOP_2:
	add rsp, r11
	pop r12
	pop rbx
	ret

