BITS 64

GLOBAL ctor
EXTERN strlen, calloc

SECTION .text

ctor:
	mov rcx, 0x30
	cmp BYTE [rsi], cl
	lea rcx, [rsi + 1]
	cmove rsi, rcx
	je ctor
	push rbp
	mov rbp, rsi
	push rbx
	mov rbx, rdi
	push r12
	mov rdi, rsi
	call strlen
	mov r12, rax
	add rax, 15
	shr rax, 4
	mov rsi, 8
	mov rdi, rax
	mov [rbx], rax
	call calloc
	mov [rbx + 8], rax
	mov rdi, r12
	add rdi, 1
	shr rdi, 1
	add rdi, rax
	dec rdi
	test r12, 0x1
	mov rsi, rbp
	jnz ctor_cond
ctor_cond_back:
ctor_loop:
	mov al, BYTE [rsi]
	test al, al
	jz ctor_loop_end
	call ctor_getchar
	mov dl, al
	shl dl, 4
	lea rsi, [rsi + 1]
	call ctor_getchar
	or dl, al
	mov BYTE [rdi], dl
	lea rdi, [rdi - 1]
	lea rsi, [rsi + 1]
	jmp ctor_loop
ctor_loop_end:
	pop r12
	pop rbx
	pop rbp
	ret
ctor_getchar:
	xor eax, eax
	xor ecx, ecx
	mov al, BYTE [rsi]
	mov cl, al
	sub cl, 0x30
	sub al, 0x61
	lea rax, [rax + 10]
	cmovs ax, cx
	ret
ctor_cond:
	call ctor_getchar
	mov BYTE [rdi], al
	lea rdi, [rdi - 1]
	lea rsi, [rsi + 1]
	jmp ctor_cond_back
