BITS 64
SECTION .text
GLOBAL sort
sort:
	push rbp
	mov rbp, rdi
	push rbx
	mov rbx, rsi
	push r12
	mov r12, rdx
	call 0
	push rax
	mov rdi, rbp
	mov rsi, rbx
	mov rdx, r12
	call 0
	push rax
	mov rdi, rbp
	mov rsi, rbx
	mov rdx, r12
	call 0
	mov rdx, rax
	pop rsi
	pop rdi
	pop r12
	pop rbx
	pop rbp
	call 0
	ret

