BITS 64
SECTION .text
GLOBAL sort
sort:
	test rdi, rdi
	jnz sort2
	mov rdi, rsi
	xor rsi, rsi
	xor rdx, rdx
	call 0
	ret
sort2:
	dec rdi
	push rsi
	push rdi
	call sort
	pop rdx
	pop rsi
	mov rdi, rax
	call 0
	ret

