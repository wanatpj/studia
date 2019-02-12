BITS 64

GLOBAL v2

SECTION .text

v2:
	mov rax, -1
	bsf rax, rdi
	ret
