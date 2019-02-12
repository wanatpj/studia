BITS 64

GLOBAL _start

SECTION .text
_start:
	lea rax, [rax +4 -5]
	
	mov eax, 60
	syscall
