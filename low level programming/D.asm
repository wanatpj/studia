BITS 64

GLOBAL _ZN7NaturalC1Ev, _ZN7NaturalC1Em, _ZN7NaturalC1EPKc, _ZN7NaturalC1ERKS_, _ZN7NaturalD1Ev, _ZN7NaturalaSERKS_, _ZN7NaturalpLERKS_, _ZN7NaturalppEv, _ZN7NaturalmLERKS_, _ZNK7NaturaleqERKS_, _ZNK7NaturalltERKS_, _ZNK7Natural4SizeEv, _ZNK7Natural5PrintEv
EXTERN malloc, free, realloc, strlen, printf, calloc

SECTION .data
	luf: db "%08x", 0
	lu: db "%x", 0


SECTION .text
_ZN7NaturalC2Ev:
	call _ZN7NaturalC1Ev
	ret

; offsets:
;   bits 0
;   cont 8

_ZN7NaturalC1Ev:
	push rbx
	mov rbx, rdi
	mov edi, 8
	call malloc
	mov QWORD [rax], 0
	mov [rbx+8], rax
	mov QWORD [rbx], 1
	pop rbx
	ret

_ZN7NaturalC2Em:
	call _ZN7NaturalC1Em
	ret

_ZN7NaturalC1Em:
	push rbp
	mov rbp, rsi
	push rbx
	mov rbx, rdi
	mov edi, 8
	call malloc
	mov [rax], rbp
	mov [rbx+8], rax
	mov QWORD [rbx], 1
	pop rbx
	pop rbp
	ret

_ZN7NaturalC2EPKc:
	call _ZN7NaturalC1EPKc
	ret

_ZN7NaturalC1EPKc:
	mov rcx, 0x30
	cmp BYTE [rsi], cl
	lea rcx, [rsi + 1]
	cmove rsi, rcx
	je _ZN7NaturalC1EPKc
	mov rcx, 0
	cmp BYTE [rsi], cl
	lea rcx, [rsi - 1]
	cmove rsi, rcx
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
	jnz _ZN7NaturalC1EPKc_cond
_ZN7NaturalC1EPKc_cond_back:
_ZN7NaturalC1EPKc_loop:
	mov al, BYTE [rsi]
	test al, al
	jz _ZN7NaturalC1EPKc_loop_end
	call _ZN7NaturalC1EPKc_getchar
	mov dl, al
	shl dl, 4
	lea rsi, [rsi + 1]
	call _ZN7NaturalC1EPKc_getchar
	or dl, al
	mov BYTE [rdi], dl
	lea rdi, [rdi - 1]
	lea rsi, [rsi + 1]
	jmp _ZN7NaturalC1EPKc_loop
_ZN7NaturalC1EPKc_loop_end:
	pop r12
	pop rbx
	pop rbp
	ret
_ZN7NaturalC1EPKc_getchar:
	xor eax, eax
	xor ecx, ecx
	mov al, BYTE [rsi]
	mov cl, al
	sub cl, 0x30
	sub al, 0x61
	lea rax, [rax + 10]
	cmovs ax, cx
	ret
_ZN7NaturalC1EPKc_cond:
	call _ZN7NaturalC1EPKc_getchar
	mov BYTE [rdi], al
	lea rdi, [rdi - 1]
	lea rsi, [rsi + 1]
	jmp _ZN7NaturalC1EPKc_cond_back

_ZN7NaturalC2ERKS_:
	call _ZN7NaturalC1ERKS_
	ret

_ZN7NaturalC1ERKS_:
	push rbp
	mov rbp, rsi
	push rbx
	mov rbx, rdi
	push r12
	mov r12, [rsi]
	mov [rdi], r12
	mov rdi, r12
	shl rdi, 3
	call malloc
	mov [rbx + 8], rax
	mov rdi, rax
	mov rsi, [rbp + 8]
	mov rcx, r12
	rep movsq
	pop r12
	pop rbx
	pop rbp
	ret

_ZN7NaturalD2Ev:
	call _ZN7NaturalD1Ev
	ret

_ZN7NaturalD1Ev:
	mov rdi, [rdi + 8]
	call free
	ret

_ZN7NaturalaSERKS_:
	mov rax, rdi
	cmp rdi, rsi
	je _ZN7NaturalaSERKS_end
	push rbp
	mov rbp, rsi
	push rbx
	mov rbx, rdi
	push r12
	mov r12, [rsi]
	mov [rdi], r12
	mov rdi, [rdi + 8]
	call free
	mov rdi, r12
	shl rdi, 3
	call malloc
	mov [rbx + 8], rax
	mov rdi, rax
	mov rsi, [rbp + 8]
	mov rcx, [rbp]
	rep movsq
	mov rax, rbx
	pop r12
	pop rbx
	pop rbp
_ZN7NaturalaSERKS_end:
	ret

_ZN7NaturalpLERKS_:
	push rbp
	mov rbp, rsi
	push rbx
	mov rbx, rdi
	push r12
	mov r12, [rdi]
	push r13
	mov r13, [rsi]
	mov rax, r12
	cmp rax, r13
	cmovl rax, r13
	mov rdi, [rbx + 8]
	mov rsi, rax
	inc rsi
	shl rsi, 3
	mov [rbx], rax
	call realloc
	mov [rbx + 8], rax
	mov rcx, r13
	sub rcx, r12
	mov rdx, 0
	cmovs rcx, rdx
	mov rdi, rax; [rbx + 8]
	lea rdi, [rdi + 8*r12]
	mov rsi, [rbp + 8]
	lea rsi, [rsi + 8*r12]
	cld
	rep movsq
	mov rdi, [rbx + 8]
	mov rsi, [rbp + 8]
	mov rcx, r12
	cmp rcx, r13
	cmovg rcx, r13
	clc
_ZN7NaturalpLERKS_loop:
	mov rax, [rdi]
	adc rax, [rsi]
	mov [rdi], rax
	lea rdi, [rdi + 8]
	lea rsi, [rsi + 8]
	loop _ZN7NaturalpLERKS_loop
_ZN7NaturalpLERKS_loop2:
	jnc _ZN7NaturalpLERKS_loop2_end
	mov eax, 0
	adc rax, [rdi]
	mov [rdi], rax
	lea rdi, [rdi + 8]
	jmp _ZN7NaturalpLERKS_loop2
_ZN7NaturalpLERKS_loop2_end:
	mov rax, [rbx]
	mov rcx, [rbx + 8]
	lea rax, [rcx + 8*rax]
	cmp rax, rdi
	mov rax, 0
	mov rcx, 1
	cmovl rax, rcx
	;mov rax, 2 ; debug
	add [rbx], rax
	mov rax, rbx
	pop r13
	pop r12
	pop rbx
	pop rbp
	ret

_ZN7NaturalppEv:
	mov rcx, [rdi]
	mov rsi, [rdi + 8]
	stc
_ZN7NaturalppEv_loop:
	adc QWORD [rsi], 0
	lea rsi, [rsi + 8]
	jnc _ZN7NaturalppEv_loop_end
	loop _ZN7NaturalppEv_loop
	push rbx
	mov rbx, rdi
	mov rdi, [rbx]
	inc rdi
	mov [rbx], rdi
	shl rdi, 3
	call malloc
	mov rdi, [rbx + 8]
	mov [rbx + 8], rax
	call free
	mov rdi, [rbx]
	dec rdi
	shl rdi, 3
	add rdi, [rbx + 8]
	mov QWORD [rdi], 1
_ZN7NaturalppEv_loop_last:
	cmp rdi, [rbx + 8]
	je _ZN7NaturalppEv_loop_last_end
	lea rdi, [rdi - 8]
	mov QWORD [rdi], 0
	jmp _ZN7NaturalppEv_loop_last
_ZN7NaturalppEv_loop_last_end:
	mov rdi, rbx
	pop rbx
_ZN7NaturalppEv_loop_end:
	mov rax, rdi
	ret

_ZN7NaturalmLERKS_:
	jmp _ZN7NaturalmLERKS_zero_test
	_ZN7NaturalmLERKS_zero_test_back:
	push rbp
	mov rbp, rsi
	push rbx
	mov rbx, rdi
	mov rdi, [rbp]
	add rdi, [rbx]
	mov rsi, 8
	call calloc
	mov rdi, rax
	mov r11, 0
_ZN7NaturalmLERKS_loop1:
	mov rcx, 0
	mov r10, 0
_ZN7NaturalmLERKS_loop2:
	lea r8, [rcx + r11]
	add [rdi + 8*r8], r10
	mov r10, 0
	adc r10, 0
	mov rax, [rbp + 8]
	mov rax, [rax + 8*rcx]
	mov r9, rax
	mov rax, [rbx + 8]
	mov rax, [rax + 8*r11]
	mul r9
	add r10, rdx
	add [rdi + 8*r8], rax
	adc r10, 0
	inc rcx
	cmp rcx, [rbp]
	jne _ZN7NaturalmLERKS_loop2
_ZN7NaturalmLERKS_loop2a:
	cmp r10, 0
	je _ZN7NaturalmLERKS_loop2a_end
	lea r8, [rcx + r11]
	add [rdi + 8*r8], r10
	mov r10, 0
	adc r10, 0
	inc rcx
	jmp _ZN7NaturalmLERKS_loop2a
_ZN7NaturalmLERKS_loop2a_end:
	inc r11
	cmp r11, [rbx]
	jne _ZN7NaturalmLERKS_loop1
	mov rsi, [rbx + 8]
	mov [rbx + 8], rdi
_ZN7NaturalmLERKS_lastloop:
	mov rcx, [rdi + 8*r8]
	test rcx, rcx
	jnz _ZN7NaturalmLERKS_lastloop_end
	test r8, r8
	jz _ZN7NaturalmLERKS_lastloop_end
	dec r8
	jmp _ZN7NaturalmLERKS_lastloop
_ZN7NaturalmLERKS_lastloop_end:
	inc r8
	mov [rbx], r8
	mov rdi, rsi
	call free
	mov rax, rbx
	pop rbx
	pop rbp
	ret
_ZN7NaturalmLERKS_zero_test:
	push rbp
	mov rbp, rsi
	push rbx
	mov rbx, rdi
	mov rax, [rdi]
	cmp rax, 1
	jne _ZN7NaturalmLERKS_zero_test2
	mov rax, [rdi + 8]
	mov rax, [rax]
	test rax, rax
	jnz _ZN7NaturalmLERKS_zero_test2
	call _ZN7NaturalD1Ev
	mov rdi, rbx
	call _ZN7NaturalC1Ev
	jmp _ZN7NaturalmLERKS_zero_test_ret
_ZN7NaturalmLERKS_zero_test2:
	mov rax, [rsi]
	cmp rax, 1
	jne _ZN7NaturalmLERKS_zero_test_end
	mov rax, [rsi + 8]
	mov rax, [rax]
	test rax, rax
	jnz _ZN7NaturalmLERKS_zero_test_end
	call _ZN7NaturalD1Ev
	mov rdi, rbx
	call _ZN7NaturalC1Ev
_ZN7NaturalmLERKS_zero_test_ret:
	mov rsi, rbp
	mov rdi, rbx
	pop rbx
	pop rbp
	ret
_ZN7NaturalmLERKS_zero_test_end:
	mov rsi, rbp
	mov rdi, rbx
	pop rbx
	pop rbp
	jmp _ZN7NaturalmLERKS_zero_test_back

_ZNK7NaturaleqERKS_:
	mov rcx, [rdi]
	cmp rcx, [rsi]
	jne _ZNK7NaturaleqERKS_end
	mov rdi, [rdi+8]
	mov rsi, [rsi+8]
	dec rcx
	shl rcx, 3
	add rdi, rcx
	add rsi, rcx
	shr rcx, 3
	inc rcx ; ?
	std
	repe cmpsq
	cld
	jmp _ZNK7NaturaleqERKS_end
_ZNK7NaturaleqERKS_end:
	sete al
	ret

_ZNK7NaturalltERKS_:
	mov rcx, [rdi]
	cmp rcx, [rsi]
	jne _ZNK7NaturalltERKS_ne
	mov rdi, [rdi + 8]
	mov rsi, [rsi + 8]
	dec rcx
	shl rcx, 3
	add rdi, rcx
	add rsi, rcx
	xchg rsi, rdi
	shr rcx, 3
	inc rcx
	std
	repe cmpsq
	cld
_ZNK7NaturalltERKS_ne:
	setl al
	ret

_ZNK7Natural4SizeEv:
	mov rax, [rdi]
	dec rax
	sal rax, 3
	mov rdx, rax
	add rdx, [rdi + 8]
	mov rdx, [rdx]
	mov rcx, 0xff
	mov r8, 0
	mov r9, 0
size_loop:
	test rdx, rcx
	cmovnz r8, r9
	inc r9
	shl rcx, 8
	test rcx, rcx
	jnz size_loop
	add rax, r8
	mov rdx, [rdi + 8]
	mov dl, BYTE [rdx + rax]
	shl rax, 3
	mov rcx, 1
	mov r8, 0
	mov r9, 0
size_loop2:
	inc r9
	test rdx, rcx
	cmovnz r8, r9
	shl rcx, 1
	test rcx, 0xff
	jnz size_loop2
	add rax, r8
	ret

_ZNK7Natural5PrintEv:
	push rbp
	mov rbp, [rdi]
	push rbx
	mov rbx, [rdi + 8]
	dec rbp
	mov rdi, [rbx + 8*rbp]
	call print_top
_ZNK7Natural5PrintEv_loop:
	dec rbp
	js _ZNK7Natural5PrintEv_loop_end
	mov rdi, [rbx + 8*rbp]
	call print_full
	jmp _ZNK7Natural5PrintEv_loop
_ZNK7Natural5PrintEv_loop_end:
	pop rbx
	pop rbp
	ret

print_full:
	push rbx
	mov rbx, rdi
	xor eax, eax
	mov rdi, luf
	mov rsi, rbx
	shr rsi, 32
	call printf
	xor eax, eax
	mov rdi, luf
	mov esi, ebx
	call printf
	pop rbx
	ret

print_top:
	push rbx
	mov rbx, rdi
	mov rdx, 0xffffffff00000000
	test rbx, rdx
	jnz print_top1
print_top1_back:
	xor eax, eax
	mov rdi, lu
	mov esi, ebx
	mov rdx, 0xffffffff00000000
	test rbx, rdx
	mov rdx, luf
	cmovnz rdi, rdx
	call printf
	pop rbx
	ret

print_top1:
	xor eax, eax
	mov rdi, lu
	mov rsi, rbx
	shr rsi, 32
	call printf
	jmp print_top1_back

