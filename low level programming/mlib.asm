BITS 64

%define NULL 0

GLOBAL sstatus:data 4, count:function, top_length:function, pop:function, push:function
EXTERN malloc, free
EXTERN _GLOBAL_OFFSET_TABLE_

SECTION .data
	num: dd 0,
	ptr: dq NULL
	debug_text: db "debug", 10
	debug2_text: db "debug2", 10
	sstatus: dd 0
	
SECTION .bss
	;sstatus resd 1

SECTION .text
count:
	mov rdi, 0
	call set
	mov eax, DWORD [rel num]
	ret
top_length:
	mov ecx, DWORD [rel num]
	cmp ecx, 0
	je top_length_error
	mov rdi, 0
	call set
	mov rax, [rel ptr]
	add rax, 8
	mov rax, [rax]
top_length_ret:
	ret
top_length_error:
	mov rdi, 1
	call set
	mov rax, -1
	jmp top_length_ret
pop:
	mov ecx, DWORD [rel num]
	cmp ecx, 0
	je pop_error
	dec ecx
	mov DWORD [rel num], ecx
	mov rax, [rel ptr]
	mov rcx, [rax]
	mov [rel ptr], rcx
	mov rcx, [rax+8]
	lea rsi, [rax+16]
	cld
	rep movsb
	mov rdi, rax
	call free wrt ..plt
	mov rax, 0
	mov rdi, 0
	call set
pop_ret:
	ret
pop_error:
	mov rdi, 1
	call set
	mov rax, -1
	jmp pop_ret
push:
	cmp rdi, 0
	jl push_error_len
	push r12
	push r13
	mov r12, rdi
	mov r13, rsi
	add rdi, 16
	call malloc wrt ..plt
	cmp rax, NULL
	je push_error_alloc
	mov ecx, DWORD [rel num]
	inc ecx
	mov DWORD [rel num], ecx
	mov rcx, [rel ptr]
	mov [rax], rcx
	mov [rel ptr], rax
	mov [rax+8], r12
	lea rdi, [rax+16]
	mov rsi, r13
	mov rcx, r12
	;call print_debug
	cld
	rep movsb
	;call print_debug
	mov rax, 0
	mov rdi, 0
	call set
push_end:
	pop r13
	pop r12
push_ret:
	ret
push_error_len:
	mov rdi, 2
	call set
	mov rax, -1
	jmp push_ret
push_error_alloc:
	mov rdi, 3
	call set
	mov rax, -1
	jmp push_end
set:
	push rbx
	push rax
	mov rbx, _GLOBAL_OFFSET_TABLE_
	mov rax, [rbx + sstatus wrt ..got]
	mov DWORD [rax], edi
	pop rax
	pop rbx
	ret
print_debug:
	push rax
	push rdi
	push rsi
	push rdx
	mov rax, 1
	mov rdi, 1
	mov rsi, debug_text
	mov rdx, 6
	syscall
	pop rdx
	pop rsi
	pop rdi
	pop rax
	ret
print_debug2:
	push rax
	push rdi
	push rsi
	push rdx
	mov rax, 1
	mov rdi, 1
	mov rsi, debug2_text
	mov rdx, 7
	syscall
	pop rdx
	pop rsi
	pop rdi
	pop rax
	ret

