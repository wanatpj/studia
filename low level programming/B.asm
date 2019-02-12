BITS 64

GLOBAL _start

%define BUF_SIZE 1024
%define STAT_SIZE 144
%define FILE_LEN 256			; real len + 1
%define DIR_LEN 4096			; real len + 1
%define PATH_LEN 4351			; FILE_LEN + DIR_LEN - 1
%define READ 0					; O_RDONLY
%define WRITE 1
%define OPEN 2
%define CLOSE 3
%define LSTAT 6
%define GETDENTS 78
%define STDOUT 1
%define READ_DIR 00200000o		; O_RDONLY | O_DIRECTORY
%define S_IFMT 00170000o
%define S_IFDIR 0040000o
%define S_IFREG 0100000o
;%define DIR_MASK 0x4000
;%define FILE_MASK 0x8000

SECTION .data
	no_such_fd_text: db "No such file or directory.", 10
	no_such_fd_len equ $ - no_such_fd_text
	no_such_dir_text: db "Not a directory.", 10
	no_such_dir_len equ $ - no_such_dir_text
	slash_char: db "/"
	hyphen_char: db "-"
	space_char: db " "
	d_char: db "d"
	q_char: db "?"
	p_char: db "rwxrwxrwx"
	enter_char: db 10
SECTION .bss
	privs: resb 9
	dirent: resb BUF_SIZE
	stat: resb STAT_SIZE
	path: resb PATH_LEN
	path_size: resd 1
SECTION .text

_start:
	jmp main
main:
	push r12
	push r13
	push r14
	push r15
	xor r14, r14
	xor r15, r15
	; r8 - main file descriptor
	mov rax, [rsp+32]
	cmp rax, 2
	jne no_such_fd
	; open: rax - file descriptor
	mov rax, OPEN
	mov rdi, [rsp+48]
	mov rsi, READ
	syscall
	cmp rax, -1
	jle no_such_fd
	; close: file
	mov rdi, rax
	mov rax, CLOSE
	syscall
	; open: as directory
	mov rax, OPEN
	mov rdi, [rsp+48]
	mov rsi, READ_DIR
	syscall
	cmp rax, -1
	jle no_such_dir
	mov r8, rax
	mov rdi, [rsp+48]
	call strlen
	mov r9, rax
	mov rdi, path
	mov rsi, [rsp+48]
	call strcpy
;	xor ecx, ecx
	mov cl, BYTE [slash_char]
	dec r9
	cmp cl, BYTE [path + r9]
	je main_loop_before
	inc r9
	mov BYTE [path + r9], cl
main_loop_before:
	inc r9
main_loop:
	call get_next_dent
	cmp rax, 0
	jle main_loop_end
	call print_data
	jmp main_loop
main_loop_end:
	; close: directory
	mov rax, CLOSE
	mov rdi, r8
	syscall
	
	jmp end_end
no_such_dir:
	mov rax, 1
	mov rdi, 1
	mov rsi, no_such_dir_text
	mov rdx, no_such_dir_len
	syscall
	jmp end_end
no_such_fd:
	mov rax, 1
	mov rdi, 1
	mov rsi, no_such_fd_text
	mov rdx, no_such_fd_len
	syscall
	jmp end_end
end_end:
	pop r15
	pop r14
	pop r13
	pop r12
	xor rdi, rdi
	mov rax, 60
	syscall

get_next_dent:
	mov rax, GETDENTS
	mov rdi, r8
	mov rsi, dirent
	mov rdx, BUF_SIZE
	syscall
	mov r14, dirent
	mov r15, dirent
	add r15, rax
	ret
print_data:
	cmp r14, r15
	je print_data_end
	lea rdi, [path + r9]
	lea rsi, [r14 + 18]
	call strcpy
	mov rax, LSTAT
	mov rdi, path
	mov rsi, stat
	syscall

	mov r12, [stat + 24]
	mov rsi, q_char
	mov rcx, hyphen_char
	mov r11, r12
	and r11, S_IFMT
	cmp r11, S_IFREG
	cmove rsi, rcx
	mov rcx, d_char
	cmp r11, S_IFDIR
	cmove rsi, rcx
	mov rax, WRITE
	mov rdi, STDOUT
	mov rdx, 1
	syscall
	
	mov r13, 0x100
	mov r10, 0
access_loop:
	test r13, r12
	mov rax, WRITE
	mov rdi, STDOUT
	mov rsi, hyphen_char
	mov rdx, 1
	lea rcx, [p_char + r10]
	cmovnz rsi, rcx
	syscall
	inc r10
	shr r13, 1
	test r13, r13
	jnz access_loop

	mov rax, WRITE
	mov rdi, STDOUT
	mov rsi, space_char
	mov rdx, 1
	syscall
	lea rdi, [r14 + 18]
	call strlen
	mov rdx, rax
	mov rax, WRITE
	mov rdi, STDOUT
	lea rsi, [r14 + 18]
;	mov	rsi, path			; debug
	syscall
	mov rax, WRITE
	mov rdi, STDOUT
	mov rsi, enter_char
	mov rdx, 1
	syscall
	xor ecx, ecx
	mov ax, WORD [r14 + 16]
;	mov r11, rax				; debug
	add r14, rax
	jmp print_data
print_data_end:
	ret
strcpy:					; destroys: rdi, rsi, rcx ; !! RETURNS: void
	mov cl, BYTE [rsi]
	mov BYTE [rdi], cl
	test cl, cl
	jz strcpy_end
	inc rdi
	inc rsi
	jmp strcpy
strcpy_end:
	ret
strlen:					; destorys: rcx;
	mov rax, rdi
strlen_loop:
	mov cl, BYTE [rax]
	cmp cl, 0
	je strlen_loop_end
	inc rax
	jmp strlen_loop
strlen_loop_end:
	sub rax, rdi
	ret

