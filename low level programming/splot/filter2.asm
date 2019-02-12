BITS 64

%macro CMPTBEG 4
	movd %4, %2
	punpcklbw %4, xmm5
	punpcklwd %4, xmm5
	shr %1, 32
	cvtdq2ps %4, %4
	mulps %4, %3
%endmacro

%macro CMPT 3
	CMPTBEG %1, %2, xmm2, xmm0
	mov al, r11b
	CMPTBEG %1, %2, xmm3, xmm1
	addps xmm0, xmm1
	haddps xmm0, xmm5
	haddps xmm0, xmm5
	cvtsi2ss xmm1, eax
	mulss xmm1, xmm4
	addss xmm0, xmm1
	shr r11, 8
	maxss xmm0, xmm5
	minss xmm0, xmm6
	cvtss2si eax, xmm0
	mov [rdi + %3], al
%endmacro

%macro COMPUTE 0
	xor eax, eax
	; B, G, R
	CMPT r10, r10d, 2
	CMPT r9, r9d, 1
	CMPT r8, r8d, 0
	lea rsi, [rsi + 3]
	lea rdi, [rdi + 3]
%endmacro

%macro LOADRG 0
	shl r8, 8
	mov r8b, al
	shr rax, 8
	shl r9, 8
	mov r9b, al
	shr rax, 8
%endmacro

%macro LOADB 0
	shl r10, 8
	mov r10b, al
	shr rax, 8
%endmacro

%macro LODRGB 0
	LOADRG
	LOADB
%endmacro

%macro LOADRGB 0
	LODRGB
	LODRGB
	LOADRG
%endmacro

%macro LOADB2 0
	shl r11, 8
	mov r11b, al
	shr rax, 8
%endmacro

%macro LOADRGB2 0
	LODRGB
	LODRGB
	LOADB2
	LOADB2
%endmacro


%macro PREPARE00 0
	xor r11, r11
	xor eax, eax
	LOADRGB
	LOADB
	mov rax, QWORD [rsi+0+r14]
	shl rax, 24
	LOADRGB
	mov al,  BYTE  [rsi+5+r14]
	LOADB
	mov rax, QWORD [rsi+0+2*r14]
	shl rax, 24
	LOADRGB2
	mov al,  BYTE  [rsi+5+2*r14]
	LOADB2
%endmacro

%macro PREPARE01 0
	xor r11, r11
	xor eax, eax
	LOADRGB
	LOADB
	mov rax, QWORD [rsi-3+r14]
	LOADRGB
	mov al,  BYTE  [rsi+5+r14]
	LOADB
	mov rax, QWORD [rsi-3+2*r14]
	LOADRGB2
	mov al,  BYTE  [rsi+5+2*r14]
	LOADB2
%endmacro

%macro PREPARE02 0
	xor r11, r11
	xor eax, eax
	LOADRGB
	LOADB
	mov rax, QWORD [rsi-5+r14]
	shr rax, 16
	LOADRGB
	LOADB
	mov rax, QWORD [rsi-5+2*r14]
	shr rax, 16
	LOADRGB2
	LOADB2
%endmacro

%macro PREPARE10 0
	xor r11, r11
	mov rax, QWORD [rsi+0]
	shl rax, 24
	LOADRGB
	mov al,  BYTE  [rsi+5]
	LOADB
	mov rax, QWORD [rsi+0+r14]
	shl rax, 24
	LOADRGB
	mov al,  BYTE  [rsi+5+r14]
	LOADB
	mov rax, QWORD [rsi+0+2*r14]
	shl rax, 24
	LOADRGB2
	mov al,  BYTE  [rsi+5+2*r14]
	LOADB2
%endmacro

%macro PREPARE11 0
	xor r11, r11
	mov rax, QWORD [rsi-3]
	LOADRGB
	mov al,  BYTE  [rsi+5]
	LOADB
	mov rax, QWORD [rsi-3+r14]
	LOADRGB
	mov al,  BYTE  [rsi+5+r14]
	LOADB
	mov rax, QWORD [rsi-3+2*r14]
	LOADRGB2
	mov al,  BYTE  [rsi+5+2*r14]
	LOADB2
%endmacro

%macro PREPARE12 0
	xor r11, r11
	mov rax, QWORD [rsi-5]
	shr rax, 16
	LOADRGB
	LOADB
	mov rax, QWORD [rsi-5+r14]
	shr rax, 16
	LOADRGB
	LOADB
	mov rax, QWORD [rsi-5+2*r14]
	shr rax, 16
	LOADRGB2
	LOADB2
%endmacro


%macro PREPARE20 0
	xor r11, r11
	mov rax, QWORD [rsi+0]
	shl rax, 24
	LOADRGB
	mov al,  BYTE  [rsi+5]
	LOADB
	mov rax, QWORD [rsi+0+r14]
	shl rax, 24
	LOADRGB
	mov al,  BYTE  [rsi+5+r14]
	LOADB
	LOADRGB2
	LOADB2
%endmacro

%macro PREPARE21 0
	xor r11, r11
	mov rax, QWORD [rsi-3]
	LOADRGB
	mov al,  BYTE  [rsi+5]
	LOADB
	mov rax, QWORD [rsi-3+r14]
	LOADRGB
	mov al,  BYTE  [rsi+5+r14]
	LOADB
	LOADRGB2
	LOADB2
%endmacro

%macro PREPARE22 0
	xor r11, r11
	mov rax, QWORD [rsi-5]
	shr rax, 16
	LOADRGB
	LOADB
	mov rax, QWORD [rsi-5+r14]
	shr rax, 16
	LOADRGB
	LOADB
	LOADRGB2
	LOADB2
%endmacro


GLOBAL filter:function

SECTION .text
; filter info header;
; rcx  - iterator
; rdx  - iterator
; rsi  - ([rsi] - 3*width)
; rdi  - [rdi]
; r8   - local:: RED - reveresed
; r9   - local:: GREEN - reveresed
; r10  - local:: BLUE - reveresed
; r11  - 9th BGR
; r12  - width
; r13  - heidht
; r14  - 3*width
; xmm2 - matrix[7..4]
; xmm3 - matrix[3..0]
; xmm4 - matrix[8..8]
; xmm5 - zero
; xmm6 - max boundary
filter:
	push r12
	mov r12d, DWORD [rsi + 8]
	push r13
	mov r13d, DWORD [rsi + 12]
	push r14
	lea r14, [r12 + 2*r12]
	sub rsp, 32
	
	mov eax, [rdx + 28]
	mov [rsp + 0], eax
	mov eax, [rdx + 24]
	mov [rsp + 4], eax
	mov eax, [rdx + 20]
	mov [rsp + 8], eax
	mov eax, [rdx + 16]
	mov [rsp + 12], eax
	mov eax, [rdx + 12]
	mov [rsp + 16], eax
	mov eax, [rdx + 8]
	mov [rsp + 20], eax
	mov eax, [rdx + 4]
	mov [rsp + 24], eax
	mov eax, [rdx + 0]
	mov [rsp + 28], eax
	
	mov eax, 0
	cvtsi2ss xmm5, eax
	mov eax, 255
	cvtsi2ss xmm6, eax
	
	; load matrix
	movups xmm2, [rsp]
	movups xmm3, [rsp + 16]
	movss xmm4, [rdx + 32]
	;movsldup xmm4
	;movlhps xmm4, xmm4
	
	; prepare source & destination
	mov rsi, [rsi]
	mov rdi, [rdi]
	sub rsi, r14
	
	; main loop
	;	H ROW
	PREPARE00
	COMPUTE
	lea rcx, [r12 - 2]
filter_intenal_loop1:
	dec rcx
	PREPARE01
	COMPUTE
	test rcx, rcx
	jnz filter_intenal_loop1
	PREPARE02
	COMPUTE
	;	C ROWS
	lea rdx, [r13 - 2]
filter_main_loop:
	dec rdx
	PREPARE10
	COMPUTE
	lea rcx, [r12 - 2]
filter_intenal_loop2:
	dec rcx
	PREPARE11
	COMPUTE
	test rcx, rcx
	jnz filter_intenal_loop2
	PREPARE12
	COMPUTE
	test rdx, rdx
	jnz filter_main_loop
	;	L ROW
	PREPARE20
	COMPUTE
	lea rcx, [r12 - 2]
filter_intenal_loop3:
	dec rcx
	PREPARE21
	COMPUTE
	test rcx, rcx
	jnz filter_intenal_loop3
	PREPARE22
	COMPUTE
	; main loop END
	add rsp, 32
	pop r14
	pop r13
	pop r12
	ret

