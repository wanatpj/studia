BITS 64

%macro PDCOMPUTE 2
	mulps %1, %2
	addps xmm0, %1
%endmacro

%macro PCOMPUTE 3
	movaps %3, %1
	mulps %3, %2
	addps xmm0, %3
%endmacro

%macro PRECOMPUTE 0
	movaps xmm0, xmm2
	mulps xmm0, xmm11
	PDCOMPUTE xmm13, xmm5
	PDCOMPUTE xmm15, xmm8
	PCOMPUTE xmm12, xmm3, xmm11
	PCOMPUTE xmm14, xmm6, xmm11
	PCOMPUTE xmm1,  xmm9, xmm11
%endmacro

%macro COMPUTE 0
	PCOMPUTE xmm12, xmm4, xmm11
	PCOMPUTE xmm14, xmm7, xmm11
	PCOMPUTE xmm1, xmm10, xmm11
	cvtps2dq xmm0, xmm0
	pxor xmm11, xmm11
	packuswb xmm0, xmm11
	packuswb xmm0, xmm11
	movd eax, xmm0
	mov WORD [rdi], ax
	shr rax, 16
	mov BYTE [rdi + 2], al
	pxor xmm0, xmm0
	movd xmm11, r8d
	punpcklbw xmm11, xmm0
	punpcklwd xmm11, xmm0
	cvtdq2ps xmm11, xmm11
	lea rdi, [rdi + 3]
%endmacro

%macro PREPAREREAD 2
	mov r9b, BYTE [rsi + 2 + %2]
	shl r9, 8
	mov r9b, BYTE [rsi + 1 + %2]
	shl r9, 8
	mov r9b, BYTE [rsi + 0 + %2]
	movd %1, r9d
	punpcklbw %1, xmm11
	punpcklwd %1, xmm11
	cvtdq2ps %1, %1
%endmacro

%macro READCOLOR0 0
	PREPAREREAD xmm1 , 2*r14
	PREPAREREAD xmm14, r14
	xorps xmm12, xmm12
	xor r9, r9
	lea rsi, [rsi + 3]
%endmacro

%macro READCOLOR1 0
	PREPAREREAD xmm1 , 2*r14
	PREPAREREAD xmm14, r14
	PREPAREREAD xmm12, 0
	lea rsi, [rsi + 3]
%endmacro

%macro READCOLOR2 0
	xorps xmm1, xmm1
	PREPAREREAD xmm14, r14
	PREPAREREAD xmm12, 0
	lea rsi, [rsi + 3]
%endmacro

%macro XMMCOPY 0
	mov r8, r9
	xorps xmm11, xmm11
	movaps xmm13, xmm14
	movaps xmm15, xmm1
%endmacro

%macro XMMXOR 0
	xorps xmm11, xmm11
	xorps xmm13, xmm13
	xorps xmm15, xmm15
%endmacro

%macro PREPREPARE0 0
	XMMXOR
	READCOLOR0
%endmacro

%macro PREPREPARE1 0
	XMMXOR
	READCOLOR1
%endmacro

%macro PREPREPARE2 0
	XMMXOR
	READCOLOR2
%endmacro

%macro PREPARE00 0
	XMMCOPY
	READCOLOR0
%endmacro

%macro PREPARE10 0
	XMMCOPY
	READCOLOR1
%endmacro

%macro PREPARE20 0
	XMMCOPY
	READCOLOR2
%endmacro

%macro PREPAREX1 0
	XMMCOPY
	xorps xmm12, xmm12
	xorps xmm14, xmm14
	xorps xmm1, xmm1
%endmacro

%macro UNPACKMULMATRIX 0
	unpcklps xmm2, xmm2
	unpcklps xmm3, xmm3
	unpcklps xmm4, xmm4
	unpcklps xmm5, xmm5
	unpcklps xmm6, xmm6
	unpcklps xmm7, xmm7
	unpcklps xmm8, xmm8
	unpcklps xmm9, xmm9
	unpcklps xmm10, xmm10
%endmacro


GLOBAL filter:function

SECTION .text
; filter info header;
; rcx  - iterator
; rdx  - iterator
; rsi  - ([rsi] - 3*width)
; rdi  - [rdi]
; r8   - saved value for xmm11
; r9   - right neighbor of r8
; r10  - 
; r11  - 
; r12  - width
; r13  - heidht
; r14  - 3*width
; xmm1 - see place for "xmm16"
; xmm2 - matrix[0..0]*4
; xmm3 - matrix[1..1]*4
; xmm4 - matrix[2..2]*4
; xmm5 - matrix[3..3]*4
; xmm6 - matrix[4..4]*4
; xmm7 - matrix[5..5]*4
; xmm8 - matrix[6..6]*4
; xmm9 - matrix[7..7]*4
; xmm10 - matrix[8..8]*4
; xmm11 - pre computed 0 0
; xmm12 - pre computed 0 1
; xmm13 - pre computed 1 0
; xmm14 - pre computed 1 1
; xmm15 - pre computed 2 0
; xmm1 - pre computed 2 1
filter:
	; load sizes
	push r12
	mov r12d, DWORD [rsi + 8]
	push r13
	mov r13d, DWORD [rsi + 12]
	push r14
	lea r14, [r12 + 2*r12]
	; load matrix multiplications
	movss xmm2, [rdx + 0]
	movss xmm3, [rdx + 4]
	movss xmm4, [rdx + 8]
	movss xmm5, [rdx + 12]
	movss xmm6, [rdx + 16]
	movss xmm7, [rdx + 20]
	movss xmm8, [rdx + 24]
	movss xmm9, [rdx + 28]
	movss xmm10, [rdx + 32]
	UNPACKMULMATRIX
	UNPACKMULMATRIX
	; prepare source & destination
	mov rsi, [rsi]
	mov rdi, [rdi]
	sub rsi, r14
	; main loop BEGIN
	;	H ROW
	PREPREPARE0
	lea rcx, [r12 - 1]
filter_intenal_loop1:
	dec rcx
	PRECOMPUTE
	PREPARE00
	COMPUTE
	test rcx, rcx
	jnz filter_intenal_loop1
	PRECOMPUTE
	PREPAREX1
	COMPUTE
	;	C ROWS
	lea rdx, [r13 - 2]
filter_main_loop:
	dec rdx
	PREPREPARE1
	lea rcx, [r12 - 1]
filter_intenal_loop2:
	dec rcx
	PRECOMPUTE
	PREPARE10
	COMPUTE
	test rcx, rcx
	jnz filter_intenal_loop2
	PRECOMPUTE
	PREPAREX1
	COMPUTE
	test rdx, rdx
	jnz filter_main_loop
	;	L ROW
	lea rcx, [r12 - 1]
	PREPREPARE2
filter_intenal_loop3:
	dec rcx
	PRECOMPUTE
	PREPARE20
	COMPUTE
	test rcx, rcx
	jnz filter_intenal_loop3
	PRECOMPUTE
	PREPAREX1
	COMPUTE
	; main loop END
	pop r14
	pop r13
	pop r12
	ret

