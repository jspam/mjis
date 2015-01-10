

	/* MJIS Standard Library (C) 1970-2038 The MiniJavaInScala Creators */


	.text
	.p2align 4,,15
	.globl System_out_println
	.type System_out_println, @function
System_out_println:
	pushq %rbp
	/* Ensure 16-byte stack alignment */
	movq %rsp, %rbp /* rbp is a callee-save register */
	andq $-0x10, %rsp

	xorq %rax, %rax /* number of SSE args */
	movq %rdi, %rsi
	movq $.System_out_println__template, %rdi
	call printf

	movq %rbp, %rsp
	popq %rbp
	ret

	.p2align 4,,15
	.globl main
	.type main, @function
main:
	call __main
	xorq %rax, %rax
	ret

	.section	.rodata
.System_out_println__template:
	.string "%d\n"
