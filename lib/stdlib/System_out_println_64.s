	.text
	.p2align 4,,15
	.globl System_out_println
	.type System_out_println, @function
System_out_println:
	/* Ensure 16-byte stack alignment */
	movq %rsp, %rax
	pushq %rax
	pushq %rax
	andq $-0x10, %rsp

	xorq %rax, %rax /* number of SSE args */
	movq %rdi, %rsi
	movq $.System_out_println__template, %rdi

	call printf
	movq 8(%rsp), %rsp
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
