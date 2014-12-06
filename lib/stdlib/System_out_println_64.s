	.text
	.p2align 4,,15
	.globl System_out_println
	.type System_out_println, @function
System_out_println:
	xorq %rax, %rax /* number of SSE args */
	movq %rdi, %rsi
	movq $.System_out_println__template, %rdi
	call printf
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
