	.p2align 4,,15
	.globl System_out_println
	.type System_out_println, @function
System_out_println:
	xorq %rax, %rax /* number of SSE args */
	movq %rdi, %rsi
	movq $.System_out_println__template, %rdi
	call printf
	xorq %rax, %rax /* return value */
	ret
	.section	.rodata
.System_out_println__template:
	.string "%d\n"
