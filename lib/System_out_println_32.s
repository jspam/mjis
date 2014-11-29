	.p2align 4,,15
	.globl System_out_println
	.type System_out_println, @function
System_out_println:
	pushl 4(%esp)
	pushl $.System_out_println__template
	call printf
	xorl %eax, %eax                   /* return value 0 */
	addl $8, %esp
	ret
	.section	.rodata
.System_out_println__template:
	.string "%d\n"
