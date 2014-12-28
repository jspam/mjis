

	/* MJIS Standard Library (C) 1970-2038 The MiniJavaInScala Creators */


	.text
	.p2align 4,,15
	.globl System_out_println
	.type System_out_println, @function
System_out_println:
	pushl 4(%esp)
	pushl $.System_out_println__template
	call printf
	addl $8, %esp
	ret

	.p2align 4,,15
	.globl main
	.type main, @function
main:
	call __main
	xorl %eax, %eax
	ret

	.section	.rodata
.System_out_println__template:
	.string "%d\n"
