

	/* MJIS Standard Library (C) 1970-2038 The MiniJavaInScala Creators */
	/* main wrapper */


	.text
	.p2align 4,,15
	.globl main
	.type main, @function
	/*
	 * int main()
	 *
	 * Wrapper around main to ensure 0 return value.
	 */
main:
	call __main
	movq $60, %rax            # system call numer for sys_exit
	movq $0, %rdi             # exit code
	syscall
