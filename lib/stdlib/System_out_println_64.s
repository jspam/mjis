

	/* MJIS Standard Library (C) 1970-2038 The MiniJavaInScala Creators */
	/* I/O */


	.text
	.p2align 4,,15
	.globl System_out_println
	.type System_out_println, @function
	/*
	 * void System_out_println(int32_t esi)
	 */
System_out_println:
	subq	$16, %rsp
	movslq	%edi, %rcx        # sign extension so we can properly print INT_MIN
	movq	%rcx, %r8

	shrq	$63, %r8            # if %rcx is negative, negate it
	movq	%rcx, %rax
	negq	%rax
	testb	%r8b, %r8b
	cmovne	%rax, %rcx
	movb	$10, 11(%rsp)       # place '\n' to end of buffer
	leaq	11(%rsp), %rdi
	movabsq	$7378697629483820647, %rsi # modular inverse of 10, for fast div

.digit_loop:
	subq	$1, %rdi
	movq	%rcx, %rax
	imulq	%rsi
	sarq	$2, %rdx
	movq	%rcx, %rax
	sarq	$63, %rax
	subq	%rax, %rdx
	leaq	(%rdx,%rdx,4), %rax
	addq	%rax, %rax
	subq	%rax, %rcx
	addl	$48, %ecx           # 48 == '0'
	movb	%cl, (%rdi)
	movq	%rdx, %rcx
	testq	%rdx, %rdx
	jne	.digit_loop

	movq	%rdi, %rax
	testb	%r8b, %r8b
	je	.n_positive
	subq	$1, %rdi
	movb	$45, -1(%rax)       # 45 == '-'
.n_positive:
	leaq	12(%rsp), %rsi
	subq	%rdi, %rsi          # number of characters to write
	movq	%rsi, %rdx
	movq	%rdi, %rsi          # buffer pointer
	movq	$1, %rax            # system call number for sys_write
	movq	$1, %rdi            # file descriptor for stdout
	syscall
	addq	$16, %rsp
	ret
