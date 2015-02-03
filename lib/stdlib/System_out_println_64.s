

	/* MJIS Standard Library (C) 1970-2038 The MiniJavaInScala Creators */
	/* I/O */


	.text
	.p2align 4,,15
	.globl	System_out_println
	.type	System_out_println, @function
	/*
	 * void System_out_println(int32_t esi)
	 */
System_out_println:
	movslq	%edi, %rdi                   # sign extension so we can properly print INT_MIN
	pushq	%rbx
	movq	%rdi, %r9

	shrq	$63, %r9                       # fetch sign bit
	movzbl	%r9b, %eax
	movq	%rax, %r8
	negq	%r8
	xorq	%r8, %rdi                      # weird binary trick for negation
	leaq	(%rdi,%rax), %r8

	movl	buf_size(%rip), %eax
	cmpl	$4083, %eax
	jle	.dont_flush

	movslq	%eax, %rdx
	movq $1,	%rax
	movq $1,	%rdi
	movq	$buf,	%rsi
	syscall
	movl	$0, buf_size(%rip)
.dont_flush:
	leaq	-16(%rsp), %rsi
	movq	$0, -16(%rsp)
	movl	$0, -8(%rsp)
	movb	$10, -16(%rsp)                 # place '\n' to end of buffer
	movabsq	$7378697629483820647, %r10   # modular inverse of 10, for fast div
	leaq	1(%rsi), %rdi
	jmp	.digit_loop
	.p2align 4,,15
	.p2align 3
.digit_loop_repeat:
	movq	%rcx, %rdi                     # we could do this via a cmovne, but we'll jump here anyways
.digit_loop:
	movq	%r8, %rax
	leaq	1(%rdi), %rcx
	imulq	%r10
	movq	%r8, %rax
	sarq	$63, %rax
	sarq	$2, %rdx
	subq	%rax, %rdx
	leaq	(%rdx,%rdx,4), %rax
	addq	%rax, %rax
	subq	%rax, %r8
	addl	$48, %r8d                      # 49 == '0'
	testq	%rdx, %rdx
	movb	%r8b, -1(%rcx)
	movq	%rdx, %r8
	jne	.digit_loop_repeat

	testb	%r9b, %r9b
	je	.n_positive
	leaq	2(%rdi), %rcx
	movb	$45, 1(%rdi)                   # 45 == '-'
.n_positive:

	leaq	-1(%rcx), %rax
	cmpq	%rsi, %rax
	jb	.copy_done
	movl	buf_size(%rip), %r10d
	leaq	-17(%rsp), %r9
	movl	%r10d, %edx

	.p2align 4,,15
	.p2align 3
.copy_loop:
	subq	$1, %rax
	movzbl	1(%rax), %r8d
	movslq	%edx, %rdi
	addl	$1, %edx
	cmpq	%r9, %rax
	movb	%r8b, buf(%rdi)
	jne	.copy_loop
	subl	%esi, %r10d
	leal	(%r10,%rcx), %eax
	movl	%eax, buf_size(%rip)
.copy_done:
	popq	%rbx
	ret

	.size	System_out_println, .-System_out_println
	.text


/*
 * Data
 */
	.comm	buf_size,4,4
	.comm	buf,4096,64
