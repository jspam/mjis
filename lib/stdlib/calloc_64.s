

	/* MJIS Standard Library (C) 1970-2038 The MiniJavaInScala Creators */
	/* memory management */

	.text
	.p2align 4,,15
	.globl	malloc
	.type	malloc, @function
	/*
	 * void *malloc(size_t cnt)
	 *
	 * 1K chunk allocator
	 */
malloc:
	testq	%rdi, %rdi
	movq	%rdi, %rsi
	je	.zero_bytes_alloc
	movq	cur_brk(%rip), %rdx
	testq	%rdx, %rdx
	je	.init_brk
.brk_initialized:
	movq	rest_bytes(%rip), %r8
	cmpq	%r8, %rsi
	jbe	.return_slab_piece
	movq	%rsi, %rax          # rsi = cnt
	shrq	$10, %rax
	testl	$1023, %esi
	leal	1(%rax), %ecx
	cmovne	%ecx, %eax
	sall	$10, %eax           # eax = new_mem
	movslq	%eax, %rcx
	addq	%rcx, %rdx          # rdx WAS old cur_brk, now cur_brk + new_mem
	movq	%rax, %r10          # syscalls trash rcx
	movq	$12, %rax
	movq	%rdx, %rdi
	syscall
	addq	%r8, %r10           # rcx = new_mem + rest_bytes
	movq	%rdx, cur_brk(%rip)
	movq	%r10, %rax
	subq	%rsi, %rax          # rax = rest_bytes + new_mem - cnt
	movq	%rax, rest_bytes(%rip)
	movq	%rdx, %rax
	subq	%r10, %rax
	ret
	.p2align 4,,10
	.p2align 3
.return_slab_piece:
	movq	%r8, %rax
	subq	%rsi, %rax
	movq	%rax, rest_bytes(%rip)
	movq	%rdx, %rax
	subq	%r8, %rax
	ret
	.p2align 4,,10
	.p2align 3
.init_brk:
	movq	$12, %rax
	movq	%rdx, %rdi
	syscall
	movq	%rax, %rdx
	movq	%rax, cur_brk(%rip)
	jmp	.brk_initialized
	.p2align 4,,10
	.p2align 3
.zero_bytes_alloc:
	xorl	%eax, %eax
	ret
	.size	malloc, .-malloc

	/*
	 * void *calloc(size_t nmemb, size_t size)
	 *
	 * Doesn't handle overflow of 64-bit multiplication.
	 * Resulting pointer is aligned to `size` bytes.
	 */
calloc:
	testq	%rsi, %rsi
	jne	.not_zero_byte_allocation
	movq	last_unique_pointer(%rip), %rax
	subq	$1, %rax
	movq	%rax, last_unique_pointer(%rip)
	ret
	.p2align 4,,10
	.p2align 3
.not_zero_byte_allocation:
	movq	cur_brk(%rip), %rax
	subq	rest_bytes(%rip), %rax  # rax = predicted_pointer
	xorl	%edx, %edx
	imulq	%rsi, %rdi
	pushq	%rbx
	movq	%rsi, %rbx
	divq	%rsi
	subq	%rdx, %rbx              # rbx holds the amount of padding needed
	addq	%rbx, %rdi
	call	malloc
	addq	%rbx, %rax
	popq	%rbx
	ret
	.size	calloc, .-calloc



	/*
	 *  Data
	 */
	.globl	rest_bytes
	.bss
	.align 8
	.type	rest_bytes, @object
	.size	rest_bytes, 8
rest_bytes:
	.zero	8
	.globl	cur_brk
	.type	cur_brk, @object
	.size	cur_brk, 8
cur_brk:
	.zero	8
	.globl	last_unique_pointer
	.type	last_unique_pointer, @object
	.size	last_unique_pointer, 8
last_unique_pointer:
	.zero	8
