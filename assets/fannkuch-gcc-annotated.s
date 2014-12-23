	.file	"fannkuch.c"
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"%d\n"
	.text
	.p2align 4,,15
	.globl	fannkuchredux$fannkuch
	.type	fannkuchredux$fannkuch, @function
fannkuchredux$fannkuch:
                                        # this => rdi
                                        # n => esi
.LFB8:
	.cfi_startproc
	pushq	%r15                    # Save callee-save registers
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	movslq	%esi, %r13              # n => r13 (sign extend long to quad)
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	movq	%r13, %rdi              # n => rdi (parameter nmemb of calloc)
	movl	%esi, %r12d             # n => r12d
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	movl	$4, %esi                # 4 => esi (parameter size of calloc)
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	subq	$24, %rsp
	.cfi_def_cfa_offset 80
	call	calloc                  # perm[] = new int[n] => rbx
	movl	$4, %esi                # 4 => esi (parameter size of calloc)
	movq	%r13, %rdi              # n => rsi (parameter nmemb of calloc)
	movq	%rax, %rbx
	call	calloc                  # perm1[] = new int[n] => rbp
	movl	$4, %esi                # 4 => esi (parameter size of calloc)
	movq	%r13, %rdi              # n => rsi (parameter nmemb of calloc)
	movq	%rax, %rbp
	call	calloc                  # count[] = new int[n] => rax

	xorl	%edx, %edx              # int i = 0 => rdx
	testl	%r12d, %r12d            # if n <= 0 jmp .L4 
	jle	.L4
	.p2align 4,,10
	.p2align 3
# while (i<n) perm1[i] = i; i = i + 1
.L31:
	movl	%edx, 0(%rbp,%rdx,4)    # perm1[i] = i
	addq	$1, %rdx                # i = i + 1
	cmpl	%edx, %r12d             # if i < n jmp .L31
	jg	.L31
.L4:
	movq	%rax, %r10              # count[] => r10
	movl	%r12d, %r8d             # int r = n => r8d
	xorl	%r15d, %r15d            # maxFlipsCount = 0 => r13d
	xorl	%r11d, %r11d            # permCount = 0 => r11d
	xorl	%r13d, %r13d            # checksum = 0 => r15d
	subq	%rbp, %r10      
	leaq	4(%rbx), %r14           # &perm[1] => r14
	leaq	4(%rbp), %rsi           # &perm1[1] => rsi
	.p2align 4,,10
	.p2align 3

# while(b) wegoptimiert durch Konstantenfaltung

# while (r != 1) count[r-1] = r; r=r-1;
.L3:
	cmpl	$1, %r8d                # if r == 1 jmp .L26
	je	.L26
	movslq	%r8d, %rdx              # r => rdx 
	leaq	-4(%rax,%rdx,4), %rdx   # ptr = &count[r] - 4 = &count[r-1]
	.p2align 4,,10
	.p2align 3
.L27:
	movl	%r8d, (%rdx)            # *ptr = r
	subl	$1, %r8d                # r = r - 1
	subq	$4, %rdx                # ptr = ptr - 4
	cmpl	$1, %r8d                # if r >= 1 jmp .L27
	jne	.L27

# {int i=0; while(i<n) { perm[i] = perm1[i]; i=i+1; }}
.L26:
	xorl	%edx, %edx              # int i = 0 => edx
	testl	%r12d, %r12d            # if n <= 0 jmp .L10
	jle	.L10
	.p2align 4,,10
	.p2align 3
.L32:
	movl	0(%rbp,%rdx,4), %ecx    # tmp = perm[i]
	movl	%ecx, (%rbx,%rdx,4)     # perm1[i] = tmp
	addq	$1, %rdx                # i = i + 1
	cmpl	%edx, %r12d             # if n > i jmp .L32
	jg	.L32

# while ( !((k=perm[0]) == 0) ) {
.L10:
	movslq	(%rbx), %rcx            # k = perm[0] => rcx
	xorl	%r9d, %r9d              # int flipsCount = 0 => r9d
	testl	%ecx, %ecx              # if k == 0 jmp .L9
	je	.L9
	.p2align 4,,10
	.p2align 3

# loop body 
.L33:
	leal	1(%rcx), %edx           # k+1 => edx
	movl	%edx, %edi     
	shrl	$31, %edi       
	addl	%edx, %edi
	sarl	%edi                    # k2 = (k+1)/2 => edi
	testl	%edi, %edi              # if k2 <= 0 jmp .L14
	jle	.L14

	subl	$1, %edi                # k2 -= 1
	movq	%rbx, %rdx              # perm => rdx
	movq	%rax, 8(%rsp)           # spill rax
	leaq	(%rbx,%rcx,4), %rcx     # ptr2 = &perm[i]
	leaq	(%r14,%rdi,4), %r8 
	.p2align 4,,10
	.p2align 3

# while (i < k2) {
.L15:
	movl	(%rcx), %eax            # tmp = perm[k-i]
	addq	$4, %rdx                # ptr1 += 1
	subq	$4, %rcx                # ptr2 -= 1
	movl	-4(%rdx), %edi          # tmp2 = perm[i]
	movl	%eax, -4(%rdx)          # perm[i] = tmp
	movl	%edi, 4(%rcx)           # perm[k-i] = tmp2
	cmpq	%r8, %rdx               # if i != k2 jmp .L15
	jne	.L15 

	movq	8(%rsp), %rax
	movslq	(%rbx), %rcx            # k = perm[0] => rcx
.L14:
	addl	$1, %r9d                # flipsCount = flipsCount + 1
	testl	%ecx, %ecx              # if k != 0 jmp .L33
	jne	.L33



.L9:
	leal	(%r15,%r9), %edx        # tmp = checksum + flipsCount

	cmpl	%r9d, %r13d             # if (flipsCount > maxFlipsCount)
	cmovl	%r9d, %r13d             # maxFlipsCount = flipsCount (conditional move)

	subl	%r9d, %r15d             # checksum = checksum - flipsCount
	testb	$1, %r11b               # if permCount%2 == 0 (if permCount & 0x00000001 == 0)
	cmove	%edx, %r15d             # checksum = tmp (== checksum + flipsCount) (conditional move)

	cmpl	$1, %r12d               # if n == 1 jmp .L24
	je	.L24
	leaq	8(%rbp), %rdi           # perm1[1] => rdi
	movl	$1, %r8d                # r => r8d (r ist immer 1 wg. while (r != 1) r = r-1 in Z. 25

# while (!brek) {
.L25:
	movl	0(%rbp), %r9d           # int perm0 = perm1[0] => r9d
	movq	%rsi, %rdx              # ptr = &perm1[1] => rdx (ptr zeigt auf perm1[j])
	.p2align 4,,10
	.p2align 3

# while (i < r) {
.L23:
	movl	(%rdx), %ecx            # ptr = &perm1[j] => ecx
	addq	$4, %rdx                # ptr++
	movl	%ecx, -8(%rdx)          # perm1[i] = perm1[j]
	cmpq	%rdi, %rdx              # if i != r jmp .L23
	jne	.L23
# }

	movl	%r9d, -4(%rdi)          # perm1[r] = perm0

	movl	-4(%r10,%rdi), %ecx     # tmp1 = count[r] => ecx
	leal	-1(%rcx), %edx          # tmp2 = tmp1 - 1 => edx
	testl	%edx, %edx          
	movl	%edx, -4(%r10,%rdi)     # count[r] = tmp2
	jle	.L41                    # if tmp2 <= 0 jmp .L41

                                        # (brek = true)
	addl	$1, %r11d               # permCount = permCount + 1 
	jmp	.L3                     # continue [while (b)]
	.p2align 4,,10
	.p2align 3

.L41:
	addl	$1, %r8d                # r++
	addq	$4, %rdi                # ptr++

# if (r == n)
	cmpl	%r8d, %r12d             # if r != n jmp .L25
	jne	.L25

# System.out.println(checksum); return maxFlipsCount;
.L24:
	movl	%r15d, %esi
	movl	$.LC0, %edi
	xorl	%eax, %eax
	call	printf                  # System.out.println(checksum)
	addq	$24, %rsp
	.cfi_def_cfa_offset 56
	movl	%r13d, %eax             # return maxFlipsCount
	popq	%rbx                    # Restore callee-save registers
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE8:
	.size	fannkuchredux$fannkuch, .-fannkuchredux$fannkuch
	.section	.text.startup,"ax",@progbits
	.p2align 4,,15
	.globl	main
	.type	main, @function
main:
.LFB9:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	xorl	%esi, %esi
	movl	$1, %edi
	call	calloc
	movl	$11, %esi
	movq	%rax, %rdi
	call	fannkuchredux$fannkuch
	movl	$.LC0, %edi
	movl	%eax, %esi
	xorl	%eax, %eax
	call	printf
	xorl	%eax, %eax
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE9:
	.size	main, .-main
	.ident	"GCC: (SUSE Linux) 4.8.1 20130909 [gcc-4_8-branch revision 202388]"
	.section	.note.GNU-stack,"",@progbits
