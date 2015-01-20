#include <inttypes.h>

// maximum length of 32-bit integer string is that of -2147483648
#define BUFSIZE (11 + 1) // \n

void write(char *ptr, uint64_t cnt) {
   asm __volatile__("movq $1, %%rax \n" \
       "movq $1, %%rdi \n" \
       "movq %0, %%rsi \n" \
       "movq %1, %%rdx \n" \
       "syscall;"
       :
       :"r"(ptr), "r"(cnt)
       : "%rax","%rdi","%rsi","%rdx","%rcx","%r11");
}


void System_out_println(int32_t n_in) {
  int64_t n = (int64_t) n_in;
  char negative = n < 0;
  if (negative)
    n = -n;

  // fill buffer backwards
  char buf[BUFSIZE] = {
    [BUFSIZE-1] = '\n',
  };
  char *ptr = &(buf[BUFSIZE-1]);
  do {
    ptr--;
    *ptr = '0' + n % 10;
    n /= 10;
  } while (n != 0);

  if (negative) {
    *(--ptr) = '-';
  }

  write(ptr, (uint64_t) (&buf[BUFSIZE] - ptr));

}
