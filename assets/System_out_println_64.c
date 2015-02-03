#include <inttypes.h>

// maximum length of 32-bit integer string is that of -2147483648
#define PRINTSIZE (11 + 1) // \n
#define BUFSIZE 4096

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

char buf[BUFSIZE];
int buf_size;

void flush() {
  write(buf, (uint64_t)buf_size);
  buf_size = 0;
}

void System_out_println(int32_t n_in) {
  int64_t n = (int64_t) n_in;
  char negative = n < 0;
  if (negative)
    n = -n;

  if (buf_size + PRINTSIZE >= BUFSIZE) {
    // flush();
    write(buf, (uint64_t)buf_size);
    buf_size = 0;
  }
  char printbuf[PRINTSIZE] = { '\n', };
  char *ptr = printbuf+1;
  do {
    *(ptr++) = '0' + n % 10;
    n /= 10;
  } while (n != 0);

  if (negative) {
    *(ptr++) = '-';
  }
  ptr--;
  while (ptr >= printbuf) {
    buf[buf_size++] = *(ptr--);
  }
}
