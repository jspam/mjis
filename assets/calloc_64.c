#include <inttypes.h>
#include <stddef.h>

#define SLABSIZE 1024
// We've had enough memory alignment bugs already.
// Just align everything to 16 bytes.
#define ALIGN 16

void *brk(char *ptr) {
  void *retval;
  asm __volatile__(
      "movq $12, %%rax \n" \
      "movq %0, %%rdi \n" \
      "syscall \n" \
      "movq %%rax, %1"
      : "=r"(retval)
      : "r"(ptr)
      : "%rax","%rdi","%rcx","%r11");
  return retval;
}

uint64_t rest_bytes = 0;
void *cur_brk = NULL;
void *last_unique_pointer = NULL;

void *malloc(uint64_t cnt) {
  if (cnt == 0)
    return NULL;
  if (cur_brk == NULL)
    // initialize brk
    cur_brk = brk(NULL);

  if (cnt <= rest_bytes) {
    void *retval = cur_brk - rest_bytes;
    rest_bytes -= cnt;
    return retval;
  }

  int new_slabs = cnt / SLABSIZE;
  if (cnt % SLABSIZE)
    new_slabs++;
  int new_mem = new_slabs * SLABSIZE;
  brk(cur_brk + new_mem);
  cur_brk += new_mem;
  rest_bytes += new_mem;
  void *retval = cur_brk - rest_bytes;
  rest_bytes -= cnt;
  return retval;
}

void *calloc(uint64_t nmemb, uint64_t size) {
  // We don't *actually* need to allocate any memory for 0 byte objects.
  // What we instead do is hand out unique pointers that don't point to usable memory
  // anyways.
  if (size == 0) {
    last_unique_pointer--;
    return last_unique_pointer;
  }

  uint64_t nbytes = nmemb * size;
  uint64_t predicted_pointer = ((uint64_t) cur_brk - rest_bytes);
  int padding = 0;

  if (predicted_pointer % ALIGN != 0)
    padding = ALIGN - (predicted_pointer % ALIGN);

  void *pointer = malloc(nbytes + padding);
  pointer = (void *) ((uint64_t) pointer + padding);
  return (void *) pointer;
}
