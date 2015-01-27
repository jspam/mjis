#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>
#include <unistd.h>
#include <libgen.h>

const char service_interp[] __attribute__((section(".interp"))) = "/lib64/ld-linux-x86-64.so.2";

int main(int, char*[]);
void _start(void);

void _start(void) {
  // Read argc and argv off stack, as specified in the SystemV AMD64 ABI
  register void *_rbp asm ("rbp");
  int argc = *(int*) (_rbp+8);
  char* argv[10];
  for (int i = 0; i < argc; i++) {
    argv[i] = *(char**) (_rbp + 16 + (i)*8);
  }
  argv[argc] = 0;
  _exit(main(argc, argv));
}

int main(int argc, char* argv[]) {

  #define MAXPATH 256
  char exe[MAXPATH];
  int length = readlink("/proc/self/exe", exe, MAXPATH);
  exe[length] = 0;  // readlink doesn't NUL-terminate
  char lib_path[MAXPATH + 16] = {
    "LD_LIBRARY_PATH="
  };
  memcpy(lib_path + 16, exe, MAXPATH);
  putenv(dirname(lib_path));

  char *args[argc + 3];
  // initializers don't work for variably-sized arrays
  args[0] = "java";
  args[1] = "-jar";
  args[2] = exe;

  for (int i = 1; i < argc; i++) {
    // copy arguments
    args[i+2] = argv[i];
  }

  execv("/usr/bin/java", args);

  printf("execv failed!\nIs Java installed?\n");
  return 1;
}
