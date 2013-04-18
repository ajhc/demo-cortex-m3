#include <errno.h>

#undef errno
extern int  errno;

int _getpid () {
  return  1;
}

int _kill (int  pid, int  sig) {
  errno = EINVAL;
  return -1;
}

void _exit (int rc) {
  while (1) { } // Wait for the sweet release of death by watchdog
}
