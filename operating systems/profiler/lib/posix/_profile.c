#include <lib.h>
#include <unistd.h>

/* m.m9i1: 0 - pstart; 1 - pstop*/
PRIVATE int profile(message* m)
{
  return !!(_syscall(MM, PROFILER, m));
}

PUBLIC int pstart(pid_t pid)
{
  message m;
  m.m9_i1 = 0;
  m.m9_i2 = (int)pid;
  return profile(&m);
}
PUBLIC int pstop(unsigned int *buff){
  message m;
  m.m9_i1 = 1;
  m.m9_p1 = buff;
  return profile(&m);
}
