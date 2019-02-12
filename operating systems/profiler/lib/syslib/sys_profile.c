#include "syslib.h"

PRIVATE int sys_profile(m)
message *m;
{
  return (_taskcall(SYSTASK, SYS_PROFILE, m));
}
PUBLIC int sys_profile_start(proc_nr)
int proc_nr;
{
  message m;
  m.PROC_TO_TRACK = proc_nr;
  m.TURN_OFF_PROFILING = 0;
  return sys_profile(&m);
}

PUBLIC int sys_profile_stop(buff, proc_nr)
unsigned int *buff;			/* table of counters */
int proc_nr;
{
  message m;
  m.PROFILING_OUTCOME = buff;
  m.STOP_PROC = proc_nr;
  m.TURN_OFF_PROFILING = 1;
  return sys_profile(&m);
}
