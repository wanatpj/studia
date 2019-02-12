#ifndef _PROFILER_H
#define _PROFILER_H

#include <sys/types.h>

_PROTOTYPE( int pstart, (pid_t pid)			);
_PROTOTYPE( int pstop, (unsigned int *buff)			);
#endif
