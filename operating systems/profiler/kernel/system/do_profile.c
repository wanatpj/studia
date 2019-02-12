/* The kernel call implemented in this file:
 *   m_type:	SYS_PROFILE
 *
 * The parameters for this kernel call are:
 *    m9_i1:	TURN_OFF_PROFILING
 *    m9_i2:	PROC_TO_TRACK
 *    m9_p1:	PROFILING_OUTCOME
 */

#include "../system.h"
#include <signal.h>
#include <minix/type.h>
#if (CHIP == INTEL)
#include "../protect.h"
#endif

#if USE_PROFILE

/*===========================================================================*
 *				do_profile					     *
 *===========================================================================*/
PUBLIC int do_profile(m_ptr)
register message *m_ptr;
{
  int i;
  struct vir_addr ker, usr;
  if(m_ptr->TURN_OFF_PROFILING==1){
    if(tracked_proc_nr == -1)
      return (EPERM);
    /*sys_vircopy(SELF, D, (vir_bytes)counter_table , m_ptr->STOP_PROC, D, (vir_bytes)m_ptr->PROFILING_OUTCOME, PROFILING_PARTS);*/
    /*   int proc_nr;
         int segment;
         vir_bytes offset;
         #   define GET_PROCTAB	   2	
         #define KERNEL		0	
    */
    ker.proc_nr = GET_PROCTAB;
    ker.segment = D;
    ker.offset = (vir_bytes)counter_table;
    usr.proc_nr = m_ptr->STOP_PROC;
    usr.segment = D;
    usr.offset = (vir_bytes)m_ptr->PROFILING_OUTCOME;
    virtual_copy(&ker, &usr, (vir_bytes)PROFILING_PARTS);
    tracked_proc_nr = -1;
  } else {
    if(tracked_proc_nr != -1)
      return (EPERM);
    tracked_proc_nr = m_ptr->PROC_TO_TRACK;
    for(i=0; i<PROFILING_PARTS; ++i)
      counter_table[i] = 0;
  }
  return(OK);
}

#endif /* USE_PROFILE */

