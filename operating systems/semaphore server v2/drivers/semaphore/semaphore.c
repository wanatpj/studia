/*
semaphore.c
Zuzywa NR_PROCS*NR_SEMAPHORES pamieci.

Semaphore generator.
*/

#include "../drivers.h"
#include "../../kernel/const.h"
#include "assert.h"

#include "semaphore.h"

#define NR_SEMAPHORES NR_PROCS

PRIVATE struct semaphore smph[NR_SEMAPHORES];
PRIVATE int last_semaphore_id;

FORWARD _PROTOTYPE( int operable, (int sem_nb)									);
FORWARD _PROTOTYPE( void save_proc_nr, (int smph_nr, int proc_nr, char type)	);
FORWARD _PROTOTYPE( int can_release, (int sem_nb)								);


PUBLIC void semaphore_server_init(void)
{
	int i;
	for(i = 0; i<NR_SEMAPHORES; ++i)
		smph[i].size = -1; /* unused semaphore */
	last_semaphore_id = -1;
	nr_released = 0;
}

PUBLIC int get_sem(min, max, init)
int min;
int max;
int init;
{
	int i;
	if(init<min||max<init) return -1;
	for(i = 0; i<NR_SEMAPHORES; ++i){
		++last_semaphore_id;
		if(last_semaphore_id==NR_SEMAPHORES)
			last_semaphore_id = 0;
		if(smph[last_semaphore_id].size==-1){
			smph[last_semaphore_id].size = 0;
			smph[last_semaphore_id].min = min;
			smph[last_semaphore_id].max = max;
			smph[last_semaphore_id].curr = init;
			return last_semaphore_id + 1;
		}
	}
	return 0;
}

PUBLIC int up(sem_nb, proc_nr, blocked)
int sem_nb;
int proc_nr;
int blocked;
{
	if(!operable(sem_nb)) return -1;
	if(smph[sem_nb].curr==smph[sem_nb].max){
		save_proc_nr(sem_nb, proc_nr, UP_BLOCK_SEMAPHORE);
		return blocked?(EIO):-(EIO);
	}
	++smph[sem_nb].curr;
	if(can_release(sem_nb)) notify_fs();
	return 0;
}

PUBLIC int down(sem_nb, proc_nr, blocked)
int sem_nb;
int proc_nr;
int blocked;
{
	if(!operable(sem_nb)) return -1;
	if(smph[sem_nb].curr==smph[sem_nb].min){
		save_proc_nr(sem_nb, proc_nr, DOWN_BLOCK_SEMAPHORE);
		return blocked?(EIO):-(EIO);
	}
	--smph[sem_nb].curr;
	if(can_release(sem_nb)) notify_fs();
	return 0;
}

PUBLIC int put_sem(sem_nb)
int sem_nb;
{
	if(smph[sem_nb].size)
		return -(EIO);
	int i;
	if(!operable(sem_nb)) return -1;
	for(i = 0; i<smph[sem_nb].size; ++i){
		save_proc_nr(sem_nb, proc_nr);
		send_neg_msg(smph[sem_nb].blocked[i]);
	}
	smph[sem_nb].size = -1;
	return 0;
}

PRIVATE int operable(sem_nb)
int sem_nb;
{
	return 0<sem_nb&&sem_nb<=NR_SEMAPHORES&&smph[sem_nb-1].size!=-1;
}


PRIVATE void save_proc_nr(smph_nr, proc_nr, type)
int smph_nr;
int proc_nr;
char type;
int sem_nb;
{
  smph[sem_nb].blocked_type[smph[sem_nb].size] = type;
  smph[sem_nb].blocked[smph[sem_nb].size++] = proc_nr;
}

PRIVATE int can_release(sem_nb)
int sem_nb;
{
  int i;
  if(smph[sem_nb].curr<smph[sem_nb].max)
    for(i=0; i<smph[sem_nb].size; ++i)
      if(smph[sem_nb].blocked_type[i]==UP_BLOCK_SEMAPHORE)
        return 1;
  if(smph[sem_nb].curr>smph[sem_nb].min)
    for(i=0; i<smph[sem_nb].size; ++i)
      if(smph[sem_nb].blocked_type[i]==DOWN_BLOCK_SEMAPHORE)
        return 1;
  return 0;
}

PRIVATE int get_proc_nr_to_send(proc_nr)
int * proc_nr;
{
  int sem_nb,j;
  for(sem_nb=0;sem_nb<NR_SEMAPHORES;++sem_nb){
    if(smph[sem_nb].curr<smph[sem_nb].max)
      for(i=0; i<smph[sem_nb].size; ++i)
        if(smph[sem_nb].blocked_type[i]==UP_BLOCK_SEMAPHORE){
          proc_nr = smph[sem_nb].blocked[i];
          return(OK);
        }
    if(smph[sem_nb].curr>smph[sem_nb].min)
      for(i=0; i<smph[sem_nb].size; ++i)
        if(smph[sem_nb].blocked_type[i]==DOWN_BLOCK_SEMAPHORE){
          proc_nr = smph[sem_nb].blocked[i];
          return(OK);
        }
  }
  return(EDONTREPLY);
}

