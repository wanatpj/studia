#ifndef _SEMAPHORE_H
#define _SEMAPHORE_H

#include <sys/types.h>

_PROTOTYPE( int get_sem, (int min, int max, int init)	);
_PROTOTYPE( int up, (int sem_nb)						);
_PROTOTYPE( int nb_up, (int sem_nb)						);
_PROTOTYPE( int down, (int sem_nb)						);
_PROTOTYPE( int nb_down, (int sem_nb)					);
_PROTOTYPE( int put_sem, (int sem_nb)					);
#endif
