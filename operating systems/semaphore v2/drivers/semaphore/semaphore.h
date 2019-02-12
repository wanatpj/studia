/*
semaphore.h

Public interface to the semaphore generator 
*/

#define UP_BLOCK_SEMAPHORE 0
#define DOWN_BLOCK_SEMAPHORE 1

struct semaphore {
int min, max, curr;
int size; /* -1 if unused semaphore, and #blocked otherwise */
int blocked[NR_PROCS];
char blocked_type[NR_PROCS];
};

_PROTOTYPE( void semaphore_server_init, (void)			);
_PROTOTYPE( int get_sem, (int min, int max, int init)	);
_PROTOTYPE( int up, (int sem_nb, int blocked)			);
_PROTOTYPE( int down, (int sem_nb, int blocked)			);
_PROTOTYPE( int put_sem, (int sem_nb)					);
_PROTOTYPE( int get_proc_nr_to_send, (int * proc_nr)	);

