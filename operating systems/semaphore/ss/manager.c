#include "ss.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <minix/dmap.h>

extern int errno;

PRIVATE char command[MAX_PATH_LEN+1];
PRIVATE char arg_buf[MAX_ARGS_LEN+1];

PRIVATE semaphore_list * slist = NULL;

/*===========================================================================*
 *				do_get_sem				     *
 *===========================================================================*/
PUBLIC int do_get_sem(message *m_ptr)
{
}

/*===========================================================================*
 *				do_up				     *
 *===========================================================================*/
PUBLIC int do_up(message *m_ptr)
{
}

/*===========================================================================*
 *				do_down				     *
 *===========================================================================*/
PUBLIC int do_down(message *m_ptr)
{
}

/*===========================================================================*
 *				do_put_sem				     *
 *===========================================================================*/
PUBLIC int do_put_sem(message *m_ptr)
{
}

