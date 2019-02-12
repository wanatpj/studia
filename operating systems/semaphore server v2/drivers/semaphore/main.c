/* This file contains the device dependent part of the drivers for the
 * following special files:
 *     /dev/semaphore	- semaphore generator
 */

#include "../drivers.h"
#include "../libdriver/driver.h"
#include <sys/ioc_memory.h>
#include "../../kernel/const.h"
#include "../../kernel/config.h"
#include "../../kernel/type.h"

#include "assert.h"
#include "semaphore.h"

#define NR_DEVS            1		/* number of minor devices */
#  define SEMAPHORE_DEV  0			/* minor device for /dev/semaphore */

PRIVATE struct device m_geom[NR_DEVS];  /* base and size of each device */
PRIVATE int m_device;			/* current device */

extern int errno;			/* error number for PM calls */

FORWARD _PROTOTYPE( char *s_name, (void) );
FORWARD _PROTOTYPE( struct device *s_prepare, (int device) );
FORWARD _PROTOTYPE( int s_transfer, (int proc_nr, int opcode, off_t position,
					iovec_t *iov, unsigned nr_req) );
FORWARD _PROTOTYPE( void s_init, (void) );

/* Entry points to this driver. */
PRIVATE struct driver s_dtab = {
  s_name,	/* current device's name */
  do_nop,	/* nothing on open or mount */
  do_nop,	/* nothing on a close */
  do_nop,	/* nothing on specify ram disk geometry */
  s_prepare,	/* prepare for I/O on a given minor device */
  s_transfer,	/* do the I/O */
  nop_cleanup,	/* no need to clean up */
  NULL,	/* device "geometry" */
  nop_signal,	/* system signals */
  nop_alarm, 	/* get semaphore from kernel (alarm) */
  nop_cancel,
  nop_select,
  s_other,
  NULL
};

/* Buffer for the /dev/semaphore number generator. */
#define SEMAPHORE_BUF_SIZE 		16
PRIVATE char semaphore_buf[SEMAPHORE_BUF_SIZE];

/*===========================================================================*
 *				   main 				     *
 *===========================================================================*/
PUBLIC int main(void)
{
  s_init();			/* initialize the memory driver */
  driver_task(&s_dtab);		/* start driver's main loop */
  return(OK);
}

/*===========================================================================*
 *				 s_name					     *
 *===========================================================================*/
PRIVATE char *s_name()
{
  static char name[] = "semaphore";
  return name;  
}

/*===========================================================================*
 *				s_prepare				     *
 *===========================================================================*/
PRIVATE struct device *s_prepare(device)
int device;
{
  if (device < 0 || device >= NR_DEVS) return(NIL_DEV);
  m_device = device;
  return(&m_geom[device]);
}

/*===========================================================================*
 *				s_transfer				     *
 *===========================================================================*/
PRIVATE int s_transfer(proc_nr, opcode, position, iov, nr_req)
int proc_nr;			/* process doing the request */
int opcode;			/* DEV_GATHER or DEV_SCATTER */
off_t position;			/* offset on device to read or write */
iovec_t *iov;			/* pointer to read or write request vector */
unsigned nr_req;		/* length of request vector */
{
/* Read or write one the driver's minor devices. */
  unsigned count;
  vir_bytes user_vir;
  int ret = (OK);

  while (nr_req > 0) {

	/* How much to transfer and where to / from. */
	count = iov->iov_size;
	user_vir = iov->iov_addr;

	switch (m_device) {
	case SEMAPHORE_DEV:
	    count = (left > SEMAPHORE_BUF_SIZE) ? SEMAPHORE_BUF_SIZE : count;
 	    if (opcode == DEV_SCATTER) { // DEV_SCATTER "=" DEV_READ
		    sys_vircopy(proc_nr, D, user_vir, 
    	        SELF, D, (vir_bytes) semaphore_buf, chunk);
    	    count = semaphore_putbytes(semaphore_buf, count);
    	    if(count<0){
    	    	ret = (SUSPEND);
    	    	count = -count;
    	    }
        } else
        	return(EINVAL);
	    break;

	default:
	    return(EINVAL);
	}

	iov->iov_addr += count;
  	if ((iov->iov_size -= count) == 0) { iov++; nr_req--; }

  }
  return ret;
}

/*===========================================================================*
 *				s_init					     *
 *===========================================================================*/
PRIVATE void s_init()
{
  semaphore_server_init();
}

/*===========================================================================*
 *				s_other					     *
 *===========================================================================*/
PRIVATE int s_other()
{
  int r;
  int proc_nr
  r = get_proc_nr_to_send(&proc_nr);
  if(r!=EDONTREPLY){
    message mess;
    mess.m_type = TASK_REPLY;
    mess.REP_PROC_NR = proc_nr;
    mess.REP_STATUS = r;
    send(device_caller, &mess);
    notify(FS);
  }
  return(EDONTREPLY);
}

