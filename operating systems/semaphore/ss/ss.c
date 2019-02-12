#include "ss.h"

/* Allocate space for the global variables. */
message m_in;		/* the input message itself */
message m_out;		/* the output message used for reply */
int who;		/* caller's proc number */
int callnr;		/* system call number */

/* Declare some local functions. */
FORWARD _PROTOTYPE(void init_server, (void)				);
FORWARD _PROTOTYPE(void get_work, (void)				);
FORWARD _PROTOTYPE(void reply, (int whom, int result)			);

/*===========================================================================*
 *				main                                         *
 *===========================================================================*/
PUBLIC int main(void)
{
  int result;                 
  sigset_t sigset;

  while (TRUE) {              

      /* Wait for incoming message, sets 'callnr' and 'who'. */
      get_work();

      switch (callnr) {
      case SEMAPHORE:
      		switch(m_in.SS_CMD){ // TODO: numbers labels
      			case 0: // get_sem
      				result = do_get_sem(&m_in);
      				break;
      			case 1: // up
      				result = do_up(&m_in);
      				break;
      			case 2: // down
      				result = do_down(&m_in);
      				break;
      			case 3: // put_sem
      				result = do_put_sem(&m_in);
      				break;
      		}
          break;
      default: 
          printf("Warning, SS got unexpected request %d from %d\n",
            	m_in.m_type, m_in.m_source);
          result = EINVAL;
      }

      /* Finally send reply message. */
      if (result != EDONTREPLY) {
          reply(who, result);
      }
  }
}


/*===========================================================================*
 *				get_work                                     *
 *===========================================================================*/
PRIVATE void get_work()
{
    int status = 0;
    status = receive(ANY, &m_in);   /* this blocks until message arrives */
    if (OK != status)
        panic("SS","failed to receive message!", status);
    who = m_in.m_source;        /* message arrived! set sender */
    callnr = m_in.m_type;       /* set function call number */
}


/*===========================================================================*
 *				reply					     *
 *===========================================================================*/
PRIVATE void reply(who, result)
int who;                           	/* destination */
int result;                           	/* report result to replyee */
{
    int send_status;
    m_out.m_type = result;  		/* build reply message */
    send_status = send(who, &m_out);    /* send the message */
    if (OK != send_status)
        panic("SS", "unable to send reply!", send_status);
}

