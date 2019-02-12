#include <lib.h>
#include <unistd.h>

char out[16];

PRIVATE void prepare_int(int n, char * buf){
	unsigned int tmp = (unsigned int) n;
	buf[0] = (char)(tmp%256); tmp /= 256;
	buf[1] = (char)(tmp%256); tmp /= 256;
	buf[2] = (char)(tmp%256); tmp /= 256;
	buf[3] = (char)(tmp%256); tmp /= 256;
}

PRIVATE int send_short(int i, int sem_nb){
  int ret;
  int fd = open("/dev/semaphore", O_WRONLY);// | I_CHAR_SPECIAL);
  prepare_int(i,		out 	);
  prepare_int(sem_nb,	out + 4	);
  ret = write(fd, out, 8);
  close(fd);
  return ret;
}

PRIVATE int get_sem(int min, int max, int init)
{
  int ret;
  int fd = open("/dev/semaphore", O_WRONLY);// | I_CHAR_SPECIAL); // comment: suspend method doesn't works for devs
  prepare_int(0   ,	out 	);
  prepare_int(min ,	out + 4 );
  prepare_int(max ,	out + 8 );
  prepare_int(init,	out + 12);
  ret = write(fd, out, 16);
  fclose(fd);
  if(ret==0) ret = -1;
  return ret;
}
PRIVATE int up(int sem_nb)
{
  return send_short(1, sem_nb);
}
PRIVATE int nb_up(int sem_nb)
{
  return send_short(2, sem_nb);
}
PRIVATE int down(int sem_nb)
{
  return send_short(3, sem_nb);
}
PRIVATE int nb_down(int sem_nb)
{
  return send_short(4, sem_nb);
}
PRIVATE int put_sem(int sem_nb)
{
  return send_short(5, sem_nb);
}

