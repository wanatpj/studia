#ifndef __THREADS_H__
#define __THREADS_H__

typedef void (*thread_t)(unsigned long, unsigned long);

int th_create(thread_t, unsigned long, unsigned long);
void th_yield();
void th_exit();
void run();
int th_getid();

#endif
