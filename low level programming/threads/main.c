#include <stdio.h>
#include "threads.h"

void thread1(unsigned long a, unsigned long b)
{
	printf("Thread #%d: Started with arguments: %lu, %lu.\n", th_getid(), a, b);
	int i, id;
	if (th_getid() < 5)
	{
		id = th_create(thread1, a+b, b+1);
		if (id < 0)
			printf("Thread #%d: Creating new thread failed.\n", th_getid());
		else
			printf("Thread #%d: New thread created, thread id = %d.\n", th_getid(), id);
	}
	
	for (i = 0; i < 10; ++i)
	{
		printf("Thread #%d: Dummy loop, index = %d.\n", th_getid(), i);
		th_yield();
	}

	fprintf(stdout, "Thread #%d: Exiting.\n", th_getid());
	th_exit();
}

int main()
{
	th_create(thread1, 0, 0);
	run();
	return 0;
}
