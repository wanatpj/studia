#include <stdio.h>
#include <profiler.h>
int buff[100];
int main()
{
	int i;
	pstop(buff);
	for(i=0;i<100;++i)
		printf("%d ", buff[i]);
	printf("\n");
	return 0;
}

