#include <stdio.h>
#include <stdlib.h>
#include <profiler.h>
int main(int argc, char * argv[])
{
	if(argc<2) { printf("usage: ./pstart pid"); return 1; }
	pstart(atoi(argv[1]));
	return 0;
}

