#include<cstdio>
int main()
{
	volatile int t[100];
	for(int i=0;i<1000000000;++i) t[0] += t[1];
	printf("%d\n", t[0]);
	return 0;
}

