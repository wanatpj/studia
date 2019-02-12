#include <stdio.h>
extern void sort(unsigned long * arr, unsigned long len);
int main(int argc, char * argv[])
{
	long i, n = 10;
	unsigned long arr[] = { 1 , 2 , 7 , 4 , 5 , 2 , 4 , 8 , 1 , 3 };
	sort(arr , n) ;
	for( i =0; i<n ; ++i )
		printf("%ld ", arr[i]);
	printf("\n") ;
}
