#include<cstdio>
int main(){
	long * v = 0, * w;
	w = v;
	++w;
	printf("%ld %ld\n", w-v, (char*)w-(char*)v);
	return 0;
}
