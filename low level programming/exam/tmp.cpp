#include<cstdio>
struct C {
	public:
	C(){}
	C(int k) {}
};
int a = 3;
int b;
int fu(int n){
	static int c;
	static int d = 5;
	static C e ;
	static C f = C(5) ;
	static int t[] = {0,0,1};
	printf("%p\n%p\n%p\n%p\n%p\n%p\n%p\n", &a, &b, &c, &d, &e, &f, t);
}
int hu(){
	int t[] = {2,3,4,5,6,7}; 
	++t[3];
	printf("%d %d %d %d %d %d\n", t[0], t[1], t[2], t[3], t[4], t[5]);
}
int main()
{
	fu(4);
	hu();
	hu();
	return 0;
}
