#include "mlib.h"
#include<stdio.h>

void print(){
	printf("sstatus: %2d ", sstatus);
	printf("count: %2d ", count());
	printf("sstatus: %2d ", sstatus);
	printf("top_length:%2d ", top_length());
	printf("sstatus: %2d\n", sstatus);
}

int main(){
	char s[30];
	int st, i;
	print();
	printf("push(-1, \"JAMNIK\"):%d\n",push(-1,(char*)"JAMNIK"));
	print();
	printf("push(7, \"JAMNIK\"):%d\n",push(7,(char*)"JAMNIK"));
	print();
	printf("push(11, \"JAMNIK LOL\"):%d\n",push(11,(char*)"JAMNIK LOL"));
	print();
	st = pop(s);
	printf("pop(%s):%d\n",s,st);
	print();
	s[20] = s[22] = 'L';
	s[21] = 'O';
	/*for(i=0;i<10;i++){
		printf("push(3, \"%s\"):%d\n",s+20,push(3,s+20));
		print();
		s[21]++;
	}
	for(i=0;i<10;i++){
		st = pop(s);
		printf("pop(%s):%d\n",s,st);
		print();
	}*/
	st = pop(s);
	printf("pop(%s):%d\n",s,st);
	print();
	st = pop(s);
	printf("pop(%s):%d\n",s,st);
	print();
	printf("push(700000000000L, \"JAMNIK\"):%d\n",push(700000000000L,(char*)"JAMNIK"));
	print();
	return 0;
}
