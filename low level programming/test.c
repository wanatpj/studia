#include <stdio.h>
#include <errno.h>
#include <unistd.h>

#include "mlib.h"

void status(){
   printf("sstatus: %d\n",sstatus);
}

int main(){
   long r;
   char buf[100];
/*   status();
   printf("count:%d\n",count());
   status();
   printf("pop:%d\n",pop(NULL));
   status();
   printf("pop:%d\n",pop(buf));
   status();
   printf("top_length:%d\n",top_length());
   status();*/
   buf[0]=1; buf[1]=2; buf[2]=3;
   r = 3;
   printf("push:%d\n",push(r,buf));
   status();/*
   printf("top_length:%d\n",top_length());
   status();
   buf[0]=9; buf[1]=7; buf[2]=6; buf[3]=5;
   r = 4;
   printf("push:%d\n",push(r,buf));
   status();
   printf("top_length:%d\n",top_length());
   status();
   printf("push:%d\n",push(-1,buf));
   status();
   printf("push:%d\n",push(2000000000,buf));
   status();
   printf("count:%d\n",count());
   status();
   printf("top_length:%d\n",top_length());
   status();*/
   printf("pop:%d\n",pop(buf+3));
   status();/*
   int i;
   for(i=0;i<7;i++)
   printf("%d ",buf[i]); printf("\n");
   printf("top_length:%d\n",top_length());
   status();
   printf("count:%d\n",count());
   status();
   printf("pop:%d\n",pop(buf+3));
   status();
   for(i=0;i<7;i++)
   printf("%d ",buf[i]); printf("\n");
   printf("count:%d\n",count());
   status();
   printf("top_length:%d\n",top_length());
   status();
   printf("pop:%d\n",pop(buf+3));
   status();*/
   return 0;
}
