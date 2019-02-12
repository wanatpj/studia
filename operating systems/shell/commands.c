#define _POSIX_SOURCE 1

#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#include "commands.h"

extern char **environ;

int echo(char*[]);
int chgdir(char*[]);
int eexit(char*[]);
int send_sig(char*[]);
int environn(char*[]);


command_pair dispatch_table[]={
	{"exit", &eexit},
	{"cd", &chgdir},
	{"echo", &echo},
	{"kill", &send_sig},
	{"lenv", &environn},
	{NULL,NULL}
};

int eexit(argv)
char * argv[];
{
	exit(0);
	return 0; // never reached
}
int chgdir(argv)
char * argv[];
{
	if(argv[1]==NULL){
		fprintf(stderr, "not implemented with no arguments; add argument to move to another directory\n");
		return 1;
	}
	chdir(argv[1]);
	return 0;
}
int echo(argv)
char * argv[];
{
	int i =1;
	if(argv[i]) printf("%s", argv[i++]);
	while(argv[i])
		printf(" %s", argv[i++]);

	printf("\n");
	//fflush(stdout);
	return 0;
}
int send_sig(argv)
char * argv[];
{
	if(argv[1]==NULL||argv[2]==NULL){
		fprintf(stderr, "too few arguments\n");
		return 997;
	}
	return kill(atoi(argv[2]), atoi(argv[1]));
}
int environn(argv)
char * argv[];
{
	char **ptr = environ;
	while(*ptr){
		printf("%s\n", *ptr);
		ptr++;
	}
}

