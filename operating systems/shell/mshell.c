#define _POSIX_SOURCE 1

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <string.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>

#include "config.h"
#include "commands.h"
#include "cparse.h"


#define BSIZE 2048
#define CHLDSIZE 2*512


int pid;
char str[BSIZE];
int cnt;
int inbg;
int ctrlc;
int chlds[CHLDSIZE];
int chld_num;
command_s* cmd;
struct sigaction act;


void print_pids(){
	while(chld_num){
		fprintf(stderr, "Process: %d", chlds[--chld_num]);
		fprintf(stderr, " ended with return status: %d\n", chlds[--chld_num]);
		fflush(stderr);
	}
}
void save_pid(int stat_loc, int pid){
	if(chld_num+2>CHLDSIZE){
		fprintf(stderr, "SIGCHLD number overflowed\n");
		print_pids();
	}
	chlds[chld_num++] = stat_loc;
	chlds[chld_num++] = pid;
}
void docommand(void){
	cnt = 0;
	while(dispatch_table[cnt].name!=NULL||dispatch_table[cnt].fun!=NULL){
		if(!strcmp(dispatch_table[cnt].name,cmd->argv[0])){
			(*dispatch_table[cnt].fun)(cmd->argv);
			return;
		}
		cnt++;
	}
	pid = fork();
	if(!pid){
		execvp(cmd->argv[0],cmd->argv);
		printf("Podana komenda nie istnieje lub nie jest zainstalowana\n");
		exit(1);
	}
	else if(!inbg) { while(waitpid(pid, NULL, 0)==-1&&errno==EINTR); }
}
void swapdsr(int a, int b){
	if(a==b) return;
	int c = dup(a);
	dup2(b, a);
	dup2(c, b);
	close(c);
}
void handler(int sig){
	int stat_loc, pid;
	if(sig==SIGINT) ctrlc = 1;
	else if(sig==SIGCHLD) {
		while(1){
			if((pid = (int)waitpid((pid_t)-1, &stat_loc, WNOHANG))!=-1){
				save_pid(stat_loc,pid);
			}
			if(errno==ECHILD)
				return;
		}
	}
}
int main(int, char**);
char prompt[10]=PROMPT;

/*
 * main
 */

int main(argc, argv)
int argc;
char* argv[];
{
	ssize_t n;
	int fd[2];
	int in = 0;
	int out = 1;
	int input;
	int output;
	int pipein = -1;
	fd[0] = fd[1] = -1;
	ctrlc = 0;
	chld_num = 0;

	act.sa_handler = handler;
	act.sa_flags = 0;/*?*/
	sigemptyset(&act.sa_mask);
	sigaction(SIGINT, &act, NULL);
	sigaction(SIGCHLD, &act, NULL);
	
	printf("%s", prompt); fflush(stdout);
	while(n = read(0, str, BSIZE)){
		if(n==-1&&errno!=EINTR) break;
		if(ctrlc||n<=0){
			if(ctrlc){
				printf("\n");
				ctrlc = 0;
			}
			printf("%s", prompt);
			fflush(stdout);
			continue;
		}
		
		str[n-1] = 0;
		
		inbg = in_background(str);
		cmd = split_commands(str);
		
		while(cmd->argv!=NULL&&cmd->argv[0]!=NULL){
			if(ctrlc) { printf("\n"); ctrlc = 0; break; }
			if((cmd+1)->argv!=NULL&&pipe(fd)<0) return 1;
			
			if(cmd->in_file_name!=NULL)
				input = open(cmd->in_file_name, O_RDONLY);
			else if(pipein!=-1)
				input = pipein;
			else input = 0;
			if(cmd->append_mode)
				output = open(cmd->out_file_name, O_WRONLY|O_APPEND|O_CREAT);
			else if(cmd->out_file_name!=NULL)
				output = open(cmd->out_file_name, O_WRONLY|O_TRUNC|O_CREAT);
			else if((cmd+1)->argv!=NULL)
				output = fd[1];
			else output = 1;
			
			swapdsr(input, in);
			swapdsr(output, out);
			docommand();
			swapdsr(input, in);
			swapdsr(output, out);
			
			if(cmd->in_file_name!=NULL) close(input);
			if(cmd->out_file_name!=NULL) close(output);
			
			if(pipein!=-1) close(pipein);
			pipein = fd[0];
			fd[0] = -1;
			if(fd[1]!=-1) { close(fd[1]); fd[1] = -1; }
			++cmd;
		}
		if(pipein!=-1) { close(pipein); pipein = -1; }
		print_pids();
		printf("%s", prompt); fflush(stdout);
	}
	printf("Exit.\n");
	return 0;
}

