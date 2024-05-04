#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <time.h>

#define STDIN 0
#define STDOUT 1
#define PIPE_RD 0
#define PIPE_WR 1

int main(int argc, char** argv){

	srand(time(NULL));

	int pid_1, pid_2;

	int fd1[2];
	int fd2[2];
	int sum=0;

	pipe(fd1);
	pipe(fd2);

	pid_1 = fork();

	if (pid_1 == 0){
		int a = 0;
		close(fd1[PIPE_RD]);
		close(STDOUT);
		while(1){
			a = (rand()%101);
			if (a%2==1){
				write(fd1[PIPE_WR], &a, sizeof(int));
			}
		}
	}
	else{
		pid_2 = fork();
	  if(pid_2 == 0){
		  int b = 0;
		  close(fd2[PIPE_RD]);
		  close(STDOUT);
		  while(1){
			  b = (rand()%101);
			  if(b%2==0){
				  write(fd2[PIPE_WR], &b, sizeof(int));
			  }
		  }
	  }
	  else{
		  int c = 0, d = 0;
		  close(fd1[PIPE_WR]);
		  close(fd2[PIPE_WR]);
		  close(STDIN);
		  while(sum<=190){
			  sum = 0;
			  read(fd1[PIPE_RD], &c, sizeof(int));
			  read(fd2[PIPE_RD], &d, sizeof(int));
			  sum = c+d;
			  printf("la somma di %d e %d e: %d\n", c, d, sum);
		  }
      kill(pid_1,SIGKILL);
      kill(pid_2,SIGKILL);
    }
	}

	close(fd1[PIPE_RD]);
	close(fd2[PIPE_RD]);
	close(fd1[PIPE_WR]);
	close(fd2[PIPE_WR]);

	waitpid(pid_1, NULL, 0);
	waitpid(pid_2, NULL, 0);

	return 0;
}
