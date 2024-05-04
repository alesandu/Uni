#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

int main(int argv, char **argc) {
  int pid = 0;

  int fd[2];
  pipe(fd);

  pid = fork();

  srand(time(NULL));

  if (pid == 0) {
    int a;
    read(fd[0], &a, sizeof(int));
    close(fd[0]);
    printf("f -> %d\n", a);
    a = a * a;
    write(fd[1], &a, sizeof(int));
    close(fd[1]);

  } else if (pid > 0) {
    int a = 0;
    int n = rand() % 101;
    write(fd[1], &n, sizeof(int));
    close(fd[1]);
    waitpid(pid, NULL, 0);
    read(fd[0], &a, sizeof(int));
    close(fd[0]);
    printf("p -> %d", a);
  }

  return 0;
}
