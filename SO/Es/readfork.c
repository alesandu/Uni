#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

int main(int argc, char **argv) {

  int fd = open("file.txt", O_RDONLY);
  int pid;
  pid = fork();

  if (pid == 0) {
    lseek(fd, 0, SEEK_SET);
    char buffer[11];
    int a = getpid();
    read(fd, buffer, 10);
    buffer[10] = '\0';
    printf("[%d] -> %s\n", a, buffer);
  }

  else if (pid > 0) {
    lseek(fd, 128, SEEK_SET);
    char buffer[11];
    int a = getpid();
    lseek(fd, -11, SEEK_END);
    read(fd, buffer, 10);
    buffer[10] = '\0';
    printf("[%d] -> %s\n", a, buffer);
  }
  waitpid(pid, NULL, 0);
  return 0;
}
