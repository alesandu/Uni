#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

int main(int argv, char **argc) {
  srand(time(NULL));
  int buffer[100] = 0;
  int fd = open("file.txt");
  for (int i = 0; i < 100; i++) {
    int c = rand() % 101;
  }
  close(fd);
  return 0;
}
