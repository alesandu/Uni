#include <stdio.h>
#include <stdlib.h>

int CercaY(int *a, int i, int f) {
  int m = (i + f) / 2;
  if (i > f) {
    return -1;
  }
  if (a[m] == 'E' && a[m - 1] == 'Y') {
    return m;
  }
  if (a[m] == 'Y') {
    CercaY(a, m + 1, f);
  } else {
    CercaY(a, i, m);
  }
}

int CercaS(int *a, int i, int f) {
  int m = (i + f) / 2;
  if (i > f) {
    return -1;
  }
  if (a[m] == 'E' && a[m + 1] == 'S') {
    return m + 1;
  }
  if (a[m] == 'S') {
    CercaS(a, i, m);
  } else {
    CercaS(a, m + 1, f);
  }
}

int main() {
  int a[] = {'Y', 'Y', 'E', 'E', 'E', 'E', 'E', 'S'};
  int n = sizeof(a) / sizeof(a[0]);
  int y = CercaY(a, 0, n);
  int s = n - CercaS(a, 0, n);
  int e = n - y - s;
  printf("%d %d %d ", y, e, s);
  return 0;
}
