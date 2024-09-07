#include <stdio.h>
#include <stdlib.h>

void Borsa(int *a, int n, int *k, int *j) {
  int min = 0, max = 0;
  int diff = 0;
  for (int i = 0; i < n; i++) {
    if (a[i] < a[min]) {
      min = i;
    } else if ((a[i] - a[min]) > diff) {
      diff = a[i] - a[min];
      *k = min;
      *j = i;
    }
  }
}

int main() {
  int min, max;
  int a[] = {20, 10, 15, 30, 31, 6, 60, 5, 2, 14, 1};
  int n = sizeof(a) / sizeof(a[0]);
  Borsa(a, n, &min, &max);
  printf("%d %d ", min, max);
  return 0;
}
