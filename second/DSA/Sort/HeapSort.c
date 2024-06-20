#include <stdio.h>
#include <stdlib.h>

void HeapSort(int *b, int n) {
  int *a = malloc((n + 1) * sizeof(int));
  for (int i = 0; i < n; i++) {
    a[i + 1] = b[i];
  }
  a[0] = n;
  Heapify(a, 1);
  int t = 0;
  for (int i = n; i > 1; i--) {
    t = a[1];
    a[1] = a[i];
    a[i] = t;
    a[0] = a[0] - 1;
    FixHeap(a, 1);
  }
  for (int i = 0; i < n; i++) {
    b[i] = a[i + 1];
  }
}

void Heapify(int a[], int i) {
  int s = 2 * i;
  int d = 2 * i + 1;
  if (a[0] > 1 && i < a[0]) {
    if ((2 * s) <= a[0]) {
      Heapify(a, s);
    }
    if ((2 * d + 1) <= a[0]) {
      Heapify(a, d);
    }
    FixHeap(a, i);
  }
}

void FixHeap(int *a, int i) {
  int s = 2 * i;
  int d = 2 * i + 1;
  int max = 0, t = 0;
  if (s <= a[0] && a[s] > a[i]) {
    max = s;
  } else {
    max = i;
  }
  if (d <= a[0] && a[d] > a[max]) {
    max = d;
  }
  if (max != i) {
    t = a[i];
    a[i] = a[max];
    a[max] = t;
    FixHeap(a, max);
  }
}

int main() {
  // int a[] = {10, 4, 16, 10, 14, 7, 9, 3, 2, 8, 1};
  int a[] = {4, 1, 3, 2, 16, 9, 10, 14, 8, 7};
  int n = sizeof(a) / sizeof(a[0]);
  for (int i = 0; i < n; i++) {
    printf("%d ", a[i]);
  }
  printf("\n");
  HeapSort(a, n);
  for (int i = 0; i < n; i++) {
    printf("%d ", a[i]);
  }
  return 0;
}
