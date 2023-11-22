#include <stdio.h>

void MergeSort(int *a, int i, int f) {
  if (i < f) {
    int m = (i + f) / 2;
    MergeSort(a, i, m);
    MergeSort(a, m + 1, f);
    Merge(a, i, m, f);
  }
}

void Merge(int *a, int in, int m, int fi) {
  int x[fi - in];
  int i = 0;
  int k1 = in, k2 = m + 1;
  while (k1 <= m && k2 <= fi) {
    if (a[k1] <= a[k2]) {
      x[i] = a[k1];
      i++;
      k1++;
    } else {
      x[i] = a[k2];
      i++;
      k2++;
    }
  }
  if (k1 <= m) {
    for (int l = k1; l <= m; l++) {
      x[i] = a[l];
      i++;
    }
  } else if (k2 <= fi) {
    for (int l = k2; l <= fi; l++) {
      x[i] = a[l];
      i++;
    }
  }
  int b = 0;
  for (int l = in; l <= fi; l++) {
    a[l] = x[b];
    b++;
  }
  for (int i = 0; i <= fi - in; i++) {
    printf("%d ", x[i]);
  }
  printf("\n");
}

int main() {
  int a[] = {4, 2, 7, 15, 10, 8, 6, 12, 9, 3};
  int n = sizeof(a) / sizeof(a[0]);
  MergeSort(a, 0, n - 1);
  for (int i = 0; i < n; i++) {
    printf("%d ", a[i]);
  }
  return 0;
}
