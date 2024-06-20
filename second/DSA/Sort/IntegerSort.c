#include <stdio.h>

void IntegerSort(int *a, int n){
  int max = a[0];
  int j = 0;
  for(int i=1; i<n; i++){
    if(a[i]>max){
      max = a[i];
    }
  }
  int *y = calloc((max+1),sizeof(int));
  int t=0;
  for(int i=0; i<n;i++){
    y[a[i]] = y[a[i]]+1;
  }
  for(int i=0;i<=max;i++){
    while(y[i]>0){
      a[j] = i;
      y[i]--;
      j++;
    }
  }
  free(y);
}

int main(){
  int a[] = {4, 1, 3, 2, 16, 9, 10, 14, 8, 7};
  int n = sizeof(a)/sizeof(a[0]);
  IntegerSort(a, n);
  for(int i=0;i<n;i++){
    printf("%d ", a[i]);
  }
  return 0;
}
