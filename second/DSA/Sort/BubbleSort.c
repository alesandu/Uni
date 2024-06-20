#include <stdio.h>

void BubbleSort(int *a, int n){
  int i=0, j=0, t=0;
  for(i=0;i<n;i++){
    for(j=0;j<n-i-1;j++){
      if(a[j]>a[j+1]){
        t = a[j+1];
        a[j+1] = a[j];
        a[j] = t;
      }
    }
  }
}

int main(){
  int a[] = {2,4,7,10,15,6,8,12,9,3};
  int n = sizeof(a)/sizeof(a[0]);
  BubbleSort(a,n);
  for(int i=0;i<n;i++){
    printf("%d ",a[i]);
  }
  return 0;
}
