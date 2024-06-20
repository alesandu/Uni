#include <stdio.h>

void InsertionSort(int *a, int n){
  int i=0, j=0, t=0, x=0;
  for(i=0;i<n-1;i++){
    x = a[i+1];
    for(j=i;j>0;j--){
      if(x<a[j]){
        t = a[j+1];
        a[j+1]=a[j];
        a[j]=t;
      }
    }
  }
}

int main(){
  int a[] = {2,4,7,10,15,6,8,12,9,3};
  int n = sizeof(a)/sizeof(a[0]);
  InsertionSort(a,n);
  for(int i=0;i<n;i++){
    printf("%d ",a[i]);
  }
  return 0;
}
