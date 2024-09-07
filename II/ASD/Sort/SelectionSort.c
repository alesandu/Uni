#include <stdio.h>

void SelectionSort(int *a,int n){
  int t=0;
  for(int i = 0;i<n-1;i++){
    int m=i;
    for(int j=i+1;j<n;j++){
      if(a[j]<a[m]){
        m=j;
      }
    }
    t = a[i];
    a[i]=a[m];
    a[m]=t;
  }
}

int main(){
  int a[] = {2,4,7,10,15,6,8,12,9,3};
  int n = sizeof(a)/sizeof(a[0]);
  SelectionSort(a,n);
  for(int i=0;i<n;i++){
    printf("%d ",a[i]);
  }

  return 0;
}
