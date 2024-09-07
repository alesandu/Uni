#include <stdio.h>

void QuickSort(int *a, int i, int f){
  int m = 0;
  if(i<f){
    m = Partition(a,i,f);
    QuickSort(a,i,m-1);
    QuickSort(a,m+1,f);
  }
}

int Partition(int *a, int i,int f){
  int x = a[i]; 
  int inf = i;
  int sup = f+1;
  int t=0;
  while(1){
    do{
      inf++;
    }while(inf < f && a[inf] <= x);
    do{
      sup--;
    }while(sup > i && a[sup] >= x);
    if(inf < sup){
      t = a[inf];
      a[inf] = a[sup];
      a[sup] = t;
    }
    else{
      break;
    }
  }
  t = a[i];
  a[i]=a[sup];
  a[sup] = t;
  return sup;
}

int main(){
  int a[] = {4,2,7,15,10,8,6,12,9,3};
  int n = sizeof(a)/sizeof(a[0]);
  QuickSort(a,0,n-1);
  for(int i=0;i<n;i++){
    printf("%d ",a[i]);
  }
  return 0;
}
