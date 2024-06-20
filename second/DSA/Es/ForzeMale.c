#include <stdio.h>
#include <stdlib.h>

void ForzeMale(int *a, int n){
  int pos = 0;
  int *x = calloc((n+1),sizeof(int));
  for(int i=n-1;i>=0;i--){
    if(a[i]==-1){
      x[i]=0;
      pos = i;
    }
    else{
      x[i]=a[i]+x[i+1];
    }
  }
  for(int i=pos;i<n;i++){
    if(a[i]==-1){
      x[i]=0;
      pos = i;
    }
    else if(i==0){
      x[i]=a[i];
    }
    else if((a[i]+x[i-1])<x[i]){
      x[i]=a[i]+x[i-1];
    }
  }
  for(int i=pos;i<n;i++){
    if(a[i]==-1){
      x[i]=0;
    }
    else{
      x[i]=a[i]+x[i-1];
    }
  }
  for(int i=0;i<n;i++){
    a[i]=x[i];
  }
}

int main(){
  int a[] = {1,4,-1,7,-1,-1,6,3,5,2,-1,3,2,9};
  int n = sizeof(a)/sizeof(a[0]);
  ForzeMale(a,n);
  for(int i=0;i<n;i++){
    printf("%d ",a[i]);
  }
  return 0;
}
