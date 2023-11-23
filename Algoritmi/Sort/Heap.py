def FixHeap(A,i,altezza):
    s=2*i+1
    d=2*i+2
    
    if(s < altezza and A[s]>A[i]):
        massimo=s
    else:
        massimo=i
    if(d < altezza and A[d]>A[massimo]):
        massimo=d
        
    if (massimo !=i):
        A[i],A[massimo]=A[massimo],A[i]
        FixHeap(A,massimo,altezza)

def Heapyfy(h):
    n=len(h)
    parent=n//2+1
    j=parent
    while(j>=0):
        FixHeap(j,h)
        j-=1

def recHeapify(h,parent):
    n=len(h)Step
    leftchild=parent*2+1
    rightchild=parent*2+2
    if(leftchild < n and rightchild < n):
        recHeapify(h,leftchild)
        recHeapify(h,rightchild)
        FixHeap(h,parent,len(h))

def heapSort(h):
    recHeapify(h,0)
    heapsize=len(h)
    i=heapsize-1
    while(i>0):
        h[0],h[i]=h[i],h[0]
        FixHeap(h,0,i)
        i-=1

h=[1,4,5,6,8,30,5,4,3,2,4,5,7,8,22]
recHeapify(h,0)
print(h)
heapSort(h)
print(h)
