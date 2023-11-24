def GameOverCheckerA(n,m,N):
    lun = len(N)
    A = [0] * n
    temp = 1
    for i in N:
        print(f"MOSSA {temp}")
        ini = i[0]-1
        fin = ini + i[1]
        if(ini-fin==0):
            maximus = A[ini]
        else:
            maximus = max(A[ini:fin])
        posini = ini
        posfin = fin
        while(posini<posfin):
            if(A[posini]<maximus):
                A[posini] = maximus
            A[posini] += i[2]
            if(A[posini]>m):
                return temp
            posini += 1
        temp += 1
    return 0


def GameOverCheckerB(n,m,N):
    lun = len(N)
    A = [0] * n
    temp = 1
    for i in N:
        print(f"MOSSA {temp}")
        ini = i[0]-1
        fin = ini + i[1]-1
        A[ini] += i[2] 
        FixHeap(A,0)
        max = A[0]
        
    return 0

def FixHeap(A,i,altezza):
    s=2*i+1
    d=2*i+2
    
    if(s <= altezza and A[s]>A[i]):
        massimo=s
    else:
        massimo=i
    if(d <= altezza and A[d]>A[massimo]):
        massimo=d
        
    if (massimo != i):
        A[i], A[massimo] = A[massimo], A[i]
        FixHeap(A,massimo,altezza)

N = ((1, 3, 12), (6, 3, 3), (2, 5, 2), (8, 1, 5), (4, 2, 3),
(2, 2, 2), (4, 3, 7))
a = GameOverCheckerA(8,16,N)
print(a)
