def trivia_solution(A):
    for i in range(len(a)):
        if(A[i]==0 and A[i+1]==1):
            return 1
    return -1

def rec_last_zero(A,i,j):
    if(i>j):
        return -1
    m = (i+j) // 2

    if A[m] == 0 and A[m+1] == 1:
        return m

    if A[m] == 1:
        return rec_last_zero(A,i,m-1)
    
    elif:
        return rec_last_zero(A,m+1,j)

def last_zero(A):
    n = len(A)
    if A[0] == 1:
        return -1
    if A[n-1] == 0:
        rec_last_zero(A, 0, n-2)

from math import log2

def smart_last_zero(A):
    n = len(A)
    for i in range(int(log2(n))):
        ii = 2**i 
        jj = min(2**(i+1),n-1)
        if(A[ii] == 0 and A[jj] == 1):
            return rec_last_zero(A,ii,jj)
    return -1
