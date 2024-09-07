def taglia(A):
    n = len(A)
    U = [0] * 0
    Z = [0] * 0

    if A[0]== 0:
        Z[0] = 1
    for i in range(1,n):
        if A[i] == 0:
            Z[i] = Z[i-1] + 1
        else:
            Z[i] = Z[i-1]
    
    for i in range(n-1,-1,-1):
        U[i] = U[i+1] + A[i+1]

    for i in range(n):
        if Z[i] == U[i]:
            return i
    return 0

def smart_taglia(A):
    return sum(A)-1
