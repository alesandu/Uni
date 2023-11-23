def Paulaner(P,B):
    i = 0
    persone = len(P)
    while(i<persone):
        if((P[i]=='M' and B[i]=='B')or(P[i]=='F' and B[i]=='A')):
            i+=1
            continue
        j = i
        if(P[i]=='M'):
            while(B[j]!='B'):
                j+=1
            B[j], B[j-1] = B[j-1], B[j]
        else:
            while(B[j]!='A'):
                j+=1
            B[j], B[j-1] = B[j-1], B[j]
        
    print(P)
    print("\n")
    print(B)





P = ['M','M','M','F','F','F']
B = ['A','A','A','B','B','B']
Paulaner(P,B)
