def GuinneSSort(P,B):
    scambi = 0
    i = 0
    c=0
    persone = len(P)-1
    while(i<persone):
        if((P[i]=='M' and B[i]=='B')or(P[i]=='F' and B[i]=='A')):
            i+=1
        else:
            j = i
            if(P[i]=='M'):
                while(B[j]!='B'):
                    j+=1
                    c+=1
                B[j], B[j-1] = B[j-1], B[j]
                scambi += 1
            else:
                while(B[j]!='A'):
                    j+=1
                    c+=1
                B[j], B[j-1] = B[j-1], B[j]
                scambi += 1
    return (B,scambi,c)

P = ['M','M','M','M','F','F','F']
B = ['A','B','B','B','A','A','B']
B = GuinneSSort(P,B)
print(P)
print(B)
