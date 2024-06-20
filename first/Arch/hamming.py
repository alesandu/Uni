#programma per calcola l'hamming de simoentta tacci sua
#(ale_sandu)
def calcRedundantBits(m):

	#calcolo dei bit
	for i in range(m):
		if(2**i >= m + i + 1):
			return i

def posRedundantBits(data, r):

	#metti i bit nella posizione giusta
	j = 0
	k = 1
	m = len(data)
	res = ''
 

	#se non è potenza di 2 metti 0
	for i in range(1, m + r+1):
		if(i == 2**j):
			res = res + '0'
			j += 1
		else:
			res = res + data[-1 * k]
			k += 1

	#posizione rigirata
	return res[::-1]


def calcParityBits(arr, r):
	n = len(arr)

	#trova il bit di parità
	for i in range(r):
		val = 0
		for j in range(1, n + 1):

			if(j & (2**i) == (2**i)):
				val = val ^ int(arr[-1 * j])

		#concatena con il bit di parità
		arr = arr[:n-(2**i)] + str(val) + arr[n-(2**i)+1:]
	return arr


def detectError(arr, nr):
	n = len(arr)
	res = 0

	#ricalcola il bit di parità
	for i in range(nr):
		val = 0
		for j in range(1, n + 1):
			if(j & (2**i) == (2**i)):
				val = val ^ int(arr[-1 * j])

		res = res + val*(10**i)

	#converti in decimale
	return int(str(res), 2)


data = input("Inserire seq: ")
m = len(data)
r = calcRedundantBits(m)
print(r)
arr = posRedundantBits(data, r)
print(arr)
arr = calcParityBits(arr, r)
print(arr)
print("L'array con i bit inseriti è: " + arr)
print("i bit delle diverse posizioni sono: ")

i = 1
c = 0
x = len(arr)
while(i < x-1):
    y = x-i
    print("b"+str(c)+": ",arr[y])
    i=2*i
    c+=1

arr = input("Inserire la sequenza di bit mandati dopo: ")
arr = calcParityBits(arr, r)
i = 1
c = 0
x = len(arr)
while(i < x-1):
    y = x-i
    print("b"+str(c)+": ",arr[y])
    i=2*i
    c+=1