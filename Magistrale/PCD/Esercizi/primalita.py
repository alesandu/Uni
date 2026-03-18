import random

def trivial_test(n):
    if n == 2 or n == 3:
        return True
    if n%2 == 0 or n < 2:
        return False
    for i in range(3, int(n**0.5)+1, 2):
        if n % i == 0:
            return False
    return True
   
   
def fermat_test(n, t=10):
    if n <= 1:
        return False
    if n <= 3:
        return True
    for _ in range(t):
        a = random.randint(2, n - 2)
        if pow(a, n - 1, n) != 1:
            return False
    return True
     
def get_primes(b):
    flag = False
    while not flag:
        n = random.randint(2**(b-1), 2**b - 1)
        if fermat_test(n):
            flag = True
    return n

print(get_primes(2**11))