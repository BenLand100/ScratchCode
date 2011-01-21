#!/usr/bin/python

def primefactors(x):
    factors = []
    while x % 2 == 0: 
        factors.append(2)
        x /= 2
    factor = 3
    while x > 1:
        while x % factor == 0:
            factors.append(factor)
            x /= factor
        factor += 2
    return factors

def gcf(a,b):
    af = primefactors(a)
    bf = primefactors(b)
    cf = []
    i,j = 0,0
    while i < len(af) and j < len(bf):
        if af[i] == bf[j]:
            cf.append(af[i])
            i += 1
            j += 1
        elif af[i] < bf[i]:
            i += 1
        else:   
            j += 1
    res = 1
    for i in cf:
        res *= i
    return res

n,d = 1,1
for a in range(10,100):
    for b in range(10,100):
        if a%11 != 0 and b % 11 != 0 and b/10 == a%10 and a*(b%10) == (a/10)*b:
            print a,b
            n *= a
            d *= b

f = gcf(n,d)
print n/f,'/',d/f
