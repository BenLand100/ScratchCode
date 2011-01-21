#!/usr/bin/python

from numpy import *

def primesbelow(x):
    nums = range(x)
    mask = [i%2 for i in nums]
    mask[1] = 0
    mask[2] = 1
    i = 3
    while i < x:
        if mask[i]:
            for j in range(i*i,x,i):
                mask[j] = 0
        i+=2
    return filter(lambda x: mask[x],nums)

def rotations(x):
    p = int(floor(log10(x)))
    e = 10**p
    res = [x for i in range(p+1)]
    for i in range(p):
        n = x % 10
        x = x/10 + n*e
        res[i+1]=x
    return set(res)

primes = set(primesbelow(1000000))
circular = set(primes)

for p in primes:
    rots = rotations(p)
    if not rots <= circular:
        circular.remove(p)  

print len(circular)
