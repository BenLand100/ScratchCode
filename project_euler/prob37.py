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

def trunkparts(x):
    res = []
    for i in range(1,floor(log10(x))+1):
        res.append(x % 10**i)
    for i in range(0,floor(log10(x))):
        x /= 10
        res.append(x)
    return res

primes = primesbelow(1000000)
pset = set(primes)
truncs = []

for p in primes[4:]:
    if set(trunkparts(p)) <= pset:
        print p
        truncs.append(p)
   
print sum(truncs)


