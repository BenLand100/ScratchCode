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

primes = primesbelow(1000000)
pset = set(primes)

for i in range(3,1000000,2):
    if i in pset: continue
    satisfied = 0
    for p in primes:
        if p > i: break
        r = i - p
        if r % 2 != 0: continue
        if sqrt(r/2) % 1 == 0: 
            satisfied = 1
            break
    if not satisfied:
        print i
        exit()
